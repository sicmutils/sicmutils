;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.polynomial.factor
  (:require [clojure.walk :as w]
            #?(:cljs [goog.string :refer [format]])
            [sicmutils.value :as v]
            [sicmutils.expression.analyze :as a]
            [sicmutils.generic :as g]
            [sicmutils.expression :as x]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial :as poly]
            [sicmutils.polynomial.gcd :refer [gcd gcd-seq]]
            [taoensso.timbre :as log]))

(defn split
  "We're not entirely certain what this algorithm does, but it would be nice
  to know."
  [p]
  (let [answer (fn [tracker const]
                 (into [const] (rest tracker)))]
    (loop [m (v/zero-like p)
           h p
           tracker []
           old-s p
           old-m (v/one-like p)]
      (if (v/one? m)
        (answer tracker h)
        (let [gg (-> h poly/partial-derivatives gcd-seq)
              new-s (g/exact-divide h (gcd h gg))
              new-m (gcd gg new-s)
              facts (g/exact-divide old-s new-s)
              doublefacts (gcd facts old-m)
              singlefacts (g/exact-divide new-s new-m)]
          (recur new-m
                 (g/exact-divide h (g/* new-m new-s))
                 (conj tracker doublefacts singlefacts)
                 new-s
                 new-m))))))

(defn actual-factors
  [factors]
  (let [expt (sym/symbolic-operator 'expt)]
    (filter (complement v/one?)
            (cons (first factors)
                  (map-indexed #(expt %2 (+ %1 1))
                               (next factors))))))

(defn factor-polynomial-expression
  [simplifier analyzer p]
  (a/expression->
    analyzer
    (x/expression-of p)
    (fn [p v]
      (map (fn [factor] (simplifier (a/->expression analyzer factor v)))
           (split p)))))

(defn- flatten-product
  "Construct a list with all the top-level products in args spliced
  in; other items left wrapped."
  [factors]
  (mapcat #(if (sym/product? %)
             (sym/operands %)
             (list %))
          factors))

(defn ->factors
  "Recursive generalization. [Rather terse comment. --Ed.]"
  [p poly-> v]
  (let [factors (map #(poly-> % v) (split p))
        ff (actual-factors factors)]
    (condp = (count ff)
      0 1
      1 (first ff)
      (cons '* (flatten-product ff)))))

(def factor
  (let [poly-analyzer (poly/->PolynomialAnalyzer)
        poly-> (partial a/->expression poly-analyzer)]
    (a/make-analyzer
     (reify a/ICanonicalize
       (expression-> [_ expr cont v-compare] (a/expression-> poly-analyzer expr cont v-compare))
       (->expression [_ p vars] (->factors p poly-> vars))
       (known-operation? [_ o] (a/known-operation? poly-analyzer o)))
     (a/monotonic-symbol-generator "-f-"))))

(defn ^:private assume!
  [thing context]
  (log/warn (format "Assuming %s in %s" thing context)))

(defn- process-sqrt [expr]
  (let [fact-exp (factor (first (sym/operands expr)))
        expt     (sym/symbolic-operator 'expt)
        *        (sym/symbolic-operator '*)]
    (loop [factors (if (sym/product? fact-exp)
                     (sym/operands fact-exp)
                     (list fact-exp))
           odds 1
           evens 1]
      (cond (nil? factors)
            (do (if (not (and (number? evens) (= evens 1)))
                  (assume! `(~'non-negative? ~evens) 'root-out-squares))
                (* (sym/sqrt odds) evens))

            (sym/expt? (first factors))
            (let [[b e] (sym/operands (first factors))]
              (if (and (integer? e) (even? e))
                (recur (next factors)
                       odds
                       (let [power (quot e 2)]
                         (cond (> power 1) (* evens (expt b power))
                               (= power 1) (* evens b)
                               :else evens)))
                (recur (next factors)
                       (* (first factors) odds)
                       evens)))

            :else
            (recur (next factors)
                   (* (first factors) odds)
                   evens)))))

(defn root-out-squares
  "Removes perfect squares from under square roots."
  [expr]
  (w/prewalk
   (fn [t]
     (if (sym/sqrt? t)
       (process-sqrt t)
       t))
   expr))
