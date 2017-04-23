;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.polynomial-factor
  (:require [clojure.tools.logging :as log]
            [clojure.walk :as w]
            [sicmutils
             [value :as v]
             [analyze :as a]
             [generic :as g]
             [expression :as x]
             [numsymb :as sym :refer [operator product? operands expt]]
             [polynomial :as poly]
             [polynomial-gcd :refer [gcd gcd-seq]]]))

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
      (if (v/unity? m)
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
  (filter #(not= % 1)
          (cons (first factors)
                (map-indexed #(expt %2 (+ %1 1)) (next factors)))))

(defn factor-polynomial-expression
  [simplifier analyzer p]
  (a/expression->
    analyzer
    (x/expression-of p)
    (fn [p v]
      (map (fn [factor] (simplifier (a/->expression analyzer factor v)))
           (split p)))))

(defn ^:private flatten-product
  "Construct a list with all the top-level products in args spliced
  in; other items left wrapped."
  [factors]
  (mapcat #(if (product? %) (operands %) (list %)) factors))

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

(defn ^:private process-sqrt
  [expr]
  (let [fact-exp (factor (first (operands expr)))]
    (loop [factors (if (product? fact-exp)
                     (operands fact-exp)
                     (list fact-exp))
           odds 1
           evens 1]
      (cond (nil? factors)
            (do (if (not (and (number? evens) (= evens 1)))
                  (assume! `(~'non-negative? ~evens) 'root-out-squares))
                (sym/mul (sym/sqrt odds) evens))

            (sym/expt? (first factors))
            (let [b (first (operands (first factors)))
                  e (second (operands (first factors)))]
              (if (and (integer? e) (even? e))
                (recur (next factors)
                       odds
                       (let [power (quot e 2)]
                         (cond (> power 1) (sym/mul evens (sym/expt b power))
                               (= power 1) (sym/mul evens b)
                               :else evens)))
                (recur (next factors)
                       (sym/mul (first factors) odds)
                       evens)))

            :else
            (recur (next factors)
                   (sym/mul (first factors) odds)
                   evens)))))

(defn root-out-squares
  "Removes perfect squares from under square roots."
  [expr]
  (w/prewalk
   (fn [t]
     (if (and (seq? t) (= (operator t) 'sqrt)) (process-sqrt t) t ))
   expr))
