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
            [sicmutils.polynomial.gcd :refer [gcd gcd-Dp]]
            [taoensso.timbre :as log]))

(defn split-polynomial
  "Note: Split a polynomial into factors of various multiplicities."
  [p]
  (letfn [(answer [[x :as tracker] const]
            (if (v/number? x)
              (into [x] (conj (pop tracker) 1))
              (into [const] (rest tracker))))]
    ;; TODO these should switch to poly:zero and friends.
    (loop [m (v/zero-like p)
           h p
           tracker []
           old-s p
           old-m (v/one-like p)]
      (if (v/one? m)
        (answer tracker h)
        (let [gg    (gcd-Dp h)
              ;; TODO: g/exact-divide => poly:quotient
              new-s (g/exact-divide h (gcd h gg))
              new-m (gcd gg new-s)
              facts (g/exact-divide old-s new-s)

              ;; facts gets all the factors that were completely
	            ;; removed last step, i.e. all those that were to
	            ;; the 1 or 2 power.  The first loop through will
	            ;; get a totally wrong facts, but its gcd with the
	            ;; initial old-m=1 will be 1, so it won't result in
	            ;; incorrect doublefacts or singlefacts.

              doublefacts (gcd facts old-m)
              ;; doublefacts gets all the factors which were to
	            ;; the power x>1, x<=2, (ergo x=2), in the last step.

              singlefacts (g/exact-divide new-s new-m)
              ;; takes out p = all factors only to the 1st power.
              ]
          (recur new-m
                 ;; the followinghas all factors to the 1 or 2 power
	               ;; completely removed, others now to the power-2.
                 ;; TODO: poly:*, keep it cheap
                 (g/exact-divide h (g/* new-m new-s))

                 ;; tracker of the form
	               ;;  h(vi) = (* (exponent (get tracker k) k))
                 (conj tracker doublefacts singlefacts)
                 new-s
                 new-m))))))

;; ### Reconstructions

(defn actual-factors [factors]
  (let [expt (sym/symbolic-operator 'expt)]
    (filter (fn [f]
              (or (not (v/number? f))
                  (not (v/one? f))))
            (cons (first factors)
                  (map-indexed (fn [i f]
                                 (expt f (+ i 1)))
                               (rest factors))))))

(defn factor-polynomial-expression
  [simplifier analyzer P]
  (a/expression->
   analyzer
   (x/expression-of P)
   (fn [p v]
     (map (fn [factor]
            (simplifier
             (a/->expression analyzer factor v)))
          (split-polynomial p)))))

(defn split-polynomial->expression [P]
  (let [factors (factor-polynomial-expression P)]
    (cons '* (actual-factors factors))))

(defn- flatten-product
  "Construct a list with all the top-level products in args spliced
  in; other items left wrapped."
  [factors]
  (mapcat (fn [factor]
            (if (sym/product? factor)
              (sym/operands factor)
              [factor]))
          factors))

(defn ->factors
  "Recursive generalization. [Rather terse comment. --Ed.]

  NOTE: this is from split-poly.scm, `pcf:->factors`.

  poly-> is pcf:->expression."
  [p poly-> v]
  (let [factors (map (fn [factor]
                       (poly-> factor v))
                     (split-polynomial p))
        ff (actual-factors factors)]
    (condp = (count ff)
      0 1
      1 (first ff)
      (cons '* (flatten-product ff)))))

(def ^:no-doc factor-analyzer
  (let [poly-analyzer (poly/->PolynomialAnalyzer)
        poly-> (partial a/->expression poly-analyzer)]
    (a/make-analyzer
     (reify a/ICanonicalize
       (expression-> [_ expr cont v-compare]
         (a/expression-> poly-analyzer expr cont v-compare))
       (->expression [_ p vars]
         (->factors p poly-> vars))
       (known-operation? [_ o]
         (a/known-operation? poly-analyzer o)))
     (a/monotonic-symbol-generator "-f-"))))

(def factor
  (a/default-simplifier factor-analyzer))

;; TODO assumptions are missing!

(defn- assume! [thing context]
  (log/warn
   (format "Assuming %s in %s" thing context)))

(defn- process-sqrt
  "NOTE: Comes from split-poly.scm."
  [expr]
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
