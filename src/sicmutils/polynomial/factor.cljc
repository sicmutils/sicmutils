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
            [sicmutils.expression :as x]
            [sicmutils.expression.analyze :as a]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial :as poly]
            [sicmutils.polynomial.gcd :refer [gcd gcd-Dp]]
            [sicmutils.util.logic :as ul]
            [sicmutils.value :as v]))

(defn split-polynomial
  "Given some polynomial `p`, returns a sequence of factors of various
  multiplicities."
  [p]
  (letfn [(answer [tracker const]
            (let [final (peek tracker)]
              (if (v/number? final)
                (into [final] (subvec (conj (pop tracker) 1) 1))
                (into [const] (subvec tracker 1)))))]
    (loop [m 0
           h p
           tracker []
           old-s p
           old-m 1]
      (if (v/one? m)
        (answer tracker h)
        (let [gg (gcd-Dp h)
              new-s (poly/evenly-divide h (gcd h gg))
              new-m (gcd gg new-s)
              facts (poly/evenly-divide old-s new-s)

              ;; facts gets all the factors that were completely
              ;; removed last step, i.e. all those that were to
              ;; the 1 or 2 power.  The first loop through will
              ;; get a totally wrong facts, but its gcd with the
              ;; initial old-m=1 will be 1, so it won't result in
              ;; incorrect doublefacts or singlefacts.
              doublefacts (gcd facts old-m)
              ;; doublefacts gets all the factors which were to
              ;; the power x>1, x<=2, (ergo x=2), in the last step.

              ;; takes out p = all factors only to the 1st power.
              singlefacts (poly/evenly-divide new-s new-m)]

          (recur new-m
                 ;; the following has all factors to the 1 or 2 power
                 ;; completely removed, others now to the power-2.
                 (poly/evenly-divide h (poly/mul new-m new-s))

                 ;; tracker of the form
                 ;;  h(vi) = (* (exponent (get tracker k) k))
                 (conj tracker doublefacts singlefacts)
                 new-s
                 new-m))))))

;; ## Reconstructions

(defn- actual-factors [factors]
  (let [expt (sym/symbolic-operator 'expt)]
    (filter (fn [f]
              (or (not (v/number? f))
                  (not (v/one? f))))
            (cons (first factors)
                  (map-indexed (fn [i f]
                                 (expt f (+ i 1)))
                               (rest factors))))))

(defn factor-polynomial-expression
  [simplifier P]
  (letfn [(cont [p vars]
            (map (fn [factor]
                   (simplifier
                    (poly/->expression factor vars)))
                 (split-polynomial p)))]
    (poly/expression->
     (x/expression-of P)
     cont)))

(defn split-polynomial->expression
  "TODO this is unused but I want it!"
  [simplifier P]
  (let [factors (factor-polynomial-expression simplifier P)]
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
  [p v]
  (let [factors (map (fn [factor]
                       (poly/->expression factor v))
                     (split-polynomial p))
        ff (actual-factors factors)]
    (condp = (count ff)
      0 1
      1 (first ff)
      (cons '* (flatten-product ff)))))

(def ^:no-doc analyzer
  (let [symgen (a/monotonic-symbol-generator "-f-")]
    (-> (reify a/ICanonicalize
          (expression-> [_ expr cont v-compare]
            (poly/expression-> expr cont v-compare))
          (->expression [_ p vars]
            (->factors p vars))
          (known-operation? [_ o]
            (a/known-operation? poly/analyzer o)))
        (a/make-analyzer symgen))))

(def factor
  (a/default-simplifier analyzer))

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
            (do (if (not (and (v/number? evens)
                              (= evens 1)))
                  (ul/assume! `(~'non-negative? ~evens) 'root-out-squares))
                (* (sym/sqrt odds) evens))

            (sym/expt? (first factors))
            (let [[b e] (sym/operands (first factors))]
              (if (and (v/native-integral? e)
                       (even? e))
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
