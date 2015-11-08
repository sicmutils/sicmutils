;
; Copyright (C) 2015 Colin Smith.
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

(ns net.littleredcomputer.math.rational-function
  (:require [clojure.set :as set]
            [net.littleredcomputer.math
             [expression :as x]
             [generic :as g]
             [euclid :as e]
             [numsymb :as sym]
             [value :as v]
             [polynomial :as p]])
  (:import [net.littleredcomputer.math.polynomial Polynomial]))

(declare operator-table operators-known)

(defrecord RationalFunction [^long arity ^Polynomial u ^Polynomial v]
  v/Value
  (nullity? [_] (v/nullity? u))
  (unity? [_] (and (v/unity? u) (v/unity? v)))
  (kind [_] ::rational-function))

(defn make
  "Make the fraction of the two polynomials p and q, after dividing
  out their greatest common divisor."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (when (v/nullity? v)
    (throw (ArithmeticException. "Can't form rational function with zero denominator")))
  ;; annoying: we are using native operations here for the base coefficients
  ;; of the polynomial. Can we do better? That would involve exposing gcd as
  ;; a generic operation (along with lcm), and binding the euclid implmentation
  ;; in for language supported integral types. Perhaps also generalizing ratio?
  ;; and denominator. TODO.
  (let [arity (p/check-same-arity u v)
        cv (p/coefficients v)
        lcv (last cv)
        cs (into (into #{} cv) (p/coefficients u))
        integerizing-factor (*
                             (if (< lcv 0) -1 1)
                             (reduce e/lcm 1 (map denominator (filter ratio? cs))))

        u' (if (not (v/unity? integerizing-factor)) (p/map-coefficients #(g/* integerizing-factor %) u) u)
        v' (if (not (v/unity? integerizing-factor)) (p/map-coefficients #(g/* integerizing-factor %) v) v)
        ]
    (RationalFunction. arity u' v')))

;;
;; Rational arithmetic is from Knuth vol 2 section 4.5.1
;;

(defn add
  "Add the ratiional functions r and s."
  [{u :u u' :v :as p} {v :u v' :v :as q}]
  (let [a (p/check-same-arity p q)
        d1 (p/gcd u' v')]
    (if (v/unity? d1)
      (RationalFunction. a (p/add (p/mul u v') (p/mul u' v)) (p/mul u' v'))
      (let [t (p/add (p/mul u (p/evenly-divide v' d1))
                     (p/mul v (p/evenly-divide u' d1)))
            d2 (p/gcd t d1)]
        (RationalFunction. a
                           (p/evenly-divide t d2)
                           (p/mul (p/evenly-divide u' d1)
                                  (p/evenly-divide v' d2)))))))

(defn negate
  [{u :u v :v arity :arity}]
  (RationalFunction. arity (p/negate u) v))

(defn sub
  [r s]
  (add r (negate s)))

(defn mul
  [{u :u u' :v :as U} {v :u v' :v :as V}]
  (let [a (p/check-same-arity U V)]
    (cond (v/nullity? U) U
          (v/nullity? V) V
          (v/unity? U) V
          (v/unity? V) U
          :else (let [d1 (p/gcd u v')
                      d2 (p/gcd u' v)]
                  (RationalFunction. a
                                     (p/mul (p/evenly-divide u d1) (p/evenly-divide v d2))
                                     (p/mul (p/evenly-divide u' d2) (p/evenly-divide v' d1)))))))

(defn invert
  [{arity :arity u :u v :v}]
  ;; use make so that the - sign will get flipped if needed
  (make v u))

(defn div
  [r s]
  (mul r (invert s)))

(defn expt
  [{pr :u qr :v :as r} {ps :u qs :v :as s}]
  (let [arity (p/check-same-arity r s)
        n (p/constant? ps)
        d (p/constant? qs)
        e (and (integer? n) (= d 1) n)]
    (when-not e
      (throw (IllegalArgumentException. (str "Can't raise rational function to " e))))
    (let [[top bottom e] (if (< e 0) [qr pr (- e)] [pr qr e])
          pexp (p/make-constant arity e)]
      (RationalFunction. arity (p/expt top pexp) (p/expt bottom pexp)))))

(defn make-constant
  [arity c]
  (RationalFunction. arity (p/make-constant arity c) (p/make-constant arity 1)))

(defn new-variables
  "Creates a sequence of identity (i.e., x) rational functions, one for each
  of arity indeterminates."
  [arity]
  (map #(RationalFunction. arity % (p/make-constant arity 1)) (p/new-variables arity)))

(defn expression->
  "Convert an expression into Rational Function canonical form. The
  expression should be an unwrapped expression, i.e., not an instance
  of the Expression type, nor should subexpressions contain type
  information. This kind of simplification proceeds purely
  symbolically over the known Rational Function operations; other
  operations outside the arithmetic available R(x...) should be
  factored out by an expression analyzer before we get here. The
  result is a RationalFunction object representing the structure of
  the input over the unknowns."
  [expr cont]
  (let [expression-vars (sort (set/difference (x/variables-in expr) operators-known))
        arity (count expression-vars)]
    (let [new-bindings (zipmap expression-vars (new-variables arity))
          environment (into operator-table new-bindings)
          transformer (x/walk-expression environment #(make-constant arity %))]
      (-> expr transformer (cont expression-vars)))))

(defn ->expression
  "This is the output stage of Rational Function canonical form simplification.
  The input is a RationalFunction, and the output is an expression
  representing the evaluation of that function over the
  indeterminates extracted from the expression at the start of this
  process."
  [r vars]
  ;; odd: this (i.e., (symbol? p)) only happens in the case of
  ;; something like (expt 'x 'y), where we can't treat it as a known
  ;; expression because 'y is not an integer. Handling it here is easy
  ;; enough, but it seems like an odd special case and perhaps should
  ;; be treated at the level above.
  (if (instance? RationalFunction r)
    (sym/div (p/->expression (:u r) vars) (p/->expression (:v r) vars))
    r))

(def ^:private operator-table
  {'+ #(reduce add %&)
   '- (fn [arg & args]
        (if (some? args) (sub arg (reduce add args)) (negate arg)))
   '* #(reduce mul %&)
   '/ (fn [arg & args]
        (if (some? args) (div arg (reduce mul args)) (invert arg)))
   'negate negate
   'invert invert
   'expt expt
   'square #(mul % %)
   'cube #(mul % (mul % %))
   ;;`'g/gcd gcd
   })

(def operators-known (set (keys operator-table)))

(defmethod g/add [::rational-function ::rational-function] [a b] (add a b))
(defmethod g/mul [::rational-function ::rational-function] [a b] (mul a b))
(defmethod g/sub [::rational-function ::rational-function] [a b] (sub a b))
(defmethod g/div [::rational-function ::rational-function] [a b] (div a b))
(defmethod g/negate ::rational-function [a] (negate a))
