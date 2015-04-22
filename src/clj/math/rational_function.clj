;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.rational-function
  (:require [clojure.set :as set]
            [math.expression :as x]
            [math.generic :as g]
            [math.numsymb :as sym]
            [math.value :as v]
            [math.polynomial :as p])
  (:import [math.polynomial Polynomial]))

(declare operator-table operators-known)

(defrecord RationalFunction [^long arity ^Polynomial p ^Polynomial q]
  v/Value
  (nullity? [_] (v/nullity? p))
  (unity? [_] (and (v/unity? p) (v/unity? q))))

(defn make
  "Make the fraction of the two polynomials p and q, after dividing
  out their greatest common divisor."
  [p q]
  (when (v/nullity? q)
    (throw (ArithmeticException. "Can't form rational function with zero denominator")))
  (let [arity (p/check-same-arity p q)
        g (p/gcd p q)
        [p' pr] (p/divide p g)
        [q' qr] (p/divide q g)]
    (when-not (or (v/nullity? pr) (v/nullity? qr))
      (throw (InternalError.  "Bad polynomial GCD")))
    (RationalFunction. arity p' q')))

(defn add
  "Add the ratiional functions r and s."
  [{pr :p qr :q} {ps :p qs :q}]
  (make (p/add (p/mul pr qs) (p/mul qr ps)) (p/mul qr qs)))

(defn negate
  [{p :p q :q arity :arity}]
  (RationalFunction. arity (p/negate p) q))

(defn sub
  [r s]
  (add r (negate s)))

(defn mul
  [{pr :p qr :q} {ps :p qs :q}]
  (make (p/mul pr ps) (p/mul qr qs)))

(defn invert
  [{p :p q :q}]
  (make q p))

(defn div
  [r s]
  (mul r (invert s)))

(defn expt
  [{pr :p qr :q :as r} {ps :p qs :q :as s}]
  (let [arity (p/check-same-arity r s)
        n (p/constant? ps)
        d (p/constant? qs)
        e (and (integer? n) (= d 1) n)]
    (when-not e
      (throw (IllegalArgumentException. (str "Can't raise rational function to " e))))
    (let [[top bottom e] (if (< e 0) [qr pr (- e)] [pr qr e])
          pexp (p/make-constant arity e)]
      (make (p/expt top pexp) (p/expt bottom pexp)))))

(defn make-constant
  [arity c]
  (make (p/make-constant arity c) (p/make-constant arity 1)))

(defn new-variables
  "Creates a sequence of identity (i.e., x) rational functions, one for each
  of arity indeterminates."
  [arity]
  (map #(make % (p/make-constant arity 1)) (p/new-variables arity)))

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
    (sym/div (p/->expression (:p r) vars) (p/->expression (:q r) vars))
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
