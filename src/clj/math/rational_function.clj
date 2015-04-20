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
  (:require [math.generic :as g]
            [math.value :as v]
            [math.polynomial :as p])
  (:import [math.polynomial Polynomial]))

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
      (throw (InternalError. "Bad polynomial GCD")))
    (RationalFunction. arity p' q')))

(defn add
  "Add the ratiional functions r and s."
  [r s]
  (make (p/add (p/mul (:p r) (:q s)) (p/mul (:q r) (:p s))) (p/mul (:q r) (:q s)))
  )

(defn negate
  [r]
  (RationalFunction. (:arity r) (p/negate (:p r)) (:q r)))

(defn sub
  [r s]
  (add r (negate s)))

(defn mul
  [r s]
  (make (p/mul (:p r) (:p s)) (p/mul (:q r) (:q s))))

(defn invert
  [r]
  (make (:q r) (:p r)))

(defn div
  [r s]
  (mul r (invert s)))

;; (defn expt
;;   [r s]
;;   (make (p/expt (:p))))

(def ^:private operator-table
  {'+ #(reduce add 0 %&)
   '- (fn [arg & args]
        (if (some? args) (sub arg (reduce add args)) (negate arg)))
   '* #(reduce mul 1 %&)
   'negate negate
   'invert invert
   ;;'expt expt
   'square #(mul % %)
   'cube #(mul % (mul % %))
   '/ (fn [arg & args]
        (if (some? args) (div arg (reduce mul args)) (invert arg)))
   ;;`'g/gcd gcd
   })

(def operators-known (set (keys operator-table)))
