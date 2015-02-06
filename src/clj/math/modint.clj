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

(ns math.modint
  (:require [math.value :as v]
            [math.generic :as g]
            [math.euclid :as e]))

(defrecord ModInt [^BigInteger a ^BigInteger m]
  v/Value
  (nullity? [i] (= (:a i) 0))
  (unity? [i] (= (:a i) 1))
  (zero-like [i] (ModInt. 0 (:a i)))
  (exact? [_] true)
  (sort-key [_] 15)
  (numerical? [_] true)
  (compound? [_] false))

(defn make [a m]
  (ModInt. (mod a m) m))

(defn modint? [x]
  (instance? ModInt x))

(defn- modular-binop [op]
  (fn [^ModInt a ^ModInt b]
    (if-not (= (.m a) (.m b))
      (throw (ArithmeticException. "unequal moduli"))
      (make (op (.a a) (.a b)) (.m a)))))

(defn- modular-inv [^ModInt m]
  (let [modulus (.m m)
        [g a _] (e/extended-euclid (:a m) modulus)]
    (if (< g 2) (make a modulus)
        (throw (ArithmeticException.
                (str m " is not invertible mod " modulus))))))

(g/defhandler :+ [modint? modint?] (modular-binop +))
(g/defhandler :+ [integer? modint?] (fn [i ^ModInt m] (make (+ i (.a m)) (.m m))))
(g/defhandler :- [modint? modint?] (modular-binop -))
(g/defhandler :* [modint? modint?] (modular-binop *))
(g/defhandler :negate [modint?] (fn [^ModInt m] (make (- (.a m)) (.m m))))
(g/defhandler :invert [modint?] modular-inv)


(println "modint initialized")
