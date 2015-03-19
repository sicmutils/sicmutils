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

(defrecord ModInt [^BigInteger i ^BigInteger m]
  v/Value
  (nullity? [_] (zero? i))
  (unity? [_] (= i 1))
  (zero-like [_] (ModInt. 0 m))
  (exact? [_] true)
  (sort-key [_] 15)
  (numerical? [_] true)
  (compound? [_] false)
  (kind [_] ::modint))

(defn make [i m]
  (ModInt. (mod i m) m))

(defn modint? [x]
  (instance? ModInt x))

(defn- modular-binop [op]
  (fn [a b]
    (if-not (= (:m a) (:m b))
      (throw (ArithmeticException. "unequal moduli"))
      (make (op (:i a) (:i b)) (:m a)))))

(defn- modular-inv [^ModInt m]
  (let [modulus (:m m)
        [g a _] (e/extended-euclid (:i m) modulus)]
    (if (< g 2) (make a modulus)
        (throw (ArithmeticException.
                (str m " is not invertible mod " modulus))))))

(def ^:private add (modular-binop +))
(def ^:private sub (modular-binop -))
(defmethod g/add [::modint ::modint] [a b] (add a b))
(defmethod g/add [java.lang.Long ::modint] [a b] (make (+ a (:i b)) (:m b)))
(defmethod g/sub [::modint ::modint] [a b] (sub a b))
(defmethod g/negate ::modint [a] (make (- (:i a)) (:m a)))
(g/defhandler :* [modint? modint?] (modular-binop *))
(g/defhandler :invert [modint?] modular-inv)


(println "modint initialized")
