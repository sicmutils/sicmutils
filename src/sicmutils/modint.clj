;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(ns sicmutils.modint
  (:require [sicmutils
             [generic :as g]
             [euclid :as e]]))

(defrecord ModInt [^BigInteger i ^BigInteger m])

(defn make [i m]
  (ModInt. (mod i m) m))

(defn ^:private modular-binop [op]
  (fn [a b]
    (if-not (= (:m a) (:m b))
      (throw (ArithmeticException. "unequal moduli"))
      (make (op (:i a) (:i b)) (:m a)))))

(defn ^:private modular-inv [^ModInt m]
  (let [modulus (:m m)
        [g a _] (e/extended-gcd (:i m) modulus)]
    (if (< g 2) (make a modulus)
        (throw (ArithmeticException.
                (str m " is not invertible mod " modulus))))))

(def ^:private add (modular-binop +))
(def ^:private sub (modular-binop -))
(def ^:private mul (modular-binop *))
(def ^:private modulo (modular-binop mod))

(defmethod g/add [ModInt ModInt] [a b] (add a b))
(defmethod g/add [Long ModInt] [a b] (make (+ a (:i b)) (:m b)))
(defmethod g/add [ModInt Long] [a b] (make (+ (:i a) b) (:m a)))
(defmethod g/mul [ModInt ModInt] [a b] (mul a b))
(defmethod g/sub [ModInt ModInt] [a b] (sub a b))
(defmethod g/negate [ModInt] [a] (make (- (:i a)) (:m a)))
(defmethod g/invert [ModInt] [a] (modular-inv a))
(defmethod g/abs [ModInt] [{:keys [i m] :as a}] (if (< i 0) (make i m) a))
(defmethod g/quotient [ModInt ModInt] [a b] (mul a (modular-inv b)))
(defmethod g/remainder [ModInt ModInt] [a b] (modulo a b))
(defmethod g/exact-divide [ModInt ModInt] [a b] (mul a (modular-inv b)))
(defmethod g/negative? [ModInt] [a] (< (:i a) 0))
(defmethod g/zero? [ModInt] [a] (= (:i a) 0))
(defmethod g/one? [ModInt] [a] (= (:i a) 1))
