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
  "This namespace contains an implementation of a [[ModInt]] datatype and various
  operations for creating and working with [[ModInt]] instances. See [\"Modular
  Arithmetic\"](https://en.wikipedia.org/wiki/Modular_arithmetic) on Wikipedia
  for more details about modular arithmetic.

  [[sicmutils.modint]] also extends many SICMUtils generic operations
  to the [[ModInt]] datatype."
  (:require [sicmutils.euclid :as e]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(defrecord ModInt [i m]
  v/Numerical
  (numerical? [_] true)

  v/Value
  (zero? [_] (v/zero? i))
  (one? [_] (v/one? i))
  (identity? [_] (v/one? i))
  (zero-like [_] (ModInt. (v/zero-like i) m))
  (one-like [_] (ModInt. (v/one-like i) m))
  (identity-like [_] (ModInt. (v/one-like i) m))
  (freeze [_] (list 'modint i m))
  (exact? [_] true)
  (kind [_] ::modint))

(defn make [i m]
  (->ModInt (g/modulo i m) m))

(defn- modular-binop [op]
  (fn [a b]
    (if-not (= (:m a) (:m b))
      (u/arithmetic-ex "unequal moduli")
      (make (op (:i a) (:i b)) (:m a)))))

(defn- invert
  "Modular inverse. JVM implementation uses the native BigInt implementation."
  ([m] (invert (:i m) (:m m)))
  ([i modulus]
   #?(:clj
      (try (-> (biginteger i)
               (.modInverse (biginteger modulus))
               (int)
               (->ModInt modulus))
           (catch ArithmeticException _
             (u/arithmetic-ex (str i " is not invertible mod " modulus))))

      :cljs
      (let [[g a _] (e/extended-gcd i modulus)]
        (if (< g 2) (make a modulus)
            (u/arithmetic-ex (str i " is not invertible mod " modulus)))))))

(defn- mod-expt
  "Modular exponentiation, more efficient on the JVM."
  [base pow modulus]
  #?(:clj (let [base (if (neg? pow)
                       (:i (invert base modulus))
                       base)]
            (-> (.modPow (biginteger base)
                         (.abs (biginteger pow))
                         (biginteger modulus))
                (int)
                (->ModInt modulus)))

     :cljs (-> (g/expt (u/bigint base)
                       (u/bigint pow))
               (g/modulo modulus)
               (js/Number)
               (->ModInt modulus))))

(def ^:private add (modular-binop g/add))
(def ^:private sub (modular-binop g/sub))
(def ^:private mul (modular-binop g/mul))
(def ^:private remainder (modular-binop g/remainder))
(def ^:private modulo (modular-binop g/modulo))

(defmethod g/integer-part [::modint] [a] (:i a))
(defmethod g/fractional-part [::modint] [a] 0)
(defmethod g/floor [::modint] [a] a)
(defmethod g/ceiling [::modint] [a] a)
(defmethod g/add [::modint ::modint] [a b] (add a b))
(defmethod g/mul [::modint ::modint] [a b] (mul a b))
(defmethod g/sub [::modint ::modint] [a b] (sub a b))
(defmethod g/negate [::modint] [a] (make (g/negate (:i a)) (:m a)))
(defmethod g/invert [::modint] [a] (invert a))
(defmethod g/magnitude [::modint] [{:keys [i m] :as a}] (g/modulo i m))
(defmethod g/abs [::modint] [{:keys [i m] :as a}]
  (if (g/negative? i)
    (make i m)
    a))
(defmethod g/quotient [::modint ::modint] [a b] (mul a (invert b)))
(defmethod g/remainder [::modint ::modint] [a b] (remainder a b))
(defmethod g/modulo [::modint ::modint] [a b] (modulo a b))
(defmethod g/exact-divide [::modint ::modint] [a b] (mul a (invert b)))
(defmethod g/negative? [::modint] [a] (g/negative? (:i a)))

;; Methods that allow interaction with other integral types. The first block is
;; perhaps slightly more efficient:
(doseq [op [g/add g/mul g/sub]]
  (defmethod op [::v/integral ::modint] [a b] (make (op a (:i b)) (:m b)))
  (defmethod op [::modint ::v/integral] [a b] (make (op (:i a) b) (:m a))))

;; A more efficient exponent implementation is available on the JVM.
(defmethod g/expt [::v/integral ::modint] [a b](mod-expt a (:i b) (:m b)))
(defmethod g/expt [::modint ::v/integral] [a b] (mod-expt (:i a) b (:m a)))

;; The second block promotes any integral type to a ModInt before operating.
(doseq [op [g/quotient g/remainder g/exact-divide]]
  (defmethod op [::v/integral ::modint] [a b] (op (make a (:m b)) b))
  (defmethod op [::modint ::v/integral] [a b] (op a (make b (:m a)))))
