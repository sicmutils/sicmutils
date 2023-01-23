#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.modint
  "This namespace contains an implementation of a [[ModInt]] datatype and various
  operations for creating and working with [[ModInt]] instances. See [\"Modular
  Arithmetic\"](https://en.wikipedia.org/wiki/Modular_arithmetic) on Wikipedia
  for more details about modular arithmetic.

  [[emmy.modint]] also extends many Emmy generic operations
  to the [[ModInt]] datatype."
  (:require #?(:cljs [emmy.euclid :as e])
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v]))

(declare mod:=)

(deftype ModInt [i m]
  v/Value
  (zero? [_] (v/zero? i))
  (one? [_] (v/one? i))
  (identity? [_] (v/one? i))
  (zero-like [_] (ModInt. (v/zero-like i) m))
  (one-like [_] (ModInt. (v/one-like i) m))
  (identity-like [_] (ModInt. (v/one-like i) m))
  (freeze [_] (list 'modint i m))
  (exact? [_] true)
  (kind [_] ::modint)

  #?@(:clj
      [Object
       (equals [this that] (mod:= this that))
       (toString [_] (str "[" i " mod " m "]"))]

      :cljs
      [IEquiv
       (-equiv [this that] (mod:= this that))

       Object
       (toString [_] (str "[" i " mod " m "]"))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer
                              "#object[emmy.modint.ModInt \""
                              (.toString x)
                              "\"]"))]))

(defn modint?
  "Returns true if `x` is an instance of [[ModInt]], false otherwise."
  [x]
  (instance? ModInt x))

(defn residue [x]
  (.-i ^ModInt x))

(defn modulus [x]
  (.-m ^ModInt x))

(defn- mod:= [this that]
  (cond (modint? that)
        (and (= (modulus this)
                (modulus that))
             (v/= (residue this)
                  (residue that)))

        (v/number? that)
        (v/= (residue this)
             (g/modulo that (modulus this)))

        :else false))

(defn make
  "Returns an instance of [[ModInt]] that represents integer `i` with integral
  modulus `m`."
  [i m]
  {:pre [(v/integral? i)
         (v/integral? m)]}
  (->ModInt (g/modulo i m) m))

(defn- modular-binop [op]
  (fn [a b]
    (if-not (= (modulus a) (modulus b))
      (u/arithmetic-ex "unequal moduli")
      (make (op (residue a) (residue b)) (modulus a)))))

(defn- invert
  "Modular inverse. JVM implementation uses the native BigInt implementation."
  ([m] (invert (residue m) (modulus m)))
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
        (if (< g 2)
          (make a modulus)
          (u/arithmetic-ex (str i " is not invertible mod " modulus)))))))

(defn- mod-expt
  "Modular exponentiation, more efficient on the JVM."
  [base pow modulus]
  #?(:clj (let [base (if (neg? pow)
                       (residue (invert base modulus))
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

(defn chinese-remainder
  "[Chinese Remainder Algorithm](https://en.wikipedia.org/wiki/Chinese_remainder_theorem).

  Accepts a sequence of [[ModInt]] instances (where the `modulus` of
  all [[ModInt]] instances are relatively prime), and returns a [[ModInt]] `x`
  such that `(residue input) == (mod x (modulus input))`.

  For example:

  ```clojure
  (let [a1 (m/make 2 5)
        a2 (m/make 3 13)]
    [(= 42 (chinese-remainder a1 a2))
     (= (residue a1) (mod cr (modulus a1)))
     (= (residue a2) (mod cr (modulus a2)))])
  ;;=> [true true true]
  ```"
  [& modints]
  (let [prod  (transduce (map modulus) g/* modints)
        xform (map (fn [mi]
		                 (let [i (residue mi)
                           m (modulus mi)
                           c (g/quotient prod m)]
                       (g/* i c (residue (invert c m))))))]
    (-> (transduce xform g/+ modints)
        (g/modulo prod))))

(def ^:private add (modular-binop g/add))
(def ^:private sub (modular-binop g/sub))
(def ^:private mul (modular-binop g/mul))
(def ^:private remainder (modular-binop g/remainder))
(def ^:private modulo (modular-binop g/modulo))

(defn- div [a b]
  (mul a (invert b)))

(defmethod v/= [::v/number ::modint] [l r] (mod:= r l))
(defmethod v/= [::modint ::v/number] [l r] (mod:= l r))

(defmethod g/integer-part [::modint] [a] (residue a))
(defmethod g/fractional-part [::modint] [_] 0)
(defmethod g/floor [::modint] [a] a)
(defmethod g/ceiling [::modint] [a] a)
(defmethod g/add [::modint ::modint] [a b] (add a b))
(defmethod g/mul [::modint ::modint] [a b] (mul a b))
(defmethod g/div [::modint ::modint] [a b] (div a b))
(defmethod g/sub [::modint ::modint] [a b] (sub a b))
(defmethod g/negate [::modint] [a] (make (g/negate (residue a)) (modulus a)))
(defmethod g/invert [::modint] [a] (invert a))
(defmethod g/magnitude [::modint] [a]
  (g/modulo (residue a)
            (modulus a)))

(defmethod g/abs [::modint] [a]
  (let [i (residue a)]
    (if (g/negative? i)
      (make i (modulus a))
      a)))

(defmethod g/quotient [::modint ::modint] [a b] (mul a (invert b)))
(defmethod g/remainder [::modint ::modint] [a b] (remainder a b))
(defmethod g/modulo [::modint ::modint] [a b] (modulo a b))
(defmethod g/exact-divide [::modint ::modint] [a b] (mul a (invert b)))
(defmethod g/negative? [::modint] [a] (g/negative? (residue a)))

;; A more efficient exponent implementation is available on the JVM.
(defmethod g/expt [::v/integral ::modint] [a b](mod-expt a (residue b) (modulus b)))
(defmethod g/expt [::modint ::v/integral] [a b] (mod-expt (residue a) b (modulus a)))

(defmethod g/solve-linear [::modint ::modint] [a b] (div b a))
(defmethod g/solve-linear-right [::modint ::modint] [a b] (div a b))

;; Methods that allow interaction with other integral types. The first block is
;; perhaps slightly more efficient:
(doseq [op [g/add g/mul g/sub]]
  (defmethod op [::v/integral ::modint] [a b] (make (op a (residue b)) (modulus b)))
  (defmethod op [::modint ::v/integral] [a b] (make (op (residue a) b) (modulus a))))

;; The second block promotes any integral type to a ModInt before operating.
(doseq [op [g/div g/solve-linear g/solve-linear-right
            g/quotient g/remainder g/exact-divide]]
  (defmethod op [::v/integral ::modint] [a b] (op (make a (modulus b)) b))
  (defmethod op [::modint ::v/integral] [a b] (op a (make b (modulus a)))))
