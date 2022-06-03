#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.numbers
  "This namespace extends of all appropriate SICMUtils generic operations
  from [[sicmutils.generic]] and [[sicmutils.value]] to the Clojure(script)
  numeric tower.

  For other numeric extensions, see [[sicmutils.ratio]]
  and [[sicmutils.complex]]."
  (:refer-clojure :exclude [zero? / + - *])
  (:require [clojure.core :as core]
            [sicmutils.complex :refer [complex]]
            #?(:cljs [sicmutils.ratio :as r])

            ;; Required to enable the generic gcd implementation.
            [sicmutils.euclid]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:cljs
     (:import (goog.math Long Integer))
     :clj
     (:import (clojure.lang BigInt)
              (java.math BigInteger)
              (org.apache.commons.math3.util ArithmeticUtils))))

;; "Backstop" implementations that apply to anything that descends from
;; ::v/real.
(defmethod g/add [::v/real ::v/real] [a b] (#?(:clj +' :cljs core/+) a b))
(defmethod g/mul [::v/real ::v/real] [a b] (#?(:clj *' :cljs core/*) a b))
(defmethod g/sub [::v/real ::v/real] [a b] (#?(:clj -' :cljs core/-) a b))
(defmethod g/negate [::v/real] [a] (core/- a))
(defmethod g/negative? [::v/real] [a] (neg? a))

(defn- expt-int [base pow]
  (loop [n pow, y (num 1), z base]
    (let [t (even? n), n (quot n 2)]
      (cond
        t (recur n y (*' z z))
        (zero? n) (*' z y)
        :else (recur n (*' z y) (*' z z))))))

;; TODO similar condp trick for integer-length.

;; TODO look at for help
;; https://github.com/clojure/math.numeric-tower/blob/master/src/main/clojure/clojure/math/numeric_tower.clj

(defn integer-length
  "Length of integer in binary"
  [n]
  ;; something like this
  (case (type n)
    BigDecimal 1M
    java.math.BigInteger (java.math.BigInteger. "1")
    clojure.lang.BigInt  (bigint 1)
    1)
  )

(defn compute-expt
  "(expt base pow) is base to the pow power.
  Returns an exact number if the base is an exact number and the power is an
  integer, otherwise returns a double."
  [base pow]
  (if (and (not (float? base)) (integer? pow))
    (cond (pos? pow)  (expt-int base pow)
          (zero? pow) (case (type base)
                        BigDecimal 1M
                        java.math.BigInteger (java.math.BigInteger. "1")
                        clojure.lang.BigInt  (bigint 1)
                        1)
          :else (/ 1 (expt-int base (-' pow))))
    (Math/pow base pow)))

(defmethod g/expt [::v/real ::v/real] [b x]
  (if (and (neg? b)
           (not (core/zero?
                 (g/fractional-part x))))
    (g/exp (g/mul x (g/log b)))
    (u/compute-expt b x)))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
    (not (number? n)) (throw (IllegalArgumentException.
			                        "abs requires a number"))
    (neg? n) (-' n)
    :else n))

(defmethod g/abs [::v/real] [a] (u/compute-abs a))
(defmethod g/magnitude [::v/real] [a] (u/compute-abs a))
(defmethod g/div [::v/real ::v/real] [a b] (core// a b))
(defmethod g/invert [::v/real] [a] (core// a))
(defmethod g/floor [::v/real] [a] (long (Math/floor a)))
(defmethod g/ceiling [::v/real] [a] (long (Math/ceil a)))
(defmethod g/integer-part [::v/real] [a]
  #?(:clj (long a)
     :cljs (Math/trunc a)))

(defmethod g/infinite? [::v/integral] [_] false)
(defmethod g/infinite? [::v/real] [a]
  #?(:clj (or (= a ##Inf)
              (= a ##-Inf))
     :cljs (infinite? a)))

;; ## Complex Operations
(defmethod g/real-part [::v/real] [a] a)
(defmethod g/imag-part [::v/real] [_] 0)

(defmethod g/angle [::v/real] [a]
  (if (neg? a)
    Math/PI
    (v/zero-like a)))

(defmethod g/conjugate [::v/real] [a] a)

;; ## Trig Operations

(defmethod g/sinc [::v/real] [a]
  (cond (v/zero? a)     1
        (g/infinite? a) 0
        :else (g// (g/sin a) a)))

(defmethod g/sin [::v/real] [a] (Math/sin a))
(defmethod g/cos [::v/real] [a] (Math/cos a))
(defmethod g/tan [::v/real] [a] (Math/tan a))

(defmethod g/cosh [::v/real] [a] (Math/cosh a))
(defmethod g/sinh [::v/real] [a] (Math/sinh a))
(defmethod g/tanh [::v/real] [a] (Math/tanh a))

(defmethod g/atan [::v/real] [a] (Math/atan a))
(defmethod g/atan [::v/real ::v/real] [a b] (Math/atan2 a b))

;; Operations which allow promotion to complex numbers when their
;; arguments would otherwise result in a NaN if computed on the real
;; line

(defmethod g/asin [::v/real] [a]
  (if (> (g/abs a) 1)
    (g/asin (complex a))
    (Math/asin a)))

(defmethod g/acos [::v/real] [a]
  (if (> (g/abs a) 1)
    (g/acos (complex a))
    (Math/acos a)))

#?(:cljs
   (do
     ;; JS makes these available natively.
     (defmethod g/acosh [::v/real] [a]
       (if (>= a 1)
         (Math/acosh a)
         (g/acosh (complex a))))

     (defmethod g/asinh [::v/real] [a]
       (if (>= a 1)
         (Math/asinh a)
         (g/asinh (complex a))))

     (defmethod g/atanh [::v/real] [a]
       (if (>= (g/abs a) 1)
         (g/atanh (complex a))
         (Math/atanh a)))))

(defmethod g/sqrt [::v/real] [a]
  (if (neg? a)
    (g/sqrt (complex a))
    (u/compute-sqrt a)))

(defmethod g/log [::v/real] [a]
  (if (neg? a)
    (g/log (complex a))
    (Math/log a)))

;; Specialized methods provided by the host platforms.

#?(:clj  (defmethod g/log10 [Double] [x]
           (if (neg? x)
             (g/log10 (complex x))
             (Math/log10 x)))

   :cljs (defmethod g/log10 [js/Number] [x]
           (if (neg? x)
             (g/log10 (complex x))
             (Math/log10 x))))

#?(:cljs (defmethod g/log2 [js/Number] [x]
           (if (neg? x)
             (g/log2 (complex x))
             (Math/log2 x))))

(defmethod g/exp [::v/real] [a]
  (if (core/zero? a)
    1
    (Math/exp a)))

;; We don't yet suport quotient, remainder with floating point, but there are a
;; few cases where it does make sense. When two numbers are identical up to
;; sign, it's fine to say that they divide evenly (and return 1 or -1). If that
;; happens, we can return a 0 remainder.

(defn- careful-divide
  "Minimum effort division. If `b` and `a` are equal or of opposite sign,
  returns 1 or -1 respectively. If `a` is 1 or -1, returns `b` or `-b`
  respectively. Else, returns nil."
  [b a]
  (cond (v/= a b)            (v/one-like a)
        (v/= a (g/negate b)) (g/negate (v/one-like a))
        (v/one? a)            b
        (v/one? (g/negate a)) (g/negate b)
        :else nil))

(defmethod g/exact-divide [::v/scalar ::v/real] [b a]
  (or (careful-divide b a)
      (u/illegal
       (str "exact-divide not allowed between: " b ", " a))))

(defmethod g/quotient [::v/scalar ::v/real] [b a]
  (or (careful-divide b a)
      (u/illegal
       (str "quotient not allowed between: " b ", " a))))

(defn ^:private exact-divide
  "Checked implementation of g/exact-divide general enough to use for any type
  that defines g/remainder and g/quotient."
  [a b]
  {:pre [(v/zero? (g/remainder a b))]}
  (g/quotient a b))

(defmethod g/exact-divide [::v/integral ::v/integral] [b a]
  (exact-divide b a))

(defmethod g/integer-part [::v/integral] [a] a)
(defmethod g/fractional-part [::v/integral] [_] 0)
(defmethod g/floor [::v/integral] [a] a)
(defmethod g/ceiling [::v/integral] [a] a)

;; TODO

;; Produces the largest integer less than or equal to the square root of n
;; Input n must be a non-negative integer

(declare integer-length)

#?(:cljs (def inc' clojure.core/inc))
#?(:cljs (def +' clojure.core/+))
#?(:cljs (def -' clojure.core/-))
#?(:cljs (def *' clojure.core/*))

(defn- integer-sqrt [n]
  (cond
    (> n 24)
    (let [n-len (integer-length n)]
      (loop [init-value (if (even? n-len)
			                    (g/expt 2 (quot n-len 2))
			                    (g/expt 2 (inc' (quot n-len 2))))]
        (let [iterated-value (quot (+' init-value (quot n init-value)) 2)]
	        (if (>= iterated-value init-value)
	          init-value
	          (recur iterated-value)))))
    (> n 15) 4
    (> n  8) 3
    (> n  3) 2
    (> n  0) 1
    (> n -1) 0))

(defn exact-integer-sqrt "(exact-integer-sqrt n) expects a non-negative integer n, and returns [s r] where n = s^2+r and n < (s+1)^2.  In other words, it returns the floor of the square root and the 'remainder'.
  For example, (exact-integer-sqrt 15) is [3 6] because 15 = 3^2+6."
  [n]
  (if (or (not (integer? n)) (neg? n))
    (throw (IllegalArgumentException. "exact-integer-sqrt requires a non-negative integer"))
    (let [isqrt (integer-sqrt n),
	        error (-' n (*' isqrt isqrt))]
      [isqrt error])))

(defmethod g/sqrt [::v/integral] [n]
  (if (neg? n) Double/NaN
      (let [isqrt (integer-sqrt n),
	          error (-' n (*' isqrt isqrt))]
	      (if (clojure.core/zero? error) isqrt
	          (Math/sqrt n)))))

(defn- sqrt-ratio [^clojure.lang.Ratio n]
  (if (neg? n) Double/NaN
      (let [numerator (.numerator n),
	          denominator (.denominator n),
	          sqrtnum (sqrt numerator)]
	      (if (float? sqrtnum)
	        (Math/sqrt n)
	        (let [sqrtden (sqrt denominator)]
	          (if (float? sqrtden)
	            (Math/sqrt n)
	            (/ sqrtnum sqrtden)))))))

(defn- sqrt-decimal [n]
  (if (neg? n) Double/NaN
      (let [frac (rationalize n),
	          sqrtfrac (sqrt frac)]
	      (if (ratio? sqrtfrac)
	        (/ (BigDecimal. (.numerator ^clojure.lang.Ratio sqrtfrac))
	           (BigDecimal. (.denominator ^clojure.lang.Ratio sqrtfrac)))
	        sqrtfrac))))

;; TODO install Math/sqrt for float, double, then sqrt-integer for others!!
;;
;; TODO floor, ceil for bigint, bigdecimal, ratio, double, float

;; All JVM and JS types that respond to ::native-integral behave correctly with
;; Clojure's native `quot`, `rem`, `mod`.
(defmethod g/quotient [::v/native-integral ::v/native-integral] [a b] (quot a b))
(defmethod g/remainder [::v/real ::v/real] [a b] (rem a b))
(defmethod g/modulo [::v/real ::v/real] [a b] (mod a b))

;; This section defines methods that act differently between Clojurescript and
;; Clojure. The clojure methods are all slightly more refined based on Java's
;; type system.
#?(:clj
   ;; Efficient, native GCD on the JVM.
   (do (defmethod g/gcd [BigInteger BigInteger] [a b]
         (.gcd ^BigInteger a
               ^BigInteger b))

       (defmethod g/gcd [BigInt BigInt] [a b]
         (.gcd (biginteger a)
               (biginteger b)))

       (defmethod g/gcd [Long Long] [a b]
         (ArithmeticUtils/gcd ^long a ^long b))

       (defmethod g/gcd [Integer Integer] [a b]
         (ArithmeticUtils/gcd ^int a ^int b))

       (doseq [from [Long BigInt Integer]]
         (defmethod g/gcd [BigInteger from] [a b]
           (.gcd ^BigInteger a (biginteger b)))

         (defmethod g/gcd [from BigInteger] [a b]
           (.gcd (biginteger a) b)))

       (doseq [from [Long Integer]]
         (defmethod g/gcd [BigInt from] [a b]
           (.gcd (biginteger a) (biginteger b)))

         (defmethod g/gcd [from BigInt] [a b]
           (.gcd (biginteger a) (biginteger b))))))

#?(:cljs
   (do
     (letfn [(abs [a]
               (if (neg? a) (core/- a) a))

             (bigint-gcd [a b]
               (loop [a (abs a)
                      b (abs b)]
                 (if (coercive-= b 0)
                   a
                   (recur b (js-mod a b)))))]

       ;; The following GCD implementations use native operations to get more
       ;; speed than the generic implementation in `sicmutils.euclid`.
       (defmethod g/gcd [::v/native-integral ::v/native-integral] [a b]
         (loop [a (abs a)
                b (abs b)]
           (if (core/zero? b)
             a
             (recur b (rem a b)))))

       (defmethod g/gcd [js/BigInt js/BigInt] [a b]
         (bigint-gcd a b))

       (defmethod g/gcd [::v/native-integral js/BigInt] [a b]
         (bigint-gcd (js/BigInt a) b))

       (defmethod g/gcd [ js/BigInt ::v/native-integral] [a b]
         (bigint-gcd a (js/BigInt b))))

     (defmethod g/expt [::v/native-integral ::v/native-integral] [a b]
       (if (neg? b)
         (g/invert (u/compute-expt a (core/- b)))
         (u/compute-expt a b)))

     (defmethod g/div [::v/integral ::v/integral] [a b]
       (let [rem (g/remainder a b)]
         (if (v/zero? rem)
           (g/quotient a b)
           (r/rationalize a b))))

     (defmethod g/invert [::v/integral] [a]
       (if (v/one? a)
         a
         (r/rationalize 1 a)))))

;; Clojurescript and Javascript have a number of numeric types available that
;; don't respond true to number? These each require their own block of method
;; implementations.
#?(:cljs
   (do
     ;; native BigInt type in JS.
     (defmethod g/add [js/BigInt js/BigInt] [a b] (core/+ a b))
     (defmethod g/mul [js/BigInt js/BigInt] [a b] (core/* a b))
     (defmethod g/modulo [js/BigInt js/BigInt] [a b] (g/modulo-default a b))
     (defmethod g/sub [js/BigInt js/BigInt] [a b] (core/- a b))
     (defmethod g/negate [js/BigInt] [a] (core/- a))

     (defmethod g/expt [js/BigInt js/BigInt] [a b]
       (if (g/negative? b)
         (g/invert (js* "~{} ** ~{}" a (core/- b)))
         (js* "~{} ** ~{}" a b)))

     (defmethod g/abs [js/BigInt] [a] (if (neg? a) (core/- a) a))
     (defmethod g/quotient [js/BigInt js/BigInt] [a b] (core// a b))
     (defmethod g/remainder [js/BigInt js/BigInt] [a b] (js-mod a b))
     (defmethod g/magnitude [js/BigInt] [a]
       (if (neg? a) (core/- a) a))

     (defmethod g/div [js/BigInt js/BigInt] [a b]
       (let [rem (js-mod a b)]
         (if (v/zero? rem)
           (core// a b)
           (r/rationalize a b))))

     (doseq [op [g/add g/mul g/sub g/div g/expt g/modulo g/remainder g/quotient]]
       ;; Compatibility between js/BigInt and the other integral types.
       (defmethod op [js/BigInt ::v/integral] [a b]
         (op a (js/BigInt b)))

       (defmethod op [::v/integral js/BigInt] [a b]
         (op (js/BigInt a) b))

       ;; For NON integrals, we currently have no choice but to downcast the
       ;; BigInt to a floating point number.
       (defmethod op [js/BigInt ::v/floating-point] [a b]
         (op (js/Number a) b))

       (defmethod op [::v/floating-point js/BigInt] [a b]
         (op a (js/Number b))))

     ;; BigInt can't handle these operations natively, so we override with a
     ;; downcast to number for now.
     (doseq [op [g/cos g/sin g/tan
                 g/asin g/acos g/atan
                 g/cosh g/sinh g/tanh
                 g/asinh g/acosh g/acosh
                 g/cot g/sec g/csc g/sech g/csch
                 g/log g/exp g/sqrt]]
       (defmethod op [js/BigInt] [a]
         (op (js/Number a))))

     (defmethod g/atan [js/BigInt ::v/real] [l r] (g/atan (js/Number l) r))
     (defmethod g/atan [::v/real js/BigInt] [l r] (g/atan l (js/Number r)))
     (defmethod g/atan [js/BigInt js/BigInt] [l r] (g/atan (js/Number l) (js/Number r)))

     ;; Google Closure library's 64-bit Long:
     (defmethod g/add [Long Long] [a b] (.add a b))
     (defmethod g/mul [Long Long] [a b] (.multiply a b))
     (defmethod g/sub [Long Long] [^Long a ^Long b] (.subtract a b))
     (defmethod g/negate [Long] [^Long a] (.negate a))
     (defmethod g/abs [Long] [^Long a] (if (.isNegative a) (.negate a) a))
     (defmethod g/remainder [Long Long] [^Long a ^Long b] (.modulo a b))
     (defmethod g/magnitude [Long] [^Long a] (if (.isNegative a) (.negate a) a))

     ;; Implementation of exponent taken from Clojure's numeric tower's
     ;; expt-int:
     ;; https://github.com/clojure/math.numeric-tower/blob/master/src/main/clojure/clojure/math/numeric_tower.clj#L72
     (letfn [(long-expt [base pow]
               (loop [^Long n pow
                      ^Long y (Long/getOne)
                      ^Long z base]
                 (let [t (not (.isOdd n))
                       n ^Long (.shiftRight n 1)]
                   (cond
                     t (recur n y (.multiply z z))
                     (.isZero n) (.multiply z y)
                     :else (recur n (.multiply z y) (.multiply z z))))))]
       (defmethod g/expt [Long Long] [a ^Long b]
         (if (.isNegative b)
           (g/invert (long-expt a (.negate b)))
           (long-expt a b))))

     ;; Compatibility between basic number type and the google numeric types.
     ;; Any operation between a number and a Long or Integer will promote the
     ;; number.
     (doseq [op [g/add g/mul g/sub g/gcd g/lcm g/expt g/remainder g/quotient]]
       (defmethod op [Long ::v/native-integral] [a b]
         (op a (Long/fromNumber b)))

       (defmethod op [::v/native-integral Long] [a b]
         (op (Long/fromNumber a) b))

       ;; If this type encounters a floating point type it should lose
       ;; precision.
       (defmethod op [Long ::v/floating-point] [a b]
         (op (js/Number a) b))

       (defmethod op [::v/floating-point Long] [a b]
         (op a (js/Number b))))

     ;; Google Closure's arbitrary-precision Integer:
     (defmethod g/add [Integer Integer] [a b] (.add a b))
     (defmethod g/mul [Integer Integer] [a b] (.multiply a b))
     (defmethod g/sub [Integer Integer] [^Integer a ^Integer b] (.subtract a b))
     (defmethod g/negate [Integer] [^Integer a] (.negate a))
     (defmethod g/abs [Integer] [^Integer a] (if (.isNegative a) (.negate a) a))
     (defmethod g/remainder [Integer Integer] [^Integer a ^Integer b] (.modulo a b))
     (defmethod g/magnitude [Integer] [^Integer a] (if (.isNegative a) (.negate a) a))

     (letfn [(int-expt [base pow]
               (loop [^Integer n pow
                      ^Integer y (.-ONE Integer)
                      ^Integer z base]
                 (let [t (not (.isOdd n))
                       ^Integer n (.shiftRight n 1)]
                   (cond
                     t (recur n y (.multiply z z))
                     (.isZero n) (.multiply z y)
                     :else (recur n (.multiply z y) (.multiply z z))))))]
       (defmethod g/expt [Integer Integer] [a ^Integer b]
         (if (.isNegative b)
           (g/invert (int-expt a (.negate b)))
           (int-expt a b))))

     ;; Compatibility between basic number type and the google numeric types.
     ;; Any operation between a number and a Long or Integer will promote the
     ;; number.
     (doseq [op [g/add g/mul g/sub g/gcd g/lcm g/expt g/remainder g/quotient]]
       (defmethod op [Integer ::v/native-integral] [a b]
         (op a (Integer/fromNumber b)))

       (defmethod op [::v/native-integral Integer] [a b]
         (op (Integer/fromNumber a) b))

       ;; If this type encounters a floating point type it should lose
       ;; precision.
       (defmethod op [Integer ::v/floating-point] [a b]
         (op (js/Number a) b))

       (defmethod op [::v/floating-point Integer] [a b]
         (op a (js/Number b)))

       ;; When they encounter each other in binary operations, Long is coerced
       ;; to Integer.
       (defmethod op [Integer Long] [a b]
         (op a (Integer/fromNumber b)))

       (defmethod op [Long Integer] [a b]
         (op (Integer/fromNumber a) b)))

     ;; These names are slightly different between the two types.
     (defmethod g/quotient [Long Long] [^Long a ^Long b] (.div a b))
     (defmethod g/quotient [Integer Integer] [^Integer a ^Integer b] (.divide a b))))
