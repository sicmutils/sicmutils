#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.generic
  "The home of most of the Emmy extensible generic operations. The bulk of
  the others live in [[emmy.value]].

  See [the `Generics`
  cljdocs](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/basics/generics)
  for a detailed discussion of how to use and extend the generic operations
  defined in [[emmy.generic]] and [[emmy.value]]."
  (:refer-clojure :exclude [/ + - * divide infinite? abs])
  (:require [emmy.util :as u]
            [emmy.util.def :refer [defgeneric]]
            [emmy.value :as v]))

;; ## Generic Numerics
;;
;; The first section introduces generic versions of
;; Clojure's [[+]], [[-]], [[*]] and [[/]] operations. Any type that can
;; implement all four of these operations forms a
;; mathematical [Field](https://en.wikipedia.org/wiki/Field_(mathematics)).
;;
;; There are, of course, other technical names for types that can only implement
;; a subset of these operations, and more specializations of those names
;; depending on whether or not the implementation of these binary operations is
;; commutative or
;; associative. (See [Semigroup](https://en.wikipedia.org/wiki/Semigroup)
;; and [Monoid](https://en.wikipedia.org/wiki/Monoid) first, and start exploring
;; the realm of abstract algebra from there.)
;;
;; This library takes a permissive stance on extensibility. Types extend the
;; arithmetic operators by extending their unary or binary cases:
;;
;; - [[add]] for [[+]]
;; - [[sub]] and [[negate]] for [[-]]
;; - [[mul]] for [[*]]
;; - [[invert]] and [[div]] for [[/]]
;;
;; And the higher arity version reduces its list of arguments using this binary
;; operation. This makes it possible and easy to make the arithmetic operators
;; combine different types! It's up to you to do this in a mathematically
;; responsible way.
;;
;; Dispatch occurs via [[value/argument-kind]]. Documentation on how to extend
;; each generic operation to some new type is sparse. Have a look
;; at [[emmy.complex]] for an example of how to do this.

(defgeneric ^:no-doc add 2
  "Returns the sum of arguments `a` and `b`.

  See [[+]] for a variadic version of [[add]]."
  {:name '+
   :dfdx (fn [_ _] 1)
   :dfdy (fn [_ _] 1)})

(defn +
  "Generic implementation of `+`. Returns the sum of all supplied arguments. `(+)`
  returns 0, the additive identity.

  When applied between numbers, acts like `clojure.core/+`. Dispatch is open,
  however, making it possible to 'add' types wherever the behavior is
  mathematically sound.

  For example:

  ```clojure
  (+ [1 2 3] [2 3 4])
  ;;=> (up 3 5 7)
  ```"
  ([] 0)
  ([x] x)
  ([x y]
   (cond (v/numeric-zero? x) y
         (v/numeric-zero? y) x
         :else (add x y)))
  ([x y & more]
   (reduce + (+ x y) more)))

(defgeneric negate 1
  "Returns the negation of `a`.

  Equivalent to `(- (v/zero-like a) a)`."
  {:name '-
   :dfdx (fn [_] -1)})

(defgeneric ^:no-doc sub 2
  "Returns the difference of `a` and `b`.

  Equivalent to `(+ a (negate b))`.

  See [[-]] for a variadic version of [[sub]]."
  {:name '-
   :dfdx (fn [_ _] 1)
   :dfdy (fn [_ _] -1)})

(defmethod sub :default [a b]
  (add a (negate b)))

(defn -
  "Generic implementation of `-`.

  If one argument is supplied, returns the negation of `a`. Else returns the
  difference of the first argument `a` and the sum of all remaining
  arguments. `(-)` returns 0.

  When applied between numbers, acts like `clojure.core/-`. Dispatch is open,
  however, making it possible to 'subtract' types wherever the behavior is
  mathematically sound.

  For example:

  ```clojure
  (- [1 2 3] [2 3 4])
  ;;=> (up -1 -1 -1)

  (- [1 10])
  ;;=> (up -1 -10)
  ```"
  ([] 0)
  ([x] (negate x))
  ([x y]
   (cond (v/numeric-zero? y) x
         (v/numeric-zero? x) (negate y)
         :else (sub x y)))
  ([x y & more]
   (- x (apply + y more))))

(defgeneric ^:no-doc mul 2
  "Returns the product of `a` and `b`.

  See [[*]] for a variadic version of [[mul]]."
  {:name '*
   :dfdx (fn [_ y] y)
   :dfdy (fn [x _] x)})

;;; In the binary arity of [[*]] we test for exact (numerical) zero because it
;;; is possible to produce a wrong-type zero here, as follows:
;;;
;;;               |0|             |0|
;;;       |a b c| |0|   |0|       |0|
;;;       |d e f| |0| = |0|, not  |0|
;;;
;;; We are less worried about the v/zero? below,
;;; because any invertible matrix is square.

(defn *
  "Generic implementation of `*`. Returns the product of all supplied
  arguments. `(*)` returns 1, the multiplicative identity.

  When applied between numbers, acts like `clojure.core/*`. Dispatch is open,
  however, making it possible to 'multiply' types wherever the behavior is
  mathematically sound.

  For example:

  ```clojure
  (* 2 #sicm/complex \"3 + 1i\")
  ;;=> #sicm/complex \"6 + 2i\"
  ```"
  ([] 1)
  ([x] x)
  ([x y]
   (let [numx? (v/numerical? x)
         numy? (v/numerical? y)]
     (cond (and numx? (v/zero? x)) (v/zero-like y)
           (and numy? (v/zero? y)) (v/zero-like x)
           (and numx? (v/one? x)) y
           (and numy? (v/one? y)) x
           :else (mul x y))))
  ([x y & more]
   (reduce * (* x y) more)))

(declare div square)

(defgeneric invert 1
  "Returns the multiplicative inverse of `a`.

  Equivalent to `(/ 1 a)`."
  {:name '/
   :dfdx (fn [x] (div -1 (square x)))})

(def ^{:dynamic true
       :no-doc true}
  *in-default-invert*
  false)

(defmethod invert :default [a]
  (binding [*in-default-invert* true]
    (div 1 a)))

(defgeneric div 2
  "Returns the result of dividing `a` and `b`.

  Equivalent to `(* a (negate b))`.

  See [[/]] for a variadic version of [[div]]."
  {:name '/
   :dfdx (fn [_ y] (div 1 y))
   :dfdy (fn [x y] (div (negate x)
                       (square y)))})

(defmethod div :default [a b]
  (if *in-default-invert*
    (throw
     (ex-info "No implementation of [[invert]] or [[div]]."
              {:method 'div :args [a b]}))
    (mul a (invert b))))

(defn /
  "Generic implementation of `/`.

  If one argument is supplied, returns the multiplicative inverse of `a`. Else
  returns the result of dividing first argument `a` by the product of all
  remaining arguments. `(/)` returns 1, the multiplicative identity.

  When applied between numbers, acts like `clojure.core//`. Dispatch is open,
  however, making it possible to 'divide' types wherever the behavior is
  mathematically sound.

  For example:

  ```clojure
  (/ [2 4 6] 2)
  ;;=> (up 1 2 3)
  ```"
  ([] 1)
  ([x] (invert x))
  ([x y]
   (if (and (v/number? y) (v/one? y))
     x
     (div x y)))
  ([x y & more]
   (/ x (apply * y more))))

(def ^{:doc "Alias for [[/]]."}
  divide
  /)

(defgeneric exact-divide 2
  "Similar to the binary case of [[/]], but throws if `(v/exact? <result>)`
  returns false.")

;; ### Exponentiation, Log, Roots
;;
;; This next batch of generics covers various forms of exponentation and its
;; inverse.

(defgeneric exp 1
  "Returns the base-e exponential of `x`. Equivalent to `(expt e x)`, given
  some properly-defined `e` symbol."
  {:dfdx exp})

(defgeneric exp2 1
  "Returns the base-2 exponential of `x`. Equivalent to `(expt 2 x)`.")

(declare expt)

(defmethod exp2 :default [x] (expt 2 x))

(defgeneric exp10 1
  "Returns the base-10 exponential of `x`. Equivalent to `(expt 10 x)`.")

(defmethod exp10 :default [x] (expt 10 x))

(defgeneric log 1
  "Returns the natural logarithm of `x`."
  {:dfdx invert})

(defgeneric log2 1
  "Returns the base-2 logarithm of `x`, ie, $log_2(x)$.")

(let [l2 (Math/log 2)]
  (defmethod log2 :default [x] (div (log x) l2)))

(defgeneric log10 1
  "Returns the base-10 logarithm of `x`, ie, $log_10(x)$.")

(let [l10 (Math/log 10)]
  (defmethod log10 :default [x] (div (log x) l10)))

(declare negative?)

(defgeneric expt 2
  {:dfdx (fn [x y]
           (mul y (expt x (sub y 1))))
   :dfdy (fn [x y]
           (if (and (v/number? x) (v/zero? x))
             (if (v/number? y)
               (if (not (negative? y))
                 0
                 (u/illegal "Derivative undefined: expt"))
               0)
             (mul (log x) (expt x y))))})

(defn ^:no-doc default-expt
  "Default implementation of exponentiation for integral exponents `e`.

  This implementation uses ['Exponentation by
  Squaring'](https://en.wikipedia.org/wiki/Exponentiation_by_squaring), and will
  work for any type that implements `g/mul`.

  The multiplication operation is looked up once and cached at the beginning of
  computation."
  [s e]
  {:pre [(v/native-integral? e)]}
  (let [kind (v/kind s)]
    (if-let [mul' (get-method mul [kind kind])]
      (letfn [(expt' [base pow]
                (loop [n pow
                       y (v/one-like base)
                       z base]
                  (let [t (even? n)
                        n (quot n 2)]
                    (cond
                      t (recur n y (mul' z z))
                      (zero? n) (mul' z y)
                      :else (recur n (mul' z y) (mul' z z))))))]
        (cond (pos? e)  (expt' s e)
              (zero? e) (v/one-like e)
              :else (invert (expt' s (negate e)))))
      (u/illegal (str "No g/mul implementation registered for kind " kind)))))

;; [[expt]] can be defined (as a default) in terms of repeated multiplication,
;; if the exponent is a (native) integer. The native requirement is simply due
;; to the [[default-expt]] implementation above, which uses functions like
;; `quot` and `even?` that are not generic.
;;
;; For all other arguments, default [[expt]] requires that [[exp]], [[mul]]
;; and [[log]] be defined already on the type.

(defmethod expt :default [s e]
  (if (v/native-integral? e)
    (default-expt s e)
    (exp (mul e (log s)))))

(defgeneric square 1
  {:dfdx (fn [x] (mul 2 x))})
(defmethod square :default [x] (expt x 2))

(defgeneric cube 1
  {:dfdx (fn [x] (mul 3 (square x)))})
(defmethod cube :default [x] (expt x 3))

(defgeneric sqrt 1
  {:dfdx (fn [x]
           (invert
            (mul (sqrt x) 2)))})

(defmethod sqrt :default [x]
  (exp (mul (div 1 2)
            (log x))))

;; ## More Generics

(defgeneric negative? 1
  "Returns true if the argument `a` is less than `(v/zero-like a)`,
  false otherwise. The default implementation depends on a proper Comparable
  implementation on the type.`")

(defmethod negative? :default [a]
  (< a (v/zero-like a)))

(defgeneric infinite? 1
  "Returns true if `a` is either numerically infinite (ie, equal to `##Inf`) or
  a compound number (complex or quaterion, for example) with some infinite
  component.")

(defmethod infinite? :default [_] false)

(defgeneric abs 1)

(declare integer-part)

(defgeneric floor 1
  "Returns the largest integer less than or equal to `a`.

  Extensions beyond real numbers may behave differently; see the [Documentation
  site](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/basics/generics)
  for more detail.")

(defmethod floor :default [a]
  (if (negative? a)
    (sub (integer-part a) 1)
    (integer-part a)))

(defgeneric ceiling 1
  "Returns the result of rounding `a` up to the next largest integer.

  Extensions beyond real numbers may behave differently; see the [Documentation
  site](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/basics/generics)
  for more detail.")

(defmethod ceiling :default [a]
  (negate (floor (negate a))))

(defgeneric integer-part 1
  "Returns the integer part of `a` by removing any fractional digits.")

(defgeneric fractional-part 1
  "Returns the fractional part of the given value, defined as `x - ⌊x⌋`.

  For positive numbers, this is identical to `(- a (integer-part a))`. For
  negative `a`, because [[floor]] truncates toward negative infinity, you might
  be surprised to find that [[fractional-part]] returns the distance between `a`
  and the next-lowest integer:

```clojure
(= 0.6 (fractional-part -0.4))
```")

(defmethod fractional-part :default [a]
  (sub a (floor a)))

(defgeneric quotient 2)

(defn ^:no-doc modulo-default
  "The default implementation for [[modulo]] depends on the identity:

  x mod y == x - y ⌊x/y⌋

  This is the Knuth definition described
  by [Wikipedia](https://en.wikipedia.org/wiki/Modulo_operation)."
  [a b]
  (sub a (mul b (floor (div a b)))))

(defgeneric modulo 2
  "Returns the result of the
  mathematical [Modulo](https://en.wikipedia.org/wiki/Modulo_operation)
  operation between `a` and `b` (using the Knuth definition listed).

 The contract satisfied by [[modulo]] is:

```clojure
(= a (+ (* b (floor (/ a b)))
        (modulo a b)))
```

 For numbers, this differs from the contract offered by [[remainder]]
 because `(floor (/ a b))` rounds toward negative infinity, while
 the [[quotient]] operation in the contract for [[remainder]] rounds toward 0.

 The result will be either `0` or of the same sign as the divisor `b`.")

(defmethod modulo :default [a b]
  (modulo-default a b))

(defn ^:no-doc remainder-default [n d]
  (let [divnd (div n d)]
    (if (= (negative? n) (negative? d))
      (mul d (sub divnd (floor divnd)))
      (mul d (sub divnd (ceiling divnd))))))

(defgeneric remainder 2
  "Returns the remainder of dividing the dividend `a` by divisor `b`.

 The contract satisfied by [[remainder]] is:

```clojure
(= a (+ (* b (quotient a b))
        (remainder a b)))
```

 For numbers, this differs from the contract offered by [[modulo]]
 because [[quotient]] rounds toward 0, while `(floor (/ a b))` rounds toward
 negative infinity.

 The result will be either `0` or of the same sign as the dividend `a`.")

(defmethod remainder :default [n d]
  (remainder-default n d))

(defgeneric gcd 2
  "Returns the [greatest common
  divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) of the two
  inputs `a` and `b`.")

(defgeneric lcm 2
  "Returns the [least common
  multiple](https://en.wikipedia.org/wiki/Least_common_multiple) of the two
  inputs `a` and `b`.")

(defmethod lcm :default [a b]
  (let [g (gcd a b)]
    (if (v/zero? g)
      g
      (abs
       (* (exact-divide a g) b)))))

;; ### Trigonometric functions
;;
;; Thanks to John D Cook's post ['Bootstrapping a minimal math
;; library'](https://www.johndcook.com/blog/2021/01/05/bootstrapping-math-library/)
;; for his inspiration on the defaults and order of functions in the following
;; sections.

(declare sin)

(defgeneric cos 1
  "Returns the [cosine](https://en.wikipedia.org/wiki/Sine_and_cosine) of the
  supplied argument `a`."
  {:dfdx (fn [x] (negate (sin x)))})

(defgeneric sin 1
  "Returns the [sine](https://en.wikipedia.org/wiki/Sine_and_cosine) of the
  supplied argument `a`."
  {:dfdx cos})

;; Given [[sin]] and [[cos]] implementations, it's possible to define the rest
;; of the trigonometric functions with proper defaults.

(defgeneric tan 1
  "Computes the trigonometric tangent function of the supplied argument `a`.

Equivalent to `(/ (sin a) (cos a))`."
  {:dfdx (fn [x]
           (invert
            (square (cos x))))})

(defmethod tan :default [x]
  (div (sin x) (cos x)))

(declare csc)

(defgeneric cot 1
  "Computes the trigonometric cotangent function of the supplied argument `a`.

Equivalent to `(invert (tan a))`, or `(/ (cos a) (sin a))`."
  {:dfdx (fn [x]
           (negate
            (square (csc x))))})

(defmethod cot :default [x]
  (div (cos x) (sin x)))

(defgeneric sec 1
  "Computes the secant of the supplied argument `a`.

Equivalent to `(invert (cos a))`."
  {:dfdx (fn [x]
           (mul (sec x)
                (tan x)))})

(defmethod sec :default [x]
  (invert (cos x)))

(defgeneric csc 1
  "Computes the cosecant of the supplied argument `a`.

Equivalent to `(invert (sin a))`."
  {:dfdx (fn [x]
           (negate
            (mul (cot x)
                 (csc x))))})

(defmethod csc :default [x]
  (invert (sin x)))

;; ### Inverse Trig Functions

(defgeneric atan [1 2]
  "Computes the inverse tangent of the supplied argument `a`. Given two
  arguments `a` and `a`, returns the inverse tangent of the angle formed by the
  point `(a, b)` in a 2-dimensional euclidean plane.

  The two-argument version is sometimes
  called [Atan2](https://en.wikipedia.org/wiki/Atan2)."
  {:dfdx (fn
           ([x]
            (invert (add 1 (square x))))
           ([y x]
            (div x (add (square x)
                        (square y)))))
   :dfdy (fn [y x]
           (div (negate y)
                (add (square x)
                     (square y))))})

;; Given an [[atan]] implementation, types automatically gain default methods
;; for the rest of the inverse trig functions.

(defgeneric asin 1
  "Computes the inverse sine of the supplied argument `a`.

Defaults to `atan(x/sqrt(1-x^2))`."
  {:dfdx (fn [x]
           (invert
            (sqrt (sub 1 (square x)))))})

(defmethod asin :default [x]
  (atan (div x (sqrt (sub 1 (square x))))))

(defgeneric acos 1
  "Computes the inverse cosine of the supplied argument `a`.

Defaults to `atan(sqrt(1-x^2)/x)`."
  {:dfdx (fn [x]
           (negate
            (invert
             (sqrt (sub 1 (square x))))))})

(defmethod acos :default [x]
  (atan (div (sqrt (sub 1 (square x))) x)))

(defgeneric acot 1
  "Computes the [inverse
 cotangent](https://mathworld.wolfram.com/InverseCotangent.html) of the supplied
 argument `a`.

defaults to `pi/2 - atan(x)`."
  {:dfdx (fn [x]
           (negate
            (invert
             (add 1 (square x)))))})

(defmethod acot :default [x]
  (sub (/ Math/PI 2) (atan x)))

(defgeneric asec 1
  "Computes the [inverse
 secant](https://mathworld.wolfram.com/InverseSecant.html) of the supplied
 argument `a`.

defaults to `atan(sqrt(x^2 - 1))`."
  {:dfdx (fn [x]
           (invert
            (mul x (sqrt (sub (square x) 1)))))})

(defmethod asec :default [x]
  (atan (sqrt (sub (square x) 1))))

(defgeneric acsc 1
  "Computes the [inverse
 cosecant](https://mathworld.wolfram.com/InverseCosecant.html) of the supplied
 argument `a`.

defaults to `atan(1 / sqrt(x^2 - 1))`."
  {:dfdx (fn [x]
           (negate
            (invert
             (mul x (sqrt (sub (square x) 1))))))})

(defmethod acsc :default [x]
  (atan (invert
         (sqrt (sub (square x) 1)))))

;; ### Hyperbolic Trig

(declare sinh)

(defgeneric cosh 1
  "Computes the [hyperbolic
 cosine](https://mathworld.wolfram.com/HyperbolicCosine.html) of the supplied
 argument `a`.

defaults to `(e^x + e^{-x}) / 2`."
  {:dfdx sinh})

(defmethod cosh :default [x]
  (div (add (exp x)
            (exp (negate x)))
       2))

(defgeneric sinh 1
  "Computes the [hyperbolic
 sine](https://mathworld.wolfram.com/HyperbolicSine.html) of the supplied
 argument `a`.

defaults to `(e^x - e^{-x}) / 2`."
  {:dfdx cosh})

(defmethod sinh :default [x]
  (div (sub (exp x)
            (exp (negate x)))
       2))

(defgeneric tanh 1
  "Computes the [hyperbolic
 tangent](https://mathworld.wolfram.com/HyperbolicTangent.html) of the supplied
 argument `a`.

defaults to `sinh(x) / cosh(x)`."
  {:dfdx (fn [x]
           (sub 1 (square (tanh x))))})

(defmethod tanh :default [x]
  (let [exp2x (exp (add x x))]
    (div (sub exp2x 1)
         (add exp2x 1))))

(defgeneric sech 1
  "Computes the [hyperbolic
 secant](https://mathworld.wolfram.com/HyperbolicSecant.html) of the supplied
 argument `a`.

defaults to `1 / cosh(x)`."
  {:dfdx (fn [x]
           (negate
            (mul (sech x)
                 (tanh x))))})

(defmethod sech :default [x]
  (div 2 (add (exp x)
              (exp (negate x)))))

(declare csch)

(defgeneric coth 1
  "Computes the [hyperbolic
 cotangent](https://mathworld.wolfram.com/HyperbolicCotangent.html) of the supplied
 argument `a`.

defaults to `cosh(x) / sinh(x)`."
  {:dfdx (fn [x]
           (negate
            (square (csch x))))})

(defmethod coth :default [x]
  (let [exp2x (exp (add x x))]
    (div (add exp2x 1)
         (sub exp2x 1))))

(defgeneric csch 1
  "Computes the [hyperbolic
 cosecant](https://mathworld.wolfram.com/HyperbolicCosecant.html) of the supplied
 argument `a`.

defaults to `1 / sinh(x)`."
  {:dfdx (fn [x]
           (negate
            (mul (coth x)
                 (csch x))))})

(defmethod csch :default [x]
  (div 2 (sub (exp x)
              (exp (negate x)))))

;; ### Inverse Hyperbolic Functions

(defgeneric acosh 1
  "Computes the [inverse hyperbolic
 cosine](https://mathworld.wolfram.com/InverseHyperbolicCosine.html) of the supplied
 argument `a`.

defaults to `2 ln(sqrt((x+1)/2) + sqrt((x-1)/2))`."
  {:dfdx (fn [x]
           (invert
            (mul (sqrt (sub x 1))
                 (sqrt (add x 1)))))})

(defmethod acosh :default [x]
  (mul 2 (log (add
               (sqrt (div (add x 1) 2))
               (sqrt (div (sub x 1) 2))))))

(defgeneric asinh 1
  "Computes the [inverse hyperbolic
 sine](https://mathworld.wolfram.com/InverseHyperbolicSine.html) of the
 supplied argument `a`.

defaults to `ln(x + sqrt(1 + x^2))`."
  {:dfdx (fn [x]
           (invert
            (sqrt
             (add 1 (square x)))))})

(defmethod asinh :default [x]
  (log
   (add x (sqrt (add 1 (square x))))))

(defgeneric atanh 1
  "Computes the [inverse hyperbolic
 tangent](https://mathworld.wolfram.com/InverseHyperbolicTangent.html) of the
 supplied argument `a`.

defaults to `1/2 ln((1+x)/(1-x))`."
  {:dfdx (fn [x]
           (invert
            (sub 1 (square x))))})

(defmethod atanh :default [x]
  (div (sub (log (add 1 x))
            (log (sub 1 x)))
       2))

(defgeneric acoth 1
  "Computes the [inverse hyperbolic
 cotangent](https://mathworld.wolfram.com/InverseHyperbolicCotangent.html) of
 the supplied argument `a`.

defaults to `1/2 ln((x+1)/(x-1))`."
  {:dfdx (fn [x]
           (invert
            (sub 1 (square x))))})

(defmethod acoth :default [x]
  (div (sub (log (add x 1))
            (log (sub x 1)))
       2))

(defgeneric asech 1
  "Computes the [inverse hyperbolic
 secant](https://mathworld.wolfram.com/InverseHyperbolicSecant.html) of the
 supplied argument `a`.

defaults to `ln((1 + sqrt(1-x^2)) / x)`."
  {:dfdx (fn [x]
           (let [x+1 (add x 1)]
             (negate
              (invert
               (mul (mul x x+1)
                    (sqrt (div (sub 1 x)
                               x+1)))))))})

(defmethod asech :default [x]
  (log
   (div (add 1 (sqrt (sub 1 (square x))))
        x)))

(defgeneric acsch 1
  "Computes the [inverse hyperbolic
 cosecant](https://mathworld.wolfram.com/InverseHyperbolicCosecant.html) of the
 supplied argument `a`.

defaults to `ln((1 + sqrt(1+x^2)) / x)`."
  {:dfdx (fn [x]
           (negate
            (invert
             (mul x (sqrt (add (square x) 1))))))})

(defmethod acsch :default [x]
  (log
   (div (add 1 (sqrt (add 1 (square x))))
        x)))

;; ## Sinc and friends
;;
;; This section defines [[sinc]] and [[tanc]], and the hyperbolic
;; variants [[sinhc]] and [[tanhc]].

(defgeneric sinc 1
  "The unnormalized [sinc
  function](https://en.wikipedia.org/wiki/Sinc_function), equivalent to
  $\\frac{\\sin x}{x}$ but defined to be equal to 1 at $x = 0$.

  ### References

   - [Wikipedia page](https://en.wikipedia.org/wiki/Sinc_function)
   - [Mathworld page on Sinc](https://mathworld.wolfram.com/SincFunction.html)
   - [Boost notes on [[sinc]]
     and [[sinch]]](https://www.boost.org/doc/libs/1_65_0/libs/math/doc/html/math_toolkit/sinc/sinc_overview.html)"
  {:dfdx (fn [x]
           (if (v/zero? x)
             x
             (sub (div (cos x) x)
                  (div (sin x) (square x)))))})

(defmethod sinc :default [x]
  (if (v/zero? x)
    (v/one-like x)
    (div (sin x) x)))

;; NOTE that we don't define `cosc`. [This StackExchange
;; post](https://math.stackexchange.com/a/2137104) has a nice explanation of why
;; the analogous `cosc` doesn't belong: "The motivation for functions such as
;; $\sinc x$, $\sinch x$, $\tanc x$, $\tanch x$ is to consider the behaviour of
;; a ratio with limit 1 as $x \to 0$. There is no such motivation for
;; $\frac{\cos x}{x}$, since $\cos 0 = 1 \neq 0$."
;;
;; The Julia language does define a `cosc`, but strangely makes it equal to the
;; derivative of `sinc`, by analogy with `cos` being the derivative of `sin`.
;; This felt to me to be a step too far. Here's the [Julia manual page for
;; `cosc`](https://web.mit.edu/julia_v0.6.2/julia/share/doc/julia/html/en/stdlib/math.html#Base.Math.cosc).
;;
;; Wikipedia does have [a page on
;; `coshc`](https://en.wikipedia.org/wiki/Coshc_function), defined as
;; $\frac{\cosh x}{x}$.

(defgeneric tanc 1
  "`tanc` is defined, by analogy with [[sinc]], to be equal to $\\frac{\\tan
  x}{x}$ for nonzero $x$ and equal to 1 at $x = 0$.

  ### References

   - [Wikipedia page](https://en.wikipedia.org/wiki/Tanc_function)
   - [Mathworld page on Sinc](https://mathworld.wolfram.com/TancFunction.html)"
  {:dfdx (fn [x]
           (if (v/zero? x)
             x
             (let [sx (sec x)]
               (sub (div (* sx sx) x)
                    (div (tan x) (square x))))))})

(defmethod tanc :default [x]
  (if (v/zero? x)
    (v/one-like x)
    (div (tan x) x)))

;; ### Hyperbolic Variants

(defgeneric sinhc 1
  "The [sinhc function](https://en.wikipedia.org/wiki/Sinhc_function),
  equivalent to $\\frac{\\sinh x}{x}$ but defined to be equal to 1 at $x = 0$.

  ### References

   - [Wikipedia page](https://en.wikipedia.org/wiki/Sinhc_function)
   - [Mathworld page on Sinhc](https://mathworld.wolfram.com/SinhcFunction.html)"
  {:dfdx (fn [x]
           (if (v/zero? x)
             x
             (sub (div (cosh x) x)
                  (div (sinh x) (square x)))))})

(defmethod sinhc :default [x]
  (if (v/zero? x)
    (v/one-like x)
    (div (sinh x) x)))

(defgeneric tanhc 1
  "The [tanhc function](https://en.wikipedia.org/wiki/Tanhc_function),
  equivalent to $\\frac{\\tanh x}{x}$ but defined to be equal to 1 at $x = 0$.

  ### References

   - [Wikipedia page](https://en.wikipedia.org/wiki/Tanhc_function)
   - [Mathworld page on Tanhc](https://mathworld.wolfram.com/TanhcFunction.html)"
  {:dfdx (fn [x]
           (if (v/zero? x)
             x
             (let [sx (sech x)]
               (sub (div (* sx sx) x)
                    (div (tanh x) (square x))))))})

(defmethod tanhc :default [x]
  (if (v/zero? x)
    (v/one-like x)
    (div (tanh x) x)))

;; ## Complex Operators

(defgeneric make-rectangular 2)
(defgeneric make-polar 2)
(defgeneric real-part 1)
(defgeneric imag-part 1)
(defgeneric magnitude 1)
(defgeneric angle 1)
(defgeneric conjugate 1)

;; ## Operations on structures

(defgeneric transpose 1)
(defgeneric trace 1)
(defgeneric determinant 1)
(defgeneric dimension 1)
(defgeneric dot-product 2)
(defgeneric inner-product 2)
(defgeneric outer-product 2)
(defgeneric cross-product 2)

;; ## Structure Defaults

(defmethod transpose [::v/scalar] [a] a)
(defmethod trace [::v/scalar] [a] a)
(defmethod determinant [::v/scalar] [a] a)
(defmethod dimension [::v/scalar] [_] 1)
(defmethod dot-product [::v/scalar ::v/scalar] [l r] (mul l r))

;; Scalars include complex numbers, but for the purposes of dot/inner-products
;; these are interpreted as pairs of real numbers, where conjugate is identity.
;; So this seems to be a sane default.
(defmethod inner-product [::v/scalar ::v/scalar] [l r]
  (dot-product l r))

;; ## Solvers

(defgeneric solve-linear 2
  "For a given `a` and `b`, returns `x` such that `a*x = b`.

  See[[solve-linear-right]] for a similar function that solves for `a = x*b`.")

(defgeneric solve-linear-right 2
  "For a given `a` and `b`, returns `x` such that `a = x*b`.

  See[[solve-linear]] for a similar function that solves for `a*x = b`.")

(defn solve-linear-left
  "Alias for [[solve-linear]]; present for compatibility with the original
  `scmutils` codebase.

  NOTE: In `scmutils`, `solve-linear-left` and `solve-linear` act identically in
  all cases except matrices. `solve-linear-left` only accepted a column
  matrix (or up structure) in the `b` position, while `solve-linear` accepted
  either a column or row (up or down structure).

  In Emmy, both functions accept either type."
  [a b]
  (solve-linear a b))

;; ### Solver Defaults

(defmethod solve-linear [::v/scalar ::v/scalar] [x y] (div y x))
(defmethod solve-linear-right [::v/scalar ::v/scalar] [x y] (div x y))

;; ## More advanced generic operations

(def ^:no-doc derivative-symbol 'D)

(defgeneric partial-derivative 2)
(defgeneric Lie-derivative 1)

(defgeneric simplify 1)
(defmethod simplify :default [a] a)

;; This call registers a symbol for any non-multimethod we care about. These
;; will be returned instead of the actual function body when the user
;; calls `(v/freeze fn)`, for example.

(v/add-object-symbols!
 {+ '+
  * '*
  - '-
  / '/
  clojure.core/+ '+
  clojure.core/* '*
  clojure.core/- '-
  clojure.core// '/
  clojure.core/mod 'modulo
  clojure.core/quot 'quotient
  clojure.core/rem 'remainder
  clojure.core/neg? 'negative?
  #?@(:cljs [cljs.core/infinite? 'infinite?])
  clojure.core/< '<
  clojure.core/<= '<=
  clojure.core/> '>
  clojure.core/>= '>=
  clojure.core/= '=})
