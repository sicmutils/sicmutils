#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.special.factorial
  "Namespace holding implementations of variations on the factorial function."
  (:require [emmy.generic :as g]
            [emmy.numbers]
            #?@(:cljs [[emmy.util :as u]])
            [emmy.util.def :refer [defgeneric]]
            [emmy.value :as v]))

#?(:cljs
   (defn ->bigint
     "If `x` is a fixed-precision integer, returns a [[emmy.util/bigint]]
     version of `x`. Else, acts as identity.

     This is useful in cases where you may want to multiply `x` by other large
     numbers, but don't want to try and convert something that can't overflow,
     like a symbol, into `bigint`."
     [x]
     (if (int? x)
       (u/bigint x)
       x)))

(defn factorial
  "Returns the factorial of `n`, ie, the product of 1 to `n` (inclusive).

  [[factorial]] will return a platform-specific [[emmy.util/bigint]] given
  some `n` that causes integer overflow."
  [n]
  {:pre [(v/native-integral? n)
         (>= n 0)]}
  (let [elems (range 1 (inc n))]
    #?(:clj
       (apply *' elems)
       :cljs
       (if (<= n 20)
         (apply * elems)
         (transduce (map u/bigint) g/* elems)))))

;; ## Falling and Rising Factorials

(declare rising-factorial)

(defgeneric falling-factorial 2
  "Returns the [falling
  factorial](https://en.wikipedia.org/wiki/Falling_and_rising_factorials), of
  `a` to the `b`, defined as the polynomial

  $$(a)_b = a^{\\underline{b}} = a(a - 1)(a - 2) \\cdots (a - b - 1)$$

  Given a negative `b`, `([[falling-factorial]] a b)` is equivalent
  to `(invert ([[rising-factorial]] (inc a) (- b)))`, or `##Inf` if the
  denominator evaluates to 0.

  The coefficients that appear in the expansions of [[falling-factorial]] called
  with a symbolic first argument and positive integral second argument are the
  Stirling numbers of the first kind (see [[stirling-first-kind]]).")

(def ^{:doc "Alias for [[falling-factorial]]."}
  factorial-power
  falling-factorial)

;; The default implementation uses generic operations throughout, and requires
;; that `n` be a native integral.
;; [Wikipedia](https://en.wikipedia.org/wiki/Falling_and_rising_factorials#Connection_coefficients_and_identities)
;; states that "the rising and falling factorials are well defined in any unital
;; ring, and therefore x can be taken to be, for example, a complex number,
;; including negative integers, or a polynomial with complex coefficients, or
;; any complex-valued function." Implementing [[falling-factorial]] as a generic
;; allows all of this (and more!) to work automatically.
;;
;; (A unital ring is an abelian group - a type with a `+`, `-` and a sensible
;; zero - as well as a `*` operation that distributes over addition.
;; The "unital" part means there is a sensible one, ie, a multiplicative
;; identity.)

(defmethod falling-factorial :default [x n]
  {:pre [(v/native-integral? n)]}
  (cond (zero? n) 1
        (neg? n)
        (let [denom (rising-factorial (g/add x 1) (g/- n))]
          (if (v/zero? denom)
            ##Inf
            (g/invert denom)))

        :else
        (transduce (comp
                    (map #(g/add x (g/- %)))
                    #?(:cljs (map ->bigint)))
                   g/*
                   (range n))))

;; Given a native integral input, we can be more efficient by using `range` to
;; generate the product terms. This is PROBABLY a case of premature
;; optimization, since how fast does [[falling-factorial]] need to be? But if
;; you see a way to keep the speed while unifying these almost-the-same
;; implementations, please let me know and open a PR!

(defmethod falling-factorial [::v/native-integral ::v/native-integral] [x n]
  (cond (zero? n) 1
        (neg? n)
        (let [denom (rising-factorial (inc x) (- n))]
          (if (v/zero? denom)
            ##Inf
            (g// 1 denom)))

        :else
        (let [elems (range x (- x n) -1)]
          #?(:clj
             (apply *' elems)
             :cljs
             (transduce (map u/bigint) g/* elems)))))

(defgeneric rising-factorial 2
  "Returns the [rising
  factorial](https://en.wikipedia.org/wiki/Falling_and_rising_factorials), of
  `a` to the `b`, defined as the polynomial

  $$(a)^b = a^{\\overline{b}} = a(a + 1)(a + 2) \\cdots (a + b - 1)$$

  Given a negative `b`, `([[rising-factorial]] a b)` is equivalent
  to `(invert ([[falling-factorial]] (dec a) (- b)))`, or `##Inf` if the
  denominator evaluates to 0.")

(def ^{:doc "Alias for [[falling-factorial]]."}
  pochhammer
  rising-factorial)

(defmethod rising-factorial :default [x n]
  {:pre [(v/native-integral? n)]}
  (cond (zero? n) 1
        (neg? n)
        (let [denom (falling-factorial (g/sub x 1) (g/- n))]
          (if (v/zero? denom)
            ##Inf
            (g/invert denom)))

        :else
        (transduce (comp
                    (map #(g/add x %))
                    #?(:cljs (map ->bigint)))
                   g/*
                   (range n))))

(defmethod rising-factorial [::v/native-integral ::v/native-integral] [x n]
  (cond (zero? n) 1
        (neg? n)
        (let [denom (falling-factorial (dec x) (- n))]
          (if (v/zero? denom)
            ##Inf
            (g// 1 denom)))

        :else
        (let [elems (range x (+ x n))]
          #?(:clj
             (apply *' elems)
             :cljs
             (transduce (map u/bigint) g/* elems)))))

;; I learned about the next group of functions from John D Cook's [Variations on
;; Factorial](https://www.johndcook.com/blog/2010/09/21/variations-on-factorial/)
;; and [Multifactorial](https://www.johndcook.com/blog/2021/10/14/multifactorial/)
;; posts.

;; https://en.wikipedia.org/wiki/Double_factorial#Generalizations

(defn multi-factorial
  "Returns the product of the positive integers up to `n` that are congruent
  to `(mod n k)`.

  When `k` equals 1, equivalent to `([[factorial]] n)`.

  See the [Wikipedia page on generalizations
  of [[double-factorial]]](https://en.wikipedia.org/wiki/Double_factorial#Generalizations)
  for more detail.

  If you need to extend [[multi-factorial]] to negative `n` or `k`, that page
  has suggestions for generalization."
  [n k]
  {:pre [(v/native-integral? n)
         (v/native-integral? k)
         (>= n 0), (> k 0)]}
  (let [elems (range n 0 (- k))]
    #?(:clj
       (reduce *' elems)
       :cljs
       (transduce (map u/bigint) g/* elems))))

(defn double-factorial
  "Returns the product of all integers from 1 up to `n` that have the same
  parity (odd or even) as `n`.

  `([[double-factorial]] 0)` is defined as an empty product and equal to 1.

  [[double-factorial]] with argument `n` is equivalent to `([[multi-factorial]]
  n 2)`, but slightly more general in that it can handle negative values of
  `n`.

  If `n` is negative and even, returns `##Inf`.

  If `n` is negative and odd, returns `(/ (double-factorial (+ n 2)) (+ n 2))`.

  For justification, see the [Wikipedia page on the extension of double
  factorial to negative
  arguments](https://en.wikipedia.org/wiki/Double_factorial#Negative_arguments)."
  [n]
  {:pre [(v/native-integral? n)]}
  (cond (zero? n) 1
        (pos? n) (multi-factorial n 2)
        (even? n) ##Inf
        :else (g/div
               (double-factorial (+ n 2))
               (+ n 2))))

(defn subfactorial
  "Returns the number of permutations of `n` objects in which no object appears in
  its original position. (Each of these permutations is called
  a ['derangement'](https://en.wikipedia.org/wiki/Derangement) of the set.)

  ## References

  - [Subfactorial page at Wolfram Mathworld](https://mathworld.wolfram.com/Subfactorial.html)
  - John Cook, [Variations on Factorial](https://www.johndcook.com/blog/2010/09/21/variations-on-factorial/)
  - John Cook, [Subfactorial](https://www.johndcook.com/blog/2010/04/06/subfactorial/)
  - ['Derangement' on Wikipedia](https://en.wikipedia.org/wiki/Derangement)"
  [n]
  (if (zero? n)
    1
    (let [nf-div-e (g/div (factorial n) Math/E)]
      (g/floor
       (g/add 0.5 nf-div-e)))))

(let [mul #?(:clj * :cljs g/*)
      div #?(:clj / :cljs g//)]
  (defn binomial-coefficient
    "Returns the [binomial
  coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient), ie, the
  coefficient of the $x^k$ term in the polynomial expansion of the binomial
  power $(1 + x)^n$.

  This quantity is sometimes pronounced \"n choose k\".

  For negative `n` or `k`, [[binomial-coefficient]] matches the behavior
  provided by Mathematica, described at [this
  page](https://mathworld.wolfram.com/BinomialCoefficient.html). Given negative
  `n`, returns

  ```clj
  ;; for k >= 0
  (* (expt -1 k)
     (binomial-coefficient (+ (- n) k -1) k))

  ;; for k >= 0
  (* (expt -1 (- n k))
     (binomial-coefficient (+ (- k) -1) (- n k)))

  ;; otherwise:
  0
  ```"
    [n k]
    {:pre [(v/native-integral? n)
           (v/native-integral? k)]}
    (cond (zero? k) 1
          (neg? n)
          (cond (> k 0) (mul
                         (if (even? k) 1 -1)
                         (binomial-coefficient
                          (+ (- n) k -1) k))

                (<= k n) (let [n-k (- n k)]
                           (mul
                            (if (even? n-k) 1 -1)
                            (binomial-coefficient
                             (- (- k) 1) n-k)))
                :else 0)
          (neg? k) 0

          (> k n) 0

          :else
          (let [k (min k (- n k))]
            (div (falling-factorial n k)
                 (factorial k))))))

(let [add   #?(:clj +' :cljs g/+)
      mul   #?(:clj *' :cljs g/*)]
  (defn stirling-first-kind
    "Given `n` and `k`, returns the number of permutations of `n` elements which
  contain exactly `k` [permutation
  cycles](https://mathworld.wolfram.com/PermutationCycle.html). This is called
  the [Stirling number s(n, k) of the first
  kind](https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind).

  By default, returns the [signed Stirling number of the first
  kind](https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind#Signs).
  Pass the `:unsigned? true` keyword option to retrieve the signed Stirling
  number. (Or take the absolute value of the result...)

  ```clj
  (stirling-first-kind 13 2)
  ;;=> -1486442880

  (stirling-first-kind 13 2 :unsigned? true)
  ;;=> 1486442880
  ```"
    [n k & {:keys [unsigned?]}]
    {:pre [(v/native-integral? n)
           (v/native-integral? k)
           (<= 0 k) (<= 0 n)]}
    (let [rec  (atom nil)
          rec* (fn [n k]
                 (if (zero? n)
                   (if (zero? k) 1 0)
                   (let [n-1    (dec n)
                         factor (if unsigned? n-1 (- n-1))]
                     (if (zero? factor)
                       (@rec n-1 (dec k))
                       (add (@rec n-1 (dec k))
                            (mul factor
                                 #?(:cljs (u/bigint (@rec n-1 k))
                                    :clj  (@rec n-1 k))))))))]
      (reset! rec (memoize rec*))
      (cond (zero? k) (if (zero? n) 1 0)
            (> k n) 0
            :else (@rec n k))))

  (defn stirling-second-kind
    "Returns $S(n,k)$, the number of ways to partition a set of `n` objects into `k`
  non-empty subsets.

  This is called a [Stirling number of the second
  kind](https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind)."
    [n k]
    {:pre [(v/native-integral? n)
           (v/native-integral? k)
           (<= 0 k) (<= 0 n)]}
    (let [rec (atom nil)
          rec* (fn [n k]
                 (cond (= k 1) 1
	                     (= n k) 1
	                     :else
	                     (let [n-1 (dec n)]
		                     (add
                          (mul k #?(:cljs (u/bigint (@rec n-1 k))
                                    :clj  (@rec n-1 k)))
		                      (@rec n-1 (dec k))))))]
      (reset! rec (memoize rec*))
      (cond (zero? k) (if (zero? n) 1 0)
            (> k n) 0
            :else (@rec n k))))

  (defn bell
    "Returns the `n`th [Bell number](https://en.wikipedia.org/wiki/Bell_number), ie,
  the number of ways a set of `n` elements can be partitioned into nonempty
  subsets.

  The `n`th Bell number is denoted $B_n$."
    [n]
    {:pre [(>= n 0)]}
    (let [xform (map #(stirling-second-kind n %))
          ks    (range (inc n))]
      (transduce xform add ks))))
