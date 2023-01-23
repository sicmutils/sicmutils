#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.rational-function
  (:refer-clojure :exclude [abs])
  (:require [clojure.set :as set]
            [emmy.complex :refer [complex?]]
            [emmy.expression :as x]
            [emmy.expression.analyze :as a]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.numsymb :as sym]
            [emmy.polynomial :as p]
            [emmy.polynomial.impl :as pi]
            [emmy.ratio :as r]
            [emmy.rational-function.interpolate :as ri]
            [emmy.structure :as ss]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj Seqable))))

;; ## Rational Functions
;;
;; This namespace contains an implementation of rational functions, or rational
;; fractions; the data structure wraps a numerator `u` and denominator `v` where
;; either or both are instances of [[p/Polynomial]]. (If both become
;; non-polynomial, the functions in this namespace drop the [[RationalFunction]]
;; instance down to whatever type is produced by `(g/divide u v)`).
;;
;; The [[RationalFunction]] type wraps up `u` and `v`, the `arity` of the
;; rational function (which must match the arity of `u` and `v`) and optional
;; metadata `m`.

(declare evaluate eq)

(deftype RationalFunction [arity u v m]
  f/IArity
  (arity [_] [:between 0 arity])

  r/IRational
  (numerator [_] u)
  (denominator [_] v)



  v/Value
  (zero? [_] (v/zero? u))
  (one? [_] (and (v/one? u) (v/one? v)))
  (identity? [_] (and (v/identity? u) (v/one? v)))

  (zero-like [_] (v/zero-like u))
  (one-like [_] (v/one-like u))
  (identity-like [_]
    (RationalFunction. arity
                       (v/identity-like u)
                       (v/one-like v)
                       m))
  (exact? [_] false)
  (freeze [_] (list '/ (v/freeze u) (v/freeze v)))
  (kind [_] ::rational-function)

  #?@(:clj
      [Object
       (equals [this that] (eq this that))
       (toString [_] (pr-str (list '/ u v)))

       IObj
       (meta [_] m)
       (withMeta [_ meta] (RationalFunction. arity u v meta))

       Seqable
       (seq [_] (list u v))

       IFn
       (invoke [this]
               (evaluate this []))
       (invoke [this a]
               (evaluate this [a]))
       (invoke [this a b]
               (evaluate this [a b]))
       (invoke [this a b c]
               (evaluate this [a b c]))
       (invoke [this a b c d]
               (evaluate this [a b c d]))
       (invoke [this a b c d e]
               (evaluate this [a b c d e]))
       (invoke [this a b c d e f]
               (evaluate this [a b c d e f]))
       (invoke [this a b c d e f g]
               (evaluate this [a b c d e f g]))
       (invoke [this a b c d e f g h]
               (evaluate this [a b c d e f g h]))
       (invoke [this a b c d e f g h i]
               (evaluate this [a b c d e f g h i]))
       (invoke [this a b c d e f g h i j]
               (evaluate this [a b c d e f g h i j]))
       (invoke [this a b c d e f g h i j k]
               (evaluate this [a b c d e f g h i j k]))
       (invoke [this a b c d e f g h i j k l]
               (evaluate this [a b c d e f g h i j k l]))
       (invoke [this a b c d e f g h i j k l m]
               (evaluate this [a b c d e f g h i j k l m]))
       (invoke [this a b c d e f g h i j k l m n]
               (evaluate this [a b c d e f g h i j k l m n]))
       (invoke [this a b c d e f g h i j k l m n o]
               (evaluate this [a b c d e f g h i j k l m n o]))
       (invoke [this a b c d e f g h i j k l m n o p]
               (evaluate this [a b c d e f g h i j k l m n o p]))
       (invoke [this a b c d e f g h i j k l m n o p q]
               (evaluate this [a b c d e f g h i j k l m n o p q]))
       (invoke [this a b c d e f g h i j k l m n o p q r]
               (evaluate this [a b c d e f g h i j k l m n o p q r]))
       (invoke [this a b c d e f g h i j k l m n o p q r s]
               (evaluate this [a b c d e f g h i j k l m n o p q r s]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t]
               (evaluate this [a b c d e f g h i j k l m n o p q r s t]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
               (evaluate this (into [a b c d e f g h i j k l m n o p q r s t] rest)))
       (applyTo [this xs] (AFn/applyToHelper this xs))]

      :cljs
      [Object
       (toString [_] (str u " : " v))

       IEquiv
       (-equiv [this that] (eq this that))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ m] (RationalFunction. arity u v m))

       ISeqable
       (-seq [_] (list u v))

       IFn
       (-invoke [this]
                (evaluate this []))
       (-invoke [this a]
                (evaluate this [a]))
       (-invoke [this a b]
                (evaluate this [a b]))
       (-invoke [this a b c]
                (evaluate this [a b c]))
       (-invoke [this a b c d]
                (evaluate this [a b c d]))
       (-invoke [this a b c d e]
                (evaluate this [a b c d e]))
       (-invoke [this a b c d e f]
                (evaluate this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (evaluate this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (evaluate this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (evaluate this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (evaluate this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (evaluate this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (evaluate this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (evaluate this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (evaluate this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (evaluate this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (evaluate this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (evaluate this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (evaluate this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (evaluate this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (evaluate this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (evaluate this (into [a b c d e f g h i j k l m n o p q r s t] rest)))

       IPrintWithWriter
       (-pr-writer
        [x writer _]
        (write-all writer
                   "#object[emmy.rational-function.RationalFunction \""
                   (.toString x)
                   "\"]"))]))

(defn rational-function?
  "Returns true if the supplied argument is an instance of [[RationalFunction]],
  false otherwise."
  [r]
  (instance? RationalFunction r))

(defn coeff?
  "Returns true if `x` is explicitly _not_ an instance of [[RationalFunction]]
  or [[polynomial/Polynomial]], false if it is."
  [x]
  (and (not (rational-function? x))
       (p/coeff? x)))

(defn ^:no-doc bare-arity
  "Given a [[RationalFunction]] instance `rf`, returns the `arity` field."
  [rf]
  (.-arity ^RationalFunction rf))

(defn ^:no-doc bare-u
  "Given a [[RationalFunction]] instance `rf`, returns the `u` (numerator) field."
  [rf]
  (.-u ^RationalFunction rf))

(defn ^:no-doc bare-v
  "Given a [[RationalFunction]] instance `rf`, returns the `v` (denominator) field."
  [^RationalFunction rf]
  (.-v rf))

(defn arity
  "Returns the declared arity of the supplied [[RationalFunction]]
  or [[polynomial/Polynomial]], or `0` for arguments of other types."
  [r]
  (if (rational-function? r)
    (bare-arity r)
    (p/arity r)))

(defn- check-same-arity
  "Given two inputs `u` and `v`, checks that their arities are equal and returns
  the value, or throws an exception if not.

  If either `p` or `q` is a coefficient with [[arity]] equal to
  0, [[check-same-arity]] successfully returns the other argument's arity."
  [u v]
  (let [ua (arity u)
        va (arity v)]
    (cond (zero? ua) va
          (zero? va) ua
          (= ua va) ua
          :else (u/illegal (str "Unequal arities: " u ", " v)))))

(defn negative?
  "Returns true if the numerator of `r` is [[polynomial/negative?]], false
  otherwise."
  [r]
  (if-not (rational-function? r)
    (p/negative? r)
    (p/negative? (bare-u r))))

(defn eq
  "Returns true if the [[RationalFunction]] this is equal to `that`. If `that` is
  a [[RationalFunction]], `this` and `that` are equal if they have equal `u` and
  `v` and equal arity. `u` and `v` entries are compared
  using [[emmy.value/=]].

  If `that` is non-[[RationalFunction]], `eq` only returns true if `u` and `v`
  respectively match the [[ratio/numerator]] and [[ratio/denominator]] of
  `that`."
  [^RationalFunction this that]
  (if (instance? RationalFunction that)
    (let [that ^RationalFunction that]
      (and (= (.-arity this) (.-arity that))
           (v/= (.-u this) (.-u that))
           (v/= (.-v this) (.-v that))))

    (and (v/= (.-v this) (r/denominator that))
         (v/= (.-u this) (r/numerator that)))))

;; ## Constructors

(defn- make-reduced
  "Accepts an explicit `arity`, numerator `u` and denominator `v` and returns
  either:

  - `0`, in the case of a [[value/zero?]] numerator
  - `u`, in the case of a [[value/one?]] denominator
  - a [[RationalFunction]] instance if _either_ `u` or `v` is a [[polynomial/Polynomial]]
  - `(g/div u v)` otherwise.

  Call this function when you've already reduced `u` and `v` such that they
  share no common factors and are dropped down to coefficients if possible, and
  want to wrap them in [[RationalFunction]] only when necessary.

  NOTE: The behavior of this mildly-opinionated constructor is similar
  to [[polynomial/terms->polynomial]]"
  [arity u v]
  (cond (v/zero? u) 0
        (v/one? v)  u

        (or (p/polynomial? u)
            (p/polynomial? v))
        (->RationalFunction arity u v nil)

        :else (g/div u v)))

(defn- coef-sgn
  "Returns `1` if the input is non-numeric or numeric and non-negative, `-1`
  otherwise. In the slightly suspect case of a complex number
  input, [[coef-sgn]] only examines the [[generic/real-part]] of the complex
  number.

  NOTE Negative [[RationalFunction]] instances attempt to keep the negative sign
  in the numerator `u`. The complex number behavior is a kludge, but allows
  canonicalization with complex coefficients."
  [x]
  (cond (v/real? x)
        (if (g/negative? x) -1 1)

        (complex? x)
        (if (g/negative? (g/real-part x)) -1 1)

        :else 1))

(defn ^:no-doc ->reduced
  "Given a numerator `u` and denominator `v`, returns the result of:

  - multiplying `u` and `v` by the least common multiple of all denominators
    found in either `u` or `v`, so that `u` and `v` contain
    no [[RationalFunction]]e or ratio-like coefficients
  - normalizing the denominator `v` to be positive by negating `u`, if
    applicable
  - Cancelling out any common divisors between `u` and `v`

  The result can be either a [[RationalFunction]], [[polynomial/Polynomial]] or
  a `(g/div u v)`. See [[make-reduced]] for the details."
  [u v]
  (when (v/zero? v)
    (u/arithmetic-ex
     (str "Can't form rational function with zero denominator: " v)))
  (let [a (check-same-arity u v)
        xform (comp (distinct)
                    (map r/denominator))
        coefs (concat
               (p/coefficients u)
               (p/coefficients v))
        factor (transduce xform (completing g/lcm) 1 coefs)
        factor (if (= 1 (coef-sgn
                         (p/leading-coefficient v)))
                 factor
                 (g/negate factor))
        [u' v'] (if (v/one? factor)
                  [u v]
                  [(g/mul factor u)
                   (g/mul factor v)])
        g (g/gcd u' v')
        [u'' v''] (if (v/one? g)
                    [u' v']
                    [(p/evenly-divide u' g)
                     (p/evenly-divide v' g)])]
    (make-reduced a u'' v'')))

(defn make
  "Given a numerator `u` and denominator `v`, attempts to form
  a [[RationalFunction]] instance by

  - cancelling out any common factors between `u` and `v`
  - normalizing `u` and `v` such that `v` is always positive
  - multiplying `u` and `v` through by a commo factor, such that neither term
    contains any rational coefficients

  Returns a [[RationalFunction]] instance if either `u` or `v` remains
  a [[polynomial/Polynomial]] after this process; else, returns `(g/div u' v')`,
  where `u'` and `v'` are the reduced numerator and denominator."
  [u v]
  (if (and (coeff? u) (coeff? v))
    (g/div u v)
    (->reduced u v)))

;; ## Rational Function Arithmetic
;;
;; The goal of this section is to implement `+`, `-`, `*` and `/` for rational
;; function instances. The catch is that we want to hold to the contract
;; that [[make]] provides - numerator and denominator should not acquire their
;; own internal denominators! - without explicitly calling the
;; expensive [[make]] each time.
;;
;; The Rational arithmetic algorithms used below come from Knuth, vol 2, section
;; 4.5.1.

(defn- binary-combine
  "Given two arguments `u` and `v`, as well as:

  - `poly-op` - a function of two numerators
  - `uv-op` - a function of four arguments, (`u-n`, `u-d`, `v-n`, `v-d` the
    numerator and denominator of `u` and `v` respectively)

  Returns the result of `(poly-op u-n v-n)` if `u-d` and `v-d` are
  both [[value/one?]], or `(uv-op u-n u-d v-n v-d)` otherwise.

  The result is reduced to a potentially-non-[[RationalFunction]] result
  using [[make-reduced]]."
  [u v poly-op uv-op]
  (let [a    (check-same-arity u v)
        u-n  (r/numerator u)
        u-d  (r/denominator u)
        v-n  (r/numerator v)
        v-d  (r/denominator v)
        [n d] (if (and (v/one? u-d) (v/one? v-d))
                [(poly-op u-n v-n) 1]
                (uv-op u-n u-d v-n v-d))]
    (make-reduced a n d)))

;; The following functions act on full numerator, denominator pairs, and are
;; suitable for use as the `uv-op` argument to [[binary-combine]].

(defn- uv:+
  "Returns the `[numerator, denominator]` pair resulting from rational function
  addition of `(/ u u')` and `(/ v v')`.

  If the denominators are equal, [[uv:+]] adds the numerators and divides out
  any factor common with the shared denominator.

  Else, if the denominators are relatively prime, [[uv:+]] multiplies each side
  by the other's denominator to create a single rational expression, then
  divides out any common factors before returning.

  In the final case, where the denominators are _not_ relatively prime, [[uv:+]]
  attempts to efficiently divide out the GCD of the denominators without
  creating large products."
  [u u' v v']
  (letfn [(divide-through [n d]
            (if (v/zero? n)
              [0 1]
              (let [g (g/gcd d n)]
                (if (v/one? g)
                  [n d]
                  [(p/evenly-divide n g)
                   (p/evenly-divide d g)]))))]
    (if (v/= u' v')
      ;; Denominators are equal:
      (let [n (p/add u v)]
        (divide-through n u'))
      (let [g (g/gcd u' v')]
        (if (v/one? g)
          ;; Denominators are relatively prime:
          (divide-through
           (p/add (p/mul u v')
                  (p/mul u' v))
           (p/mul u' v'))

          ;; Denominators are NOT relatively prime:
          (let [u':g (p/evenly-divide u' g)
                v':g (p/evenly-divide v' g)]
            (divide-through
             (p/add (p/mul u v':g)
                    (p/mul u':g v))
             (p/mul u':g v'))))))))

(defn- uv:-
  "Returns the `[numerator, denominator]` pair resulting from rational function
  difference of `(/ u u')` and `(/ v v')`.

  Similar to [[uv:+]]; inverts `v` before calling [[uv:+]] with the supplied arguments."
  [u u' v v']
  (uv:+ u u' (p/negate v) v'))

(defn- uv:*
  "Returns the `[numerator, denominator]` pair resulting from rational function
  multiplication of `(/ u u')` and `(/ v v')`."
  [u u' v v']
  (if (or (v/zero? u) (v/zero? v))
    [0 1]
    (let [d1 (g/gcd u v')
          d2 (g/gcd u' v)
          u'' (p/mul (p/evenly-divide u d1)
                     (p/evenly-divide v d2))
          v'' (p/mul (p/evenly-divide u' d2)
                     (p/evenly-divide v' d1))]
      [u'' v''])))

(defn- uv:gcd
  "Returns the `[numerator, denominator]` pair that represents the greatest common
  divisor of `(/ u u')` and `(/ v v')`."
  [u u' v v']
  (let [d1     (g/gcd u v)
        d2     (g/lcm u' v')
        result (make d1 d2)]
    [(r/numerator result)
     (r/denominator result)]))

;; ## RationalFunction versions
;;
;; Armed with [[binary-combine]] and the functions above, we can now implement
;; the full set of arithmetic functions for [[RationalFunction]] instances.

(defn negate
  "Returns the negation of rational function `r`, ie, a [[RationalFunction]] with
  its numerator negated.

  Acts as [[generic/negate]] for non-[[RationalFunction]] inputs."
  [r]
  (if-not (rational-function? r)
    (p/negate r)
    (->RationalFunction (bare-arity r)
                        (p/negate (bare-u r))
                        (bare-v r)
                        (meta r))))

(defn abs
  "If the numerator of `r` is negative, returns `(negate r)`, else acts as
  identity."
  [r]
  (if (negative? r)
    (negate r)
    r))

(defn add
  "Returns the sum of rational functions `r` and `s`, with appropriate handling
  of [[RationalFunction]], [[polynomial/Polynomial]] or coefficients of neither
  type on either side."
  [r s]
  (cond (v/zero? r) s
        (v/zero? s) r
        :else (binary-combine r s p/add uv:+)))



(defn sub
  "Returns the difference of rational functions `r` and `s`, with appropriate
  handling of [[RationalFunction]], [[polynomial/Polynomial]] or coefficients of
  neither type on either side."
  [r s]
  (cond (v/zero? r) (negate s)
        (v/zero? s) r
        :else (binary-combine r s p/sub uv:-)))

(defn mul
  "Returns the product of rational functions `r` and `s`, with appropriate
  handling of [[RationalFunction]], [[polynomial/Polynomial]] or coefficients of
  neither type on either side."
  [r s]
  (cond (v/zero? r) r
        (v/zero? s) s
        (v/one? r) s
        (v/one? s) r
        :else (binary-combine r s p/mul uv:*)))

(defn square
  "Returns the square of rational function `r`. Equivalent to `(mul r r)`."
  [r]
  (if-not (rational-function? r)
    (p/square r)
    (->RationalFunction (bare-arity r)
                        (p/square (bare-u r))
                        (p/square (bare-v r))
                        (meta r))))

(defn cube
  "Returns the cube of rational function `r`. Equivalent to `(mul r (mul r r))`."
  [r]
  (if-not (rational-function? r)
    (p/cube r)
    (->RationalFunction (bare-arity r)
                        (p/cube (bare-u r))
                        (p/cube (bare-v r))
                        (meta r))))

(defn expt
  "Returns a rational function generated by raising the input rational function
  `r` to the (integer) power `n`."
  [r n]
  {:pre [(v/native-integral? n)]}
  (if-not (rational-function? r)
    (p/expt r n)
    (let [u (bare-u r)
          v (bare-v r)
          [top bottom e] (if (neg? n)
                           [v u (- n)]
                           [u v n])]
      (->RationalFunction (bare-arity r)
                          (p/expt top e)
                          (p/expt bottom e)
                          (meta r)))))

(defn invert
  "Given some rational function `r`, returns the inverse of `r`, ie, a rational
  function with numerator and denominator reversed. The returned rational
  function guarantees a positive denominator.

  Acts as [[generic/invert]] for non-[[RationalFunction]] inputs."
  [r]
  (if-not (rational-function? r)
    (g/invert r)
    (let [u (bare-u r)
          v (bare-v r)]
      (cond (v/zero? u)
            (u/arithmetic-ex
             "Can't form rational function with zero denominator.")

            (g/negative? u)
            (->RationalFunction (bare-arity r)
                                (g/negate v)
                                (g/negate u)
                                (meta r))

            :else (->RationalFunction (bare-arity r) v u (meta r))))))

(defn div
  "Returns the quotient of rational functions `r` and `s`, with appropriate
  handling of [[RationalFunction]], [[polynomial/Polynomial]] or coefficients of
  neither type on either side."
  [r s]
  (mul r (invert s)))

(defn gcd
  "Returns the greatest common divisor of rational functions `r` and `s`, with
  appropriate handling of [[RationalFunction]], [[polynomial/Polynomial]] or
  coefficients of neither type on either side. "
  [r s]
  (binary-combine r s g/gcd uv:gcd))

;; ## Function Evaluation, Composition
;;
;; The following functions provide the ability to compose rational functions
;; together without wrapping them in black-box functions.

(defn evaluate
  "Given some rational function `xs` and a sequence of arguments with length >= 0
  and < the [[arity]] of `r`, returns the result of evaluating the numerator and
  denominator using `xs` and re-forming a rational function with the results.

  Supplying fewer arguments than the arity will result in a partial evaluation.
  Supplying too many arguments will error."
  [r xs]
  (if-not (rational-function? r)
    (p/evaluate r xs)
    (g/div (p/evaluate (bare-u r) xs)
           (p/evaluate (bare-v r) xs))))

(defn arg-scale
  "Given some [[RationalFunction]] `r`, returns a new [[RationalFunction]]
  generated by substituting each indeterminate `x_i` for `f_i * x_i`, where
  `f_i` is a factor supplied in the `factors` sequence.

  Given a non-[[RationalFunction]], delegates to [[polynomial/arg-scale]]."
  [r factors]
  (if-not (rational-function? r)
    (p/arg-scale r factors)
    (div (p/arg-scale (bare-u r) factors)
         (p/arg-scale (bare-v r) factors))))

(defn arg-shift
  "Given some [[RationalFunction]] `r`, returns a new [[RationalFunction]]
  generated by substituting each indeterminate `x_i` for `s_i + x_i`, where
  `s_i` is a shift supplied in the `shifts` sequence.

  Given a non-[[RationalFunction]], delegates to [[polynomial/arg-shift]]."
  [r shifts]
  (if-not (rational-function? r)
    (p/arg-shift r shifts)
    (div (p/arg-shift (bare-u r) shifts)
         (p/arg-shift (bare-v r) shifts))))

;; ## Derivatives

(defn partial-derivative
  "Given some [[RationalFunction]] or [[polynomial/Polynomial]] `r`, returns the
  partial derivative of `r` with respect to the `i`th indeterminate. Throws if
  `i` is an invalid indeterminate index for `r`.

  For non-polynomial or rational function inputs, returns `0`."
  [r i]
  (if-not (rational-function? r)
    (p/partial-derivative r i)
    (let [u (bare-u r)
          v (bare-v r)]
      (div (p/sub (p/mul (p/partial-derivative u i) v)
                  (p/mul u (p/partial-derivative v i)))
           (p/square v)))))

(defn partial-derivatives
  "Returns the sequence of partial derivatives
  of [[RationalFunction]] (or [[polynomial/Polynomial]]) `r` with respect to
  each indeterminate. The returned sequence has length equal to the [[arity]] of
  `r`.

  For non-polynomial or rational function inputs, returns an empty sequence."
  [r]
  (if-not (rational-function? r)
    (p/partial-derivatives r)
    (for [i (range (bare-arity r))]
      (partial-derivative r i))))

;; ## Canonicalizer
;;
;; This section defines functions that allow conversion back and forth
;; between [[RationalFunction]] instances and symbolic expressions.
;;
;; The `operator-table` represents the operations that can be understood from
;; the point of view of a rational function over some field.

(def ^{:no-doc true
       :doc "These operations are those allowed
       between [[RationalFunction]], [[polynomial/Polynomial]] and coefficient
       instances."}
  operator-table
  (assoc p/operator-table
         '/ (ua/group g/div g/mul g/invert 1 v/zero?)
         'invert g/invert))

(def ^{:no-doc true
       :doc "Set of all arithmetic functions allowed
       between [[RationalFunction]], [[polynomial/Polynomial]] and coefficient
       instances."}
  operators-known
  (u/keyset operator-table))

(defn expression->
  "Converts the supplied symbolic expression `expr` into Rational Function
  canonical form (ie, a [[RationalFunction]] instance). `expr` should be a bare,
  unwrapped expression built out of Clojure data structures.

  Returns the result of calling continuation `cont` with
  the [[RationalFunction]] and the list of variables corresponding to each
  indeterminate in the [[RationalFunction]]. (`cont `defaults to `vector`).

  The second optional argument `v-compare` allows you to provide a Comparator
  between variables. Sorting indeterminates by `v-compare` will determine the
  order of the indeterminates in the generated [[RationalFunction]]. The list of
  variables passed to `cont` will be sorted using `v-compare`.

  Absorbing an expression with [[expression->]] and emitting it again
  with [[->expression]] will generate the canonical form of an expression, with
  respect to the operations in the [[operators-known]] set.

  This kind of simplification proceeds purely symbolically over the known
  Rational Function operations; other operations outside the arithmetic
  available should be factored out by an expression
  analyzer (see [[emmy.expression.analyze/make-analyzer]]) before
  calling [[expression->]].

  NOTE that `cont` might receive a scalar, fraction or [[polynomial/Polynomial]]
  instance; both are valid 'rational functions'. The latter as a rational
  function with a denominator equal to `1`, and the former 2 result from
  non-polynomial numerator and denominator.

  NOTE See [[analyzer]] for an instance usable
  by [[emmy.expression.analyze/make-analyzer]]."
  ([expr]
   (expression-> expr vector compare))
  ([expr cont]
   (expression-> expr cont compare))
  ([expr cont v-compare]
   (let [vars     (-> (x/variables-in expr)
                      (set/difference operators-known))
         arity    (count vars)
         sorted   (sort v-compare vars)
         sym->var (zipmap sorted (p/new-variables arity))
         rf       (x/evaluate expr sym->var operator-table)]
     (cont rf sorted))))

(defn from-points
  "Given a sequence of points of the form `[x, f(x)]`, returns a rational function
  that passes through each input point."
  [xs]
  (g/simplify
   (ri/bulirsch-stoer-recursive xs (p/identity))))

(defn ->expression
  "Accepts a [[RationalFunction]] `r` and a sequence of symbols for each indeterminate,
  and emits the canonical form of the symbolic expression that
  represents [[RationalFunction]] `r`.

  NOTE: this is the output stage of Rational Function canonical form
  simplification. The input stage is handled by [[expression->]].

  NOTE See [[analyzer]] for an instance usable
  by [[emmy.expression.analyze/make-analyzer]]."
  [r vars]
  (if-not (rational-function? r)
    (p/->expression r vars)
    ((sym/symbolic-operator '/)
     (p/->expression (bare-u r) vars)
     (p/->expression (bare-v r) vars))))

(def ^{:doc "Singleton [[a/ICanonicalize]] instance."}
  analyzer
  (reify a/ICanonicalize
    (expression-> [_ expr cont]
      (expression-> expr cont))

    (expression-> [_ expr cont v-compare]
      (expression-> expr cont v-compare))

    (->expression [_ rf vars]
      (->expression rf vars))

    (known-operation? [_ o]
      (contains? operators-known o))))

;; ## Generic Implementations
;;
;; [[polynomial/Polynomial]] gains a few more functions; inverting a polynomial,
;; for example, results in a [[RationalFunction]] instance, so the generic
;; installation of `g/invert` and `g/div` belong here.

(defmethod g/invert [::p/polynomial] [p]
  (let [a (p/bare-arity p)]
    (if (g/negative? p)
      (->RationalFunction a -1 (g/negate p) (meta p))
      (->RationalFunction a 1 p (meta p)))))

(p/defbinary g/div make)
(p/defbinary g/solve-linear-right make)
(p/defbinary g/solve-linear (fn [l r] (div r l)))

(defmethod g/exact-divide [::p/coeff ::p/polynomial] [c p]
  (let [[term :as terms] (p/bare-terms p)]
    (if (and (= (count terms) 1)
             (pi/constant-term? term))
      (g/exact-divide c (pi/coefficient term))
      (make c p))))

;; ### Rational Function Generics
;;
;; TODO: `g/quotient`, `g/remainder` and `g/lcm` feel like valid methods to
;; install for [[RationalFunction]] instances.
;; Close [#365](https://github.com/emmy/emmy/issues/365) when these
;; are implemented.

(defn ^:no-doc defbinary
  "Installs the supplied function `f` into `generic-op` such that it will act
  between [[RationalFunction]] instances, or allow [[polynomial/Polynomial]]
  instances or non-[[polynomial/Polynomial]] coefficients on either side."
  [generic-op f]
  (let [pairs [[::rational-function ::rational-function]
               [::p/polynomial ::rational-function]
               [::p/coeff ::rational-function]
               [::rational-function ::p/polynomial]
               [::rational-function ::p/coeff]]]
    (doseq [[l r] pairs]
      (defmethod generic-op [l r] [r s]
        (f r s)))))

;; `v/=` is not implemented with [[defbinary]] because the variable order needs
;; to change so that a [[RationalFunction]] is always on the left.

(defmethod v/= [::rational-function ::rational-function] [l r] (eq l r))
(defmethod v/= [::p/polynomial ::rational-function] [l r] (eq r l))
(defmethod v/= [::p/coeff ::rational-function] [l r] (eq r l))
(defmethod v/= [::rational-function ::p/polynomial] [l r] (eq l r))
(defmethod v/= [::rational-function ::p/coeff] [l r] (eq l r))

(defbinary g/add add)
(defbinary g/sub sub)
(defbinary g/mul mul)
(defbinary g/div div)
(defbinary g/exact-divide div)
(defbinary g/solve-linear-right div)
(defbinary g/solve-linear (fn [l r] (div r l)))
(defbinary g/gcd gcd)

(defmethod g/negative? [::rational-function] [a] (negative? a))
(defmethod g/abs [::rational-function] [a] (abs a))
(defmethod g/negate [::rational-function] [a] (negate a))
(defmethod g/invert [::rational-function] [a] (invert a))
(defmethod g/square [::rational-function] [a] (square a))
(defmethod g/cube [::rational-function] [a] (cube a))

(defmethod g/expt [::rational-function ::v/integral] [b x]
  (expt b x))

(defmethod g/simplify [::rational-function] [r]
  (-> (make (g/simplify (bare-u r))
            (g/simplify (bare-v r)))
      (with-meta (meta r))))

(defmethod g/partial-derivative [::rational-function v/seqtype]
  [r selectors]
  (cond (empty? selectors)
        (if (= 1 (bare-arity r))
          (partial-derivative r 0)
          (ss/down* (partial-derivatives r)))

        (= 1 (count selectors))
        (partial-derivative r (first selectors))

        :else
        (u/illegal
         (str "Invalid selector! Only 1 deep supported."))))
