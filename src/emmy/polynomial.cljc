#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.polynomial
  (:refer-clojure :exclude [extend divide identity abs])
  (:require [clojure.set :as set]
            [clojure.string :as cs]
            [emmy.collection]
            [emmy.differential :as sd]
            [emmy.expression :as x]
            [emmy.expression.analyze :as a]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.modint :as mi]
            [emmy.numsymb :as sym]
            [emmy.polynomial.exponent :as xpt]
            [emmy.polynomial.impl :as i]
            [emmy.polynomial.interpolate :as pi]
            [emmy.series :as series]
            [emmy.special.factorial :as sf]
            [emmy.structure :as ss]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj Seqable))))

;; # Flat Polynomial Form, for Commutative Rings
;;
;; This namespace builds up an implementation of a
;; multivariate [polynomial](https://en.wikipedia.org/wiki/Polynomial) data
;; structure and installs it into the tower of generic operations.
;;
;; Summarizing the [Wikipedia entry](https://en.wikipedia.org/wiki/Polynomial),
;; a polynomial is an expression of any number of 'variables', or
;; 'indeterminates' and concrete 'coefficients', combined using only the
;; operations of addition, subtraction and multiplication of coefficients and
;; variables.
;;
;; Here's an example of a polynomial of arity 2, ie, a polynomial in two
;; variables:
;;
;; $$4 + 3x^2y + 5y^3 + 6x^4$$
;;
;; ## Terminology
;;
;; In the above example:
;;
;; - The full expression is called a 'polynomial'.
;;
;; - A polynomial is a sum of
;;   many ['monomial'](https://en.wikipedia.org/wiki/Monomial) terms; these are
;;   $4$, $3x^2y$, $5y^3$ and $6x^4$ in the example above.
;;
;; - Each monomial term is a product of a coefficient and a sequence of
;;   'exponents', some product of the term's variables. The monomial $3x^2y$ has
;;   coefficient $3$ and exponents $x^2y$.
;;
;; NOTE that sometimes the exponents here are called "monomials", without the
;; coefficient. If you find that usage, the full 'coefficient * exponents' is
;; usually called a 'polynomial term'.
;;
;; - The number of variables present in a polynomial is called the 'arity' of
;;   the polynomial. (The arity of our example is 2.)
;;
;; - A polynomial with a single variable is called a 'univariate' polynomial; a
;;   multivariable polynomial is called a 'multivariate' polynomial.
;;
;; - The [degree](https://en.wikipedia.org/wiki/Degree_of_a_polynomial#Multiplication)
;;   of a monomial term is the sum of the exponents of each variable in the
;;   term, with the special case that the [degree of a 0
;;   term](https://en.wikipedia.org/wiki/Degree_of_a_polynomial#Degree_of_the_zero_polynomial)
;;   is defined as -1. The degrees of each term in our example are 0, 3, 3 and
;;   4, respectively.
;;
;; - The degree of a polynomial is the maximum of the degrees of the
;;   polynomial's terms.
;;
;; There are, of course, more definitions! Whenever these come up in the
;; implementation below, see the documentation for explanation.
;;
;; ## Implementation
;;
;; Emmy makes the following implementation choices:
;;
;; - The exponents of a monomial are represented by an ordered mapping of
;;   variable index => the exponent of that variable. $x^2z^3$ is represented as
;;   `{0 2, 2 3}`, for example. See `emmy.polynomial.exponent` for the full
;;   set of operations you can perform on a term's exponents.
;;
;;  This representation is called `sparse` because variables with a 0 exponent
;;  aren't included.
;;
;; - A monomial term is a vector of the form `[<exponents>, <coefficient>]`. The
;;   coefficient can be any type! It's up to the user to supply coefficients
;;   drawn from a commutative ring. See [[emmy.laws/ring]] for a
;;   description of the properties coefficients should satisfy.
;;
;; - A polynomial is a sorted vector of monomials, sorted in some
;;   consistent [Monomial order](https://en.wikipedia.org/wiki/Monomial_order).
;;   See [[emmy.polynomial.impl/*monomial-order*]] for the current default.
;;
;; `emmy.polynomial.impl` builds up polynomial arithmetic on bare vectors
;; of monomials, for efficiency's sake. This namespace builds this base out into
;; a full [[Polynomial]] data structure with a fleshed-out API.
;;
;; To follow along in full, first read `emmy.polynomial.exponent`, then
;; `emmy.polynomial.impl`... then come back and continue from here.
;;
;; ## Polynomial Type Definition
;;
;; As noted above, a polynomial is defined by a sorted vector of terms.
;; The [[Polynomial]] type wraps up these `terms`, the `arity` of the polynomial
;; and optional metadata `m`.

(declare evaluate constant ->str eq map-coefficients)

(deftype Polynomial [arity terms m]
  ;; Polynomial evaluation works for any number of arguments up to and including
  ;; the full arity. Evaluating a polynomial with fewer arguments `n` than arity
  ;; triggers a partial evaluation of the first `n` indeterminates.
  f/IArity
  (arity [_] [:between 0 arity])

  sd/IPerturbed
  (perturbed? [_]
    (let [coefs (map i/coefficient terms)]
      (boolean (some sd/perturbed? coefs))))

  (replace-tag [this old new]
    (map-coefficients #(sd/replace-tag % old new) this))

  (extract-tangent [this tag]
    (map-coefficients #(sd/extract-tangent % tag) this))

  v/Value
  (zero? [_]
    (empty? terms))

  (one? [_]
    (and (= (count terms) 1)
         (let [[term] terms]
           (and (i/constant-term? term)
                (v/one? (i/coefficient term))))))

  (identity? [_]
    (and (v/one? arity)
         (= (count terms) 1)
         (let [[term] terms]
           (and (= {0 1} (i/exponents term))
                (v/one? (i/coefficient term))))))

  (zero-like [_]
    (if-let [term (nth terms 0)]
      (v/zero-like (i/coefficient term))
      0))

  (one-like [_]
    (if-let [term (nth terms 0)]
      (v/one-like (i/coefficient term))
      1))

  (identity-like [_]
    (assert (v/one? arity)
            "identity-like unsupported on multivariate monomials!")
    (let [one (if-let [term (nth terms 0)]
                (v/one-like (i/coefficient term))
                1)
          term (i/make-term (xpt/make 0 1) one)]
      (Polynomial. 1 [term] m)))

  (exact? [_] false)
  (freeze [_] `(~'polynomial ~arity ~terms))
  (kind [_] ::polynomial)

  #?@(:clj
      [Object
       (equals [this that] (eq this that))
       (toString [p] (->str p))

       IObj
       (meta [_] m)
       (withMeta [_ meta] (Polynomial. arity terms meta))

       Seqable
       (seq [_] (seq terms))

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
       (toString [p] (->str p))

       IEquiv
       (-equiv [this that] (eq this that))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ m] (Polynomial. arity terms m))

       ISeqable
       (-seq [_] (seq terms))

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
       (-pr-writer [x writer _]
                   (write-all writer
                              "#object[emmy.polynomial.Polynomial \""
                              (.toString x)
                              "\"]"))]))

(defn polynomial?
  "Returns true if the supplied argument is an instance of [[Polynomial]], false
  otherwise."
  [x]
  (instance? Polynomial x))

(defn coeff?
  "Returns true if the input `x` is explicitly _not_ an instance
  of [[Polynomial]], false otherwise.

  Equivalent to `(not (polynomial? x))`."
  [x]
  (not (polynomial? x)))

(defn ^:no-doc bare-arity
  "Given a [[Polynomial]] instance `p`, returns the `arity` field."
  [p]
  {:pre [(polynomial? p)]}
  (.-arity ^Polynomial p))

(defn ^:no-doc bare-terms
  "Given a [[Polynomial]] instance `p`, returns the `terms` field."
  [p]
  {:pre [(polynomial? p)]}
  (.-terms ^Polynomial p))

;; ## Constructors

(defn ^:no-doc terms->polynomial
  "Accepts an explicit `arity` and a vector of terms and returns either:

  - `0`, in the case of an empty list
  - a bare coefficient, given a singleton term list with a constant term
  - else, a [[Polynomial]] instance.

  In the second case, if the coefficient is _itself_ a [[Polynomial]], wraps
  that [[Polynomial]] instance up in an explicit [[Polynomial]]. In cases where
  polynomials have polynomial coefficients, this flattening should never happen
  automatically.

  NOTE this method assumes that the terms are properly sorted, and contain no
  zero coefficients."
  [arity terms]
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (i/constant-term? (nth terms 0)))
        (let [c (i/coefficient (nth terms 0))]
          (if (polynomial? c)
            (->Polynomial arity terms nil)
            c))

        :else (->Polynomial arity terms nil)))

(defn make
  "Generates a [[Polynomial]] instance (or a bare coefficient!) from either:

  - a sequence of dense coefficients of a univariate polynomial (in ascending
  order)
  - an explicit `arity`, and a sparse mapping (or sequence of pairs) of exponent
  => coefficient

  In the first case, the sequence is interpreted as a dense sequence of
  coefficients of an arity-1 (univariate) polynomial. The coefficients begin
  with the constant term and proceed to each higher power of the indeterminate.
  For example, x^2 - 1 can be constructed by (make [-1 0 1]).

  In the 2-arity case,

  - `arity` is the number of indeterminates
  - `expts->coef` is a map of an exponent representation to a coefficient.

  The `exponent` portion of the mapping can be any of:

  - a proper exponent entry created by `emmy.polynomial.exponent`
  - a map of the form `{variable-index, power}`
  - a dense vector of variable powers, like `[3 0 1]` for $x^3z$. The length of
    each vector should be equal to `arity`, in this case.

  For example, any of the following would generate $4x^2y + 5xy^2$:

  ```clojure
  (make 2 [[[2 1] 4] [[1 2] 5]])
  (make 2 {[2 1] 4, [1 2] 5})
  (make 2 {{0 2, 1 1} 4, {0 1, 1 2} 5})
  ```

  NOTE: [[make]] will try and return a bare coefficient if possible. For
  example, the following form will return a constant, since there are no
  explicit indeterminates with powers > 0:

  ```clojure
  (make 10 {{} 1 {} 2})
  ;;=> 3
  ```

  See [[constant]] if you need an explicit [[Polynomial]] instance wrapping a
  constant."
  ([dense-coefficients]
   (let [terms (i/dense->terms dense-coefficients)]
     (terms->polynomial 1 terms)))
  ([arity expts->coef]
   (let [terms (i/sparse->terms expts->coef)]
     (terms->polynomial arity terms))))

(defn constant
  "Given some coefficient `c`, returns a [[Polynomial]] instance with a single
  constant term referencing `c`.

  `arity` defaults to 1; supply it to set the arity of the
  returned [[Polynomial]]."
  ([c] (constant 1 c))
  ([arity c]
   (->Polynomial arity (i/constant->terms c) nil)))

(defn identity
  "Generates a [[Polynomial]] instance representing a single indeterminate with
  constant 1.

  When called with no arguments, returns a monomial of arity 1 that acts as
  identity in the first indeterminate.

  The one-argument version takes an explicit `arity`, but still sets the
  identity to the first indeterminate.

  The two-argument version takes an explicit `i` and returns a monomial of arity
  `arity` with an exponent of 1 in the `i`th indeterminate."
  ([]
   (identity 1 0))
  ([arity]
   (identity arity 0))
  ([arity i]
   {:pre [(and (>= i 0) (< i arity))]}
   (let [expts (xpt/make i 1)]
     (->Polynomial arity [(i/make-term expts 1)] nil))))

(defn new-variables
  "Returns a sequence of `n` monomials of arity `n`, each with an exponent of `1`
  for the `i`th indeterminate (where `i` matches the position in the returned
  sequence)."
  [n]
  (map #(identity n %)
       (range 0 n)))

(defn from-points
  "Given a sequence of points of the form `[x, f(x)]`, returns a univariate
  polynomial that passes through each input point.

  The degree of the returned polynomial is equal to `(dec (count xs))`."
  [xs]
  (g/simplify
   (pi/lagrange xs (identity))))

(declare add)

(defn linear
  "Given some `arity`, an indeterminate index `i` and some constant `root`,
  returns a polynomial of the form `x_i - root`. The returned polynomial
  represents a linear equation in the `i`th indeterminate.

  If `root` is 0, [[linear]] is equivalent to the two-argument version
  of [[identity]]."
  [arity i root]
  (if (v/zero? root)
    (identity arity i)
    (add (constant arity (g/negate root))
         (identity arity i))))

(defn c*xn
  "Given some `arity`, a coefficient `c` and an exponent `n`, returns a monomial
  representing $c{x_0}^n$. The first indeterminate is always exponentiated.

  Similar to [[make]], this function attempts to drop down to scalar-land if
  possible:

  - If `c` is [[emmy.value/zero?]], returns `c`
  - if `n` is `zero?`, returns `(constant arity c)`

  NOTE that negative exponents are not allowed."
  [arity c n]
  {:pre [(>= n 0)]}
  (cond (v/zero? c) c
        (zero? n)   (constant arity c)
        :else
        (let [term (i/make-term (xpt/make 0 n) c)]
          (->Polynomial arity [term] nil))))

;; ## Constructors of Special Polynomials

(defn touchard
  "Returns the nth [Touchard
  polynomial](https://en.wikipedia.org/wiki/Touchard_polynomials).

  These are also called [Bell
  polynomials](https://mathworld.wolfram.com/BellPolynomial.html) (in
  Mathematica, implemented as `BellB`) or /exponential polynomials/."
  [n]
  (make
   (map #(sf/stirling-second-kind n %)
        (range (inc n)))))

;; ###  Accessors, Predicates
;;
;; The functions in the next section all work on both explicit [[Polynomial]]
;; instances _and_ on bare coefficients. Any non-polynomial type is treated as a
;; constant polynomial.

(def ^:no-doc coeff-arity 0)
(def ^:no-doc zero-degree -1)

(defn arity
  "Returns the declared arity of the supplied [[Polynomial]], or `0` for
  non-polynomial arguments."
  [p]
  (if (polynomial? p)
    (bare-arity p)
    coeff-arity))

(defn ->terms
  "Given some [[Polynomial]], returns the `terms` entry of the type. Handles other types as well:

  - Acts as identity on vectors, interpreting them as vectors of terms
  - any zero-valued `p` returns `[]`
  - any other coefficient returns a vector of a single constant term."
  [p]
  (cond (polynomial? p) (bare-terms p)
        (vector? p) p
        (v/zero? p) []
        :else [(i/make-term p)]))

(defn ^:no-doc check-same-arity
  "Given two polynomials (or coefficients) `p` and `q`, checks that their arities
  are equal and returns the value, or throws an exception if not.

  If either `p` or `q` is a coefficient, [[check-same-arity]] successfully
  returns the other argument's arity."
  [p q]
  (let [poly-p? (polynomial? p)
        poly-q? (polynomial? q)]
    (cond (and poly-p? poly-q?)
          (let [ap (bare-arity p)
                aq (bare-arity q)]
            (if (= ap aq)
              ap
              (u/arithmetic-ex
               (str "mismatched polynomial arity: " ap ", " aq))))
          poly-p? (bare-arity p)
          poly-q? (bare-arity q)
          :else coeff-arity)))

(defn valid-arity?
  "Given some input `p` and an indeterminate index `i`, returns true if `0 <= i
  < (arity p)`, false otherwise."
  [p i]
  (and (>= i 0)
       (< i (arity p))))

(defn ^:no-doc validate-arity!
  "Given some input `p` and an indeterminate index `i`, returns `i` if `0 <= i
  < (arity p)`, and throws an exception otherwise.

  NOTE [[validate-arity]] is meant to validate indeterminate indices; thus it
  will always throw for non-[[Polynomial]] inputs."
  [p i]
  (if (valid-arity? p i)
    i
    (u/arithmetic-ex
     (str "Supplied i " i " outside the bounds of arity " (arity p) " for input " p))))

(declare leading-term)

(defn degree
  "Returns the [degree](https://en.wikipedia.org/wiki/Degree_of_a_polynomial) of
  the supplied polynomial.

  the degree of a polynomial is the highest of the degrees of the polynomial's
  individual terms with non-zero coefficients. The degree of an individual term
  is the sum of all exponents in the term.

  Optionally, [[degree]] takes an indeterminate index `i`; in this
  case, [[degree]] returns the maximum power found for the `i`th indeterminate
  across all terms.

  NOTE when passed either a `0` or a zero-polynomial, [[degree]] returns -1. See
  Wikipedia's ['degree of the zero
  polynomial'](https://en.wikipedia.org/wiki/Degree_of_a_polynomial#Degree_of_the_zero_polynomial)
  for color on why this is the case.
  "
  ([p]
   (cond (v/zero? p) zero-degree
         (polynomial? p)
         (xpt/monomial-degree
          (i/exponents
           (leading-term p)))
         :else coeff-arity))
  ([p i]
   (let [i (validate-arity! p i)]
     (cond (v/zero? p) zero-degree
           (polynomial? p)
           (letfn [(i-degree [term]
                     (-> (i/exponents term)
                         (xpt/monomial-degree i)))]
             (transduce (map i-degree)
                        max
                        0
                        (bare-terms p)))
           :else coeff-arity))))

(defn eq
  "Returns true if the [[Polynomial]] this is equal to `that`. If `that` is
  a [[Polynomial]], `this` and `that` are equal if they have equal terms and
  equal arity. Coefficients are compared using [[emmy.value/=]].

  If `that` is non-[[Polynomial]], `eq` only returns true if `this` is a
  monomial and its coefficient is equal to `that` (again
  using [[emmy.value/=]])."
  [^Polynomial this that]
  (if (instance? Polynomial that)
    (let [p ^Polynomial that]
      (and (= (.-arity this) (.-arity p))
           (v/= (.-terms this)
                (.-terms p))))

    (let [terms (.-terms this)]
      (and (<= (count terms) 1)
           (let [term (peek terms)]
             (and (i/constant-term? term)
                  (v/= that (i/coefficient term))))))))

(defn ->str
  "Returns a string representation of the supplied [[Polynomial]] instance `p`.

  The optional argument `n` specifies how many terms to include in the returned
  string before an ellipsis cuts them off."
  ([p] (->str p 10))
  ([p n]
   {:pre [polynomial? p]}
   (let [terms     (bare-terms p)
         arity     (bare-arity p)
         n-terms   (count terms)
         term-strs (take n (map i/term->str terms))
         suffix    (when (> n-terms n)
                     (str "... and " (- n-terms n) " more terms"))]
     (str arity  ": (" (cs/join " + " term-strs) suffix ")"))))

(defn coefficients
  "Returns a sequence of the coefficients of the supplied polynomial `p`. A
  coefficient is treated here as a monomial, and returns a sequence of itself.

  If `p` is zero, returns an empty list."
  [p]
  (cond (polynomial? p) (map i/coefficient (->terms p))
        (v/zero? p) []
        :else [p]))

(defn leading-term
  "Returns the leading (highest degree) term of the [[Polynomial]] `p`.

  If `p` is a non-[[Polynomial]] coefficient, returns a term with zero exponents
  and `p` as its coefficient."
  [p]
  (or (peek (->terms p))
      [xpt/empty 0]))

(defn leading-coefficient
  "Returns the coefficient of the leading (highest degree) term of
  the [[Polynomial]] `p`.

  If `p` is a non-[[Polynomial]] coefficient, acts as identity."
  [p]
  (if (polynomial? p)
    (i/coefficient
     (peek (bare-terms p)))
    p))

(defn leading-exponents
  "Returns the exponents of the leading (highest degree) term of
  the [[Polynomial]] `p`.

  If `p` is a non-[[Polynomial]] coefficient, returns [[exponent/empty]]."
  [p]
  (if (polynomial? p)
    (i/exponents
     (peek (bare-terms p)))
    xpt/empty))

(defn leading-base-coefficient
  "Similar to [[leading-coefficient]], but of the coefficient itself is
  a [[Polynomial]], recurses down until it reaches a non-[[Polynomial]] lead
  coefficient.

  If `p` is a non-[[Polynomial]] coefficient, acts as identity."
  [p]
  (if (polynomial? p)
    (recur (leading-coefficient p))
    p))

(defn trailing-coefficient
  "Returns the coefficient of the trailing (lowest degree) term of
  the [[Polynomial]] `p`.

  If `p` is a non-[[Polynomial]] coefficient, acts as identity."
  [p]
  (if (polynomial? p)
    (i/coefficient
     (nth (bare-terms p) 0 []))
    p))

(defn lowest-degree
  "Returns the lowest degree found across any term in the supplied [[Polynomial]].
  If a non-[[Polynomial]] is supplied, returns either `0` or `-1` if the input
  is itself a `0`.

  See [[degree]] for a discussion of this `-1` case."
  [p]
  (cond (polynomial? p)
        (xpt/monomial-degree
         (i/exponents
          (nth (bare-terms p) 0)))
        (v/zero? p) zero-degree
        :else coeff-arity))

(defn monomial?
  "Returns true if `p` is either:

  - a [[Polynomial]] instance with a single term, or
  - a non-[[Polynomial]] coefficient,

  false otherwise."
  [p]
  (or (not (polynomial? p))
      (= 1 (count (bare-terms p)))))

(defn monic?
  "Returns true if `p` is a [monic
  polynomial](https://en.wikipedia.org/wiki/Monic_polynomial), false otherwise.

  A monic polynomial is a univariate polynomial with a leading coefficient that
  responds `true` to [[emmy.value/one?]]. This means that any coefficient
  that responds `true` to [[emmy.value/one?]] also qualifies as a monic
  polynomial."
  [p]
  (if (polynomial? p)
    (and (= 1 (arity p))
         (v/one?
          (leading-coefficient p)))
    (v/one? p)))

(defn univariate?
  "Returns true if `p` is a [[Polynomial]] of arity 1, false otherwise."
  [p]
  (and (polynomial? p)
       (= (bare-arity p) 1)))

(defn multivariate?
  "Returns true if `p` is a [[Polynomial]] of arity > 1, false otherwise."
  [p]
  (and (polynomial? p)
       (> (bare-arity p) 1)))

(defn negative?
  "Returns true if the [[leading-base-coefficient]] of `p`
  is [[generic/negative?]], false otherwise."
  [p]
  (g/negative?
   (leading-base-coefficient p)))

;; ## Polynomial API

(defn map-coefficients
  "Given a [[Polynomial]], returns a new [[Polynomial]] instance generated by
  applying `f` to the coefficient of each term in `p` and filtering out all
  resulting zeros.

  Given a non-[[Polynomial]] coefficient, returns `(f p)`.

  NOTE that [[map-coefficients]] will return a non-[[Polynomial]] if the result
  of the mapping has only a constant term."
  [f p]
  (if (polynomial? p)
    (terms->polynomial
     (bare-arity p)
     (i/map-coefficients f (bare-terms p)))
    (f p)))

(defn map-exponents
  "Given a [[Polynomial]], returns a new [[Polynomial]] instance generated by
  applying `f` to the exponents of each term in `p` and filtering out all
  resulting zeros. The resulting [[Polynomial]] will have either the
  same [[arity]] as `p`, or the explicit, optional `new-arity` argument. (This
  is because `f` might increase or decrease the total arity.)

  Given a non-[[Polynomial]] coefficient, if `(f empty-exponents)` produces a
  non-zero result, errors without an explicit `new-arity` argument..

  NOTE that [[map-exponents]] will return a non-[[Polynomial]] if the result
  of the mapping has only a constant term."
  ([f p]
   (map-exponents f p nil))
  ([f p new-arity]
   (letfn [(force-arity []
             (or new-arity
                 (u/illegal
                  "`new-arity` argument to `map-exponents` required when promoting constant.")))
           (handle-constant []
             (let [f-expts (f xpt/empty)]
               (if (empty? f-expts)
                 p
                 (let [arity (force-arity)]
                   (->Polynomial arity [(i/make-term f-expts p)] nil)))))]
     (cond (polynomial? p)
           (make (or new-arity (bare-arity p))
                 (for [[expts c] (bare-terms p)
                       :let [f-expts (f expts)]]
                   (i/make-term f-expts c)))
           (v/zero? p) p
           :else (handle-constant)))))

;; ## Manipulations

(defn univariate->dense
  "Given a univariate [[Polynomial]] (see [[univariate?]]) returns a dense vector
  of the coefficients of each term in ascending order.

  For example:

  ```clojure
  (univariate->dense (make [1 0 0 2 3 4]))
  ;;=> [1 0 0 2 3 4]
  ```

  Supplying the second argument `x-degree` will pad the right side of the
  returning coefficient vector to be the max of `x-degree` and `(degree x)`.

  NOTE use [[lower-arity]] to generate a univariate polynomial in the first
  indeterminate, given a multivariate polynomial."
  ([x] (univariate->dense x (degree x)))
  ([x x-degree]
   (if (coeff? x)
     (into [x] (repeat x-degree 0))
     (do (assert (univariate? x))
         (let [d (degree x)]
           (loop [terms (bare-terms x)
                  acc (transient [])
                  i 0]
             (if (> i d)
               (into (persistent! acc)
                     (repeat (- x-degree d) 0))
               (let [t  (first terms)
                     e  (i/exponents t)
                     md (xpt/monomial-degree e 0)]
                 (if (= md i)
                   (recur (rest terms)
                          (conj! acc (i/coefficient t))
                          (inc i))
                   (recur terms
                          (conj! acc 0)
                          (inc i)))))))))))

(defn ->power-series
  "Given a univariate polynomial `p`, returns a [[series/PowerSeries]]
  representation of the supplied [[Polynomial]].

  Given a [[series/PowerSeries]], acts as identity.

  Non-[[Polynomial]] coefficients return [[series/PowerSeries]] instances
  via [[series/constant]]; any multivariate [[Polynomial]] throws an exception.

  NOTE use [[lower-arity]] to generate a univariate polynomial in the first
  indeterminate, given a multivariate polynomial."
  [p]
  (cond (series/power-series? p) p

        (univariate? p)
        (series/power-series*
         (univariate->dense p))

        (polynomial? p)
        (u/illegal
         "Only univariate polynomials can be converted to [[PowerSeries]].
         Use [[polynomial/lower]] to generate a univariate.")

        :else (series/constant p)))

(defn from-power-series
  "Returns a univariate polynomial of all terms in the
  supplied [[series/PowerSeries]] instance, up to (and including) order
  `n-terms`.

  ```clojure
  (g/simplify
    ((from-power-series series/exp-series 3) 'x))
  ;; => (+ (* 1/6 (expt x 3)) (* 1/2 (expt x 2)) x 1)
  ```"
  [s n-terms]
  {:pre [(series/power-series? s)]}
  (-> (s (identity))
      (series/sum n-terms)))

(defn scale
  "Given some polynomial `p` and a coefficient `c`, returns a new [[Polynomial]]
  generated by multiplying each coefficient of `p` by `c` (on the right).

  See [[scale-l]] if left multiplication is important.

  NOTE that [[scale]] will return a non-[[Polynomial]] if the result of the
  mapping has only a constant term."
  [p c]
  (if (v/zero? c)
    c
    (map-coefficients #(g/* % c) p)))

(defn scale-l
  "Given some polynomial `p` and a coefficient `c`, returns a new [[Polynomial]]
  generated by multiplying each coefficient of `p` by `c` (on the left).

  See [[scale]] if right multiplication is important.

  NOTE that [[scale-l]] will return a non-[[Polynomial]] if the result of the
  mapping has only a constant term."
  [c p]
  (if (v/zero? c)
    c
    (map-coefficients #(g/* c %) p)))

(declare evenly-divide)

(defn normalize
  "Given a polynomial `p`, returns a normalized polynomial generated by dividing
  through either the [[leading-coefficient]] of `p` or an optional, explicitly
  supplied scaling factor `c`.

  For example:

  ```clojure
  (let [p (make [5 3 2 2 10])]
    (univariate->dense (normalize p)))
  ;;=> [1/2 3/10 1/5 1/5 1]
  ```"
  ([p]
   (normalize p (leading-coefficient p)))
  ([p c]
   (cond (v/one? c) p
         (v/zero? c) (u/arithmetic-ex
                      (str "Divide by zero: " p c))
         (polynomial? c) (evenly-divide p c)
         :else (scale p (g/invert c)))))

(defn reciprocal
  "Given a polynomial `p`, returns the [reciprocal
  polynomial](https://en.wikipedia.org/wiki/Reciprocal_polynomial) with respect
  to the `i`th indeterminate. `i` defaults to 0.

  The reciprocal polynomial of `p` with respect to `i` is generated by

  - treating the polynomial as univariate with respect to `i` and pushing all
    other terms into the coefficients of the polynomial
  - reversing the order of these coefficients
  - flattening the polynomial out again

  For example, note that the entries for the first indeterminate are reversed:

  ```clojure
  (= (make 3 {[3 0 0] 5 [2 0 1] 2 [0 2 1] 3})
     (reciprocal
       (make 3 {[0 0 0] 5 [1 0 1] 2 [3 2 1] 3})))
  ```"
  ([p] (reciprocal p 0))
  ([p i]
   (if (polynomial? p)
     (let [d (degree p i)]
       (if (zero? d)
         p
         (map-exponents
          (fn [m]
            (let [v (xpt/monomial-degree m i)
                  v' (- d v)]
              (xpt/assoc m i v')))
          p)))
     p)))

(defn drop-leading-term
  "Given some [[Polynomial]] `p`, returns `p` without its [[leading-term]].
  non-[[Polynomial]] `p` inputs are treated at constant polynomials and return
  `0`.

  NOTE that [[drop-leading-term]] will return a non-[[Polynomial]] if the result
  of the mapping has only a constant term."
  [p]
  (if (polynomial? p)
    (let [a (bare-arity p)
          terms (pop (bare-terms p))]
      (terms->polynomial a terms))
    0))

;; ## Polynomial Arithmetic

(defn- binary-combine
  "Accepts

  - two inputs `l` and `r`
  - A `coeff-op`, used when both inputs are non-[[Polynomial]]
  - a `terms-op` that acts on two vectors of bare terms

  And returns:

  - `(coeff-op l r)` when both inputs are non-[[Polynomial]],

  - `(terms-op l r)` otherwise, appropriately converting `l` or `r` into a
    singleton term vector when needed.

  The result is converted back to a [[Polynomial]] (or dropped to a coefficient)
  via [[terms->polynomial]], or a custom function supplied by the optional
  `:->poly` argument."
  [l r coeff-op terms-op
   & {:keys [->poly]
      :or {->poly terms->polynomial}}]
  (let [l-poly? (polynomial? l)
        r-poly? (polynomial? r)]
    (cond (and l-poly? r-poly?)
          (->poly
           (check-same-arity l r)
           (terms-op (bare-terms l)
                     (bare-terms r)))

          l-poly?
          (->poly
           (bare-arity l)
           (terms-op (bare-terms l)
                     (i/constant->terms r)))

          r-poly?
          (->poly
           (bare-arity r)
           (terms-op (i/constant->terms l)
                     (bare-terms r)))

          :else (coeff-op l r))))

(defn negate
  "Returns the negation of polynomial `p`, ie, a polynomial with all coefficients
  negated."
  [p]
  (map-coefficients g/negate p))

(defn abs
  "If the [[leading-coefficient]] of `p` is negative, returns `(negate p)`, else
  acts as identity."
  [p]
  (if (negative? p)
    (negate p)
    p))

(defn add
  "Returns the sum of polynomials `p` and `q`, with appropriate handling for
  non-[[Polynomial]] coefficient inputs on either or both sides."
  [p q]
  (binary-combine p q g/add i/add))

(defn sub
  "Returns the difference of polynomials `p` and `q`, with appropriate handling
  for non-[[Polynomial]] coefficient inputs on either or both sides."
  [p q]
  (binary-combine p q g/sub i/sub))

(defn mul
  "Returns the product of polynomials `p` and `q`, with appropriate handling for
  non-[[Polynomial]] coefficient inputs on either or both sides."
  [p q]
  (binary-combine p q g/mul i/mul))

(defn square
  "Returns the square of polynomial `p`. Equivalent to `(mul p p)`."
  [p]
  (mul p p))

(defn cube
  "Returns the cube of polynomial `p`. Equivalent to `(mul p (mul p p))`."
  [p]
  (mul p (mul p p)))

(defn expt
  "Returns a polynomial generated by raising the input polynomial `p` to
  the (integer) power `n`.

  Negative exponents are not supported. For negative polynomial exponentation,
  see [[rational-function/expt]]."
  [p n]
  (letfn [(expt-iter [x n answer]
            (cond (zero? n) answer
                  (even? n) (recur (mul x x) (quot n 2) answer)
                  :else     (recur x (dec n) (mul x answer))))]
    (cond
      (coeff? p) (g/expt p n)

      (not (v/native-integral? n))
      (u/illegal
       (str "Can only raise an FPF to an exact integer power: " p n))

      (neg? n)
      (u/illegal (str "No inverse -- FPF:EXPT:" p n))

      (v/one? p)  p
      (v/zero? p) (if (v/zero? n)
                    (u/arithmetic-ex "poly 0^0")
                    p)

      :else (expt-iter p n 1))))

(defn divide
  "Given two polynomials `u` and `v`, returns a pair of polynomials of the form
  `[quotient, remainder]` using [polynomial long
  division](https://en.wikipedia.org/wiki/Polynomial_long_division).

  The contract satisfied by this returned pair is that

  ```
  u == (add (mul quotient v) remainder)
  ```"
  [u v]
  (cond (v/zero? v)
        (u/illegal "internal polynomial division by zero")

        (or (v/zero? u) (v/one? v))
        [u 0]

        :else
        (letfn [(coeff:div [l r]
                  [(g/quotient l r) (g/remainder l r)])]
          (binary-combine u v coeff:div i/div
                          :->poly
                          (fn [a [q r]]
                            [(terms->polynomial a q)
                             (terms->polynomial a r)])))))

(defn divisible?
  "Returns true if the numerator `n` is evenly divisible by `d` (ie, leaves no
  remainder), false otherwise.

  NOTE that this performs a full division with [[divide]]. If you're planning on
  doing this, you may as well call [[divide]] and check that the remainder
  satisfies [[emmy.value/zero?]]."
  [n d]
  (let [[_ r] (divide n d)]
    (v/zero? r)))

(defn evenly-divide
  "Returns the result of dividing the polynomial `u` by `v` (non-[[Polynomial]]
  instances are allowed).

  Throws an exception if the division leaves a remainder. Else, returns the
  quotient."
  [u v]
  (if (v/one? v)
    u
    (let [[q r] (divide u v)]
      (when-not (v/zero? r)
        (u/illegal-state
         (str "expected even division left a remainder! " u " / " v " r " r)))
      q)))

;; A comment from GJS on the next function: "Pseudo division produces only a
;; remainder--no quotient. This can be used to generalize Euclid's algorithm for
;; polynomials over a [unique factorization
;; domain](https://en.wikipedia.org/wiki/Unique_factorization_domain) (UFD)."


#?(:cljs
   (defn- ->big
     "ClojureScript multiplication doesn't autopromote; we expect large values
     in [[pseudo-remainder]], and use [[->big]] to pre-cast the factors
     to [[util/bigint]] so they don't overflow."
     [c]
     (if (v/integral? c)
       (u/bigint c)
       c)))

(defn pseudo-remainder
  "Returns the pseudo-remainder of univariate polynomials `u` and `v`.

  NOTE: Fractions won't appear in the result; instead the divisor is multiplied
  by the leading coefficient of the dividend before quotient terms are generated
  so that division will not result in fractions.

  Returns a pair of

  - the remainder
  - the integerizing factor needed to make this happen.

  Similar in spirit to Knuth's algorithm 4.6.1R, except we don't multiply the
  remainder through during gaps in the remainder. Since you don't know up front
  how many times the integerizing multiplication will be done, we also return
  the number d for which d * u = q * v + r."
  [u v]
  {:pre [(univariate? u)
         (univariate? v)
         (not (v/zero? v))]}
  (let [[vn-expts vn-coeff] (leading-term v)
        #?@(:cljs [vn-coeff (->big vn-coeff)])
        *vn (fn [p] (scale p vn-coeff))
        n (xpt/monomial-degree vn-expts)]
    (loop [remainder u
           d 0]
      (let [m (degree remainder)
            c (leading-coefficient remainder)
            #?@(:cljs [c (->big c)])]
        (if (< m n)
          [remainder d]
          ;; this handles symbolic coefficients.
          (recur (g/simplify
                  (sub (*vn remainder)
                       (mul (c*xn 1 c (- m n))
                            v)))
                 (inc d)))))))

;; ## Polynomial Contraction and Expansion
;;
;; The following functions are helpful for modifying the arity and variable
;; assignment within a polynomial, either by expanding and contracting a
;; flattened polynomial (adding new variable indices, or removing unused
;; indices) or by pushing some of the variables into the coefficient slots of a
;; new univariate polynomial.

(defn contractible?
  "Returns true if `n` is a valid variable index for the [[Polynomial]] `p`, and
  the variable with that index has no powers greater than `0` in `p`, false
  otherwise."
  [p n]
  (and (valid-arity? p n)
       (zero?
        (degree p n))))

(defn contract
  "If `p` is [[contractible?]] at index `n`, returns a new [[Polynomial]] instance
  of [[arity]] `1` less than `p` with all variable indices > `n` decremented.

  For non-[[Polynomial]] inputs, acts as identity. Throws if `p` is not
  explicitly [[contractible?]].

  For example:

  ```clojure
  (= (make 2 {[1 2] 3 [3 4] 5})
     (contract
       (make 3 {[0 1 2] 3 [0 3 4] 5}) 0))
  ```"
  [p n]
  (cond (not (polynomial? p)) p

        (contractible? p n)
        (map-exponents #(xpt/lower % n)
                       p
                       (dec (bare-arity p)))

        :else
        (u/illegal
         (str "Polynomial not contractible: " p " in position " n))))

(defn extend
  "Interpolates a new variable into the supplied [[Polynomial]] `p` at index `n`
  by incrementing any existing variable index >= `n`.

  Returns a new [[Polynomial]] of [[arity]] 1 greater than the [[arity]] of `p`,
  or equal to `(inc n)` if `n` is greater than the [[arity]] of `p`.

  For non-[[Polynomial]] inputs (or negative `n`), acts as identity."
  [p n]
  (if (or (not (polynomial? p))
          (< n 0))
    p
    (let [a (bare-arity p)]
      (if (> n a)
        (->Polynomial (inc n) (bare-terms p) (meta p))
        (map-exponents #(xpt/raise % n 0)
                       p
                       (inc a))))))

;; The next two functions allow you to pivot a multivariate polynomial into a
;; univariate polynomial in the first variable. All other variables are pushed
;; into [[Polynomial]] instances attached as coefficients.

(defn lower-arity
  "Given a multivariate [[Polynomial]] `p`, returns an equivalent
  univariate [[Polynomial]] whose coefficients are polynomials of [[arity]]
  equal to one less than the [[arity]] of `p`.

  Use [[raise-arity]] to undo this transformation. See [[with-lower-arity]] for
  a function that packages these two transformations.

  NOTE that [[lower-arity]] will drop a coefficient down to a non-[[Polynomial]]
  if the result of extracting the first variable leaves a constant term."
  [p]
  {:pre [(multivariate? p)]}
  (let [A (bare-arity p)]
    (letfn [(lower-terms [terms]
              (make (dec A)
                    (for [[xs c] terms]
                      [(xpt/lower xs 0) c])))]
      (->> (bare-terms p)
           (group-by #(xpt/monomial-degree (i/exponents %) 0))
           (map (fn [[x terms]]
                  (let [expts (if (zero? x)
                                xpt/empty
                                (xpt/make 0 x))]
                    (i/make-term expts (lower-terms terms)))))
           (make 1)))))

(defn raise-arity
  "Given either a non-[[Polynomial]] coefficient or a univariate [[Polynomial]]
  with possibly-[[Polynomial]] coefficients, returns a new [[Polynomial]] of
  arity `a` generated by attaching the polynomial coefficients back as variables
  starting with `1`.

  [[raise-arity]] undoes the transformation of [[lower-arity]].
  See [[with-lower-arity]] for a function that packages these two
  transformations."
  [p a]
  (if (polynomial? p)
    (do (assert (= (bare-arity p) 1))
        (let [terms (i/sparse->terms
                     (for [[x q] (bare-terms p)
                           [ys c] (->terms q)
                           :let [expts (xpt/raise
                                        ys 0 (xpt/monomial-degree x 0))]]
                       (i/make-term expts c)))]
          (->Polynomial a terms (meta p))))
    (constant a p)))

(defn with-lower-arity
  "Given:

  - multivariate [[Polynomial]]s `u` and `v`
  - a `continue` function that accepts two univariate [[Polynomial]]s with
    possibly-[[Polynomial]] coefficients,

  Returns the result of calling [[lower-arity]] on `u` and `v`, passing the
  results to `continue` and using [[raise-arity]] to raise the result back to
  the original shared [[arity]] of `u` and `v`.

  The exception is that if `continue` returns a
  non-[[Polynomial]], [[with-lower-arity]] will not attempt to re-package it as
  a [[Polynomial]]."
  [u v continue]
  (let [a (check-same-arity u v)
        result (continue
                (lower-arity u)
                (lower-arity v))]
    (if (polynomial? result)
      (raise-arity result a)
      result)))

;; ## Evaluation

(defn- evaluate-1
  "Returns the result of evaluating the univariate [[Polynomial]] `p` at `x`
  using [Horner's rule](https://en.wikipedia.org/wiki/Horner%27s_method).

  If `p` is a non-[[Polynomial]] coefficient, acts as identity.

  NOTE In general, the coefficients of the `p` will themselves
  be [[Polynomial]]s, which must then evaluated with values for their
  indeterminates. See [[evaluate]] for a multivariate version
  of [[evaluate-1]]."
  [p x]
  (if-not (polynomial? p)
    p
    (do (assert (= 1 (bare-arity p))
                "`evaluate-1` requires a univariate polynomial!")
        (loop [terms (bare-terms p)
               result 0
               x**e   1
               e      0]
          (if-let [[expts c] (nth terms 0)]
            (let [e' (xpt/monomial-degree expts 0)
                  x**e' (g/* x**e (g/expt x (- e' e)))]
              (recur (next terms)
                     (g/+ result (g/* c x**e'))
                     x**e'
                     e'))
            result)))))

(defn evaluate
  "Returns the result of evaluating a multivariate [[Polynomial]] `p` at the
  values in the sequence `xs` using [Horner's
  rule](https://en.wikipedia.org/wiki/Horner%27s_method).

  If `p` is a non-[[Polynomial]] coefficient, acts as identity.

  Supplying too many arguments in `xs` (ie, a greater number than the [[arity]]
  of `p`) will throw an exception. Too few arguments will result in a partial
  evaluation of `p`, leaving the remaining indeterminates with their variable
  indices shifted down.

  For example:

  ```clojure
  (= (make [0 0 '(* 3 (expt x 2) y) 0 '(* 5 (expt x 2) (expt y 3))])
     (simplify
       (evaluate
         (make 3 {[2 1 2] 3 [2 3 4] 5}) ['x 'y])))
  ```"
  [p xs]
  (if-not (polynomial? p)
    p
    (let [a (bare-arity p)]
      (assert (<= (count xs) a)
              (str "Too many args: " xs))
      (cond (empty? xs) p
            (v/zero? p) 0
            :else
            (let [x (first xs)
                  x (if (and (polynomial? x) (> a 1))
                      (constant (dec a) x)
                      x)]
              (if (= a 1)
                (evaluate-1 p x)
                (let [L (evaluate-1 (lower-arity p) x)]
                  (if (polynomial? L)
                    (recur L (next xs))
                    L))))))))

(defn horner-with-error
  "Takes a univariate polynomial `a`, an argument `z` and a continuation
  `cont` (`vector` by default) and calls the continuation with (SEE BELOW).

  This Horner's rule evaluator is restricted to numerical coefficients and
  univariate polynomials. It returns by calling `cont` with 4 arguments:

  - the computed value
  - the values of the first two derivatives
  - an estimate of the roundoff error incurred in computing the value

  The recurrences used are from Kahan's 18 Nov 1986 paper ['Roundoff in
  Polynomial
  Evaluation'](https://people.eecs.berkeley.edu/~wkahan/Math128/Poly.pdf),
  generalized for sparse representations and another derivative by GJS.

  For $p = A(z)$, $q = A'(z)$, $r = A''(z)$, and $e$ = error in $A(x)$,

  $$p_{j+n} = z^n p_j + a_{j+n}$$

  $$e_{j+n} = |z|^n ( e_j + (n-1) p_j ) + |p_{j+n}|$$

  $$q_{j+n} = z^n q_j + n z^{n-1} p_j$$

  $$r_{j+n} = z^n r_j + n z^{n-1} q_j + 1/2 n (n-1) z^{n-2} p_j$$"
  ([a z]
   (horner-with-error a z vector))
  ([a z cont]
   {:pre [(univariate? a)
          (number? z)]}
   (letfn [(call [d p q r e a]
             (let [next-degree (degree a)
                   n (if (polynomial? a)
                       (- d next-degree)
                       d)
                   finish (fn [np nq nr ne]
                            (if (polynomial? a)
                              (call next-degree np nq nr ne
                                    (drop-leading-term a))
                              (cont np nq (* 2 nr)
                                    (* v/machine-epsilon
                                       (+ (- ne (Math/abs ^double np)) ne)))))]
               (cond (= n 1)
                     (let [np (+ (* z p) (leading-coefficient a))
                           nq (+ (* z q) p)
                           nr (+ (* z r) q)
                           ne (+ (* (Math/abs ^double z) e) (Math/abs ^double np))]
                       (finish np nq nr ne))
                     (= n 2)
                     (let [z-n (* z z)
                           az-n (Math/abs ^double z-n)
                           np (+ (* z-n p) (leading-coefficient a))
                           nq (+ (* z-n q) (* 2 (* z p)))
                           nr (+ (* z-n r) (* 2 z q) p)
                           ne (+ (* az-n (+ e p)) (Math/abs ^double np))]
                       (finish np nq nr ne))
                     :else
                     (let [z-n-2 (expt z (- n 2))
                           z-n-1 (* z-n-2 z)
                           z-n (* z-n-1 z)
                           az-n (Math/abs ^double z-n)
                           np (+ (* z-n p)
                                 (leading-coefficient a))
                           nq (+ (* z-n q)
                                 (* n (* z-n-1 p)))
                           nr (+ (* z-n r)
                                 (* n z-n-1 q)
                                 (* (/ 1 2) n (- n 1) z-n-2 p))
                           ne (+ (* az-n (+ e (* (- n 1) p)))
                                 (Math/abs ^double np))]
                       (finish np nq nr ne)))))]
     (let [lc (leading-coefficient a)]
       (call (degree a)
             lc
             0
             0
             (* (/ 1 2) (Math/abs ^double lc))
             (drop-leading-term a))))))

;; ## Scale and Shift

(defn arg-scale
  "Given some [[Polynomial]] `p`, returns a new [[Polynomial]] generated by
  substituting each indeterminate `x_i` for `f_i * x_i`, where `f_i` is a factor
  supplied in the `factors` sequence.

  When `p` is a multivariate [[Polynomial]], each factor must be either a
  non-[[Polynomial]] or a [[Polynomial]] with the same [[arity]] as `p`."
  [p factors]
  {:pre (= (arity p) (count factors))}
  (evaluate p (map mul
                   factors
                   (new-variables (arity p)))))

(defn arg-shift
  "Given some [[Polynomial]] `p`, returns a new [[Polynomial]] generated by
  substituting each indeterminate `x_i` for `s_i + x_i`, where `s_i` is a shift
  supplied in the `shifts` sequence.

  When `p` is a multivariate [[Polynomial]], each shift must be either a
  non-[[Polynomial]] or a [[Polynomial]] with the same [[arity]] as `p`."
  [p shifts]
  {:pre (= (arity p) (count shifts))}
  (evaluate p (map add
                   shifts
                   (new-variables (arity p)))))

;; ## Derivatives

(defn partial-derivative
  "Given some [[Polynomial]] `p`, returns the partial derivative of `p` with
  respect to the `i`th indeterminate. Throws if `i` is an invalid indeterminate
  index for `p`.

  For non-[[Polynomial]] inputs, returns `0`."
  [p i]
  (if-not (polynomial? p)
    0
    (do (validate-arity! p i)
        (make (bare-arity p)
              (for [[xs c] (bare-terms p)
                    :let [xi (xs i 0)]
                    :when (not= 0 xi)]
                (let [expts (if (= 1 xi)
                              (dissoc xs i)
                              (update xs i dec))
                      coeff (g/* xi c)]
                  (i/make-term expts coeff)))))))

(defn partial-derivatives
  "Returns the sequence of partial derivatives of [[Polynomial]] `p` with respect
  to each indeterminate. The returned sequence has length equal to the [[arity]]
  of `p`.

  For non-[[Polynomial]] inputs, returns an empty sequence."
  [p]
  (if-not (polynomial? p)
    []
    (for [i (range (bare-arity p))]
      (partial-derivative p i))))

;; ## Canonicalizer
;;
;; This section defines functions that allow conversion back and forth
;; between [[Polynomial]] instances and symbolic expressions.
;;
;; The `operator-table` represents the operations that can be understood from
;; the point of view of a polynomial over a commutative ring.

(def ^{:no-doc true
       :doc "These operations are those allowed between [[Polynomial]] and
       coefficient instances."}
  operator-table
  {'+ (ua/monoid g/add 0)
   '- (ua/group g/sub g/add g/negate 0)
   '* (ua/monoid g/mul 1 v/zero?)
   'negate g/negate
   'expt g/expt
   'square g/square
   'cube g/cube
   'gcd (ua/monoid g/gcd 0)
   'lcm (ua/monoid g/lcm 1 v/zero?)})

(def ^{:no-doc true
       :doc "Set of all arithmetic functions allowed between [[Polynomial]] and
       coefficient instances."}
  operators-known
  (u/keyset operator-table))

(defn expression->
  "Converts the supplied symbolic expression `expr` into Flat Polynomial canonical
  form (ie, a [[Polynomial]] instance). `expr` should be a bare, unwrapped
  expression built out of Clojure data structures.

  Returns the result of calling continuation `cont` with the [[Polynomial]] and
  the list of variables corresponding to each indeterminate in
  the [[Polynomial]]. (`cont `defaults to `vector`).

  The second optional argument `v-compare` allows you to provide a Comparator
  between variables. Sorting indeterminates by `v-compare` will determine the
  order of the indeterminates in the generated [[Polynomial]]. The list of
  variables passed to `cont` will be sorted using `v-compare`.

  Absorbing an expression with [[expression->]] and emitting it again
  with [[->expression]] will generate the canonical form of an expression, with
  respect to the operations in the [[operators-known]] set.

  This kind of simplification proceeds purely symbolically over the known Flat
  Polynomial operations; other operations outside the arithmetic available in
  polynomials over commutative rings should be factored out by an expression
  analyzer (see [[emmy.expression.analyze/make-analyzer]]) before
  calling [[expression->]].

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
         sym->var (zipmap sorted (new-variables arity))
         poly     (x/evaluate expr sym->var operator-table)]
     (cont poly sorted))))

(let [* (sym/symbolic-operator '*)
      + (sym/symbolic-operator '+)
      expt (sym/symbolic-operator 'expt)]
  (defn ->expression
    "Accepts a [[Polynomial]] `p` and a sequence of symbols for each indeterminate,
  and emits the canonical form of the symbolic expression that
  represents [[Polynomial]] `p`.

  A similar result could be achieved by calling `(apply p vars)`;
  but [[Polynomial]] application uses [Horner's
  rule](https://en.wikipedia.org/wiki/Horner%27s_method), and form of the
  returned result will be different.

  NOTE: this is the output stage of Flat Polynomial canonical form
  simplification. The input stage is handled by [[expression->]].

  NOTE See [[analyzer]] for an instance usable
  by [[emmy.expression.analyze/make-analyzer]]."
    [p vars]
    (if-not (polynomial? p)
      (x/expression-of p)
      (let [xform (map (fn [[expts c]]
                         (transduce
                          (map-indexed
                           (fn [i v]
                             (let [pow (xpt/monomial-degree expts i)]
                               (expt v pow))))
                          *
                          (x/expression-of c)
                          vars)))
            high->low (rseq (bare-terms p))]
        (transduce xform + high->low)))))

(def ^{:doc "Singleton [[emmy.expression.analyze/ICanonicalize]]
  instance."}
  analyzer
  (reify a/ICanonicalize
    (expression-> [_ expr cont]
      (expression-> expr cont))

    (expression-> [_ expr cont v-compare]
      (expression-> expr cont v-compare))

    (->expression [_ p vars]
      (->expression p vars))

    (known-operation? [_ o]
      (contains? operators-known o))))

;; ## Generic Implementations
;;
;; The functions above were written to handle any non-[[Polynomial]]
;; coefficient; it's up to the user to provide only commutative rings.
;;
;; The `::coeff` keyword allows the Emmy generic dispatch system to act as
;; a guard here. By default, modular integers and anything deriving from
;; `::v/scalar` will interact with [[Polynomial]] instances when they meet via
;; generic `+`, `-`, `*` and a few other functions defined below. To install
;; another type, simply use a `derive` form like those below.

(derive ::v/scalar ::coeff)
(derive ::mi/modint ::coeff)

(defn ^:no-doc defbinary
  "Installs the supplied function `f` into `generic-op` such that it will act
  between [[Polynomial]] instances, or allow non-[[Polynomial]] coefficients on
  either side."
  [generic-op f]
  (let [pairs [[::polynomial ::polynomial]
               [::coeff ::polynomial]
               [::polynomial ::coeff]]]
    (doseq [[l r] pairs]
      (defmethod generic-op [l r] [r s]
        (f r s)))))

;; `v/=` is not implemented with [[defbinary]] because the variable order needs
;; to change so that a [[Polynomial]] is always on the left.

(defmethod v/= [::polynomial ::polynomial] [l r] (eq l r))
(defmethod v/= [::polynomial ::coeff] [l r] (eq l r))
(defmethod v/= [::coeff ::polynomial] [l r] (eq r l))

(defbinary g/add add)
(defbinary g/sub sub)
(defbinary g/mul mul)
(defbinary g/quotient (fn [p q] (nth (divide p q) 0)))
(defbinary g/remainder (fn [p q] (nth (divide p q) 1)))

;; NOTE: What about `g/modulo`? Does that belong for [[Polynomial]] instances?
;; How does it differ from `g/remainder`?

(defmethod g/negative? [::polynomial] [a] (negative? a))
(defmethod g/abs [::polynomial] [a] (abs a))
(defmethod g/negate [::polynomial] [a] (negate a))
(defmethod g/square [::polynomial] [a] (square a))
(defmethod g/cube [::polynomial] [a] (cube a))

(defmethod g/expt [::polynomial ::v/native-integral] [b x] (expt b x))

(defmethod g/exact-divide [::polynomial ::polynomial] [p q] (evenly-divide p q))
(defmethod g/exact-divide [::polynomial ::coeff] [p c] (evenly-divide p c))

(defmethod g/simplify [::polynomial] [p]
  (map-coefficients g/simplify p))

(defmethod g/partial-derivative [::polynomial v/seqtype] [p selectors]
  (cond (empty? selectors)
        (if (= 1 (bare-arity p))
          (partial-derivative p 0)
          (ss/down* (partial-derivatives p)))

        (= 1 (count selectors))
        (partial-derivative p (first selectors))

        :else
        (u/illegal
         (str "Invalid selector! Only 1 deep supported."))))
