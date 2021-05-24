;;
;; Copyright © 2021 Sam Richie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.polynomial
  (:refer-clojure :exclude [extend divide identity])
  (:require [clojure.set :as set]
            [clojure.string :as cs]
            [sicmutils.collection]
            [sicmutils.differential :as sd]
            [sicmutils.expression.analyze :as a]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.modint :as mi]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial.exponent :as xpt]
            [sicmutils.polynomial.impl :as i]
            [sicmutils.series :as series]
            [sicmutils.structure :as ss]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v])
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
;; SICMUtils makes the following implementation choices:
;;
;; - The exponents of a monomial are represented by an ordered mapping of
;;   variable index => the exponent of that variable. $x^2z^3$ is represented as
;;   `{0 2, 2 3}`, for example. See `sicmutils.polynomial.exponent` for the full
;;   set of operations you can perform on a term's exponents.
;;
;;  This representation is called `sparse` because variables with a 0 exponent
;;  aren't included.
;;
;; - A monomial term is a vector of the form `[<exponents>, <coefficient>]`. The
;;   coefficient can be any type! It's up to the user to supply coefficients
;;   drawn from a commutative ring. See [[sicmutils.laws/ring]] for a
;;   description of the properties coefficients should satisfy.
;;
;; - A polynomial is a sorted vector of monomials, sorted in some
;;   consistent [Monomial order](https://en.wikipedia.org/wiki/Monomial_order).
;;   See [[sicmutils.polynomial.impl/*monomial-order*]] for the current default.
;;
;; `sicmutils.polynomial.impl` builds up polynomial arithmetic on bare vectors
;; of monomials, for efficiency's sake. This namespace builds this base out into
;; a full [[Polynomial]] data structure with a fleshed-out API.
;;
;; To follow along in full, first read `sicmutils.polynomial.exponent`, then
;; `sicmutils.polynomial.impl`... then come back and continue from here.
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
       (withMeta [_ meta] (Polynomial. arity terms m))

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
               (evaluate this [a b c d e f g h i j k l m n o p q r s t rest]))
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
                (evaluate this [a b c d e f g h i j k l m n o p q r s t rest]))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer
                              "#object[sicmutils.polynomial.Polynomial \""
                              (.toString x)
                              "\"]"))]))

(do (ns-unmap 'sicmutils.polynomial '->Polynomial)
    (defn ->Polynomial
      "Positional factory function for [[Polynomial]].

  The final argument `m` defaults to nil if not supplied."
      ([arity terms]
       (Polynomial. arity terms nil))
      ([arity terms m]
       (Polynomial. arity terms m))))

(defn polynomial?
  "Returns true if the supplied argument is an instance of [[Polynomial]], false
  otherwise."
  [x]
  (instance? Polynomial x))

(defn coeff?
  "Returns true if the input `x` is explicitly _not_ an instance of [[Polynomial]], false otherwise.

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
            (->Polynomial arity terms)
            c))

        :else (->Polynomial arity terms)))

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

  - a proper exponent entry created by `sicmutils.polynomial.exponent`
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
  (make 10 [[{} 1] [{} 2]])
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
   (->Polynomial arity (i/constant->terms c))))

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
   (identity 1 1))
  ([arity]
   (identity arity 1))
  ([arity i]
   {:pre [(and (> i 0) (<= i arity))]}
   (let [expts (xpt/make (dec i) 1)]
     (->Polynomial arity [(i/make-term expts 1)]))))

(defn new-variables
  "Returns a sequence of `n` monomials of arity `n`, each with an exponent of `1`
  for the `i`th indeterminate (where `i` matches the position in the returned
  sequence)."
  [n]
  (map #(identity n %)
       (range 1 (inc n))))

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

  - If `c` is [[sicmutils.value/zero?]], returns `c`
  - if `n` is `zero?`, returns `(constant arity c)`

  NOTE that negative exponents are not allowed."
  [arity c n]
  {:pre [(>= n 0)]}
  (cond (v/zero? c) c
        (zero? n)   (constant arity c)
        :else
        (let [term (i/make-term (xpt/make 0 n) c)]
          (->Polynomial arity [term]))))

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
          poly-p? (bare-arity q)
          poly-q? (bare-arity p)
          :else coeff-arity)))

(defn ^:no-doc validate-arity
  "Given some input `p` and an indeterminate index `i`, returns `i` if `0 <= i
  < (arity p)`, and throws an exception otherwise.

  NOTE [[validate-arity]] is meant to validate indeterminate indices; thus it
  will always throw for non-[[Polynomial]] inputs."
  [p i]
  (let [a (arity p)]
    (if (or (< i 0)
            (>= i (arity p)))
      (u/arithmetic-ex
       (str "Supplied i " i " outside the bounds of arity " a " for input " p))
      i)))

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
   (let [i (validate-arity p i)]
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
  equal arity. Coefficients are compared using [[sicmutils.value/=]].

  If `that` Non-[[Polynomial]], `eq` only returns true if `this` is a monomial
  and its coefficient is equal to `that` (again using [[sicmutils.value/=]])."
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
     (nth (bare-terms p) 0))
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
  responds `true` to [[sicmutils.value/one?]]. This means that any coefficient
  that responds `true` to [[sicmutils.value/one?]] also qualifies as a monic
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
  resulting zeros.

  Given a non-[[Polynomial]] coefficient, acts as identity.

  NOTE that [[map-exponents]] will return a non-[[Polynomial]] if the result
  of the mapping has only a constant term."
  ([f p]
   (map-exponents f p (arity p)))
  ([f p new-arity]
   (if (polynomial? p)
     (make new-arity
           (for [[expts c] (bare-terms p)
                 :let [f-expts (f expts)]]
             (i/make-term f-expts c)))
     p)))

;; ## Manipulations

(defn univariate->dense
  "Given a univariate [[Polynomial]] (see [[univariate?]]) returns a dense vector
  of the coefficients of each term in ascending order.

  For example:

  ```clojure
  (univariate->dense (make [1 0 0 2 3 4]))
  ;;=> [1 0 0 2 3 4]
  ```

  NOTE use [[lower-arity]] to generate a univariate polynomial in the first
  indeterminate, given a multivariate polynomial."
  [x]
  {:pre [(univariate? x)]}
  (let [d (degree x)]
    (loop [terms (bare-terms x)
           acc (transient [])
           i 0]
      (if (> i d)
        (persistent! acc)
        (let [t  (first terms)
              e  (i/exponents t)
              md (xpt/monomial-degree e 0)]
          (if (= md i)
            (recur (rest terms)
                   (conj! acc (i/coefficient t))
                   (inc i))
            (recur terms
                   (conj! acc 0)
                   (inc i))))))))

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
  satisfies [[sicmutils.value/zero?]]."
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
     "Clojurescript multiplication doesn't autopromote; we expect large values
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
          (recur (sub (*vn remainder)
                      (mul (c*xn 1 c (- m n))
                           v))
                 (inc d)))))))

;; ## Polynomial Contraction and Expansion
;;
;; The following functions are helpful for TODO finish, collapsing etc

(defn contractible? [n p]
  (zero?
   (degree p n)))

(defn contract
  "IF the variable is dead everywhere, contracts it!"
  [p n]
  (cond (not (polynomial? p)) p

        (contractible? n p)
        (map-exponents #(xpt/lower % n)
                       p
                       (dec (bare-arity p)))

        :else
        (u/illegal
         (str "Polynomial not contractible: " p " in position " n))))

(defn extend
  "interpolates a new variable in the `n` spot by bumping all indices higher than
  the `n` supplied."
  [p n]
  (if-not (polynomial? p)
    p
    (let [a (bare-arity p)
          new-arity (inc (max a n))]
      (if (> n a)
        (->Polynomial (inc n) (bare-terms p))
        (map-exponents #(xpt/raise % n 0)
                       p
                       (inc a))))))

(defn lower-arity
  "Given a polynomial of arity A > 1, return an equivalent polynomial of arity 1
  whose coefficients are polynomials of arity A-1.

  NOTE that this will drop down to a constant."
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
  "The opposite of lower-arity. This needs a polynomial with terms that are
  THEMSELVES coefficients."
  [p a]
  (if (polynomial? p)
    (do (assert (= (bare-arity p) 1))
        (let [terms (i/sparse->terms
                     (for [[x q] (bare-terms p)
                           [ys c] (->terms q)
                           :let [expts (xpt/raise ys 0 (xpt/monomial-degree x 0))]]
                       (i/make-term expts c)))]
          (->Polynomial a terms)))
    (constant a p)))

;; ## Evaluation
;;
;; `evaluate-1` is used to evaluate a polynomial for a particular value of the
;; indeterminate. In general, the coefficients of the polynomial will themselves
;; be polynomials, which must be evaluated with values for their indeterminates.

(defn- evaluate-1
  "Evaluates a univariate polynomial p at x using Horner's rule."
  [p x]
  (if-not (polynomial? p)
    p
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
        result))))

(defn evaluate
  "Evaluates a multivariate polynomial p at xs using Horner's rule.

  If you provide too FEW arguments, we just do a partial application. Too many
  args throw an error."
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
                  x (if (polynomial? x)
                      (constant (dec a) x)
                      x)]
              (if (= a 1)
                (evaluate-1 p x)
                (let [L (evaluate-1 (lower-arity p) x)]
                  (if (polynomial? L)
                    (recur L (next xs))
                    L))))))))

(defn horner-with-error
  "Takes a univariate polynomial `a`, an argument `z` and a continuation and calls
  the continuation with (SEE BELOW).

  This Horner's rule evaluator is restricted to numerical coefficients and
  univariate polynomials. It returns, by calling a continuation procedure, a
  value, two derivatives, and an estimate of the roundoff error incurred in
  computing that value.

  The recurrences used are from Kahan's 18 Nov 1986 paper ['Roundoff in
  Polynomial
  Evaluation'](https://people.eecs.berkeley.edu/~wkahan/Math128/Poly.pdf),
  generalized for sparse representations and another derivative by GJS. For p =
  A(z), q = A'(z), r = A''(z), and e = error in A(x),

  p_{j+n} = z^n p_j + a_{j+n}

  e_{j+n} = |z|^n ( e_j + (n-1) p_j ) + |p_{j+n}|

  q_{j+n} = z^n q_j + n z^{n-1} p_j

  r_{j+n} = z^n r_j + n z^{n-1} q_j + 1/2 n (n-1) z^{n-2} p_j"
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
                                       (+ (- ne (Math/abs np)) ne)))))]
	             (cond (= n 1)
		                 (let [np (+ (* z p) (leading-coefficient a))
			                     nq (+ (* z q) p)
			                     nr (+ (* z r) q)
			                     ne (+ (* (Math/abs z) e) (Math/abs np))]
		                   (finish np nq nr ne))
		                 (= n 2)
		                 (let [z-n (* z z)
			                     az-n (Math/abs z-n)
			                     np (+ (* z-n p) (leading-coefficient a))
			                     nq (+ (* z-n q) (* 2 (* z p)))
			                     nr (+ (* z-n r) (* 2 z q) p)
			                     ne (+ (* az-n (+ e p)) (Math/abs np))]
		                   (finish np nq nr ne))
		                 :else
		                 (let [z-n-2 (expt z (- n 2))
			                     z-n-1 (* z-n-2 z)
			                     z-n (* z-n-1 z)
			                     az-n (Math/abs z-n)
			                     np (+ (* z-n p)
				                         (leading-coefficient a))
			                     nq (+ (* z-n q)
				                         (* n (* z-n-1 p)))
			                     nr (+ (* z-n r)
				                         (* n z-n-1 q)
				                         (* (/ 1 2) n (- n 1) z-n-2 p))
			                     ne (+ (* az-n (+ e (* (- n 1) p)))
				                         (Math/abs np))]
		                   (finish np nq nr ne)))))]
     (let [lc (leading-coefficient a)]
       (call (degree a)
		         lc
		         0
		         0
		         (* (/ 1 2) (Math/abs lc))
		         (drop-leading-term a))))))

;; ## Scale and Shift

(defn arg-scale
  "Given polynomial P(x), substitute x = r*y and compute the resulting polynomial
  Q(y) = P(y*r). When a multivariate polynomial is scaled, each factor must have
  the same arity as the given polynomial... or a base constant.
  "
  [p factors]
  {:pre (= (arity p) (count factors))}
  (evaluate p (map mul
		               factors
		               (new-variables (arity p)))))

(defn arg-shift
  "Given polynomial P(x), substitute x = y+h and compute the resulting polynomial
  Q(y) = P(y+h). When a multivariate polynomial is shifted, each shift must have
  the same arity as the given polynomial... or a base constant."
  [p shifts]
  {:pre (= (arity p) (count shifts))}
  (evaluate p (map add
                   shifts
		               (new-variables (arity p)))))

;; ## Derivatives

(defn partial-derivative
  "The partial derivative of the polynomial with respect to the
  i-th indeterminate.

  TODO this is confused, with its special case for numbers."
  [p i]
  (if-not (polynomial? p)
    0
    (make (bare-arity p)
          (for [[xs c] (bare-terms p)
                :let [xi (xs i 0)]
                :when (not= 0 xi)]
            (let [expts (if (= 1 xi)
                          (dissoc xs i)
                          (update xs i dec))
                  coeff (g/* xi c)]
              (i/make-term expts coeff))))))

(defn partial-derivatives
  "The sequence of partial derivatives of p with respect to each
  indeterminate. Return value has length `(arity p)`."
  [p]
  (if-not (polynomial? p)
    []
    (for [i (range (bare-arity p))]
      (partial-derivative p i))))

;; ## Canonicalizer
;;
;; The operator-table represents the operations that can be understood from the
;; point of view of a polynomial over a commutative ring. The functions take
;; polynomial inputs and return polynomials.

(def ^:no-doc operator-table
  {'+ (ua/monoid g/add 0)
   '- (ua/group g/sub g/add g/negate 0)
   '* (ua/monoid g/mul 1 v/zero?)
   'negate g/negate
   'expt g/expt
   'square g/square
   'cube g/cube
   'gcd (ua/monoid g/gcd 0)
   'lcm (ua/monoid g/lcm 1 v/zero?)})

(def ^:no-doc operators-known
  (u/keyset operator-table))

(let [* (sym/symbolic-operator '*)
      + (sym/symbolic-operator '+)
      expt (sym/symbolic-operator 'expt)]
  (defn ->expression
    "This is the output stage of Flat Polynomial canonical form simplification. The
  input is a Polynomial object, and the output is an expression representing the
  evaluation of that polynomial over the indeterminates extracted from the
  expression at the start of this process.

  TODO NOTE, why are we not using horner evaluation?"
    [p vars]
    (if-not (polynomial? p)
      p
      (let [xform (map (fn [[expts c]]
                         (transduce
                          (map-indexed
                           (fn [i v]
                             (let [pow (xpt/monomial-degree expts i)]
                               (expt v pow))))
                          * c vars)))
            high->low (rseq (bare-terms p))]
        (transduce xform + high->low)))))

(defn expression->
  "Convert an expression into Flat Polynomial canonical form.

  The expression should be an unwrapped expression, i.e., not an instance of the
  Literal type, nor should subexpressions contain type information.

  This kind of simplification proceeds purely symbolically over the known Flat
  Polynomial operations; other operations outside the arithmetic available in
  polynomials over commutative rings should be factored out by an expression
  analyzer before we get here. The result is a Polynomial object representing
  the polynomial structure of the input over the unknowns."
  ([expr]
   (expression-> expr vector compare))
  ([expr cont]
   (expression-> expr cont compare))
  ([expr cont v-compare]
   (let [vars     (set/difference (x/variables-in expr) operators-known)
         arity    (count vars)
         sorted   (sort v-compare vars)
         sym->var (zipmap sorted (new-variables arity))
         expr'    (x/evaluate expr sym->var operator-table)]
     (cont expr' sorted))))

(def ^{:doc "Singleton [[a/ICanonicalize]] instance."}
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

(derive ::v/scalar ::coeff)
(derive ::mi/modint ::coeff)

(defn ^:no-doc defbinary [generic-op f]
  (let [pairs [[::polynomial ::polynomial]
               [::coeff ::polynomial]
               [::polynomial ::coeff]]]
    (doseq [[l r] pairs]
      (defmethod generic-op [l r] [r s]
        (f r s)))))

;; These aren't implemented with [[defbinary]] because the variable order needs
;; to change so that a [[Polynomial]] is always on the left.

(defmethod v/= [::polynomial ::polynomial] [l r] (eq l r))
(defmethod v/= [::polynomial ::coeff] [l r] (eq l r))
(defmethod v/= [::coeff ::polynomial] [l r] (eq r l))

(defbinary g/add add)
(defbinary g/sub sub)
(defbinary g/mul mul)
(defbinary g/quotient (fn [p q] (nth (divide p q) 0)))
(defbinary g/remainder (fn [p q] (nth (divide p q) 1)))

(defmethod g/negative? [::polynomial] [a] (negative? a))
(defmethod g/abs [::polynomial] [a] (abs a))
(defmethod g/negate [::polynomial] [a] (negate a))
(defmethod g/square [::polynomial] [a] (square a))
(defmethod g/cube [::polynomial] [a] (cube a))

(defmethod g/expt [::polynomial ::v/native-integral] [b x] (expt b x))

(defmethod g/exact-divide [::polynomial ::polynomial] [p q] (evenly-divide p q))
(defmethod g/exact-divide [::polynomial ::coeff] [p c] (evenly-divide p c))
(defmethod g/exact-divide [::coeff ::polynomial] [c p]
  (let [[term :as terms] (bare-terms p)]
    (if (and (= (count terms) 1)
             (i/constant-term? term))
      (g/exact-divide c (i/coefficient term))
      (u/illegal (str "Can't divide coefficient by polynomial: " c ", " p)))))

(defmethod g/simplify [::polynomial] [p]
  (map-coefficients g/simplify p))

(defmethod g/partial-derivative [::polynomial v/seqtype] [p selectors]
  (cond (empty? selectors)
        (ss/down* (partial-derivatives p))

        (= 1 (count selectors))
        (partial-derivative p (first selectors))

        :else
        (u/illegal
         (str "Invalid selector! Only 1 deep supported."))))
