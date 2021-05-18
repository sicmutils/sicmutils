;;
;; Copyright © 2017 Colin Smith.
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
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj Seqable))))

;; # Flat Polynomial Form, for Commutative Rings
;;
;; The namespace starts by defining exponents (also called monomials), then
;; builds these into terms, then polynomials with a proper type definition.
;;
;; ## Monomials, Exponents, Coefficients
;;
;; Note about the terminology here, the two definitions you could use.
;; Explored [here](https://en.wikipedia.org/wiki/Monomial_order).
;;
;; A monomial is a single term of a single term of a polynomial. NOTE we need to
;; clarify this vs the polynomial single term with coef. that's called a `term`... okay, whatever.
;;
;; We represent the exponents of a monomial with a sorted map, keyed by integers
;; representing the indeterminates over some ring, with values set to each
;; indeterminate's exponent. For example; we would represent x^2 as {0 2}, and
;; xy^2 as {0 1, 1 2}. Polynomials are linear combinations of the exponents.
;;
;; TODO tidy this up!

(def ^{:doc "constructor."}
  expt:make
  #'sorted-map)

(def ^:no-doc empty-expts
  (expt:make))

(defn dense->exponents
  "Accepts a sequence of pairs of indeterminate index => power, and returns a
  sparse representation of the monomial."
  [idx->pow]
  (reduce-kv (fn [acc i x]
               (if (zero? x)
                 acc
                 (assoc acc i x)))
             empty-expts
             idx->pow))

(defn ^:no-doc expt:+ [l r]
  (merge-with + l r))

(defn ^:no-doc expt:- [l r]
  (dense->exponents
   (merge-with + l (u/map-vals - r))))

(defn expt:gcd
  "TODO: Boom, document. What if we have a multi-arity version that grabs all
  keysets, does a full intersection, THEN does the min? Faster?"
  ([l] l)
  ([l r]
   (let [l' (select-keys l (keys r))
         r' (select-keys r (keys l))]
     (merge-with min l' r'))))

(defn expt:max
  ([l] l)
  ([l r]
   (merge-with max l r)))

(defn expt:assoc [m i v]
  (if (zero? v)
    (dissoc m i)
    (assoc m i v)))

(defn- monomial-degree
  "Compute the degree of a monomial. This is just the sum of the exponents. If you pass an `i`, you'll get the degree of that term.

  TODO see where we use this... should we pass the full term?"
  ([m]
   (apply + (vals m)))
  ([m i]
   (m i 0)))

;; ### Monomial Orderings
;;
;; https://en.wikipedia.org/wiki/Monomial_order
;;
;; These comparators are in the sense of Java: x.compareTo(y), so that this
;; returns 1 if x > y, -1 if x < y, and 0 if x = y.

(defn ^:no-doc lex-order
  "Lex order for monomials considers the power of x, then the power of y, etc."
  [xs ys]
  (let [xs (vec xs)
        ys (vec ys)]
    (loop [i (long 0)]
      (let [x (nth xs i nil)
            y (nth ys i nil)]
        (cond (and (not x) (not y)) 0
              (not x) -1
              (not y)  1
              :else (let [bit (compare (nth x 0) (nth y 0))]
                      (cond (zero? bit)
                            (let [xv (nth x 1)
                                  yv (nth y 1)]
                              (if (= xv yv)
                                (recur (inc i))
                                (- xv yv)))
                            (neg? bit) 1
                            :else -1)))))))

(defn ^:no-doc graded-lex-order [xs ys]
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd)
      (lex-order xs ys)
      (- xd yd))))

(defn ^:no-doc graded-reverse-lex-order [xs ys]
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd)
      (lex-order (rseq ys)
                 (rseq xs))
      (- xd yd))))

;; the default.

(def ^{:dynamic true
       :doc "The order. NOTE that this currently breaks if we customize it."}
  *monomial-order*
  graded-lex-order)

;; ## Polynomial Terms
;;
;; Terms are represented as pairs of [<exponents>, <coef>]. A polynomial (called
;; an `fpf` in scmutils, for Flat Polynomial Form), is a sorted list of terms. A
;; single term is called a 'monomial' below.

(def ^:no-doc empty-terms [])

(defn make-term
  "Takes a monomial and a coefficient and returns a polynomial term."
  ([coef] [empty-expts coef])
  ([expts coef] [expts coef]))

(defn constant->terms [coef]
  (if (v/zero? coef)
    empty-terms
    (let [term [empty-expts coef]]
      [term])))

(defn exponents
  "Returns the exponent vector of the term.

  TODO rename to monomial."
  [term]
  (nth term 0 empty-expts))

(defn coefficient
  "Returns the coefficient entry of the term."
  [term]
  (nth term 1 0))

(defn- term->str [term]
  (let [expts (exponents term)
        coef  (coefficient term)]
    (str (pr-str coef) "*" (pr-str expts))))

(defn constant-term?
  "Returns true if the term has monomial with that is all zeros, false
  otherwise."
  [term]
  (v/zero?
   (exponents term)))

;; ## Polynomial Type Definition
;;
;; A polynomial is a sorted sequence of terms, plus an `arity` and maybe some
;; metadata.
;;
;; TODO implement:
;;
;; IComparable?, same with RF.
;;
;; TODO look at PowerSeries, see what we're missing.

(declare evaluate constant ->str eq
         map-coefficients)

(deftype Polynomial [arity terms m]
  f/IArity
  (arity [_] [:between 0 arity])

  sd/IPerturbed
  (perturbed? [_]
    (let [coefs (map coefficient terms)]
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
           (and (constant-term? term)
                (v/one? (coefficient term))))))

  (identity? [_]
    (and (v/one? arity)
         (= (count terms) 1)
         (let [[term] terms]
           (and (= {0 1} (exponents term))
                (v/one? (coefficient term))))))

  (zero-like [_]
    (Polynomial. arity empty-terms m))

  (one-like [_]
    (let [one (if-let [term (nth terms 0)]
                (v/one-like (coefficient term))
                1)]
      (constant arity one)))

  (identity-like [_]
    (assert (v/one? arity)
            "identity-like unsupported on multivariate monomials!")
    (let [one (if-let [term (nth terms 0)]
                (v/one-like (coefficient term))
                1)
          term (make-term (expt:make 0 1) one)]
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
       (invoke [this a b c d e f g h i j k l m-arg]
               (evaluate this [a b c d e f g h i j k l m-arg]))
       (invoke [this a b c d e f g h i j k l m-arg n]
               (evaluate this [a b c d e f g h i j k l m-arg n]))
       (invoke [this a b c d e f g h i j k l m-arg n o]
               (evaluate this [a b c d e f g h i j k l m-arg n o]))
       (invoke [this a b c d e f g h i j k l m-arg n o p]
               (evaluate this [a b c d e f g h i j k l m-arg n o p]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s t]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s t rest]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t rest]))
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
       (-invoke [this a b c d e f g h i j k l m-arg]
                (evaluate this [a b c d e f g h i j k l m-arg]))
       (-invoke [this a b c d e f g h i j k l m-arg n]
                (evaluate this [a b c d e f g h i j k l m-arg n]))
       (-invoke [this a b c d e f g h i j k l m-arg n o]
                (evaluate this [a b c d e f g h i j k l m-arg n o]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p]
                (evaluate this [a b c d e f g h i j k l m-arg n o p]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s t]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s t rest]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t rest]))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer
                              "#object[sicmutils.structure.Polynomial \""
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

;; ### Predicates

(defn polynomial?
  "Returns true if the supplied argument is an instance of [[Polynomial]], false
  otherwise."
  [x]
  (instance? Polynomial x))

(defn coeff?
  "Anything that is NOT an explicit polynomial is, helpfully, a potential coefficient."
  [x]
  (not (polynomial? x)))

(defn- sparse->terms
  "NOTE: Optionally takes a comparator."
  ([expts->coef]
   (sparse->terms expts->coef *monomial-order*))
  ([expts->coef comparator]
   (if (empty? expts->coef)
     []
     (->> (for [[expts terms] (group-by exponents expts->coef)
                :let [coef-sum (transduce
                                (map coefficient) g/+ terms)
                      expts (if (vector? expts)
                              (dense->exponents expts)
                              expts)]
                :when (not (v/zero? coef-sum))]
            (make-term expts coef-sum))
          (sort-by exponents comparator)
          (into empty-terms)))))

(defn- dense->terms
  "Takes a sequence of coefficients of a univariate polynomial and returns a
  sequence of terms."
  [coefs]
  (let [->term (fn [i coef]
                 (if (v/zero? coef)
                   []
                   [(make-term (if (zero? i)
                                 empty-expts
                                 (expt:make 0 i))
                               coef)]))
        xform  (comp (map-indexed ->term)
                     cat)]
    (into empty-terms xform coefs)))

(defn ^:no-doc terms->polynomial
  "Returns a [[Polynomial]] instance generated from a vector of terms. This method
  will do some mild cleanup:

  - any empty term list will return 0
  - a singleton term list with all zero exponents will return its coefficient

  NOTE this method assumes that the terms are properly sorted, and contain no
  zero coefficients.

  NOTE: Analogous to to `terms->differential`."
  [arity terms]
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (constant-term? (nth terms 0)))
        (let [c (coefficient (nth terms 0))]
          (if (polynomial? c)
            (->Polynomial arity terms)
            c))

        :else (->Polynomial arity terms)))

(defn make
  "When called with two arguments, the first is the arity
  (number of indeterminates) of the polynomial followed by a sequence of
  exponent-coefficient pairs. Each exponent should be a vector with length equal
  to the arity, with integer exponent values. To make 4 x^2 y + 5 x y^2, an
  arity 2 polynomial (since it has two variables, x and y), we could write the
  following for xc-pairs:
   [[[2 1] 4] [[1 2] 5]]

  When called with one argument, the sequence is interpreted as a dense sequence
  of coefficients of an arity-1 (univariate) polynomial. The coefficients begin
  with the constant term and proceed to each higher power of the indeterminate.
  For example, x^2 - 1 can be constructed by (make [-1 0 1]).

  TODO note that we now return a coefficient for constant, even if they sum to a
  constant. We try hard to get OUT of poly land!

  TODO note that we have to have ALL the same arity!

  TODO ALSO check that this is bailing out correctly."
  ([dense-coefficients]
   (let [terms (dense->terms dense-coefficients)]
     (terms->polynomial 1 terms)))
  ([arity expts->coef]
   (let [terms (sparse->terms expts->coef)]
     (terms->polynomial arity terms))))

(defn constant
  "Return a constant polynomial of the given arity.

  NOTE that zero coefficients always get filtered out."
  ([c] (constant 1 c))
  ([arity c]
   (->Polynomial arity (constant->terms c))))

(defn identity
  "TODO modeled on sparse-identity-term, check interface."
  ([]
   (identity 1 1))
  ([arity]
   (identity arity 1))
  ([arity i]
   {:pre [(and (> i 0) (<= i arity))]}
   (let [expts (expt:make (dec i) 1)]
     (->Polynomial arity [(make-term expts 1)]))))

(declare poly:+)

(defn linear
  "Makes a polynomial representing a linear equation."
  [arity i root]
  (if (v/zero? root)
    (identity arity i)
    (poly:+ (constant arity (g/negate root))
            (identity arity i))))

(defn c*xn
  "Polynomial representing c*x^n, where x is the first indeterminate."
  [arity c n]
  (cond (<= n -1)   0
        (v/zero? c) c
        (v/zero? n) (constant arity c)
        :else
        (let [term (make-term (expt:make 0 n) c)]
          (->Polynomial arity [term]))))

(defn ^:no-doc bare-arity [p]
  {:pre [(polynomial? p)]}
  (.-arity ^Polynomial p))

(defn ^:no-doc bare-terms [p]
  {:pre [(polynomial? p)]}
  (.-terms ^Polynomial p))

(def ^:no-doc zero-arity -1)
(def ^:no-doc coeff-arity 0)

(defn arity
  "TODO what's the difference between arity and degree?"
  [p]
  (if (polynomial? p)
    (bare-arity p)
    coeff-arity))

(defn ->terms
  "NOTE this is JUST like `->terms` in `differential`."
  [p]
  (cond (polynomial? p) (bare-terms p)
        (vector? p) p
        (v/zero? p) []
        :else [(make-term p)]))

(defn ^:no-doc check-same-arity
  "TODO works now for constants, check!

  TODO this will NOT return proper zero arity. Check?"
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

(defn ^:no-doc validate-arity [p i]
  (let [a (arity p)]
    (if (or (< i 0)
            (> i (arity p)))
      (u/arithmetic-ex
       (str "Supplied i " i " outside the bounds of arity " a " for input " p))
      i)))

(declare leading-term)

(defn degree
  "TODO what's the difference between arity and degree?

  https://en.wikipedia.org/wiki/Degree_of_a_polynomial

  zero polynomial: https://en.wikipedia.org/wiki/Degree_of_a_polynomial#Degree_of_the_zero_polynomial

  If you supply an arity, AND you have a polynomial, you'll get the max degree in that position."
  ([p]
   (cond (v/zero? p) zero-arity
         (polynomial? p)
         (monomial-degree
          (exponents
           (leading-term p)))
         :else coeff-arity))
  ([p i]
   (let [i (validate-arity p i)]
     (cond (v/zero? p) zero-arity
           (polynomial? p)
           (letfn [(i-degree [term]
                     (-> (exponents term)
                         (monomial-degree i)))]
             (transduce (map i-degree)
                        max 0
                        (bare-terms p)))
           :else coeff-arity))))

(defn eq
  "Polynomials are equal to a number if the polynomial is constant; otherwise
  it's only equal to other polynomials."
  [^Polynomial this that]
  (if (instance? Polynomial that)
    (let [p ^Polynomial that]
      (and (= (.-arity this) (.-arity p))
           (v/= (.-terms this)
                (.-terms p))))

    (let [terms (.-terms this)]
      (and (<= (count terms) 1)
           (let [term (peek terms)]
             (and (constant-term? term)
                  (v/= that (coefficient term))))))))

(defn- ->str
  ([p] (->str p 10))
  ([p n]
   {:pre [polynomial? p]}
   (let [terms     (bare-terms p)
         arity     (bare-arity p)
         n-terms   (count terms)
         term-strs (take n (map term->str terms))
         suffix    (when (> n-terms n)
                     (str "... and " (- n-terms n) " more terms"))]
     (str arity  ": (" (cs/join " + " term-strs) suffix ")"))))

;; ## Constructors



;; ## Relaxed Accessors



(defn coefficients
  "TODO see where this is used. Return a vector?"
  [p]
  (if (polynomial? p)
    (map coefficient (->terms p))
    [p]))

(defn leading-term
  "Return the leading (i.e., highest degree) term of the polynomial p. The return
  value is a pair of [exponents coefficient].

  TODO this is a change, returning an explicit term always. NOTE, test."
  [p]
  (or (peek (->terms p))
      [empty-expts 0]))

(defn leading-coefficient [p]
  (if (polynomial? p)
    (coefficient
     (peek (bare-terms p)))
    p))

(defn leading-monomial [p]
  (if (polynomial? p)
    (exponents
     (peek (bare-terms p)))
    empty-expts))

(defn monomial?
  "Returns true if `p` is a polynomial with a single term OR not a [[Polynomial]]
  at all, false otherwise."
  [p]
  (or (not (polynomial? p))
      (= 1 (count (bare-terms p)))))

(defn monic?
  "Returns true if `p` is a [monic
  polynomial](https://en.wikipedia.org/wiki/Monic_polynomial), false otherwise.

  TODO test that you can normalize by the lead coefficient to get a monic.
  Generate a dense then do that."
  [p]
  (if (polynomial? p)
    (and (= 1 (arity p))
         (v/one?
          (leading-coefficient p)))
    (v/one? p)))

(defn univariate? [p]
  (and (polynomial? p)
       (= (bare-arity p) 1)))





;; TODO check what this actually generates... I bet we do NOT want to generate
;; NON polynomials here, if the original does not!!
;;
;; TODO do we take an optional thing that flags the right `x` index?







;; ## Polynomial API

(defn new-variables
  "TODO NOTE: returns a sequence of `n` new polynomials of arity `n`, with the
  coefficient 1 and new indeterminates for each."
  [n]
  (map #(identity n %)
       (range 1 (inc n))))

(defn map-coefficients
  "Map the function f over the coefficients of p, returning a new Polynomial.

  TODO this demotes to coefficient when it needs to, and can take a bare coef."
  [f p]
  (if (polynomial? p)
    (terms->polynomial
     (bare-arity p)
     (into empty-terms
           (for [[xs c] (bare-terms p)
                 :let [fc (f c)]
                 :when (not (v/zero? fc))]
             [xs fc])))
    (f p)))

(defn map-exponents
  "Map the function f over the exponents of each monomial in p,
  returning a new Polynomial.

  TODO can handle bare coef now."
  [f p]
  (if (polynomial? p)
    (make (bare-arity p)
          (for [term (bare-terms p)]
            (make-term
             (f (exponents term))
             (coefficient term))))
    p))

;; ## Manipulations

(defn reciprocal
  "Returns the reciprical polynomial with respect to `i`, defaults to 0.
  https://en.wikipedia.org/wiki/Reciprocal_polynomial"
  ([p] (reciprocal p 0))
  ([p i]
   (if (polynomial? p)
     (let [d (degree p i)]
       (if (zero? d)
         p
         (map-exponents
          (fn [m]
            (let [v (monomial-degree m i)
                  v' (- d v)]
              (expt:assoc m i v')))
          p)))
     p)))

(defn extend
  "TODO interpolate a new variable in the `n` spot by expanding all vectors."
  [p n]
  )

(declare except-leading-term)

(defn contractible? [n p]
  (or (not (polynomial? p))
      (if (zero? n)
	      (= (degree p) 0)
        (and (< n (bare-arity p))
	           (cond (v/zero? p) true

                   ;; weird case... IF the coefficient of the lead term is
                   ;; contractible, then check if the whole thing is
                   ;; contractible WITHOUT the lead term.
		               (contractible?
                    (dec n)
                    ;; TODO WRONG... this is not the way for sparse!!
					          (coefficient (leading-term p)))

			             (contractible?
                    n
			              (except-leading-term (bare-arity p) p))

                   ;; Weird... this makes no sense!
		               :else false)))))

(declare poly:adjoin trailing-coefficient)

(defn contract [p n]
  (if (contractible? n p)
    (let [a (arity p)]
	    (if (zero? n)
	      (trailing-coefficient p)
        (letfn [(rec [p]
                  (if (v/zero? p)
                    p
		                (poly:adjoin (dec a)
				                         (degree p)
				                         (contract (dec n)
				                                   (coefficient (leading-term p)))
				                         (rec (except-leading-term a p)))))]
          (rec p))))
    (u/illegal (str "Poly not contractible" n p))))

(defn normalize
  "Note that we can take coefs on the left too..."
  [p c]
  (cond (v/zero? c) (u/arithmetic-ex
                     (str "Divide by zero: " p c))
        (v/one? c) p
        :else
        (let [c' (g/invert c)]
          ;; TODO why not divide??
          (map-coefficients #(g/* c' %) p))))

;; ## Polynomial Arithmetic

(defn negate [p]
  (map-coefficients g/negate p))

(def terms:+
  (ua/merge-fn #'*monomial-order* g/add v/zero? make-term))

(defn- binary-combine
  "TODO this is inefficient as hell for the TWO places we use it."
  [l r coeff-op terms-op]
  (let [l-poly? (polynomial? l)
        r-poly? (polynomial? r)]
    (cond (and l-poly? r-poly?)
          (terms->polynomial
           (check-same-arity l r)
           (terms-op (bare-terms l)
                     (bare-terms r)))

          l-poly?
          (terms->polynomial
           (bare-arity l)
	         (terms-op (bare-terms l)
			               (constant->terms r)))

          r-poly?
          (terms->polynomial
           (bare-arity r)
           (terms-op (constant->terms l)
                     (bare-terms r)))

          :else (coeff-op l r))))

(defn poly:+
  "Adds the polynomials p and q"
  [p q]
  (binary-combine p q g/add terms:+))

(defn poly:-
  "Subtract the polynomial q from the polynomial p."
  [p q]
  (poly:+ p (negate q)))

(defn- t*ts
  "Multiplies a single term on the left by a vector of `terms` on the right.
  Returns a new vector of terms."
  [[tags coeff] terms]
  (loop [acc []
         i 0]
    (let [t (nth terms i nil)]
      (if (nil? t)
        acc
	      (let [[tags1 coeff1] t]
	        (recur (conj acc (make-term
		                        (expt:+ tags tags1)
		                        (g/* coeff coeff1)))
		             (inc i)))))))

(defn mul-terms [xlist ylist]
  (letfn [(call [i]
            (let [x (nth xlist i nil)]
              (if (nil? x)
                []
                (terms:+ (t*ts x ylist)
	                       (call (inc i))))))]
    (call 0)))

(defn poly:*
  "Multiply polynomials p and q, and return the product."
  [p q]
  (binary-combine p q g/mul mul-terms))

(defn poly*coeff [p c]
  (map-coefficients #(g/* % c) p))

(defn coeff*poly [c p]
  (map-coefficients #(g/* c %) p))

(defn expt
  "Raise the polynomial p to the (integer) power n."
  [p n]
  (letfn [(expt-iter [x n answer]
            (cond (zero? n) answer
                  (even? n) (recur (poly:* x x) (quot n 2) answer)
                  :else     (recur x (dec n) (poly:* x answer))))]
    (cond
      (not (polynomial? p)) (g/expt p n)

      (not (v/native-integral? n))
      (u/illegal (str "Can only raise an FPF to an exact integer power: " p n))

      ;; Why not bump to a rational function? Not if you get into here...
      (neg? n)
      (u/illegal (str "No inverse -- FPF:EXPT:" p n))

      (v/one? p)  p
      (v/zero? p) (if (v/zero? n)
                    (u/arithmetic-ex "poly 0^0")
                    p)

      :else (expt-iter p n 1))))

(defn square [p]
  (poly:* p p))

(defn cube [p]
  (poly:* p (poly:* p p)))

;; TODO go from here! divide, then horner-eval, get those working (raise and
;; lower come along for the ride).
;;
;; then lock in to and from expressions... that covers fpf if that's all done.
;;
;; TODO oh! get accumulation going so that all this stuff actually works. That
;; will be a nice PR for itself.
;;
;; TODO implement more protocols, like IFn for SURE.
;;
;; TODO to power series, for arity == 1...
;;
;; TODO monic?
;;
;; TODO put `abs` back in! weird, it is `poly/abs`...
;;
;; TODO make proper equality method... use v/=
;;
;; TODO get notes from pcf, poly/div

(defn- poly:div
  "divide explicit polynomials."
  [u v]
  (let [arity (check-same-arity u v)
        [vn-exponents vn-coefficient] (leading-term v)
        ;; TODO note that this takes expts.
        good? (fn [residues]
                (every? (complement neg?) (vals residues)))]
    (if (zero? arity)
      [(g/div (leading-coefficient u) vn-coefficient) 0]
      (loop [quotient  0
             remainder u]
        ;; find a term in the remainder into which the
        ;; lead term of the divisor can be divided.
        (if (v/zero? remainder)
          [quotient remainder]
          (let [[r-exponents r-coefficient] (leading-term remainder)
                residues (expt:- r-exponents vn-exponents)]
            (if (good? residues)
              (let [new-coef (g/div r-coefficient vn-coefficient)
                    new-term (->Polynomial arity [(make-term residues new-coef)])]
                (recur (poly:+ quotient new-term)
                       (poly:- remainder (poly:* new-term v))))
              [quotient remainder])))))))

;; TODO: test
;; return test = (lambda (q r)
;;          (assert (poly/equal? u (poly/add (poly/mul q v) r))))

(defn divide
  "Divide polynomial u by v, and return the pair of [quotient, remainder]
  polynomials.

  This assumes that the coefficients are drawn from a field, and so support
  division."
  [u v]
  (cond (v/zero? v)
        (u/illegal "internal polynomial division by zero")

        (or (v/zero? u) (v/one? v))
        [u 0]

        (and (polynomial? u)
             (polynomial? v))
        (poly:div u v)

        (polynomial? u)
        (poly:div u (constant (bare-arity u) v))

        (polynomial? v)
        (poly:div (constant (bare-arity v) u) v)

        :else [(g/quotient u v) (g/remainder u v)]))

(defn divisible?
  "Returns true of the numerator `n` is evenly divisible by `d`, false otherwise."
  [n d]
  (let [[_ r] (divide n d)]
    (v/zero? r)))

(defn evenly-divide
  "Divides the polynomial u by the polynomial v. Throws an IllegalStateException
  if the division leaves a remainder. Otherwise returns the quotient."
  [u v]
  (let [[q r] (divide u v)]
    (when-not (v/zero? r)
      (u/illegal-state
       (str "expected even division left a remainder! " u " / " v " r " r)))
    q))

(defn abs
  "TODO this seems suspicious... how could this possibly be true? Maybe keep it
  but don't install?"
  [p]
  (if (g/negative?
       (leading-coefficient p))
    (negate p)
    p))

(defn- expt-down [expts]
  (reduce-kv (fn [acc k v]
               (assoc acc (dec k) v))
             empty-expts
             (dissoc expts 0)))

(defn expt-up [zero-expt expts]
  (let [m (reduce-kv (fn [acc k v]
                       (assoc acc (inc k) v))
                     empty-expts
                     expts)]
    (if (zero? zero-expt)
      m
      (assoc m 0 zero-expt))))

(defn lower-arity
  "Given a nonzero polynomial of arity A > 1, return an equivalent polynomial
  of arity 1 whose coefficients are polynomials of arity A-1."
  [p]
  {:pre [(polynomial? p)
         (> (bare-arity p) 1)
         (not (v/zero? p))]}
  ;; XXX observation:
  ;; XXX we often create polynomials of "one lower arity"
  ;; which are EFFECTIVELY UNIVARIATE. When this happens,
  ;; we should notice.
  ;; (but univariate in which variable? is it really that
  ;; common that it's the first one?)
  (let [A (bare-arity p)]
    (letfn [(lower-terms [terms]
              (let [coef-terms (sparse->terms
                                (for [[xs c] terms]
                                  [(expt-down xs) c]))]
                (->Polynomial (dec A) coef-terms)))]
      (->> (bare-terms p)
           (group-by #((exponents %) 0 0))
           (map (fn [[x terms]]
                  (let [expts (if (zero? x)
                                empty-expts
                                (expt:make 0 x))]
                    (make-term expts (lower-terms terms)))))
           (sparse->terms)
           (->Polynomial 1)))))

(comment
  ;; TODO this works... BUT I think we want to drop those constants down to
  ;; actual constants. But then what would raise do??
  (is (= (make 1 {[1] (constant 2 2)
                  [2] (constant 2 2)})
         (lower-arity
          (make 3 {[1 0 0] 2 [2 0 0] 2})))))

(defn raise-arity
  "The opposite of lower-arity. This needs a polynomial with terms that are
  THEMSELVES coefficients."
  [p a]
  (if (polynomial? p)
    (do (assert (= (bare-arity p) 1))
        (let [terms (sparse->terms
                     (for [[x q] (bare-terms p)
                           [ys c] (->terms q)]
                       [(expt-up (x 0 0) ys) c]))]
          (->Polynomial a terms)))
    (constant a p)))

(comment
  ;; TODO what about horner-with-error?
  (defn- horner-eval-general [terms args]
    (loop [[term & terms] terms
           expts (exponents term)
		       sum   (coefficient term)]
	    (if (empty? terms)
        (apply g/* (cons sum (map g/expt args expts)))
	      (let [new-expts (exponents (first terms))
              diff (expt:- expts new-expts)]
	        (recur (rest terms)
		             new-expts
		             (g/+ (coefficient (first terms))
                      (apply g/* (cons sum (map g/expt args diff))))))))))

;; TODO examine the horner univariate method to see if this is the same.
;;
;; TODO fix raise, lower
;;
;; TODO arg-shift, arg-scale for series, AND for polynomials...

;; POLY/HORNER-HELPER is used to evaluate a polynomial for a particular value of
;; the indeterminate. In general, the coefficients of the polynomial will
;; themselves be polynomials, which must be evaluated with values for their
;; indeterminates. The EVAL-COEFF argument will be used in the process of
;; lifting this system to multivariate polynomials. It will encapsulate the
;; process of evaluating the coefficients of the current polynomial on the rest
;; of the polynomial arguments.

(defn evaluate-1
  "Evaluates a univariate polynomial p at x.

  TODO check is this well formed, and proper horner?"
  [p x]
  (loop [terms  (->terms p)
         result 0
         x**e   1
         e      0]
    (if-let [[expts c] (first terms)]
      (let [e'    (expts 0 0)
            x**e' (g/* x**e (g/expt x (- e' e)))]
        (recur (next terms)
               (g/+ result (g/* c x**e'))
               x**e'
               e'))
      result)))

(defn evaluate
  "Evaluates a multivariate polynomial p at xs.

  If you provide too FEW, we just do a partial application.

  Too many args are ignored."
  [p xs]
  (let [a (arity p)]
    (cond (not (polynomial? p)) p
          (empty? xs) p
          (v/zero? p) 0
          (= a 1) (evaluate-1 p (first xs))
          :else (let [L (evaluate-1 (lower-arity p) (first xs))]
                  (if (polynomial? L)
                    (recur L (next xs))
                    L)))))

;; ## Scale and Shift

(declare horner)

(defn arg-scale
  "Given polynomial P(x), substitute x = r*y and compute the resulting polynomial
  Q(y) = P(y*r). When a multivariate polynomial is scaled, each factor must have
  the same arity as the given polynomial... or a base constant.
  "
  [p factors]
  {:pre (= (arity p) (count factors))}
  (horner p (map poly:*
		             factors
		             (new-variables (arity p)))))

(defn arg-shift
  "Given polynomial P(x), substitute x = y+h and compute the resulting polynomial
  Q(y) = P(y+h). When a multivariate polynomial is shifted, each shift must have
  the same arity as the given polynomial... or a base constant."
  [p shifts]
  {:pre (= (arity p) (count shifts))}
  (horner p (map poly:+
                 shifts
		             (new-variables (arity p)))))

;; ## GCD Related Things
;;
;; Pseudo division produces only a remainder--no quotient. This can be used to
;; generalize Euclid's algorithm for polynomials over a unique factorization
;; domain (UFD).
;;
;; Comment from GJS that does NOT apply to our implementation:
;;
;; This implementation differs from Knuth's Algorithm R in that Knuth's
;; contributes to the integerizing factor, making it l(v)^(m-n+1), even though
;; no factor of l(v) is needed if a u_j is zero for some n<j<m. This matters a
;; great deal in the multivariate case.
;;
;; Sussman's code -- good for Euclid Algorithm

(comment
  ;; somehow this gets arity 2!!! TODO make a test.
  (let [a1 (make 1 {[0] (make 2 {[1 0] 1})
                    [1] 1})
        a2 (make 1 {[0] (make 2 {[0 1] 1})
                    [1] 1})]
    (g/- a1 a2))

  (let [a1 (make 1 {[0] (make 2 {[1 0] 1})
                    [1] (constant 2 1)})
        a2 (make 1 {[0] (make 2 {[0 1] 1})
                    [1] (constant 2 1)})]
    (g/- a1 a2)))

(defn pseudo-remainder
  "Compute the pseudo-remainder of univariate polynomials p and q.

  Fractions won't appear in the result; instead the divisor is multiplied by the
  leading coefficient of the dividend before quotient terms are generated so
  that division will not result in fractions. Only the remainder is returned,
  together with the integerizing factor needed to make this happen.

  Similar in spirit to Knuth's algorithm 4.6.1R, except we don't multiply the
  remainder through during gaps in the remainder. Since you don't know up front
  how many times the integerizing multiplication will be done, we also return
  the number d for which d * u = q * v + r.

  TODO note that `d` is the integerizing coefficient."
  [u v]
  {:pre [(polynomial? u)
         (= (bare-arity u) 1)
         (polynomial? v)
         (= (bare-arity v) 1)
         (not (v/zero? v))]}
  (let [[vn-exponents vn-coefficient] (leading-term v)
        *vn (fn [p] (poly*coeff p vn-coefficient))
        n (monomial-degree vn-exponents)]
    (loop [remainder u
           d         0]
      (let [m (degree remainder)
            c (leading-coefficient remainder)]
        (if (< m n)
          [remainder d]
          (recur (poly:- (*vn remainder)
                         (poly:* (c*xn 1 c (g/- m n))
                                 v))
                 (inc d)))))))

;; ## Derivatives

(defn partial-derivative
  "The partial derivative of the polynomial with respect to the
  i-th indeterminate.

  TODO this is confused, with its special case for numbers."
  [p i]
  (if (polynomial? p)
    (make (bare-arity p)
          (for [[xs c] (bare-terms p)
                :let [xi (xs i 0)]
                :when (not= 0 xi)]
            (let [expts (if (= 1 xi)
                          (dissoc xs i)
                          (update xs i dec))]
              [expts (g/* xi c)])))
    0))

(defn partial-derivatives
  "The sequence of partial derivatives of p with respect to each
  indeterminate. Return value has length `(arity p)`."
  [p]
  (if (polynomial? p)
    (for [i (range (bare-arity p))]
      (partial-derivative p i))
    []))

;; ## Canonicalizer

;; The operator-table represents the operations that can be understood
;; from the point of view of a polynomial over a commutative ring. The
;; functions take polynomial inputs and return polynomials.

(def ^:private operator-table
  {'+ (ua/monoid poly:+ 0)
   '- (ua/group poly:- poly:+ negate 0)
   '* (ua/monoid poly:* 1 v/zero?)
   'negate negate
   'expt expt
   'square square
   'cube cube
   'gcd g/gcd})

(def ^:no-doc operators-known
  (u/keyset operator-table))

(let [*     (sym/symbolic-operator '*)
      +     (sym/symbolic-operator '+)
      expt  (sym/symbolic-operator 'expt)]
  (defn ->expression
    "This is the output stage of Flat Polynomial canonical form simplification. The
  input is a Polynomial object, and the output is an expression representing the
  evaluation of that polynomial over the indeterminates extracted from the
  expression at the start of this process."
    [p vars]
    (if (polynomial? p)
      (let [xform (map (fn [[xs c]]
                         (->> (map-indexed (fn [i v]
                                             (expt v (xs i 0)))
                                           vars)
                              (reduce *)
                              (* c))))]
        (->> (bare-terms p)
             (sort-by exponents #(*monomial-order* %2 %1))
             (transduce xform +)))
      p)))

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
         sorted   (sort v-compare vars)
         sym->var (zipmap sorted (new-variables (count sorted)))
         expr'    (x/evaluate expr sym->var operator-table)]
     (cont expr' sorted))))

;; ## Analyzer Wrapper

(deftype PolynomialAnalyzer []
  a/ICanonicalize
  (expression-> [_ expr cont]
    (expression-> expr cont))

  (expression-> [_ expr cont v-compare]
    (expression-> expr cont v-compare))

  (->expression [_ p vars]
    (->expression p vars))

  (known-operation? [_ o]
    (contains? operators-known o)))

(def ^{:doc "Singleton [[a/ICanonicalize]] instance."}
  analyzer
  (->PolynomialAnalyzer))

;; ## Generic Implementations

(derive ::v/scalar ::coeff)
(derive ::mi/modint ::coeff)

;; TODO: add `partial-derivative`., `simplify`, `solve-linear-right`,
;; `solve-linear`,

(defmethod v/= [::polynomial ::coeff] [l r] (eq l r))
(defmethod v/= [::coeff ::polynomial] [l r] (eq r l))

(defmethod g/negate [::polynomial] [a] (negate a))

(defmethod g/add [::polynomial ::polynomial] [a b] (poly:+ a b))
(defmethod g/mul [::polynomial ::polynomial] [a b] (poly:* a b))
(defmethod g/sub [::polynomial ::polynomial] [a b] (poly:- a b))

(defmethod g/exact-divide [::polynomial ::polynomial] [p q]
  (evenly-divide p q))

(defmethod g/exact-divide [::polynomial ::coeff] [p c]
  (evenly-divide p c))

(defmethod g/exact-divide [::coeff ::polynomial] [c p]
  (let [[term :as terms] (bare-terms p)]
    (if (and (= (count terms) 1)
             (constant-term? term))
      (g/exact-divide c (coefficient term))
      (u/illegal (str "Can't divide coefficient by polynomial: " c ", " p)))))

;; quotient, remainder, modulo... TODO search for more functions!

(defmethod g/square [::polynomial] [a] (poly:* a a))
(defmethod g/abs [::polynomial] [a] (abs a))

(defmethod g/mul [::coeff ::polynomial] [c p]
  (coeff*poly c p))

(defmethod g/mul [::polynomial ::coeff] [p c]
  (poly*coeff p c))

(defmethod g/add [::coeff ::polynomial] [c p]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:+ (constant (bare-arity p) c) p))

(defmethod g/add [::polynomial ::coeff] [p c]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:+ p (constant (bare-arity p) c)))

(defmethod g/sub [::coeff ::polynomial] [c p]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:- (constant (bare-arity p) c) p))

(defmethod g/sub [::polynomial ::coeff] [p c]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:- p (constant (bare-arity p) c)))

(defmethod g/div [::polynomial ::coeff] [p c]
  ;; TODO pull this out into its own thing.
  (map-coefficients #(g/divide % c) p))

(defmethod g/expt [::polynomial ::v/native-integral] [b x] (expt b x))
