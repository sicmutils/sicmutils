;;xpt
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
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj Seqable))))

;; # Flat Polynomial Form, for Commutative Rings
;;
;; The namespace starts by defining exponents (also called monomials), then
;; builds these into terms, then polynomials with a proper type definition.

(def ^{:dynamic true
       :doc "The order. NOTE that this currently breaks if we customize it."}
  *monomial-order*
  xpt/graded-lex-order)

;; ## Polynomial Terms
;;
;; Terms are represented as pairs of [<exponents>, <coef>]. A polynomial (called
;; an `fpf` in scmutils, for Flat Polynomial Form), is a sorted list of terms. A
;; single term is called a 'monomial' below.

(def ^:no-doc empty-terms [])

(defn make-term
  "Takes a monomial and a coefficient and returns a polynomial term."
  ([coef] [xpt/empty coef])
  ([expts coef] [expts coef]))

(defn exponents
  "Returns the exponent vector of the term.

  TODO rename to monomial."
  [term]
  (nth term 0 xpt/empty))

(defn coefficient
  "Returns the coefficient entry of the term."
  [term]
  (nth term 1 0))

(defn- term->str [term]
  (let [expts (exponents term)
        coef  (coefficient term)]
    (str (pr-str coef) "*" (pr-str expts))))

(defn constant->terms [coef]
  (if (v/zero? coef)
    empty-terms
    [(make-term xpt/empty coef)]))

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

(declare evaluate constant ->str eq map-coefficients)

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
    (if-let [term (nth terms 0)]
      (v/zero-like (coefficient term))
      0))

  (one-like [_]
    (if-let [term (nth terms 0)]
      (v/one-like (coefficient term))
      1))

  (identity-like [_]
    (assert (v/one? arity)
            "identity-like unsupported on multivariate monomials!")
    (let [one (if-let [term (nth terms 0)]
                (v/one-like (coefficient term))
                1)
          term (make-term (xpt/make 0 1) one)]
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
  "Anything that is NOT an explicit polynomial is, helpfully, a potential coefficient."
  [x]
  (not (polynomial? x)))

(defn ^:no-doc bare-arity [p]
  {:pre [(polynomial? p)]}
  (.-arity ^Polynomial p))

(defn ^:no-doc bare-terms [p]
  {:pre [(polynomial? p)]}
  (.-terms ^Polynomial p))

;; ## Constructors

(defn- sparse->terms
  "NOTE: Optionally takes a comparator, defaults to the dynamically bound one."
  ([expts->coef]
   (sparse->terms expts->coef *monomial-order*))
  ([expts->coef comparator]
   (if (empty? expts->coef)
     empty-terms
     (->> (for [[expts terms] (group-by exponents expts->coef)
                :let [coef-sum (transduce
                                (map coefficient) g/+ terms)]
                :when (not (v/zero? coef-sum))
                :let [expts (if (vector? expts)
                              (xpt/dense->exponents expts)
                              expts)]]
            (make-term expts coef-sum))
          (sort-by exponents comparator)
          (into empty-terms)))))

(defn- dense->terms
  "Takes a sequence of coefficients of a univariate polynomial and returns a
  sequence of terms."
  [coefs]
  (let [->term (fn [i coef]
                 (when-not (v/zero? coef)
                   (let [expts (if (zero? i)
                                 xpt/empty
                                 (xpt/make 0 i))]
                     [(make-term expts coef)])))
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
   (let [expts (xpt/make (dec i) 1)]
     (->Polynomial arity [(make-term expts 1)]))))

(defn new-variables
  "Returns a lazy sequence of new variables.

  TODO NOTE: returns a sequence of `n` new polynomials of arity `n`, with the
  coefficient 1 and new indeterminates for each."
  [n]
  (map #(identity n %)
       (range 1 (inc n))))

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
        (zero? n) (constant arity c)
        :else
        (let [term (make-term (xpt/make 0 n) c)]
          (->Polynomial arity [term]))))

;; ###  Accessors, Predicates

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
            (>= i (arity p)))
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
         (xpt/monomial-degree
          (exponents
           (leading-term p)))
         :else coeff-arity))
  ([p i]
   (let [i (validate-arity p i)]
     (cond (v/zero? p) zero-arity
           (polynomial? p)
           (letfn [(i-degree [term]
                     (-> (exponents term)
                         (xpt/monomial-degree i)))]
             (transduce (map i-degree)
                        max
                        0
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

(defn ->str
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

;; ## Relaxed Accessors

(defn coefficients
  "Returns a sequence of the coefficients of the polynomial."
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
      [xpt/empty 0]))

(defn leading-coefficient [p]
  (if (polynomial? p)
    (coefficient
     (peek (bare-terms p)))
    p))

(defn leading-base-coefficient [p]
  (if (polynomial? p)
    (recur (leading-coefficient p))
    p))

(defn trailing-coefficient [p]
  (if (polynomial? p)
    (coefficient
     (nth (bare-terms p) 0))
    p))

(defn leading-monomial [p]
  (if (polynomial? p)
    (exponents
     (peek (bare-terms p)))
    xpt/empty))

(defn lowest-order [p]
  (if (polynomial? p)
    (xpt/monomial-degree
     (exponents
      (nth (bare-terms p) 0)))
    coeff-arity))

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

(defn multivariate? [p]
  (and (polynomial? p)
       (> (bare-arity p) 1)))

(defn negative? [p]
  (g/negative?
   (leading-base-coefficient p)))

;; ## Polynomial API

(defn map-coefficients
  "Map the function f over the coefficients of p, returning a new Polynomial.

  TODO this demotes to coefficient when it needs to, and can take a bare coef."
  [f p]
  (if (polynomial? p)
    (terms->polynomial
     (bare-arity p)
     (into empty-terms
           (for [[expts c] (bare-terms p)
                 :let [f-c (f c)]
                 :when (not (v/zero? f-c))]
             (make-term expts f-c))))
    (f p)))

(defn map-exponents
  "Map the function f over the exponents of each monomial in p,
  returning a new Polynomial.

  TODO can handle bare coef now."
  ([f p]
   (map-exponents f p (arity p)))
  ([f p new-arity]
   (if (polynomial? p)
     (make new-arity
           (for [[expts c] (bare-terms p)
                 :let [f-expts (f expts)]]
             (make-term f-expts c)))
     p)))

;; ## Manipulations

(defn univariate->dense [x]
  {:pre [(univariate? x)]}
  (let [d (degree x)]
    (loop [terms (bare-terms x)
           acc (transient [])
           i 0]
      (if (> i d)
        (persistent! acc)
        (let [t  (first terms)
              e  (exponents t)
              md (xpt/monomial-degree e 0)]
          (if (= md i)
            (recur (rest terms)
                   (conj! acc (coefficient t))
                   (inc i))
            (recur terms
                   (conj! acc 0)
                   (inc i))))))))

(defn scale [p c]
  (if (v/zero? c)
    c
    (map-coefficients #(g/* % c) p)))

(defn scale-l [c p]
  (if (v/zero? c)
    c
    (map-coefficients #(g/* c %) p)))

(declare evenly-divide)

(defn normalize
  "Note that we can take coefs on the left too..."
  ([p]
   (normalize p (leading-coefficient p)))
  ([p c]
   (cond (v/one? c) p
         (v/zero? c) (u/arithmetic-ex
                      (str "Divide by zero: " p c))
         (polynomial? c) (evenly-divide p c)
         :else (scale p (g/invert c)))))

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
            (let [v (xpt/monomial-degree m i)
                  v' (- d v)]
              (xpt/assoc m i v')))
          p)))
     p)))

(defn drop-leading-term
  "Returns `p` without its leading term, which MIGHT drop it down to a constant.
  Any non-polynomial drops to 0."
  [p]
  (if (polynomial? p)
    (let [a (bare-arity p)
          terms (pop (bare-terms p))]
      (terms->polynomial a terms))
    0))

;; ## Polynomial Arithmetic

(defn negate [p]
  (map-coefficients g/negate p))

(defn abs [p]
  (if (negative? p)
    (negate p)
    p))

(def terms:+
  (ua/merge-fn #'*monomial-order* g/add v/zero? make-term))

(defn- binary-combine [l r coeff-op terms-op]
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
  (loop [acc (transient [])
         i 0]
    (let [t (nth terms i nil)]
      (if (nil? t)
        (persistent! acc)
	      (let [[tags1 coeff1] t]
	        (recur (conj! acc (make-term
		                         (xpt/+ tags tags1)
		                         (g/mul coeff coeff1)))
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

(defn square [p]
  (poly:* p p))

(defn cube [p]
  (poly:* p (poly:* p p)))

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

(defn- poly:div
  "divide explicit polynomials.

  TODO to be REALLY slick and match plus etc... this would work JUST on terms.
  DO IT! That would let return constants when we need to. And you can see we want it below!"
  [u v]
  {:pre [(polynomial? u)
         (polynomial? v)]}
  (let [arity (check-same-arity u v)
        v-terms             (bare-terms v)
        [vn-expts vn-coeff] (leading-term v)
        good?  #(xpt/every-power? pos? %)
        term*v #(terms->polynomial arity (t*ts % v-terms))]
    (loop [quotient  0
           remainder u]
      ;; find a term in the remainder into which the
      ;; lead term of the divisor can be divided.
      (if (v/zero? remainder)
        [quotient remainder]
        (let [[r-exponents r-coeff] (leading-term remainder)
              residues (xpt/- r-exponents vn-expts)]
          (if (good? residues)
            (let [new-coeff (g/div r-coeff vn-coeff)
                  new-term  (make-term residues new-coeff)]
              (recur (poly:+ quotient (->Polynomial arity [new-term]))
                     (poly:- remainder (term*v new-term))))
            [quotient remainder]))))))

(comment
  ;; TODO test:
  ;;
  ;; TODO make this work JUST on terms and then turn the result into a poly!
  (let [u (make [10])
        v (make [10 20])
        [q r] (divide u v)]
    (g/+ (g/* q v) r)))

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

        ;; TODO change these to work on terms.
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
  (if (v/one? v)
    u
    (let [[q r] (divide u v)]
      (when-not (v/zero? r)
        (u/illegal-state
         (str "expected even division left a remainder! " u " / " v " r " r)))
      q)))

(defn contractible? [n p]
  (zero? (degree p n)))

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
           (group-by #(xpt/monomial-degree (exponents %) 0))
           (map (fn [[x terms]]
                  (let [expts (if (zero? x)
                                xpt/empty
                                (xpt/make 0 x))]
                    (make-term expts (lower-terms terms)))))
           (make 1)))))

(defn raise-arity
  "The opposite of lower-arity. This needs a polynomial with terms that are
  THEMSELVES coefficients."
  [p a]
  (if (polynomial? p)
    (do (assert (= (bare-arity p) 1))
        (let [terms (sparse->terms
                     (for [[x q] (bare-terms p)
                           [ys c] (->terms q)
                           :let [expts (xpt/raise ys 0 (xpt/monomial-degree x 0))]]
                       (make-term expts c)))]
          (->Polynomial a terms)))
    (constant a p)))

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
  (evaluate p (map poly:*
		               factors
		               (new-variables (arity p)))))

(defn arg-shift
  "Given polynomial P(x), substitute x = y+h and compute the resulting polynomial
  Q(y) = P(y+h). When a multivariate polynomial is shifted, each shift must have
  the same arity as the given polynomial... or a base constant."
  [p shifts]
  {:pre (= (arity p) (count shifts))}
  (evaluate p (map poly:+
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

#?(:cljs
   (defn ->big [c]
     (if (v/integral? c)
       (u/bigint c)
       c)))

(defn pseudo-remainder
  "Compute the pseudo-remainder of univariate polynomials u and v.

  Fractions won't appear in the result; instead the divisor is multiplied by the
  leading coefficient of the dividend before quotient terms are generated so
  that division will not result in fractions. Only the remainder is returned,
  together with the integerizing factor needed to make this happen.

  Similar in spirit to Knuth's algorithm 4.6.1R, except we don't multiply the
  remainder through during gaps in the remainder. Since you don't know up front
  how many times the integerizing multiplication will be done, we also return
  the number d for which d * u = q * v + r.

  TODO note that `d` is the integerizing coefficient.

  Notes from GJS that we took on:

  Pseudo division produces only a remainder--no quotient. This can be used to
  generalize Euclid's algorithm for polynomials over a unique factorization
  domain (UFD)."
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
          (recur (poly:- (*vn remainder)
                         (poly:* (c*xn 1 c (- m n))
                                 v))
                 (inc d)))))))

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
              (make-term expts coeff))))))

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

(def ^:private operator-table
  {'+ (ua/monoid poly:+ 0)
   '- (ua/group poly:- poly:+ negate 0)
   '* (ua/monoid poly:* 1 v/zero?)
   'negate negate
   'expt expt
   'square square
   'cube cube
   'gcd (ua/monoid g/gcd 0 v/one?)
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

(defmethod v/= [::polynomial ::coeff] [l r] (eq l r))
(defmethod v/= [::coeff ::polynomial] [l r] (eq r l))

;; TODO put this somewhere where I can depend on the `down` structure!
#_
(defmethod g/partial-derivative [::polynomial v/seqtype] [p selectors]
  (cond (empty? selectors)
        (ss/down* (partial-derivatives p))

        (= 1 (count selectors))
        (partial-derivatives p (first selectors))

        :else
        (u/illegal
         (str "Invalid selector! Only 1 deep supported."))))

(defmethod g/simplify [::polynomial] [p]
  (map-coefficients g/simplify p))

(defmethod g/negate [::polynomial] [a] (negate a))
(defmethod g/negative? [::polynomial] [a] (negative? a))
(defmethod g/abs [::polynomial] [a] (abs a))

(defmethod g/add [::polynomial ::polynomial] [a b]
  (poly:+ a b))

(defmethod g/add [::coeff ::polynomial] [c p]
  (poly:+ (constant (bare-arity p) c) p))

(defmethod g/add [::polynomial ::coeff] [p c]
  (poly:+ p (constant (bare-arity p) c)))

(defmethod g/sub [::polynomial ::polynomial] [a b]
  (poly:- a b))

(defmethod g/sub [::coeff ::polynomial] [c p]
  (poly:- (constant (bare-arity p) c) p))

(defmethod g/sub [::polynomial ::coeff] [p c]
  (poly:- p (constant (bare-arity p) c)))

(defmethod g/mul [::polynomial ::polynomial] [a b] (poly:* a b))
(defmethod g/mul [::coeff ::polynomial] [c p] (scale-l c p))
(defmethod g/mul [::polynomial ::coeff] [p c] (scale p c))

(defmethod g/square [::polynomial] [a] (square a))
(defmethod g/cube [::polynomial] [a] (cube a))
(defmethod g/expt [::polynomial ::v/native-integral] [b x] (expt b x))

;; TODO `modulo`, how to handle?

(defmethod g/quotient [::polynomial ::polynomial] [p q] (nth (divide p q) 0))
(defmethod g/quotient [::polynomial ::coeff] [p q] (nth (divide p q) 0))
(defmethod g/quotient [::coeff ::polynomial] [p q] (nth (divide p q) 0))

(defmethod g/remainder [::polynomial ::polynomial] [p q] (nth (divide p q) 1))
(defmethod g/remainder [::polynomial ::coeff] [p q] (nth (divide p q) 1))
(defmethod g/remainder [::coeff ::polynomial] [p q] (nth (divide p q) 1))

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
