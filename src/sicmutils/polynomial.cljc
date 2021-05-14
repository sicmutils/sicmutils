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
  (:refer-clojure :exclude [extend divide])
  (:require [clojure.set :as set]
            [clojure.string :as cs]
            [sicmutils.collection]
            [sicmutils.expression.analyze :as a]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.modint :as mi]
            [sicmutils.numsymb :as sym]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj))))

;; TODO note that this differs from scmutils because we can extend the
;; coefficient space.
;;
;; TODO note that we should be treating ANYTHING as a coefficient, and just
;; special-casing the polynoimal type.

;;
;; # Flat Polynomial Form, for Commutative Rings
;;
;; The namespace starts by defining monomials, then builds these into
;; polynomials with a proper type definition.
;;
;; ## Monomials
;;
;; A monomial is a single term of a polynomial. NOTE we need to clarify this vs
;; the polynomial single term with coef.
;;
;; We represent a monomial as a vector of integers representing the exponents of
;; the indeterminates over some ring. For example; we would represent x^2
;; as [2], and xy^2 as [1 2], though the indeterminates have no name.
;; Polynomials are linear combinations of the monomials. When these are formed,
;; it is important that the monomial vectors all contain the same number of
;; slots, so that 3x + 2y^2 would be represented as: 3*[1 0] + 2*[0 2].

(defn- monomial-degree
  "Compute the degree of a monomial. This is just the sum of the exponents."
  [m]
  (apply + m))

;; ### Monomial Comparators
;;
;; These comparators are in the sense of Java: x.compareTo(y), so that this
;; returns 1 if x > y, -1 if x < y, and 0 if x = y.

(defn ^:no-doc lex-order
  "Lex order for monomials considers the power of x, then the power of y, etc."
  [xs ys]
  {:pre (= (count xs) (count ys))}
  (compare xs ys))

(defn ^:no-doc graded-lex-order [xs ys]
  {:pre (= (count xs) (count ys))}
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd)
      (lex-order xs ys)
      (- xd yd))))

(defn ^:no-doc graded-reverse-lex-order [xs ys]
  {:pre (= (count xs) (count ys))}
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd)
      (compare (vec (rseq ys))
               (vec (rseq xs)))
      (- xd yd))))

;; the default.

(def ^:no-doc monomial-order
  graded-lex-order)

;; ## Polynomial Terms

(derive ::v/scalar ::coeff)
(derive ::mi/modint ::coeff)

;; TODO remove this... just do it via the generic installation? no internal coeff? checks.

(defn coeff? [x]
  (isa? (v/kind x) ::coeff))

;; ## Term List
;;
;; Terms are represented as pairs of [<monomial>, <coef>]. A polynomial (called
;; an `fpf` in scmutils, for Flat Polynomial Form), is a sorted list of terms.

(defn make-term
  "Takes a monomial and a coefficient and returns a polynomial term."
  ;; TODO this first arity us busted!!
  ([coef] [[] coef])
  ([expts coef] [expts coef]))

(defn constant->terms [arity coef]
  (if (v/zero? coef)
    []
    (let [term [(into [] (repeat arity 0)) coef]]
      [term])))

(defn exponents
  "Returns the exponent vector of the term."
  [term]
  (nth term 0 []))

(defn coefficient
  "Returns the coefficient entry of the term."
  [term]
  (nth term 1 0))

(def ^:no-doc empty-terms [])

(defn constant-term?
  "Returns true if the term has an exponents vector that is all zeros, false
  otherwise."
  [term]
  (v/zero?
   (exponents term)))

;; ## Polynomial Type Definition
;;
;; implement:
;;
;; IPerturbed, IComparable?, same with RF ;; TODO look at PowerSeries, see what
;; we're missing.

(declare evaluate make-constant poly->str poly:=)

(deftype Polynomial [arity terms m]
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
         (let [[[[e] c]] terms]
           (and (v/one? e)
                (v/one? c)))))

  (zero-like [_]
    (Polynomial. arity empty-terms m))

  (one-like [_]
    (let [one (if-let [term (nth terms 0)]
                (v/one-like (coefficient term))
                1)]
      (make-constant arity one)))

  (identity-like [_]
    (assert (v/one? arity)
            "identity-like unsupported on multivariate monomials!")
    (let [one (if-let [term (nth terms 0)]
                (v/one-like (coefficient term))
                1)
          term (make-term [1] one)]
      (Polynomial. arity [term] m)))

  (exact? [_] false)
  (freeze [_] `(~'polynomial ~arity ~terms))
  (kind [_] ::polynomial)

  f/IArity
  ;; TODO we CAN actually evaluate this thing with less... so it's always really
  ;; between 0 and arity, right??
  (arity [_] [:exactly arity])

  #?@(:clj
      [Object
       (equals [this that] (poly:= this that))
       (toString [p] (poly->str p))

       IObj
       (meta [_] m)
       (withMeta [_ meta] (Polynomial. arity terms m))

       IFn
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
       (toString [p] (poly->str p))

       IEquiv
       (-equiv [this that] (poly:= this that))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ m] (Polynomial. arity terms m))

       IFn
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

(defn explicit-polynomial?
  "Returns true if the supplied argument is an instance of [[Polynomial]], false
  otherwise."
  [x]
  (instance? Polynomial x))

(defn polynomial?
  "TODO is this helpful? I think maybe we should just... assume that this is more
  open. Delete all uses of this and change `explicit-` to `polynomial?`."
  [x]
  (or (coeff? x)
      (explicit-polynomial? x)))

(defn ^:no-doc bare-arity [p]
  {:pre [(explicit-polynomial? p)]}
  (.-arity ^Polynomial p))

(defn ^:no-doc bare-terms [p]
  {:pre [(explicit-polynomial? p)]}
  (.-terms ^Polynomial p))

(defn- poly:=
  "Polynomials are equal to a number if the polynomial is constant; otherwise
  it's only equal to other polynomials."
  [^Polynomial this that]
  (if (instance? Polynomial that)
    (let [p ^Polynomial that]
      (and (= (.-arity this) (.-arity p))
           (v/= (.-terms this) (.-terms p))))

    (let [terms (.-terms this)]
      (and (<= (count terms) 1)
           (let [term (peek terms)]
             (and (constant-term? term)
                  (v/= that (coefficient term))))))))

(defn ->terms
  "TODO this is JUST like `->terms` in `differential`."
  [p]
  (cond (explicit-polynomial? p) (bare-terms p)
        (vector? p) p
        (v/zero? p) []
        ;; TODO THIS is troubling.
        :else [(make-term p)]))

(defn ^:no-doc lead-term
  "Return the leading (i.e., highest degree) term of the polynomial p. The return
  value is a pair of [exponents coefficient].

  TODO this is a change, returning an explicit term always. NOTE, test."
  [p]
  (or (peek (->terms p))
      [[] 0]))

(defn ^:no-doc lead-coefficient [p]
  (if (explicit-polynomial? p)
    (coefficient (lead-term p))
    p))

(defn arity
  "TODO what's the difference between arity and degree?"
  [p]
  (if (explicit-polynomial? p)
    (bare-arity p)
    0))

(defn degree
  "TODO what's the difference between arity and degree?"
  [p]
  (cond (v/zero? p) -1

        (explicit-polynomial? p)
        (monomial-degree
         (exponents
          (lead-term p)))

        :else 0))

(defn principal-reverse [p]
  ;; TODO figure out what the heck this is trying to do... get some more stuff
  ;; from pcf around line 1243.
  #_(let [d (degree p)]
	    (loop [p      (bare-terms p)
             result []]
	      (if (empty? p)
	        result
	        (recur (rest p)
		             (cons (cons (- d (caar p)) (cdar p))
			                 result))))))

(defn monomial? [p]
  (or (not (explicit-polynomial? p))
      (= 1 (count (bare-terms p)))))

(defn monic?
  "Returns true if `p` is a [monic
  polynomial](https://en.wikipedia.org/wiki/Monic_polynomial), false otherwise.

  TODO test that you can normalize by the lead coefficient to get a monic.
  Generate a dense then do that."
  [p]
  (if (explicit-polynomial? p)
    (and (= 1 (arity p))
         (v/one?
          (coefficient
           (lead-term p))))
    (v/one? p)))

(defn coefficients
  "TODO see where this is used. Return a vector?"
  [p]
  (if (explicit-polynomial? p)
    (map coefficient (->terms p))
    [p]))

;; String methods...

(defn- term->str [term]
  (let [expts (exponents term)
        coef  (coefficient term)]
    (str (pr-str coef) "*[" (cs/join "," expts) "]")))

(defn- poly->str
  ([p] (poly->str p 10))
  ([p n]
   {:pre [explicit-polynomial? p]}
   (let [terms     (bare-terms p)
         arity     (bare-arity p)
         n-terms   (count terms)
         term-strs (take n (map term->str terms))
         suffix    (when (> n-terms n)
                     (str "... and " (g/- n-terms n) " more terms"))]
     (str arity  ": (" (cs/join " + " term-strs) suffix ")"))))

;; ## Constructors

(defn- sparse->terms
  "NOTE: Optionally takes a comparator."
  ([expts->coef]
   (sparse->terms expts->coef monomial-order))
  ([expts->coef comparator]
   (if (empty? expts->coef)
     []
     (->> (for [[expts terms] (group-by exponents expts->coef)
                :let [coef-sum (transduce
                                (map coefficient) g/+ terms)]
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
                   [(make-term [i] coef)]))
        xform  (comp (map-indexed ->term)
                     cat)]
    (into empty-terms xform coefs)))

;; NOTE: Analogous to to `terms->differential`.

(defn ^:no-doc terms->polynomial
  "Returns a [[Polynomial]] instance generated from a vector of terms. This method
  will do some mild cleanup:

  - any empty term list will return 0
  - a singleton term list with all zero exponents will return its coefficient

  NOTE this method assumes that the terms are properly sorted, and contain no
  zero coefficients."
  [arity terms]
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (constant-term? (nth terms 0)))
        (let [c (coefficient (nth terms 0))]
          (if (coeff? c)
            c
            (->Polynomial arity terms)))

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

;; TODO keep reading here!

(defn poly:identity
  "TODO modeled on sparse-identity-term, check interface."
  ([]
   (poly:identity 1 1))
  ([arity]
   (poly:identity arity 1))
  ([arity varnum]
   {:pre [(<= varnum arity)]}
   (let [expts (-> (into [] (repeat arity 0))
                   (assoc (dec varnum) 1))]
     (make arity [(make-term expts 1)]))))

(defn make-constant
  "Return a constant polynomial of the given arity.

  NOTE that zero coefficients always get filtered out."
  ([c] (make-constant 1 c))
  ([arity c]
   (let [terms (if (v/zero? c)
                 empty-terms
                 [(make-term (into [] (repeat arity 0)) c)])]
     (->Polynomial arity terms))))

;; TODO check what this actually generates... I bet we do NOT want to generate
;; NON polynomials here, if the original does not!!
;;
;; TODO do we take an optional thing that flags the right `x` index?

(defn make-c*xn
  "Polynomial representing c*x^n, where x is the first indeterminate."
  [arity c n]
  (cond (<= n -1)   0
        (v/zero? c) c
        (v/zero? n) (make-constant arity c)
        :else
        (let [term (make-term (into [n] (repeat (dec arity) 0))
                              c)]
          (->Polynomial arity [term]))))

(declare poly:+)

(defn make-linear
  "Makes a polynomial representing a linear equation."
  [arity varnum root]
  (if (v/zero? root)
    (poly:identity arity varnum)
    (poly:+ (make-constant arity (g/negate root))
            (poly:identity arity varnum))))

;; ## Polynomial API

(defn check-same-arity
  "TODO works now for constants, check!"
  [p q]
  (cond (coeff? p) (arity q)
        (coeff? q) (arity p)
        :else (let [ap (arity p)
                    aq (arity q)]
                (if (= ap aq)
                  ap
                  (u/arithmetic-ex
                   (str "mismatched polynomial arity: " ap ", " aq))))))

(defn new-variables
  "TODO NOTE: returns a sequence of `n` new polynomials of arity `n`, with the
  coefficient 1 and new indeterminates for each."
  [n]
  (map #(poly:identity n %)
       (range 1 (inc n))))

(defn map-coefficients
  "Map the function f over the coefficients of p, returning a new Polynomial.

  TODO this demotes to coefficient when it needs to, and can take a bare coef."
  [f p]
  (if (explicit-polynomial? p)
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
  (if (explicit-polynomial? p)
    (make (bare-arity p)
          (for [term (bare-terms p)]
            (make-term
             (f (exponents term))
             (coefficient term))))
    p))

;; ## Manipulations

(defn extend
  "TODO interpolate a new variable in the `n` spot by expanding all vectors."
  [p n]
  )

(declare except-leading-term)

(defn contractible? [n p]
  (or (not (explicit-polynomial? p))
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
					          (coefficient (lead-term p)))

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
				                                   (coefficient (lead-term p)))
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

;; TODO I KNOW we can do a better job here, rather than calling `make` each
;; time.

(defn merge-fn [compare add zero? make]
  (fn
    ([] [])
    ([xs] xs)
    ([xs ys]
     (loop [i (long 0)
            j (long 0)
            result (transient [])]
       (let [x (nth xs i nil)
             y (nth ys j nil)]
         (cond (not x) (into (persistent! result) (subvec ys j))
               (not y) (into (persistent! result) (subvec xs i))
               :else (let [[x-tags x-coef] x
                           [y-tags y-coef] y
                           compare-flag (compare x-tags y-tags)]
                       (cond
                         ;; If the terms have the same tag set, add the coefficients
                         ;; together. Include the term in the result only if the new
                         ;; coefficient is non-zero.
                         (zero? compare-flag)
                         (let [sum (add x-coef y-coef)]
                           (recur (inc i)
                                  (inc j)
                                  (if (zero? sum)
                                    result
                                    (conj! result (make x-tags sum)))))

                         ;; Else, pass the smaller term on unchanged and proceed.
                         (neg? compare-flag)
                         (recur (inc i) j (conj! result x))

                         :else
                         (recur i (inc j) (conj! result y))))))))))

(def terms:+
  (merge-fn monomial-order g/add v/zero? make-term))

(defn- binary-combine
  "TODO this is inefficient as hell for the TWO places we use it."
  [l r coeff-op terms-op]
  (let [l-poly? (explicit-polynomial? l)
        r-poly? (explicit-polynomial? r)]
    (cond (and l-poly? r-poly?)
          (terms->polynomial
           (check-same-arity l r)
           (terms-op (bare-terms l)
                     (bare-terms r)))

          l-poly?
          (terms->polynomial
           (bare-arity l)
	         (terms-op (bare-terms l)
			               (constant->terms (bare-arity l) r)))

          r-poly?
          (terms->polynomial
           (bare-arity r)
           (terms-op (constant->terms (bare-arity r) l)
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
		                        (mapv + tags tags1)
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
    (cond (coeff? p)  (g/expt p n)

          (not (explicit-polynomial? p))
          (u/illegal (str "Wrong type :" p))

          (not (v/native-integral? n))
          (u/illegal (str "Can only raise an FPF to an exact integer power: " p n))

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
        [vn-exponents vn-coefficient] (lead-term v)
        good? (fn [residues]
                (and (seq residues)
                     (every? (complement neg?) residues)))]
    (if (zero? arity)
      [(g/div (lead-coefficient u) vn-coefficient) 0]
      (loop [quotient  0
             remainder u]
        ;; find a term in the remainder into which the
        ;; lead term of the divisor can be divided.
        (if (v/zero? remainder)
          [quotient remainder]
          (let [[r-exponents r-coefficient] (if (coeff? remainder)
                                              [(into [] (repeat arity 0)) remainder]
                                              (lead-term remainder))
                residues (mapv g/- r-exponents vn-exponents)]
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
  {:pre [(polynomial? u)
         (polynomial? v)]}
  (cond (v/zero? v)
        (u/illegal "internal polynomial division by zero")

        (or (v/zero? u) (v/one? v))
        [u 0]

        (and (coeff? u) (coeff? v))
        [(g/quotient u v) (g/remainder u v)]

        (and (coeff? u) (explicit-polynomial? v))
        (poly:div (make-constant (bare-arity v) u) v)

        (and (explicit-polynomial? u) (coeff? v))
        (poly:div u (make-constant (bare-arity u) v))

        (and (explicit-polynomial? u)
             (explicit-polynomial? v))
        (poly:div u v)

        :else (u/illegal (str "Bad arguments to divide: " u v))))

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
       (lead-coefficient p))
    (negate p)
    p))

(defn lower-arity
  "Given a nonzero polynomial of arity A > 1, return an equivalent polynomial
  of arity 1 whose coefficients are polynomials of arity A-1."
  [p]
  {:pre [(explicit-polynomial? p)
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
                                  [(subvec xs 1) c]))]
                (->Polynomial (dec A) coef-terms)))]
      (->> (bare-terms p)
           (group-by #(nth (exponents %) 0))
           (map (fn [[x terms]]
                  (make-term [x] (lower-terms terms))))
           (sparse->terms)
           (->Polynomial 1)))))

(comment
  ;; TODO this works... BUT I think we want to drop those constants down to
  ;; actual constants. But then what would raise do??
  (is (= (make 1 {[1] (make-constant 2 2)
                  [2] (make-constant 2 2)})
         (lower-arity
          (make 3 {[1 0 0] 2 [2 0 0] 2})))))

(defn raise-arity
  "The opposite of lower-arity. This needs a polynomial with terms that are
  THEMSELVES coefficients."
  [p a]
  {:pre [(polynomial? p)
         (<= (arity p) 1)]}
  (if (coeff? p)
    (make-constant a p)
    (let [terms (doall
                 (sparse->terms
                  (for [[x q]  (bare-terms p)
                        [ys c] (->terms q)
                        :let [ys (if (empty? ys)
                                   (repeat (dec a) 0)
                                   ys)]]
                    [(into x ys) c])))]
      (->Polynomial a terms))))

(comment
  ;; TODO what about horner-with-error?
  (defn- horner-eval-general [terms args]
    (loop [[term & terms] terms
           expts (exponents term)
		       sum   (coefficient term)]
	    (if (empty? terms)
        (apply g/* (cons sum (map g/expt args expts)))
	      (let [new-expts (exponents (first terms))
              diff (map g/- expts new-expts)]
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
    (if-let [[[e'] c] (first terms)]
      (let [x**e' (g/* x**e (g/expt x (g/- e' e)))]
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
  {:pre [(polynomial? p)]}
  (let [a (arity p)]
    (cond (coeff? p)  p
          (empty? xs) p
          (v/zero? p) 0
          (= a 1)     (evaluate-1 p (first xs))
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
                    [1] (make-constant 2 1)})
        a2 (make 1 {[0] (make 2 {[0 1] 1})
                    [1] (make-constant 2 1)})]
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
  {:pre [(explicit-polynomial? u)
         (= (bare-arity u) 1)
         (explicit-polynomial? v)
         (= (bare-arity v) 1)
         (not (v/zero? v))]}
  (let [[vn-exponents vn-coefficient] (lead-term v)
        *vn (fn [p] (poly*coeff p vn-coefficient))
        n (monomial-degree vn-exponents)]
    (loop [remainder u
           d         0]
      (let [m (degree remainder)
            c (lead-coefficient remainder)]
        (if (< m n)
          [remainder d]
          (recur (poly:- (*vn remainder)
                         (poly:* (make-c*xn 1 c (g/- m n))
                                 v))
                 (inc d)))))))

;; ## Derivatives

(defn partial-derivative
  "The partial derivative of the polynomial with respect to the
  i-th indeterminate.

  TODO this is confused, with its special case for numbers."
  [p i]
  (if (explicit-polynomial? p)
    (make (bare-arity p)
          (for [[xs c] (bare-terms p)
                :let [xi (xs i)]
                :when (not= 0 xi)]
            [(update xs i dec) (g/* xi c)]))
    0))

(defn partial-derivatives
  "The sequence of partial derivatives of p with respect to each
  indeterminate. Return value has length `(arity p)`."
  [p]
  (if (explicit-polynomial? p)
    (for [i (range (bare-arity p))]
      (partial-derivative p i))
    []))

;; ## Canonicalizer

;; The operator-table represents the operations that can be understood
;; from the point of view of a polynomial over a commutative ring. The
;; functions take polynomial inputs and return polynomials.

(def ^:private operator-table
  {'+ (fn
        ([] 0)
        ([x] x)
        ([x y] (poly:+ x y))
        ([x y & more]
         (reduce poly:+ (poly:+ x y) more)))
   '- (fn
        ([x] (negate x))
        ([x & more]
         (poly:- x (apply (operator-table '+) more))))
   '* (fn
        ([] 1)
        ([x] x)
        ([x y] (poly:* x y))
        ([x y & more]
         (reduce poly:* (poly:* x y) more)))
   'negate negate
   'expt expt
   'square square
   'cube cube})

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
    (if (explicit-polynomial? p)
      (let [xform (map (fn [[xs c]]
                         (->> (map expt vars xs)
                              (reduce *)
                              (* c))))]
        (->> (bare-terms p)
             (sort-by exponents #(monomial-order %2 %1))
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

;; TODO: add `partial-derivative`., `simplify`, `solve-linear-right`,
;; `solve-linear`,

(defmethod v/= [::polynomial ::coeff] [l r] (poly:= l r))
(defmethod v/= [::coeff ::polynomial] [l r] (poly:= r l))

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
  (poly:+ (make-constant (bare-arity p) c) p))

(defmethod g/add [::polynomial ::coeff] [p c]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:+ p (make-constant (bare-arity p) c)))

(defmethod g/sub [::coeff ::polynomial] [c p]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:- (make-constant (bare-arity p) c) p))

(defmethod g/sub [::polynomial ::coeff] [p c]
  ;; TODO make THIS more efficient, check scmutils!
  (poly:- p (make-constant (bare-arity p) c)))

(defmethod g/div [::polynomial ::coeff] [p c]
  ;; TODO pull this out into its own thing.
  (map-coefficients #(g/divide % c) p))

(defmethod g/expt [::polynomial ::v/native-integral] [b x] (expt b x))
