#_"SPDX-License-Identifier: GPL-3.0"

(ns ^:no-doc emmy.polynomial.impl
  (:require [emmy.generic :as g]
            [emmy.polynomial.exponent :as xpt]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v]))

;; ## Flat Polynomial Form
;;
;; This namespace uses the sparse exponent representation introduced in
;; `emmy.polynomial.exponent` to build up a polynomial arithmetic package
;; using Clojure vectors. `emmy.polynomial` uses this representation to
;; represent polynomial terms in the [[Polynomial]] data structure.
;;
;; Polynomials are sums of monomial terms; a monomial is a pair of some non-zero
;; coefficient and a product of some number of variables, each raised to some
;; power. The latter are represented by the "exponents" data structure defined
;; in `emmy.polynomial.exponent`.
;;
;; More concisely, polynomials are linear combinations of the exponents.
;;
;;
;; ## Polynomial Terms
;;
;; Terms are represented as pairs of [<exponents>, <coef>].

(defn make-term
  "Constructs a polynomial term out of the supplied coefficient `coef` and
  exponents `expts`. Retrieve these with [[coefficient]] and [[exponents]].

  Optionally, passing a single coefficient argument sets the exponents to a
  default value of [[exponent/empty]]."
  ([coef] [xpt/empty coef])
  ([expts coef] [expts coef]))

(defn exponents
  "Returns the exponents portion of the supplied `term`. Defaults to returning
  [[exponent/empty]] if some non-compatible input is supplied."
  [term]
  (nth term 0 xpt/empty))

(defn coefficient
  "Returns the coefficient portion of the supplied `term`. Defaults to returning
  `0` if some non-compatible input is supplied."
  [term]
  (nth term 1 0))

(defn constant-term?
  "Returns true if the term has an empty exponent portion, false otherwise."
  [term]
  (empty?
   (exponents term)))

(defn term->str
  "Returns a string representation of the supplied `term`."
  [term]
  (let [expts (exponents term)
        coef  (coefficient term)]
    (str (pr-str coef) "*" (pr-str expts))))

;; Polynomial Terms
;;
;; Polynomials are represented as ordered vectors of terms. the empty vector
;; needs no ordering:

(def ^:no-doc empty-terms [])

;; For multiple terms, terms are ordered with a [monomial
;; order](https://en.wikipedia.org/wiki/Monomial_order).
;; `emmy.polynomial.exponent` supplies a few of these that you can choose.
;; Set the ordering via the [[*monomial-order*]] dynamic variable.

(def ^{:dynamic true
       :doc "This variable defines monomial order used in the construction and
  arithmetic of polynomials. Bind this variable to a comparator on the exponents
  of each monomial term.

  Defaults to [[exponent/graded-lex-order]]."}
  *monomial-order*
  xpt/graded-lex-order)

;; The next-simplest polynomial is a constant polynomial:

(defn constant->terms
  "Given some constant coefficient `coef`, returns a constant polynomial."
  [coef]
  (if (v/zero? coef)
    empty-terms
    [(make-term xpt/empty coef)]))

;; Univariate polynomials can be specified by a sequence of their coefficients:

(defn dense->terms
  "Accepts a sequence of dense coefficients of a univariate polynomial (in
  ascending order), and returns a polynomial in flat polynomial form that
  matches the supplied coefficient sequence.

  For example:

  ```clojure
  (dense->terms [1 0 0 4 5])
  ;;=> [[{} 1] [{0 3} 4] [{0 4} 5]]
  ```"
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

;; When multivariate polynomials have high arity, it can be quite a task to
;; supply every possible term. [[sparse->terms]] allows you to specify a mapping
;; of exponents => coefficient, where "exponents" can be:
;;
;; - a proper exponent entry created by `emmy.polynomial.exponent`
;; - a map of the form `{variable-index, power}`
;; - a dense vector of variable powers, like `[3 0 1]` for $x^3z$.

(defn sparse->terms
  "Accepts a sparse mapping (or sequence of pairs) of exponent => coefficient, and
  returns a proper polynomial. Optionally takes a `comparator` on exponent
  entries; the returned polynomial will be sorted using that comparator.

  `comparator` defaults to [[*monomial-order*]].

  The `exponent` portion of the mapping can be any of:

  - a proper exponent entry created by `emmy.polynomial.exponent`
  - a map of the form `{variable-index, power}`
  - a dense vector of variable powers, like `[3 0 1]` for $x^3z$.

  For example:

  ```clojure
  (sparse->terms {{1 2 3 1} 4 [0 2 0 0] 2})
  ;;=> [[{1 2} 2] [{1 2, 3 1} 4]]
  ```"
  ([expts->coef]
   (sparse->terms expts->coef *monomial-order*))
  ([expts->coef comparator]
   (if (empty? expts->coef)
     empty-terms
     (->> (for [[expts terms] (group-by exponents expts->coef)
                :let [coef-sum (transduce
                                (map coefficient) g/+ terms)]
                :when (not (v/zero? coef-sum))
                :let [expts (cond (vector? expts) (xpt/dense->exponents expts)
                                  (sorted? expts) expts
                                  (map? expts) (into xpt/empty expts)
                                  :else
                                  (u/illegal "Invalid inputs to sparse->terms TODO"))]]
            (make-term expts coef-sum))
          (sort-by exponents comparator)
          (into empty-terms)))))

;; ## API
;;
;; We can make polynomials. What can we do with them?
;;
;; The main operations we want are `+`, `*` and `-`. We can _also_ divide
;; polynomials; the catch is that we always have to return an explicit remainder
;; as well.

(defn map-coefficients
  "Returns a new polynomial generated by applying `f` to the coefficient portion
  of each term in `terms`."
  [f terms]
  (into empty-terms
        (for [[expts c] terms
              :let [f-c (f c)]
              :when (not (v/zero? f-c))]
          (make-term expts f-c))))

(def ^{:doc "Returns the sum of polynomials `u` and `v`. Coefficients paired
  with matching exponents are combined with [[emmy.generic/add]]."
       :arglists '([u v])}
  add
  (ua/merge-fn #'*monomial-order* g/add v/zero? make-term))

(defn sub
  "Returns the difference of polynomials `u` and `v`.

  NOTE that coefficients paired with matching exponents are combined by `(g/add
  u (g/negate v))`, rather than an explicit call to [[emmy.generic/sub]]."
  [u v]
  (add u (map-coefficients g/negate v)))

;; Multiplication works by multiplying the polynomial on the right by each term
;; on the left and summing up all results. These operations are split into two
;; functions:

(defn t*ts
  "Multiplies a single term on the left by a vector `v` of terms on the right.
  Returns a new polynomial (ie, vector of terms)."
  [[tags coeff] v]
  (loop [acc (transient [])
         i 0]
    (let [t (nth v i nil)]
      (if (nil? t)
        (persistent! acc)
	      (let [[tags1 coeff1] t]
	        (recur (conj! acc (make-term
		                         (xpt/mul tags tags1)
		                         (g/mul coeff coeff1)))
		             (inc i)))))))

(defn mul
  "Returns the product of the two polynomial term vectors `u` and `v`."
  [u v]
  (letfn [(call [i]
            (let [x (nth u i nil)]
              (if (nil? x)
                []
                (add (t*ts x v)
	                   (call (inc i))))))]
    (call 0)))

;; Division works by examining each term of `u` (in descending order) and
;; checking whether or not the lead term of `v` can divide into it. If it can,
;; the algorithm performs the division and tries again with `u-(new-term*v)`, on down until no terms remain.
;;
;; See the Wikipedia article on [Polynomial long
;; division](https://en.wikipedia.org/wiki/Polynomial_long_division) for more
;; details.

(defn div
  "Given two polynomials `u` and `v`, returns a pair of the form `[quotient,
  remainder]` using [polynomial long
  division](https://en.wikipedia.org/wiki/Polynomial_long_division).

  The contract satisfied is that

  ```
  u == (add (mul quotient v) remainder)
  ```"
  [u v]
  (let [[vn-expts vn-coeff] (peek v)
        good? #(xpt/every-power? pos? %)]
    (loop [quotient []
           remainder u]
      (if (empty? remainder)
        [quotient remainder]
        ;; find a term in the remainder into which the lead term of `v` can be
        ;; divided.
        (let [[r-exponents r-coeff] (peek remainder)
              residues (xpt/div r-exponents vn-expts)]
          (if (good? residues)
            (let [new-coeff (g/div r-coeff vn-coeff)
                  new-term  (make-term residues new-coeff)]
              (recur (add quotient [new-term])
                     (sub remainder (t*ts new-term v))))
            [quotient remainder]))))))
