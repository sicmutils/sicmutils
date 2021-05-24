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

(ns ^:no-doc sicmutils.polynomial.impl
  (:require [sicmutils.generic :as g]
            [sicmutils.polynomial.exponent :as xpt]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v]))

;; # Flat Polynomial Form, for Commutative Rings
;;
;; The namespace starts by defining exponents (also called monomials), then
;; builds these into terms, then polynomials with a proper type definition.

(def ^{:dynamic true
       :doc "The order. NOTE that this currently breaks if we customize it."}
  *monomial-order*
  xpt/graded-reverse-lex-order)

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
  "Returns the exponent vector of the term."
  [term]
  (nth term 0 xpt/empty))

(defn coefficient
  "Returns the coefficient entry of the term."
  [term]
  (nth term 1 0))

(defn term->str [term]
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

;; ## Constructors

(defn sparse->terms
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
                :let [expts (cond (vector? expts) (xpt/dense->exponents expts)
                                  (sorted? expts) expts
                                  (map? expts) (into xpt/empty expts)
                                  :else (u/illegal "Invalid inputs to sparse->terms TODO"))]]
            (make-term expts coef-sum))
          (sort-by exponents comparator)
          (into empty-terms)))))

(defn dense->terms
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

;; ## API

(defn map-coefficients [f terms]
  (into empty-terms
        (for [[expts c] terms
              :let [f-c (f c)]
              :when (not (v/zero? f-c))]
          (make-term expts f-c))))

(def add
  (ua/merge-fn #'*monomial-order* g/add v/zero? make-term))

(defn sub [l r]
  (add l (map-coefficients g/negate r)))

;; ## Polynomial Arithmetic

(defn t*ts
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
		                         (xpt/mul tags tags1)
		                         (g/mul coeff coeff1)))
		             (inc i)))))))

(defn mul [xlist ylist]
  (letfn [(call [i]
            (let [x (nth xlist i nil)]
              (if (nil? x)
                []
                (add (t*ts x ylist)
	                   (call (inc i))))))]
    (call 0)))

(defn div
  "divide explicit polynomials."
  [u v]
  (let [[vn-expts vn-coeff] (peek v)
        good?  #(xpt/every-power? pos? %)]
    (loop [quotient []
           remainder u]
      ;; find a term in the remainder into which the
      ;; lead term of the divisor can be divided.
      (if (empty? remainder)
        [quotient remainder]
        (let [[r-exponents r-coeff] (peek remainder)
              residues (xpt/div r-exponents vn-expts)]
          (if (good? residues)
            (let [new-coeff (g/div r-coeff vn-coeff)
                  new-term  (make-term residues new-coeff)]
              (recur (add quotient [new-term])
                     (sub remainder (t*ts new-term v))))
            [quotient remainder]))))))
