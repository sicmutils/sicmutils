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

(ns sicmutils.polynomial.exponent
  "This namespace provides an implementation of a sparse representation of the
  exponent portion of a term of a polynomial, sometimes called a 'monomial'."
  (:refer-clojure :exclude [empty #?@(:cljs [+ - assoc max])]
                  :rename {+ core+
                           - core-
                           assoc core-assoc
                           max core-max})
  (:require [sicmutils.util :as u]))

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

(def ^{:doc "Accepts alternating pairs of integers representing <indeterminate
 index>, <exponent value> and returns a `sorted-map` representing the exponent
 portion of a polynomial term."
       :arglists '([& i-expt-pairs])}
  make
  #'sorted-map)

(def ^{:doc "Singleton instance of an empty exponent map."}
  empty
  (make))

(defn dense->exponents
  "Accepts a sequence of pairs of indeterminate index => power, and returns a
  sparse representation of the monomial."
  [idx->pow]
  (reduce-kv (fn [acc i x]
               (if (zero? x)
                 acc
                 (core-assoc acc i x)))
             empty
             idx->pow))

(defn + [l r]
  (merge-with core+ l r))

(defn - [l r]
  (dense->exponents
   (merge-with core+ l (u/map-vals core- r))))

(defn gcd
  "TODO: Boom, document. What if we have a multi-arity version that grabs all
  keysets, does a full intersection, THEN does the min? Faster?"
  ([l] l)
  ([l r]
   (let [l' (select-keys l (keys r))
         r' (select-keys r (keys l))]
     (merge-with min l' r'))))

(defn max
  ([l] l)
  ([l r]
   (merge-with core-max l r)))

(defn assoc [m i v]
  (if (zero? v)
    (dissoc m i)
    (core-assoc m i v)))

(defn lower
  "TODO if `i` is the largest key than any key, you COULD go faster by just
  dissocing it... but that won't happen in practice."
  ([expts]
   (lower expts 0))
  ([expts i]
   (reduce-kv (fn [acc k v]
                (if (> k i)
                  (core-assoc acc (dec k) v)
                  (core-assoc acc k v)))
              empty
              (dissoc expts i))))

(defn raise
  ([expts v]
   (raise expts 0 v))
  ([expts i v]
   (let [m (reduce-kv (fn [acc k v]
                        (if (>= k i)
                          (core-assoc acc (inc k) v)
                          (core-assoc acc k v)))
                      empty
                      expts)]
     (if (zero? v)
       m
       (core-assoc m i v)))))

(defn monomial-degree
  "Compute the degree of a monomial. This is just the sum of the exponents. If you
  pass an `i`, you'll get the degree of that term.

  TODO see where we use this... should we pass the full term?"
  ([m]
   (apply core+ (vals m)))
  ([m i]
   (m i 0)))

;; ### Monomial Orderings
;;
;; https://en.wikipedia.org/wiki/Monomial_order
;;
;; These comparators are in the sense of Java: x.compareTo(y), so that this
;; returns 1 if x > y, -1 if x < y, and 0 if x = y.

(defn lex-order
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
                                (core- xv yv)))
                            (neg? bit) 1
                            :else -1)))))))

(defn graded-lex-order [xs ys]
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd)
      (lex-order xs ys)
      (core- xd yd))))

(defn graded-reverse-lex-order [xs ys]
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd)
      (lex-order (rseq ys)
                 (rseq xs))
      (core- xd yd))))
