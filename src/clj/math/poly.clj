;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.poly
  (:import (clojure.lang PersistentTreeMap IFn))
  (:refer-clojure :rename {zero? core-zero?})
  (:require [clojure.set :as set]
            [math.value :as v]
            [math.generic :as g]
            [math.numsymb :as sym]
            [math.expression :as x]))

(declare operator-table operators-known)

(defrecord Poly [^long arity ^PersistentTreeMap xs->c]
  v/Value
  (nullity? [p] (empty? (:xs->c p)))
  (numerical? [_] false)
  (unity? [p] (and (= (count (:xs->c p)) 1)
                 (let [[exponents coef] (first (:xs->c p))]
                   (and (every? core-zero? exponents)
                        (g/one? coef)))))
  IFn
  (applyTo [_ _] (throw (IllegalArgumentException. "how did we get here?")))
  )

(def ^:private base? number?)

(defn- make-with-arity [a xc-pairs]
  (let [xs->c (into (sorted-map) (filter (fn [[_ c]] (not (g/zero? c))) xc-pairs))]
    (cond (empty? xs->c) 0
          (and (= (count xs->c) 1) (every? core-zero? (first (first xs->c)))) (second (first xs->c))
          :else (Poly. a xs->c))))

(defn make
  "Create a polynomial specifying the terms in dense form, supplying
  the coefficients of the terms starting with the constant term and
  proceeding as far as needed. For example, x^2 - 1 can be constructed
  by (make -1 0 1). The order of the coefficients corresponds to the
  order of the terms, and zeros must be filled in to get to higher
  powers."
  [& coefficients]
  (make-with-arity 1 (zipmap (map vector (iterate inc 0)) coefficients)))

;; should we rely on the constructors and manipulators never to allow
;; a zero coefficient into the list, or should we change degree to
;; scan for nonzero coefficients? In the normal case, there would be
;; none, but in corner cases it would still be robust.

(defn degree
  [p]
  (cond (g/zero? p) -1
        (base? p) 0
        :else (reduce max (map #(reduce + 0 %) (keys (:xs->c p))))))

;; ARITY

(defn- arity
  [p]
  (if (base? p)
    0
    (:arity p)))

(defn- check-same-arity [p q]
  (let [ap (arity p)
        aq (arity q)]
    (cond (base? p) aq
          (base? q) ap
          (= ap aq) ap
          :else (throw (ArithmeticException. "mismatched polynomial arity")))))

(defn- normalize-with-arity
  [p a]
  (if (base? p) p
      (let [fp (->> p (filter #(not (g/zero? (second %)))) (into (sorted-map)))]
        (if-let [[xs coef] (first fp)]
         (if (and (= (count p) 1) (every? core-zero? xs)) coef
             (Poly. a fp))
         0))))

(defn- poly-map
  "Map the function f over the coefficients of p, returning a new Poly."
  [f p]
  (normalize-with-arity (for [[xs c] (:xs->c p)] [xs (f c)]) (:arity p)))

(defn- poly-merge
  "Merge the polynomials together, combining corresponding coefficients with f.
  The result is not a polynomial object, but rather a sequence of [exponent, coefficient] pairs,
  suitable for further processing or canonicalization. Merged monomials with zero coefficient
  are dropped."
  [f p q]
  (loop [P (:xs->c p)
         Q (:xs->c q)
         R (sorted-map)]
    (cond
      (empty? P) (into R (for [[xs c] Q :let [c1 (f 0 c)] :when (not (core-zero? c1))] [xs c1]))
      (empty? Q) (into R (for [[xs c] P :let [c1 (f c 0)] :when (not (core-zero? c1))] [xs c1]))
      :else (let [[xp cp] (first P)
                  [xq cq] (first Q)
                  order (compare xp xq)]
              (cond
                (core-zero? order) (let [v (f cp cq)]
                                     (recur (rest P) (rest Q)
                                            (if (not (g/zero? v))
                                              (assoc R xp v)
                                              R)))
                (< order 0) (recur (rest P) Q (assoc R xp (f cp 0)))
                :else (recur P (rest Q) (assoc R xq (f 0 cq))))))))

(defn new-variables
  "Creates a sequence of identity (i.e., x) polynomials, one for each of arity indeterminates."
  [arity]
  (for [a (range arity)]
    (make-with-arity arity [[(vec (map #(if (= % a) 1 0) (range arity))) 1]])))

(def negate (partial poly-map g/negate))

(defn- zero-term
  "Creates the key corresponding to the constant term of a polynomial with the given arity."
  [arity]
  (vec (repeat arity 0)))

(defn- add-constant
  "Adds the constant c to poly. "
  [poly c]
  (if (base? poly) (g/+ poly c)
                   (let [{:keys [arity xs->c]} poly]
                     (-> xs->c
                         (update-in [(zero-term arity)] #(g/+ (or % 0) c))
                         (normalize-with-arity arity)))))

(defn add
  [p q]
  (cond (and (base? p) (base? q)) (g/+ p q)
        (g/zero? p) q
        (g/zero? q) p
        (base? p) (add-constant q p)
        (base? q) (add-constant p q)
        :else (let [a (check-same-arity p q)
                    sum (poly-merge g/+ p q)]
                (normalize-with-arity sum a))))

(defn- add-denormal
  "Add-denormal adds the (order, coefficient) pair to the polynomial p,
  expecting that p is currently in sparse form (i.e., not a primitive number)
  and without normalizing the result (e.g., to see if the polynomial has
  become constant or a term has dropped out). Useful in intermediate steps
  of polynomial computations."
  [ocs [exponents coefficient]]
  (assoc ocs exponents (g/+ (get ocs exponents 0) coefficient)))

(defn sub
  "Subtract the polynomial q from the polynomial p."
  [p q]
  (cond (and (base? p) (base? q)) (g/- p q)
        (g/zero? p) (g/negate q)
        (g/zero? q) p
        (base? p) (add-constant (negate q) p)
        (base? q) (add-constant p (- q))
        :else (let [a (check-same-arity p q)
                    diff (poly-merge g/- p q)]
                (normalize-with-arity diff a))))

(defn mul
  "Multiply polynomials p and q, and return the product."
  [p q]
  (cond (and (base? p) (base? q)) (g/* p q)
        (g/zero? p) 0
        (g/zero? q) 0
        (g/one? p) q
        (g/one? q) p
        (base? p) (poly-map #(g/* p %) q)
        (base? q) (poly-map #(g/* % q) p)
        :else (let [a (check-same-arity p q)]
                (normalize-with-arity (reduce add-denormal (sorted-map)
                                                (for [[xp cp] (:xs->c p)
                                                      [xq cq] (:xs->c q)]
                                                  [(vec (map + xp xq)) (g/* cp cq)]))
                                      a))))

(defn expt
  "Raise the polynomial p to the (integer) power n."
  [p n]
  (cond (base? p) (g/expt p n)
        (or
         (not (integer? n))
         (< n 0)) (throw (ArithmeticException. (str "can't raise poly to " n)))
        (g/one? p) p
        (g/zero? p) (if (core-zero? n)
                      (throw (ArithmeticException. "poly 0^0"))
                    p)
        (core-zero? n) 1
        :else (loop [x p c n a (make 1)]
                (if (core-zero? c) a
                    (if (even? c)
                      (recur (mul x x) (quot c 2) a)
                      (recur x (dec c) (mul x a)))))))

(defn expression->
  "Convert an expression into Flat Polynomial canonical form. The
  expression should be an unwrapped expression, i.e., not an instance
  of the Expression type, nor should subexpressions contain type
  information. This kind of simplification proceeds purely
  symbolically over the known Flat Polynomial operations; other
  operations outside the arithmetic available in polynomials over
  commutative rings should be factored out by an expression analyzer
  before we get here. The result is a Poly object representing the
  polynomial structure of the input over the unknowns."
  [expr cont]
  ;; TODO: if we had a variable sort ordering, that sort would go in place of the
  ;; call to seq immediately below.
  (let [expression-vars (seq (set/difference (x/variables-in expr) operators-known))
        new-bindings (zipmap expression-vars (new-variables (count expression-vars)))
        environment (into operator-table new-bindings)]
   (cont ((x/walk-expression environment) expr) expression-vars)))

(defn- graded-reverse-lex-order
  "An ordering on monomials. X < Y if X has higher total
  degree than Y. In case of ties, X < Y if Y < X lexicographically.
  This is intended, when used as the comparator in an ascending
  sort, to produce an ordering like:
     x^2 + xy + y^2 + x + y + 1.
  "
  [xs ys]
  (let [deg (fn [xs] (if (= xs [0]) -1 (reduce + xs)))
        xd (deg xs)
        yd (deg ys)]
    (cond (> xd yd) -1
          (< xd yd) 1
          :else (compare ys xs))))

(defn ->expression
  "This is the output stage of Flat Polynomial canonical form simplification.
  The input is a Poly object, and the output is an expression
  representing the evaluation of that polynomial over the
  indeterminates extracted from the expression at the start of this
  process."
  [^Poly p vars]
  (if (base? p)
    p
    (reduce sym/add 0 (map (fn [[exponents coefficient]]
                             (sym/mul coefficient
                                      (reduce sym/mul 1 (map (fn [exponent var]
                                                               (sym/expt var exponent))
                                                             exponents vars))))
                           (->> p :xs->c (sort-by first graded-reverse-lex-order))))))

;; The operator-table represents the operations that can be understood
;; from the point of view of a polynomial over a commutative ring. The
;; functions take polynomial inputs (perhaps simply elements of the
;; base ring, in the case of constant polynomials) and return
;; polynomials (again, it is possible for any arithmetic operation
;; over the polynomial ring to have a value lying in the base
;; ring; when this happens, the type is lowered to the base ring.

(def ^:private operator-table
  {`g/+ #(reduce add 0 %&)
   `g/- (fn [arg & args] (if (some? args) (sub arg (reduce add args)) (negate arg)))
   `g/* #(reduce mul 1 %&)
   `g/negate negate
   `g/expt expt
   `g/square #(mul % %)
   ;`'g/gcd gcd
   })

(def operators-known (set (keys operator-table)))
