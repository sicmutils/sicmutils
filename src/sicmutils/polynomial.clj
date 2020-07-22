;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.polynomial
  (:import (clojure.lang BigInt Ratio))
  (:require [clojure.set :as set]
            [clojure.string]
            [sicmutils
             [analyze :as a]
             [value :as v]
             [generic :as g]
             [numsymb :as sym]
             [expression :as x]]))

(declare operator-table operators-known make-constant)
(def coefficient second)
(def exponents first)

;; Monomials
;;
;; We represent a monomial as a vector of integers representing
;; the exponents of the indeterminates over some ring. For example;
;; we would represent x^2 as [2], and xy^2 as [1 2], though the
;; indeterminates have no name. Polynomials are linear combinations
;; of the monomials. When these are formed, it is important that the
;; monomial vectors all contain the same number of slots, so that
;; 3x + 2y^2 would be represented as: 3*[1 0] + 2*[0 2].

(defn ^:private monomial-degree
  "Compute the degree of a monomial. This is just the sum of the exponents."
  [m]
  (reduce + m))

;; Monomial Orderings
;;
;; These orderings are in the sense of Java: x.compareTo(y), so that
;; this returns 1 if x > y, -1 if x < y, and 0 if x = y.

(defn lex-order
  "Lex order for monomials considers the power of x, then the power of y, etc."
  [xs ys]
  {:pre (= (count xs) (count ys))}
  (compare xs ys))

(defn graded-lex-order
  ""
  [xs ys]
  {:pre (= (count xs) (count ys))}
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd) (lex-order xs ys) (- xd yd))))

(defn graded-reverse-lex-order
  ""
  [xs ys]
  {:pre (= (count xs) (count ys))}
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd) (compare (vec (rseq ys)) (vec (rseq xs))) (- xd yd))))

(def ^:private monomial-order graded-lex-order)
(def ^:private empty-coefficients [])

;;
;; Polynomials
;;

(declare evaluate)

(deftype Polynomial [arity xs->c]
  v/Value
  (nullity? [_] (empty? xs->c))
  (numerical? [_] false)
  (exact? [_] false)
  (zero-like [_] (Polynomial. arity empty-coefficients))
  (one-like [_] (make-constant arity (v/one-like (coefficient (exponents xs->c)))))
  (unity? [_] (and (= (count xs->c) 1)
                   (let [[xs c] (first xs->c)]
                     (and (every? zero? xs)
                          (v/unity? c)))))
  (freeze [_] `(~'polynomial ~arity ~xs->c))
  (kind [_] ::polynomial)
  Object
  (equals [_ b]
    (and (instance? Polynomial b)
         (let [^Polynomial bp b]
           (and (= arity (.arity bp))
                (= xs->c (.xs->c bp))))))
  (toString [_]
    (let [n 10
          c (count xs->c)]
      (str "("
           (clojure.string/join ";"
                                (take n (for [[k v] xs->c]
                                          (str v "*" (clojure.string/join "," k)))))
           (if (> c n) (format " ...and %d more terms" (- c n)))
           ")"))))

(defn make
  "When called with two arguments, the first is the arity
  (number of indeterminates) of the polynomial followed by a sequence
  of exponent-coefficient pairs. Each exponent should be a vector with
  length equal to the arity, with integer exponent values. To
  make 4 x^2 y + 5 x y^2, an arity 2 polynomial (since it has two
  variables, x and y), we could write the following for xc-pairs:
  [[[2 1] 4] [[1 2] 5]]

  When called with one argument, the sequence is interpreted as a
  dense sequence of coefficients of an arity-1 (univariate)
  polynomial. The coefficients begin with the constant term and
  proceed to each higher power of the indeterminate. For example, x^2
  - 1 can be constructed by (make -1 0 1)."
  ([arity xc-pairs]
   (Polynomial. arity
                (->> (for [[xs cs] (group-by exponents xc-pairs)
                           :let    [sum-cs (reduce #(g/+ %1 (coefficient %2)) 0 cs)]
                           :when   (not (v/nullity? sum-cs))]
                       [xs sum-cs])
                     (sort-by exponents monomial-order)
                     (into empty-coefficients))))
  ([dense-coefficients]
   (make 1 (zipmap (map vector (iterate inc 0)) dense-coefficients))))

(defn ^:private lead-term
  "Return the leading (i.e., highest degree) term of the polynomial
  p. The return value is [exponents coefficient]."
  [^Polynomial p]
  (-> p .xs->c peek))

(defn degree
  [p]
  (if (v/nullity? p) -1
      (->> p lead-term exponents (reduce +))))

(defn monomial?
  [^Polynomial p]
  (-> p .xs->c count (= 1)))

(defn coefficients
  [^Polynomial p]
  (->> p .xs->c (map coefficient)))

(defn check-same-arity [^Polynomial p ^Polynomial q]
  (let [ap (.arity p)
        aq (.arity q)]
    (if (= ap aq) ap
        (throw (ArithmeticException. "mismatched polynomial arity")))))

(defn map-coefficients
  "Map the function f over the coefficients of p, returning a new Polynomial."
  [f ^Polynomial p]
  (Polynomial. (.arity p) (into empty-coefficients
                                (for [[xs c] (.xs->c p)
                                      :let [fc (f c)]
                                      :when (not (v/nullity? fc))]
                                  [xs fc]))))

(defn map-exponents
  "Map the function f over the exponents of each monomial in p,
  returning a new Polynomial."
  [f ^Polynomial p]
  (make (.arity p) (for [[xs c] (.xs->c p)]
                      [(f xs) c])))

(def negate (partial map-coefficients g/negate))

(defn make-constant
  "Return a constant polynomial of the given arity."
  [arity c]
  (Polynomial. arity (if (v/nullity? c) empty-coefficients
                         (conj empty-coefficients [(vec (repeat arity 0)) c]))))

(defn add
  "Adds the polynomials p and q"
  [^Polynomial p ^Polynomial q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (cond (v/nullity? p) q
        (v/nullity? q) p
        :else (make (check-same-arity p q) (concat (.xs->c p) (.xs->c q)))))

(defn sub
  "Subtract the polynomial q from the polynomial p."
  [^Polynomial p ^Polynomial q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (cond (v/nullity? p) (negate q)
        (v/nullity? q) p
        :else (make (check-same-arity p q)
                    (concat (.xs->c p) (for [[xs c] (.xs->c q)]
                                         [xs (g/negate c)])))))

(defn mul
  "Multiply polynomials p and q, and return the product."
  [^Polynomial p ^Polynomial q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (cond (v/nullity? p) p
        (v/nullity? q) q
        (v/unity? p) q
        (v/unity? q) p
        :else (let [a (check-same-arity p q)]
                (make a (for [[xp cp] (.xs->c p)
                              [xq cq] (.xs->c q)]
                          [(mapv + xp xq) (g/* cp cq)])))))

(defn raise-arity
  "The opposite of lower-arity."
  [^Polynomial p]
  {:pre [(instance? Polynomial p)
         (= (.arity p) 1)]}
  (let [terms (for [[x ^Polynomial q] (.xs->c p)
                    [ys c] (.xs->c q)]
                [(into x ys) c])
        ^Polynomial ltc (coefficient (lead-term p))]
    (make (inc (.arity ltc)) terms)))

(defn lower-arity
  "Given a nonzero polynomial of arity A > 1, return an equivalent polynomial
  of arity 1 whose coefficients are polynomials of arity A-1."
  [^Polynomial p]
  {:pre [(instance? Polynomial p)
         (> (.arity p) 1)
         (not (v/nullity? p))]}
  ;; XXX observation:
  ;; XXX we often create polynomials of "one lower arity"
  ;; which are EFFECTIVELY UNIVARIATE. When this happens,
  ;; we should notice.
  ;; (but univariate in which variable? is it really that
  ;; common that it's the first one?)
  (let [A (.arity p)]
    (->> p
         .xs->c
         (group-by #(-> % exponents first))
         (map (fn [[x cs]]
                [[x] (make (dec A) (for [[xs c] cs]
                                     [(subvec xs 1) c]))]))
         (make 1))))

(defn ^:private evaluate-1
  "Evaluates a univariate polynomial p at x."
  [^Polynomial p x]
  (loop [xs->c (.xs->c p)
         result 0
         x**e 1
         e 0]
    (if-let [[[e'] c] (first xs->c)]
      (let [x**e' (g/* x**e (g/expt x (- e' e)))]
        (recur (next xs->c)
               (g/+ result (g/* c x**e'))
               x**e'
               e'))
      result)))

(defn evaluate
  "Evaluates a multivariate polynomial p at xs."
  [^Polynomial p xs]
  {:pre [(instance? Polynomial p)]}
  (cond (nil? xs) p
        (v/nullity? p) 0
        (= (.arity p) 1) (evaluate-1 p (first xs))
        :else (let [L (evaluate-1 (lower-arity p) (first xs))]
                (if (instance? Polynomial L)
                  (recur L (next xs))
                  L))))

(defn divide
  "Divide polynomial u by v, and return the pair of [quotient, remainder]
  polynomials. This assumes that the coefficients are drawn from a field,
  and so support division."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (cond (v/nullity? v) (throw (IllegalArgumentException. "internal polynomial division by zero"))
        (v/nullity? u) [u u]
        (v/unity? v) [u (v/zero-like u)]
        :else (let [arity (check-same-arity u v)
                    [vn-exponents vn-coefficient] (lead-term v)
                    good? (fn [residues]
                            (and (not-empty residues)
                                 (every? (complement neg?) residues)))]
                (if (zero? arity)
                  [(make 0 [[[] (g/divide (coefficient (lead-term u)) vn-coefficient)]])
                   (make 0 [[[] 0]])]
                  (loop [quotient (make arity [])
                         remainder u]
                    ;; find a term in the remainder into which the
                    ;; lead term of the divisor can be divided.
                    (let [[r-exponents r-coefficient] (lead-term remainder)
                          residues (mapv - r-exponents vn-exponents)]
                      (if (good? residues)
                        (let [new-coefficient (g/divide r-coefficient vn-coefficient)
                              new-term (make arity [[residues new-coefficient]])]
                          (recur (add quotient new-term)
                                 (sub remainder (mul new-term v))))
                        [quotient remainder])))))))

(defn pseudo-remainder
  "Compute the pseudo-remainder of univariate polynomials p and
  q. Fractions won't appear in the result; instead the divisor is
  multiplied by the leading coefficient of the dividend before
  quotient terms are generated so that division will not result in
  fractions. Only the remainder is returned, together with the
  integerizing factor needed to make this happen. Similar in spirit to
  Knuth's algorithm 4.6.1R, except we don't multiply the remainder
  through during gaps in the remainder. Since you don't know up front
  how many times the integerizing multiplication will be done, we also
  return the number d for which d * u = q * v + r."
  [^Polynomial u ^Polynomial v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)
         (not (v/nullity? v))
         (= (.arity u) (.arity v) 1)]}
  (let [a (check-same-arity u v)
        [vn-exponents vn-coefficient] (lead-term v)
        *vn (fn [p] (map-coefficients #(g/* vn-coefficient %) p))
        n (reduce + vn-exponents)]
    (loop [remainder u d 0]
      (let [m (degree remainder)
            c (-> remainder lead-term coefficient)]
        (if (< m n)
          [remainder d]
          (recur (sub (*vn remainder)
                      (mul v (Polynomial. a [[[(- m n)] c]])))
                 (inc d)))))))

(defn evenly-divide
  "Divides the polynomial u by the polynomial v. Throws an IllegalStateException
  if the division leaves a remainder. Otherwise returns the quotient."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [[q r] (divide u v)]
    (when-not (v/nullity? r)
      (throw (IllegalStateException. (str "expected even division left a remainder!" u " / " v " r " r))))
    q))

(defn abs
  [p]
  (if (-> p lead-term coefficient g/negative?)
    (negate p)
    p))

(defn expt
  "Raise the polynomial p to the (integer) power n."
  [^Polynomial p n]
  (when-not (and (integer? n) (>= n 0))
    (throw (ArithmeticException.
            (str "can't raise poly to " n))))
  (cond (v/unity? p) p
        (v/nullity? p) (if (zero? n)
                      (throw (ArithmeticException. "poly 0^0"))
                      p)
        (zero? n) (make-constant (.arity p) 1)
        :else (loop [x p c n a (make-constant (.arity p) 1)]
                (if (zero? c) a
                    (if (even? c)
                      (recur (mul x x) (quot c 2) a)
                      (recur x (dec c) (mul x a)))))))

(defn partial-derivative
  "The partial derivative of the polynomial with respect to the
  i-th indeterminate."
  [^Polynomial p i]
  (make (.arity p)
        (for [[xs c] (.xs->c p)
              :let [xi (xs i)]
              :when (not= 0 xi)]
          [(update xs i dec) (g/* xi c)])))

(defn partial-derivatives
  "The sequence of partial derivatives of p with respect to each
  indeterminate"
  [^Polynomial p]
  (for [i (range (.arity p))]
    (partial-derivative p i)))

;; The operator-table represents the operations that can be understood
;; from the point of view of a polynomial over a commutative ring. The
;; functions take polynomial inputs and return polynomials.

(def ^:private operator-table
  {'+ #(reduce g/add %&)
   '- (fn [arg & args]
        (if (some? args) (g/sub arg (reduce g/add args)) (g/negate arg)))
   '* #(reduce g/mul %&)
   'negate negate
   'expt g/expt
   'square #(mul % %)
   'cube #(mul % (mul % %))
   ;;`'g/gcd gcd
   })

(def ^:private operators-known (into #{} (keys operator-table)))

(deftype PolynomialAnalyzer []
  a/ICanonicalize
  (expression-> [this expr cont] (a/expression-> this expr cont compare))
  (expression-> [this expr cont v-compare]
    ;; Convert an expression into Flat Polynomial canonical form. The
    ;; expression should be an unwrapped expression, i.e., not an instance
    ;; of the Expression type, nor should subexpressions contain type
    ;; information. This kind of simplification proceeds purely
    ;; symbolically over the known Flat Polynomial operations; other
    ;; operations outside the arithmetic available in polynomials over
    ;; commutative rings should be factored out by an expression analyzer
    ;; before we get here. The result is a Polynomial object representing
    ;; the polynomial structure of the input over the unknowns.
    (let [expression-vars (sort v-compare (set/difference (x/variables-in expr) operators-known))
          variables (zipmap expression-vars (a/new-variables this (count expression-vars)))]
      (-> expr (x/walk-expression variables operator-table) (cont expression-vars))))
  (->expression [this p vars]
    ;; This is the output stage of Flat Polynomial canonical form simplification.
    ;; The input is a Polynomial object, and the output is an expression
    ;; representing the evaluation of that polynomial over the
    ;; indeterminates extracted from the expression at the start of this
    ;; process.
    (if (instance? Polynomial p)
      (let [^Polynomial p p]
        (reduce
         sym/add 0
         (map (fn [[xs c]]
                (sym/mul c
                         (reduce sym/mul 1 (map (fn [exponent var]
                                                  (sym/expt var exponent))
                                                xs vars))))
              (->> p .xs->c (sort-by exponents #(monomial-order %2 %1))))))
      p))
  (known-operation? [_ o] (operator-table o))
  (new-variables [_ arity] (for [a (range arity)]
                             (make arity [[(mapv #(if (= % a) 1 0) (range arity)) 1]]))))

(defmethod g/add [::polynomial ::polynomial] [a b] (add a b))
(defmethod g/mul [::polynomial ::polynomial] [a b] (mul a b))
(defmethod g/sub [::polynomial ::polynomial] [a b] (sub a b))
(defmethod g/exact-divide [::polynomial ::polynomial] [p q] (evenly-divide p q))
(defmethod g/square [::polynomial] [a] (mul a a))

(doseq [t [Long BigInt BigInteger Double Ratio]]
  (defmethod g/mul
    [t ::polynomial]
    [c p]
    (map-coefficients #(g/* c %) p))
  (defmethod g/mul
    [::polynomial t]
    [p c]
    (map-coefficients #(g/* % c) p))
  (defmethod g/add
    [t ::polynomial]
    [c ^Polynomial p]
    (add (make-constant (.arity p) c) p))
  (defmethod g/add
    [::polynomial t]
    [^Polynomial p c]
    (add p (make-constant (.arity p) c)))
  (defmethod g/sub
    [t ::polynomial]
    [c ^Polynomial p]
    (sub (make-constant (.arity p) c) p))
  (defmethod g/sub
    [::polynomial t]
    [^Polynomial p c]
    (sub p (make-constant (.arity p) c)))
  (defmethod g/div
    [::polynomial t]
    [p c]
    (map-coefficients #(g/divide % c) p)))

(defmethod g/expt [::polynomial Integer] [b x] (expt b x))
(defmethod g/expt [::polynomial Long] [b x] (expt b x))
(defmethod g/negate [::polynomial] [a] (negate a))
