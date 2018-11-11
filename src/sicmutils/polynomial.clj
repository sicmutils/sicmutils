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
  (:require [clojure.tools.logging :as log];;XXX
            [clojure.string :refer [join]]
            [clojure.set :as set]
            [clojure.string]
            [sicmutils
             [analyze :as a]
             [generic :as g]
             [euclid :as euclid]
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

(declare evaluate polynomial-one?)

(deftype Polynomial [arity xs->c]
  g/INumericType
  (zero? [_] (empty? xs->c))
  (one? [a] (polynomial-one? a))
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

(defn polynomial-constant?
  "If the polynomial is constant (i.e., degree zero or negative), then
  the constant value is returned, else nil."
  [^Polynomial p]
  (let [xs->c (.xs->c p)]
    (if (= (count xs->c) 1)
      (let [[xs c] (first xs->c)]
        (if (every? zero? xs) c)))))

(defn ^:private polynomial-one?
  [^Polynomial p]
  (and (polynomial-constant? p)
       (g/one? (coefficient (first (.xs->c p))))))

(defn polynomial-zero? [^Polynomial u] (empty? (.xs->c u)))

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
                           :when   (not (g/zero? sum-cs))]
                       [xs sum-cs])
                     (sort-by exponents monomial-order)
                     (into empty-coefficients))))
  ([dense-coefficients]
   (make 1 (zipmap (map vector (iterate inc 0)) dense-coefficients))))

(defn lead-term
  "Return the leading (i.e., highest degree) term of the polynomial
  p. The return value is [exponents coefficient]."
  [^Polynomial p]
  (-> p .xs->c peek))

(defn lead-coefficient
  "Returns the coefficient of the lead term of the polynomial."
  [p]
  (coefficient (lead-term p)))

(defn degree
  [p]
  (if (polynomial-zero? p) -1
      (->> p lead-term exponents (reduce +))))

(defn monomial?
  [^Polynomial p]
  (-> p .xs->c count (= 1)))

(defn coefficients
  [^Polynomial p]
  (map coefficient (.xs->c p)))

(defn ->skeleton
  "This is called sparse-exponents in scmutils; we call it skeleton
  because that is the name given to this operation in the literature
  on multivariate GCD algorithms."
  [^Polynomial p]
  (map exponents (.xs->c p)))

(defn check-same-arity [^Polynomial p ^Polynomial q]
  (let [ap (.arity p)
        aq (.arity q)]
    (if (= ap aq) ap
        (throw (ArithmeticException. "mismatched polynomial arity")))))

(defn map-coefficients
  "Map the function f over the coefficients of p, returning a new Polynomial."
  [f ^Polynomial p]
  (Polynomial. (.arity p) (loop [[old-term & old-terms] (.xs->c p)
                                 new-terms empty-coefficients]
                            (if (nil? old-term) new-terms
                                (let [fc (f (coefficient old-term))]
                                  (recur old-terms (if (g/zero? fc) new-terms
                                                       (conj new-terms [(exponents old-term) fc]))))))))

(defn map-exponents
  "Map the function f over the exponents of each monomial in p,
  returning a new Polynomial."
  [f ^Polynomial p]
  (make (.arity p) (for [[xs c] (.xs->c p)]
                      [(f xs) c])))

(def negate (partial map-coefficients g/negate))

(defn scale
  [c u]
  (map-coefficients #(*' c %) u))

(defn make-constant
  "Return a constant polynomial of the given arity."
  [arity c]
  (Polynomial. arity (if (g/zero? c) empty-coefficients
                         (conj empty-coefficients [(vec (repeat arity 0)) c]))))

(defn ^:private make-linear
  "Given c, return the univariate polynomial x - c."
  [c]
  (Polynomial. 1 [[[0] (- c)] [[1] 1]]))

(defn combine-like-terms
  "Merges the polynomials p and q. Where p and q have a like term, f is
  called on the old coefficients to produce the new one. Where there
  are terms in one with no companion in the other, zero is substituted
  for the missing coefficient."
  [^Polynomial p ^Polynomial q f]
  (let [a (check-same-arity p q)
        terms (loop [[p-term & p-terms :as all-ps] (.xs->c p)
                     [q-term & q-terms :as all-qs] (.xs->c q)
                     r-terms empty-coefficients]
                (cond
                  (nil? p-term) (if (nil? q-term) r-terms
                                    (let [fc (f 0 (coefficient q-term))]
                                      (recur nil q-terms
                                             (if (g/zero? fc) r-terms
                                                 (conj r-terms [(exponents q-term) fc])))))
                  (nil? q-term) (let [fc (f (coefficient p-term) 0)]
                                  (recur p-terms nil
                                         (if (g/zero? fc) r-terms
                                             (conj r-terms [(exponents p-term) fc]))))

                  :else (let [o (monomial-order (exponents p-term) (exponents q-term))]
                          (cond (neg? o) (let [fc (f (coefficient p-term) 0)]
                                           (recur p-terms all-qs
                                                  (if (g/zero? fc) r-terms (conj r-terms [(exponents p-term) fc]))))
                                (pos? o) (let [fc (f 0 (coefficient q-term))]
                                           (recur all-ps q-terms
                                                  (if (g/zero? fc) r-terms (conj r-terms [(exponents q-term) fc]))))
                                :else (let [fc (f (coefficient p-term) (coefficient q-term))]
                                        (recur p-terms q-terms
                                               (if (g/zero? fc) r-terms (conj r-terms [(exponents p-term) fc]))))))))]
    (Polynomial. a terms)))

(defn add
  "Adds the polynomials p and q"
  [^Polynomial p ^Polynomial q]
  (cond (polynomial-zero? p) q
        (polynomial-zero? q) p
        :else (combine-like-terms p q g/+)))


(defn sub
  "Subtract the polynomial q from the polynomial p."
  [^Polynomial p ^Polynomial q]
  (cond (polynomial-zero? p) (negate q)
        (polynomial-zero? q) p
        :else (combine-like-terms p q g/-)))

(defn mul
  "Multiply polynomials p and q, and return the product."
  [^Polynomial p ^Polynomial q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (cond (polynomial-zero? p) p
        (polynomial-zero? q) q
        (g/one? p) q
        (g/one? q) p
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
        ^Polynomial ltc (lead-coefficient p)]
    (make (inc (.arity ltc)) terms)))

(defn lower-arity
  "Given a nonzero polynomial of arity A > 1, return an equivalent polynomial
  of arity 1 whose coefficients are polynomials of arity A-1."
  [^Polynomial p]
  {:pre [(instance? Polynomial p)
         (> (.arity p) 1)
         (not (polynomial-zero? p))]}
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

(defn evaluate-1
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
               (long e')))
      result)))

(defn partial-evaluate
  "Evaluates a multivariate polynomial p at xs. The result is a
  polynomial with arity lowered by one for each element of xs."
  ^Polynomial
  [^Polynomial p xs & {:keys [direction] :or {direction :left}}]
  (let [p-arity (.arity p)
        x-arity (count xs)
        new-arity (- p-arity x-arity)]
    (when (> x-arity p-arity)
      (throw (IllegalArgumentException. "too many values supplied for polynomial")))
    (make new-arity
          (for [[es c] (.xs->c p)]
            (if (= direction :left)
              [(subvec es x-arity) (reduce g/* c (map g/expt xs (subvec es 0 x-arity)))]
              [(subvec es 0 new-arity) (reduce g/* c (map g/expt xs (subvec es new-arity)))])))))

(defn divide
  "Divide polynomial u by v, and return the pair of [quotient, remainder]
  polynomials. This assumes that the coefficients are drawn from a field,
  and so support division."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [arity (check-same-arity u v)]
    (cond (polynomial-zero? v) (throw (IllegalArgumentException. "internal polynomial division by zero"))
          (polynomial-zero? u) [u u]
          ;; nb: we are thinking of getting out of the business of having
          ;; the polynomials exist over general rings, and having the
          ;; polynomials function only over Z, so we wouldn't call zero-like.
          ;; On the other hand, we went to a great deal of trouble to
          ;; genericize the polynomial arithmetic... but maybe this should
          ;; not have been done?
          (g/one? v) [u (make arity [])]
          :else (let [[vn-exponents vn-coefficient] (lead-term v)
                      good? (fn [residues]
                              (and (not-empty residues)
                                   (every? (complement neg?) residues)))]
                  (if (zero? arity)
                    [(make 0 [[[] (g/divide (lead-coefficient u) vn-coefficient)]])
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
                          [quotient remainder]))))))))

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
         (not (polynomial-zero? v))
         (= (.arity u) (.arity v) 1)]}
  (let [a (check-same-arity u v)
        [vn-exponents vn-coefficient] (lead-term v)
        *vn (fn [p] (map-coefficients #(g/* vn-coefficient %) p))
        n (reduce + vn-exponents)]
    (loop [remainder u d 0]
      (let [m (degree remainder)
            c (lead-coefficient remainder)]
        (if (< m n)
          [remainder d]
          (recur (sub (*vn remainder)
                      ;; The following is v * c^(m-n). We can compute this cheaply
                      (Polynomial. a (mapv (fn [[[e] k]] [[(- (+ e m) n)] (g/* k c)]) (.xs->c v))))
                 (inc d)))))))

(defn ^:private balance
  [a m]
  (let [b (mod a m)]
    (if (> b (quot m 2)) (- b m) b)))

;; XXX qq: why do we get an infinite loop when we use balance and euclidean GCD?

(defn polynomial-reduce-mod
  [m p]
  (map-coefficients #(mod % m) p))

(defn univariate-modular-remainder
  "Divide polynomial u by v (in ℤ/pℤ), and return the remainder."
  [p ^Polynomial u ^Polynomial v]
  {:pre [(= (.arity u) (.arity v) 1)]}
  (when (polynomial-zero? v)
    (throw (IllegalArgumentException. "internal polynomial division by zero")))
  (let [v (polynomial-reduce-mod p v)
        vn-inv (euclid/modular-inverse p (lead-coefficient v))]
    (loop [r u]
      (let [delta (- (degree r) (degree v))]
        (if (neg? delta) r
            (recur (polynomial-reduce-mod
                    p
                    (sub r (mul v (Polynomial. 1 [[[delta] (* vn-inv (lead-coefficient r))]]))))))))))

(defn evenly-divide
  "Divides the polynomial u by the polynomial v. Throws an IllegalStateException
  if the division leaves a remainder. Otherwise returns the quotient."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [[q r] (divide u v)]
    (when-not (polynomial-zero? r)
      (throw (IllegalStateException. (str "expected even division left a remainder!" u " / " v " r " r))))
    q))

(defn abs
  [p]
  (if (g/negative? (lead-coefficient p))
    (negate p)
    p))

(defn expt
  "Raise the polynomial p to the (integer) power n."
  [^Polynomial p n]
  (when-not (and (integer? n) (>= n 0))
    (throw (ArithmeticException.
            (str "can't raise poly to " n))))
  (cond (g/one? p) p
        (polynomial-zero? p) (if (zero? n)
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

(defn lagrange-interpolating-polynomial
  [xs ys]
  (let [q (transduce (map make-linear) (completing mul) (make-constant 1 1) xs)]
    (loop [f (make 1 [])
           xs xs
           ys ys]
      (if (nil? xs) f
          (let [x (first xs)
                qi (evenly-divide q (make-linear x))
                pi (scale (/ (evaluate-1 qi x)) qi)]
            (recur (add f (scale (first ys) pi))
                   (next xs)
                   (next ys)))))))


;; The operator-table represents the operations that can be unerstood
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

(defmethod g/add [Polynomial Polynomial] [a b] (add a b))
(defmethod g/mul [Polynomial Polynomial] [a b] (mul a b))
(defmethod g/sub [Polynomial Polynomial] [a b] (sub a b))
(defmethod g/exact-divide [Polynomial Polynomial] [p q] (evenly-divide p q))
(defmethod g/square [Polynomial] [a] (mul a a))
(defmethod g/zero-like [Polynomial] [^Polynomial a] (make (.arity a) []))
(defmethod g/one-like [Polynomial] [^Polynomial a] (make-constant (.arity a) (g/one-like (coefficient (first (.xs->c a))))))
(defmethod g/mul [Polynomial ::sym/numeric-type] [p a] (map-coefficients #(g/* a %) p))
(defmethod g/mul [::sym/numeric-type Polynomial] [a p] (map-coefficients #(g/* % a) p))
(defmethod g/add [Polynomial ::sym/numeric-type] [^Polynomial p a] (add p (make-constant (.arity p) a)))
(defmethod g/add [::sym/numeric-type Polynomial] [a ^Polynomial p] (add (make-constant (.arity p) a) p))
(defmethod g/sub [Polynomial ::sym/numeric-type] [^Polynomial p a] (sub p (make-constant (.arity p) a)))
(defmethod g/sub [::sym/numeric-type Polynomial] [a ^Polynomial p] (sub (make-constant (.arity p) a) p))
(defmethod g/div [Polynomial ::sym/numeric-type] [p a] (map-coefficients #(g/divide % a) p))
(defmethod g/expt [Polynomial ::sym/native-integral-type] [b x] (expt b x))
(defmethod g/negate [Polynomial] [a] (negate a))
(defmethod g/freeze [Polynomial] [^Polynomial a] `(~'polynomial ~(.arity a) ~(.xs->c a)))
