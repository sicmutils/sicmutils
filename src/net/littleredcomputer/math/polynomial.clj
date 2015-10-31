;
; Copyright (C) 2015 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns net.littleredcomputer.math.polynomial
  (:import (clojure.lang PersistentTreeMap))
  (:require [clojure.set :as set]
            [clojure.string]
            [net.littleredcomputer.math
             [value :as v]
             [euclid :as euclid]
             [generic :as g]
             [numsymb :as sym]
             [expression :as x]]))

(declare operator-table operators-known make-constant)
(def coefficient second)
(def exponents first)

(defrecord Polynomial [^long arity ^PersistentTreeMap xs->c]
  v/Value
  (nullity? [_] (empty? xs->c))
  (numerical? [_] false)
  (zero-like [_] (Polynomial. arity {}))
  (one-like [o] (make-constant arity (v/one-like (coefficient (first xs->c)))))
  (unity? [_] (and (= (count xs->c) 1)
                   (let [[xs c] (first xs->c)]
                     (and (every? zero? xs)
                          (v/unity? c)))))
  (kind [_] ::polynomial)
  Object
  (toString [_]
    (str "("
         (clojure.string/join ";"
                          (for [[k v] xs->c]
                            (str v "*" (clojure.string/join "," k))))
         ")")))

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
    (cond (> xd yd) 1
          (< xd yd) -1
          :else (lex-order xs ys))))

(defn graded-reverse-lex-order
  ""
  [xs ys]
  {:pre (= (count xs) (count ys))}
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (cond (> xd yd) 1
          (< xd yd) -1
          :else (compare (vec (rseq ys)) (vec (rseq xs))))))

(def ^:private monomial-order graded-lex-order)
(def ^:private empty-coefficients (sorted-map-by monomial-order))

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
   (->> xc-pairs
        (filter (fn [[_ c]] (not (g/zero? c))))
        (into empty-coefficients)
        (Polynomial. arity)))
  ([dense-coefficients]
   (make 1 (zipmap (map vector (iterate inc 0)) dense-coefficients))))

(defn degree
  [p]
  (if (v/nullity? p) -1
      (reduce max 0 (map #(reduce + 0 %) (keys (:xs->c p))))))

(defn constant?
  "If p is a constant polynomial, return that constant, else nil"
  [^Polynomial p]
  (let [xs->c (:xs->c p)]
    (cond (nil? xs->c) nil
          (empty? xs->c) 0
          (and (= (count xs->c) 1)
               (every? zero? (exponents (first xs->c)))) (coefficient (first xs->c)))))

(defn constant-term
  "Return the constant term of the polynomial."
  [{:keys [arity xs->c]}]
  (or (xs->c (vec (repeat arity 0))) 0))

(defn check-same-arity [p q]
  (let [ap (:arity p)
        aq (:arity q)]
    (cond (= ap aq) ap
          :else (throw (ArithmeticException. "mismatched polynomial arity")))))

(defn- poly-map
  "Map the function f over the coefficients of p, returning a new Polynomial."
  [f {:keys [arity xs->c]}]
  (make arity (for [[xs c] xs->c] [xs (f c)])))

(defn- poly-merge
  "Merge the polynomials together, combining corresponding coefficients with f.
  The result is not a polynomial object, but rather a sequence
  of [exponent, coefficient] pairs, suitable for further processing or
  canonicalization. Merged monomials with zero coefficient are
  dropped."
  [f p q]
  (loop [P (:xs->c p)
         Q (:xs->c q)
         R empty-coefficients]
    (cond
      (empty? P) (into R (for [[xs c] Q
                               :let [c1 (f 0 c)]
                               :when (not (g/zero? c1))] [xs c1]))
      (empty? Q) (into R (for [[xs c] P
                               :let [c1 (f c 0)]
                               :when (not (g/zero? c1))] [xs c1]))
      :else (let [[xp cp] (first P)
                  [xq cq] (first Q)
                  order (monomial-order xp xq)]
              (cond
                (zero? order) (let [v (f cp cq)]
                                (recur (rest P) (rest Q)
                                       (if-not (g/zero? v)
                                         (assoc R xp v)
                                         R)))
                (< order 0) (recur (rest P) Q (assoc R xp (f cp 0)))
                :else (recur P (rest Q) (assoc R xq (f 0 cq))))))))

(defn new-variables
  "Creates a sequence of identity (i.e., x) polynomials, one for each
  of arity indeterminates."
  [arity]
  (for [a (range arity)]
    (make arity [[(mapv #(if (= % a) 1 0) (range arity)) 1]])))

(def negate (partial poly-map g/negate))

(defn make-constant
  "Return a constant polynomial of the given arity."
  [arity c]
  (make arity [[(vec (repeat arity 0)) c]]))

(defn add
  "Adds the polynomials p and q (either or both of which might just be
  constants in the base ring)."
  [p q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (cond (g/zero? p) q
        (g/zero? q) p
        :else (let [a (check-same-arity p q)
                    sum (poly-merge g/+ p q)]
                (make a sum))))

(defn- add-denormal
  "Add-denormal adds the (order, coefficient) pair to the polynomial p,
  expecting that p is currently in sparse form (i.e., not a primitive number)
  and without normalizing the result (e.g., to see if the polynomial has
  become constant or a term has dropped out). Useful in intermediate steps
  of polynomial computations."
  [xs->c [xs c]]
  (assoc xs->c xs (g/+ (get xs->c xs 0) c)))

(defn sub
  "Subtract the polynomial q from the polynomial p."
  [p q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (cond (g/zero? p) (negate q)
        (g/zero? q) p
        :else (let [a (check-same-arity p q)
                    diff (poly-merge g/- p q)]
                (make a diff))))

(defn mul
  "Multiply polynomials p and q, and return the product."
  [p q]
  {:pre [(instance? Polynomial p)
         (instance? Polynomial q)]}
  (let [arity (check-same-arity p q)]
    (cond (g/zero? p) (make-constant arity 0)
          (g/zero? q) (make-constant arity 0)
          (g/one? p) q
          (g/one? q) p
          :else (let [a (check-same-arity p q)]
                  (make a (reduce add-denormal
                                  empty-coefficients
                                  (for [[xp cp] (:xs->c p)
                                        [xq cq] (:xs->c q)]
                                    [(mapv + xp xq) (g/* cp cq)])))))))

(defn- lead-term
  "Return the leading (i.e., highest degree) term of the polynomial
  p. The return value is [exponents coefficient]."
  [p]
  (-> p :xs->c rseq first))

(defn lower-arity
  "Given a polynomial of arity A > 1, return an equivalent polynomial
  of arity 1 whose coefficients are polynomials of arity A-1."
  [p]
  {:pre [(instance? Polynomial p)
         (> (:arity p) 1)]}
  (let [A (:arity p)]
    (->> p
         :xs->c
         (group-by #(-> % exponents first))
         (map (fn [[x cs]]
                [[x] (make (dec A) (for [[xs c] cs]
                                     [(subvec xs 1) c]))]))
         (make 1))))

(defn raise-arity
  [p]
  {:pre [(instance? Polynomial p)
         (= (:arity p) 1)]}
  (let [terms (for [[x q] (:xs->c p)
                    [ys c] (:xs->c q)]
                [(into x ys) c])]
    (make (inc (:arity (coefficient (lead-term p)))) terms)))

(declare gcd)

(defn divide
  "Divide polynomial u by v, and return the pair of [quotient, remainder]
  polynomials. This assumes that the coefficients are drawn from a field,
  and so support division. If you want pseudo-division instead, you can
  set {:pseudo true} in the options. In this case fractions won't appear;
  instead the divisor is multiplied by the leading coefficient of the
  dividend before quotient terms are generated so that division will not
  result in fractions. In that event, a third term is returned containing
  the power of the leading coefficient needed to relate the pseudo-quotient
  and pseudo-remainder returned in the first two terms. Similar in spirit
  to Knuth's algorithm 4.6.1R, except we don't multiply the remainder
  through during gaps in the remainder. Since you don't know up front
  how many times the integerizing multiplication will be done, we also return
  the number m for which m * u = q * v + r."
  [u v & [{:keys [pseudo]}]]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [[q r m] (let [arity (check-same-arity u v)
                      [vn-exponents vn-coefficient] (lead-term v)
                      *vn (fn [p] (poly-map #(g/* % vn-coefficient) p))]
                  (if (zero? arity)
                    ;; XXX: we're sort of breaking the pseudo-division promise
                    ;; in this case, but both of the polynomials were constant,
                    ;; so what do they expect?
                    [(make 0 [[[] (g/divide (coefficient (lead-term u)) vn-coefficient)]])
                     (make 0 [[[] 0]])
                     1]
                    (loop [quotient (make arity [])
                           remainder u
                           multiplier (v/one-like vn-coefficient)]
                      ;; find a term in the remainder into which the
                      ;; lead term of the divisor can be divided.
                      (let [remainder' (if pseudo (*vn remainder)
                                           remainder)
                            good-terms (->> remainder'
                                            :xs->c rseq
                                            (map (fn [[xs c]]
                                                   [(map - xs vn-exponents) c]))
                                            (filter (fn [[residues _]]
                                                      (and (not-empty residues)
                                                           (every? (complement neg?) residues)))))]
                        ;; where we left off: g/divide is returning an array of [quotient, remainder]
                        ;; so maybe we should try evenly-divide here for the generic op.
                        ;; Maybe we want to special-case polynomials here. Maybe what we want is
                        ;; a pseudo-division. I can't shake the feeling at this point that
                        ;; we maybe shouldn't even be here. Or, that if we recurse on division,
                        ;; we should carry the psuedo field along. g/divide seems to be
                        ;; where it all goes wrong so we should be careful with the definition
                        ;; of that.
                        (if-let [[residues c] (first good-terms)]
                          (let [new-coefficient (g/divide c vn-coefficient)
                                new-term (make arity [[(vec residues) new-coefficient]])]
                            (recur (add (if pseudo (*vn quotient) quotient) new-term)
                                   (sub remainder' (mul new-term v))
                                   (if pseudo (g/* multiplier vn-coefficient) multiplier)))
                          [quotient remainder multiplier])))))]
    (if pseudo [q r m] [q r])))

(defn ^:private evenly-divide
  "Divides the polynomial u by the polynomial v. Throws an IllegalStateException
  if the division leaves a remainder. Otherwise returns the quotient."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [[q r] (divide u v)]
    (when-not (v/nullity? r)
      (throw (IllegalStateException. (str "expected even division left a remainder!" u " / " v " r " r))))
    q))

(defn ^:private gcd1
  "Knuth's algorithm 4.6.1E for UNIVARIATE polynomials."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)
         (= (:arity u) 1)
         (= (:arity v) 1)]}
  (cond
    (v/nullity? u) v
    (v/nullity? v) u
    :else (let [content1 #(->> % :xs->c vals (reduce euclid/gcd))
                attach-content1 (fn [p c] (poly-map #(g/* c %) p))
                divide-coefs (fn [p c] (poly-map #(g/divide % c) p))
                ku (content1 u)
                kv (content1 v)
                pu (divide-coefs u ku)
                pv (divide-coefs v kv)
                d (euclid/gcd ku kv)]
            (loop [u pu
                   v pv]
              (let [[_ r _] (divide u v {:pseudo true})]
                (cond (v/nullity? r)
                      (if (< (coefficient (lead-term v)) 0)
                        (attach-content1 (negate v) d)
                        (attach-content1 v d))

                      (zero? (degree r))
                      #_(make-constant arity d)
                      (make [d])

                      :else
                      (recur v (divide-coefs r (content1 r)))))))))

(defn gcd
  "Knuth's algorithm 4.6.1E. Delegates to gcd1 for univariate polynomials."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [arity (check-same-arity u v)]
    (cond
              (zero? arity) (make 0 [[[] (euclid/gcd (constant-term u) (constant-term v))]])
              (= arity 1) (gcd1 u v)
              (v/nullity? u) v
              (v/nullity? v) u
              :else (let [u1 (lower-arity u)
                          v1 (lower-arity v)
                          content #(->> % :xs->c vals (reduce gcd))
                          ku (content u1)
                          kv (content v1)
                          pu (poly-map #(evenly-divide % ku) u1)
                          pv (poly-map #(evenly-divide % kv) v1)
                          d (gcd ku kv)]
                      (loop [u pu
                             v pv]
                        (let [[_ r _] (divide u v {:pseudo true})]
                          (cond (v/nullity? r) (raise-arity (poly-map #(g/* d %) v))
                                (zero? (degree r)) (raise-arity (make-constant 1 d))
                                :else (let [cr (content r)]
                                        (recur v (poly-map #(evenly-divide % cr) r))))))))))

(defn expt
  "Raise the polynomial p to the (integer) power n. Of course, n
  is a polynomial, so it must be a constant integer polynomial."
  [p n]
  (let [e (constant? n)]
    (when-not (and (integer? e) (>= e 0))
      (throw (ArithmeticException.
              (str "can't raise poly to " n))))
    (cond (g/one? p) p
          (g/zero? p) (if (zero? e)
                        (throw (ArithmeticException. "poly 0^0"))
                        p)
          (zero? e) (make-constant (:arity p) 1)
          :else (loop [x p c e a (make-constant (:arity p) 1)]
                  (if (zero? c) a
                      (if (even? c)
                        (recur (mul x x) (quot c 2) a)
                        (recur x (dec c) (mul x a))))))))

(defn expression->
  "Convert an expression into Flat Polynomial canonical form. The
  expression should be an unwrapped expression, i.e., not an instance
  of the Expression type, nor should subexpressions contain type
  information. This kind of simplification proceeds purely
  symbolically over the known Flat Polynomial operations; other
  operations outside the arithmetic available in polynomials over
  commutative rings should be factored out by an expression analyzer
  before we get here. The result is a Polynomial object representing
  the polynomial structure of the input over the unknowns."
  [expr cont]
  (let [expression-vars (sort (set/difference (x/variables-in expr) operators-known))
        arity (count expression-vars)
        new-bindings (zipmap expression-vars (new-variables arity))
        environment (into operator-table new-bindings)
        transformer (x/walk-expression environment #(make-constant arity %))]
    (-> expr transformer (cont expression-vars))))

(defn ->expression
  "This is the output stage of Flat Polynomial canonical form simplification.
  The input is a Polynomial object, and the output is an expression
  representing the evaluation of that polynomial over the
  indeterminates extracted from the expression at the start of this
  process."
  [^Polynomial p vars]
  ;; odd: this (i.e., (symbol? p))only happens in the case of
  ;; something like (expt 'x 'y), where we can't treat it as a known
  ;; expression because 'y is not an integer. Handling it here is easy
  ;; enough, but it seems like an odd special case and perhaps should
  ;; be treated at the level above.
  (if (symbol? p) p
      (reduce
       sym/add 0
       (map (fn [[xs c]]
              (sym/mul c
                       (reduce sym/mul 1 (map (fn [exponent var]
                                                (sym/expt var exponent))
                                              xs vars))))
            (->> p :xs->c (sort-by exponents #(monomial-order %2 %1)))))))

;; The operator-table represents the operations that can be understood
;; from the point of view of a polynomial over a commutative ring. The
;; functions take polynomial inputs and return
;; polynomials.

(def ^:private operator-table
  {'+ #(reduce add %&)
   '- (fn [arg & args]
        (if (some? args) (sub arg (reduce add args)) (negate arg)))
   '* #(reduce mul %&)
   'negate negate
   'expt expt
   'square #(mul % %)
   'cube #(mul % (mul % %))
   ;;`'g/gcd gcd
   })

(def operators-known (set (keys operator-table)))

(defmethod g/add [::polynomial ::polynomial] [a b] (add a b))
(defmethod g/mul [::polynomial ::polynomial] [a b] (mul a b))
(defmethod g/sub [::polynomial ::polynomial] [a b] (sub a b))
(defmethod g/div [::polynomial ::polynomial] [a b] (evenly-divide a b))  ;; reconsider: is this right??? XXX
(defmethod g/negate ::polynomial [a] (negate a))
