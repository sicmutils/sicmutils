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
  (:import (clojure.lang PersistentTreeMap BigInt Ratio IFn)
           (com.google.common.base Stopwatch)
           (java.util.concurrent TimeUnit TimeoutException))
  (:require [clojure.set :as set]
            [clojure.tools.logging :as log]
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

;;
;; Polynomials
;;

(declare evaluate)

(defrecord Polynomial [^long arity ^PersistentTreeMap xs->c]
  v/Value
  (nullity? [_] (-> xs->c .count zero?))
  (numerical? [_] false)
  (zero-like [_] (Polynomial. arity empty-coefficients))
  (one-like [o] (make-constant arity (v/one-like (coefficient (first xs->c)))))
  (unity? [_] (and (= (.count xs->c) 1)
                   (let [[xs c] (first xs->c)]
                     (and (every? zero? xs)
                          (v/unity? c)))))
  (kind [_] ::polynomial)
  IFn
  (invoke [p] (evaluate p))
  (invoke [p x] (evaluate p x))
  (invoke [p x y] (evaluate p x y))
  (invoke [p x y z] (evaluate p x y z))
  (invoke [p w x y z] (evaluate p w x y z))
  (invoke [p v w x y z] (evaluate p v w x y z))
  (applyTo [p xs] (apply (partial evaluate p) xs))
  Object
  (toString [_]
    (str "("
         (clojure.string/join ";"
                              (for [[k v] xs->c]
                                (str v "*" (clojure.string/join "," k))))
         ")")))

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
   {:pre [(->> xc-pairs (map first) (every? vector?))]}
   (Polynomial. arity (into empty-coefficients
                            (for [p xc-pairs :when (not (g/zero? (second p)))]
                              p))))
  ([dense-coefficients]
   (make 1 (zipmap (map vector (iterate inc 0)) dense-coefficients))))

(defn ^:private lead-term
  "Return the leading (i.e., highest degree) term of the polynomial
  p. The return value is [exponents coefficient]."
  [p]
  (-> p :xs->c rseq first))

(defn degree
  [p]
  (if (v/nullity? p) -1
      (->> p lead-term exponents (reduce +))))

(defn constant?
  "If p is a constant polynomial, return that constant, else nil"
  [^Polynomial p]
  (let [xs->c (:xs->c p)]
    (cond (nil? xs->c) nil
          (empty? xs->c) 0
          (and (= (count xs->c) 1)
               (every? zero? (exponents (first xs->c)))) (coefficient (first xs->c)))))

(defn coefficients
  [^Polynomial p]
  (-> p :xs->c vals))

(defn constant-term
  "Return the constant term of the polynomial."
  [{:keys [arity xs->c]}]
  (or (xs->c (vec (repeat arity 0))) 0))

(defn check-same-arity [p q]
  (let [ap (:arity p)
        aq (:arity q)]
    (cond (= ap aq) ap
          :else (throw (ArithmeticException. "mismatched polynomial arity")))))

(defn map-coefficients
  "Map the function f over the coefficients of p, returning a new Polynomial."
  [f {:keys [arity xs->c]}]
  (let [m (into empty-coefficients (for [[xs c] xs->c
                                         :let [fc (f c)]
                                         :when (not (g/zero? fc))]
                                     [xs fc]))]
    (Polynomial. arity m)))

(defn ^:private poly-merge
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

(def negate (partial map-coefficients g/negate))

(defn make-constant
  "Return a constant polynomial of the given arity."
  [arity c]
  (Polynomial. arity (if (g/zero? c) empty-coefficients
                         (conj empty-coefficients [(vec (repeat arity 0)) c]))))

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

(defn ^:private add-denormal
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
    (cond (g/zero? p) p
          (g/zero? q) q
          (g/one? p) q
          (g/one? q) p
          :else (let [a (check-same-arity p q)]
                  (make a (reduce add-denormal
                                  empty-coefficients
                                  (for [[xp cp] (:xs->c p)
                                        [xq cq] (:xs->c q)]
                                    [(mapv + xp xq) (g/* cp cq)])))))))

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
  "The opposite of lower-arity."
  [p]
  {:pre [(instance? Polynomial p)
         (= (:arity p) 1)]}
  (let [terms (for [[x q] (:xs->c p)
                    [ys c] (:xs->c q)]
                [(into x ys) c])]
    (make (inc (:arity (coefficient (lead-term p)))) terms)))

(defn ^:private evaluate-1
  "Evaluates a univariate polynomial p at x."
  [p x]
  (loop [xs->c (:xs->c p)
         result 0
         x**e 1
         e 0]
    (if xs->c
      (let [[[e'] c] (first xs->c)
            x**e' (g/* x**e (g/expt x (- e' e)))]
        (recur (next xs->c)
               (g/+ result (g/* c x**e'))
               x**e'
               e'))
      result)))

(defn ^:private evaluate
  "Evaluates a multivariate polynomial p at xs. Partial application
  is supported. Supplying too many arguments will throw."
  [p & xs]
  (if-not xs p
          (if (= (:arity p) 1)
            (do (when-not (= (count xs) 1)
                  (throw (IllegalArgumentException. "too many arguments for polynomial")))
                (evaluate-1 p (first xs)))
            (apply evaluate ((lower-arity p) (first xs)) (next xs)))))

(declare gcd)

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
  "Compute the pseudo-remainder of p by q. Fractions won't appear in
  the result; instead the divisor is multiplied by the leading
  coefficient of the dividend before quotient terms are generated so
  that division will not result in fractions. Only the remainder is
  returned, together with the integerizing factor needed to make this
  happen. Similar in spirit to Knuth's algorithm 4.6.1R, except we
  don't multiply the remainder through during gaps in the
  remainder. Since you don't know up front how many times the
  integerizing multiplication will be done, we also return the number
  m for which m * u = q * v + r."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (cond (v/nullity? v) (throw (IllegalArgumentException. "internal polynomial division by zero"))
        (v/nullity? u) [u 1]
        (v/unity? v) [(v/zero-like u) 1]
        :else (let [arity (check-same-arity u v)
                    [vn-exponents vn-coefficient] (lead-term v)
                    *vn (fn [p] (map-coefficients #(g/* % vn-coefficient) p))
                    good? (fn [residues]
                            (and (not-empty residues)
                                 (every? (complement neg?) residues)))]
                (if (zero? arity)
                  (throw (IllegalArgumentException. "can't compute pseudo-remainder of zero-arity polynomial"))
                  (loop [remainder u
                         multiplier (v/one-like vn-coefficient)]
                    ;; find a term in the remainder into which the
                    ;; lead term of the divisor can be divided.
                    (let [remainder' (*vn remainder)
                          [r-exponents r-coefficient] (lead-term remainder')
                          residues (mapv - r-exponents vn-exponents)]
                      (if (good? residues)
                        (let [new-coefficient (g/exact-div r-coefficient vn-coefficient)
                              new-term (make arity [[residues new-coefficient]])]
                          (recur (sub remainder' (mul new-term v))
                                 (g/* multiplier vn-coefficient)))
                        [remainder multiplier])))))))

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

(def ^:dynamic *poly-gcd-time-limit* [1000 TimeUnit/MILLISECONDS])
(def ^:dynamic *poly-gcd-cache-enable* true)
(def ^:private ^:dynamic *poly-gcd-bail-out* (fn []))
(def ^:private gcd-memo (atom {}))
(def ^:private gcd-cache-hit (atom 0))
(def ^:private gcd-cache-miss (atom 0))

(defn gcd-stats []
  (when (> (count @gcd-memo) 0)
    (log/info (format "GCD cache hit rate %.2f%% (%d entries)"
                      (* 100. (/ @gcd-cache-hit (+ @gcd-cache-hit @gcd-cache-miss)))
                      (count @gcd-memo)))))

(defn ^:private with-content-removed
  "For multivariate polynomials. u and v are considered here as
  univariate polynomials with polynomial coefficients. Using the
  supplied gcd function, the content of u and v is divided out and the
  primitive parts are supplied to the continuation, along with two
  callbacks: one for success (which will multiply back content into
  the result of that callback) and one for failure (in which case the
  content itself is returned.)"
  [u v gcd continue]
  (let [content #(->> % :xs->c vals (reduce gcd))
        ku (content u)
        kv (content v)
        pu (map-coefficients #(g/exact-div % ku) u)
        pv (map-coefficients #(g/exact-div % kv) v)
        d (gcd ku kv)]
    (continue pu pv
              (fn [v] (map-coefficients #(g/* d %) v))
              (fn [] (make-constant 1 d)))))

(defn ^:private with-lower-arity
  [u v continue]
  (raise-arity (continue (lower-arity u) (lower-arity v))))

(defn ^:private euclid-inner-loop
  [coefficient-gcd]
  (let [content #(->> % :xs->c vals (reduce coefficient-gcd))]
    (fn [u v succeed fail]
      (loop [u u v v]
        (*poly-gcd-bail-out*)
        (let [[r _] (pseudo-remainder u v)]
          (cond (v/nullity? r) (succeed v)
                ;(zero? (degree r)) (fail)
                :else (let [kr (content r)]
                        (recur v (map-coefficients #(g/exact-div % kr) r)))))))))

(def ^:private univariate-euclid-inner-loop
  (euclid-inner-loop euclid/gcd))

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
    (v/unity? u) u
    (v/unity? v) v
    (= u v) u
    :else (with-content-removed u v euclid/gcd univariate-euclid-inner-loop)))

(declare ^:private inner-gcd-fn)

(defn ^:private inner-gcd
  "gcd is just a wrapper for this function, which does the real work
  of computing a polynomial gcd. Delegates to gcd1 for univariate
  polynomials."
  [u v]
  (let [arity (check-same-arity u v)]
    (if-let [g (and *poly-gcd-cache-enable* (@gcd-memo [u v]))]
      (do (swap! gcd-cache-hit inc) g)
      (let [g (cond
                (v/nullity? u) v
                (v/nullity? v) u
                (v/unity? u) u
                (v/unity? v) v
                (= u v) u
                (= arity 1) (gcd1 u v)
                :else (do
                        (*poly-gcd-bail-out*)
                        (with-lower-arity u v
                          (fn [u v]
                            (with-content-removed u v inner-gcd
                              (euclid-inner-loop inner-gcd))))))]
        (when *poly-gcd-cache-enable*
          (swap! gcd-cache-miss inc)
          (swap! gcd-memo assoc [u v] g))
        g))))

(defn gcd
  "Knuth's algorithm 4.6.1E.
  This can take a long time, unfortunately, and so we bail if it seems to
  be taking too long."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [clock (Stopwatch/createStarted)
        arity (check-same-arity u v)
        g (if (= arity 1)
            (gcd1 u v)
            (binding [*poly-gcd-bail-out* #(when (> (.elapsed clock (second *poly-gcd-time-limit*))
                                                    (first *poly-gcd-time-limit*))
                                             (throw (TimeoutException.
                                                     (str "Took too long to find multivariate polynomial GCD: "
                                                          clock))))]
              (inner-gcd u v)))]
    (if (-> g lead-term coefficient g/negative?)
      (negate g)
      g)
    ;;(log/info (str "gcd took: " clock " arity " (:arity u) " degrees " (degree u) " " (degree v) " : " (degree g)))
    ))

(defn expt
  "Raise the polynomial p to the (integer) power n."
  [p n]
  (when-not (and (integer? n) (>= n 0))
    (throw (ArithmeticException.
            (str "can't raise poly to " n))))
  (cond (g/one? p) p
        (g/zero? p) (if (zero? n)
                      (throw (ArithmeticException. "poly 0^0"))
                      p)
        (zero? n) (make-constant (:arity p) 1)
        :else (loop [x p c n a (make-constant (:arity p) 1)]
                (if (zero? c) a
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
  before we get here. The result is a Polynomial object representing
  the polynomial structure of the input over the unknowns."
  [expr cont]
  (let [expression-vars (sort (set/difference (x/variables-in expr) operators-known))
        arity (count expression-vars)
        new-bindings (zipmap expression-vars (new-variables arity))
        environment (into operator-table new-bindings)
        transformer (x/walk-expression environment)]
    (-> expr transformer (cont expression-vars))))

(defn ->expression
  "This is the output stage of Flat Polynomial canonical form simplification.
  The input is a Polynomial object, and the output is an expression
  representing the evaluation of that polynomial over the
  indeterminates extracted from the expression at the start of this
  process."
  [^Polynomial p vars]
  (if (instance? Polynomial p)
    (reduce
     sym/add 0
     (map (fn [[xs c]]
            (sym/mul c
                     (reduce sym/mul 1 (map (fn [exponent var]
                                              (sym/expt var exponent))
                                            xs vars))))
          (->> p :xs->c (sort-by exponents #(monomial-order %2 %1)))))
    p))

;; The operator-table represents the operations that can be understood
;; from the point of view of a polynomial over a commutative ring. The
;; functions take polynomial inputs and return
;; polynomials.

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

(def operators-known (set (keys operator-table)))

(defmethod g/add [::polynomial ::polynomial] [a b] (add a b))
(defmethod g/add [Number ::polynomial] [n p] (add (make-constant (:arity p) n) p))
(defmethod g/add [::polynomial Number] [p n] (add p (make-constant (:arity p) n)))
(defmethod g/mul [::polynomial ::polynomial] [a b] (mul a b))
(defmethod g/mul [Number ::polynomial] [c p] (map-coefficients #(g/* c %) p))
(defmethod g/mul [::polynomial Number] [p c] (map-coefficients #(g/* % c) p))
(defmethod g/sub [::polynomial ::polynomial] [a b] (sub a b))
(defmethod g/sub [::polynomial Number] [p c] (sub p (make-constant (:arity p) c)))
(defmethod g/sub [Number ::polynomial] [c p] (sub (make-constant (:arity p) c) p))
(defmethod g/div [::polynomial Number] [p c] (map-coefficients #(g/divide % c) p))
(defmethod g/expt [::polynomial Integer] [b x] (expt b x))
(defmethod g/expt [::polynomial Long] [b x] (expt b x))
(defmethod g/exact-div [::polynomial ::polynomial] [p q] (evenly-divide p q))
(defmethod g/negate ::polynomial [a] (negate a))
