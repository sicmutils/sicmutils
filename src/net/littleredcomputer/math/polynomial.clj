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
    (if (= xd yd) (lex-order xs ys) (- xd yd))))

(defn graded-reverse-lex-order
  ""
  [xs ys]
  {:pre (= (count xs) (count ys))}
  (let [xd (monomial-degree xs)
        yd (monomial-degree ys)]
    (if (= xd yd) (compare (vec (rseq ys)) (vec (rseq xs))) (- xd yd))))

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
  (applyTo [p xs] (apply evaluate p xs))
  Object
  (toString [_]
    (let [n 10
          c (count xs->c)]
      (str "("
           (clojure.string/join ";"
                                (take n (for [[k v] xs->c]
                                           (str v "*" (clojure.string/join "," k)))))
           (if (> c n) (format " ...and %d more terms" (- c n)))
           ")"))))

(def ^:dynamic *poly-require-euclidean-coefficients* true)

(defn ^:private euclidean?
  "True if x is a member of a Euclidean domain. For us, this means any
  kind of integer, or any kind of polynomial. This isn't airtight, as
  we would allow a polynomial with non-Euclidean coefficients to slip
  past us here. The real motivation is not mathematical purity but
  debugging.  For the use of the polynomial library as the engine of
  algebraic simplification, we don't want polynomials with rational
  coefficients to be created, so we attempt to block it with this
  function. Rational coefficients should instead be handled with
  rational functions of polynomials with integer coefficients."
  [x]
  (or (integer? x)
      (instance? Polynomial x)))

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
   (when (and *poly-require-euclidean-coefficients*
              (not (every? euclidean? (map second xc-pairs))))
     (throw (IllegalArgumentException. (str "tried to make a funny polynomial " xc-pairs))))
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

(defn ^:private monomial?
  [p]
  (-> p :xs->c count (= 1)))

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

(defn raise-arity
  "The opposite of lower-arity."
  [p]
  {:pre [(instance? Polynomial p)
         (= (:arity p) 1)]}
  (let [terms (for [[x q] (:xs->c p)
                    [ys c] (:xs->c q)]
                [(into x ys) c])]
    (make (inc (:arity (coefficient (lead-term p)))) terms)))

(defn lower-arity
  "Given a polynomial of arity A > 1, return an equivalent polynomial
  of arity 1 whose coefficients are polynomials of arity A-1."
  [p]
  {:pre [(instance? Polynomial p)
         (> (:arity p) 1)]}
  ;; XXX observation:
  ;; XXX we often create polynomials of "one lower arity"
  ;; which are EFFECTIVELY UNIVARIATE. When this happens,
  ;; we should notice.
  ;; (but univariate in which variable? is it really that
  ;; common that it's the first one?)
  (let [A (:arity p)]
    (->> p
         :xs->c
         (group-by #(-> % exponents first))
         (map (fn [[x cs]]
                [[x] (make (dec A) (for [[xs c] cs]
                                     [(subvec xs 1) c]))]))
         (make 1))))

(defn ^:private evaluate-1
  "Evaluates a univariate polynomial p at x."
  [p x]
  (loop [xs->c (:xs->c p)
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

(defn ^:private evaluate
  "Evaluates a multivariate polynomial p at xs. Partial application
  is supported. Supplying too many arguments will throw."
  [p & xs]
  (cond (not xs) p
        (not (instance? Polynomial p)) p
        (= (:arity p) 1) (do (when-not (= (count xs) 1)
                               (throw (IllegalArgumentException. "too many arguments for polynomial")))
                             (evaluate-1 p (first xs)))
        :else (apply evaluate ((lower-arity p) (first xs)) (next xs))))

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
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (when (v/nullity? v)
    (throw (IllegalArgumentException. "internal polynomial division by zero")))
  (when (not (= 1 (:arity u) (:arity v)))
    (throw (IllegalArgumentException. "pseudo remainder of poly arity != 1")))

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
                      (mul v (Polynomial. a (assoc empty-coefficients [(- m n)] c))))
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

(def ^:dynamic *poly-gcd-time-limit* [1000 TimeUnit/MILLISECONDS])
(def ^:dynamic *poly-gcd-cache-enable* true)
(def ^:private ^:dynamic *poly-gcd-bail-out* (fn []))
(def ^:dynamic *poly-gcd-debug* false)
(def ^:private gcd-memo (atom {}))
(def ^:private gcd-cache-hit (atom 0))
(def ^:private gcd-cache-miss (atom 0))
(def ^:private gcd-trivial-constant (atom 0))
(def ^:private gcd-probabilistic-unit (atom 0))
(def ^:private gcd-monomials (atom 0))

(defn gcd-stats []
  (when (> (count @gcd-memo) 0)
    (log/info (format "GCD cache hit rate %.2f%% (%d entries)"
                      (* 100. (/ @gcd-cache-hit (+ @gcd-cache-hit @gcd-cache-miss)))
                      (count @gcd-memo)))
    (log/info (format "GCD triv %d prob %d mono %d"
                      @gcd-trivial-constant
                      @gcd-probabilistic-unit
                      @gcd-monomials))))

(defn ^:private reduce-until
  "Returns a reducer over the function f which will exit early
  if done? becomes true."
  [done? f]
  (let [rf (fn [a b]
             (let [c (f a b)]
               (if (done? c) (reduced c) c)))]
    #(reduce rf %)))

(defn ^:private with-content-removed
  "For multivariate polynomials. u and v are considered here as
  univariate polynomials with polynomial coefficients. Using the
  supplied gcd function, the content of u and v is divided out and the
  primitive parts are supplied to the continuation, the result of which
  has the content reattached and is returned."
  [u v gcd continue]
  (let [gcd-reducer (reduce-until v/unity? gcd)
        content #(-> % :xs->c vals gcd-reducer)
        ku (content u)
        kv (content v)
        pu (map-coefficients #(g/exact-div % ku) u)
        pv (map-coefficients #(g/exact-div % kv) v)
        d (gcd ku kv)]
    (map-coefficients #(g/* d %) (continue pu pv))))

(defn ^:private with-lower-arity
  [u v continue]
  (raise-arity (continue (lower-arity u) (lower-arity v))))

(defn ^:private euclid-inner-loop
  [coefficient-gcd]
  (let [content #(->> % :xs->c vals (reduce coefficient-gcd))]
    (fn [u v]
      (loop [u u v v]
        (*poly-gcd-bail-out*)
        (let [[r _] (pseudo-remainder u v)]
          (if (v/nullity? r) v
              (let [kr (content r)]
                (recur v (map-coefficients #(g/exact-div % kr) r)))))))))

(def ^:private prime-collection
  "The four-digit primes"
  [1009   1013
   1019   1021   1031   1033   1039   1049   1051   1061   1063   1069
   1087   1091   1093   1097   1103   1109   1117   1123   1129   1151
   1153   1163   1171   1181   1187   1193   1201   1213   1217   1223
   1229   1231   1237   1249   1259   1277   1279   1283   1289   1291
   1297   1301   1303   1307   1319   1321   1327   1361   1367   1373
   1381   1399   1409   1423   1427   1429   1433   1439   1447   1451
   1453   1459   1471   1481   1483   1487   1489   1493   1499   1511
   1523   1531   1543   1549   1553   1559   1567   1571   1579   1583
   1597   1601   1607   1609   1613   1619   1621   1627   1637   1657
   1663   1667   1669   1693   1697   1699   1709   1721   1723   1733
   1741   1747   1753   1759   1777   1783   1787   1789   1801   1811
   1823   1831   1847   1861   1867   1871   1873   1877   1879   1889
   1901   1907   1913   1931   1933   1949   1951   1973   1979   1987
   1993   1997   1999   2003   2011   2017   2027   2029   2039   2053
   2063   2069   2081   2083   2087   2089   2099   2111   2113   2129
   2131   2137   2141   2143   2153   2161   2179   2203   2207   2213
   2221   2237   2239   2243   2251   2267   2269   2273   2281   2287
   2293   2297   2309   2311   2333   2339   2341   2347   2351   2357
   2371   2377   2381   2383   2389   2393   2399   2411   2417   2423
   2437   2441   2447   2459   2467   2473   2477   2503   2521   2531
   2539   2543   2549   2551   2557   2579   2591   2593   2609   2617
   2621   2633   2647   2657   2659   2663   2671   2677   2683   2687
   2689   2693   2699   2707   2711   2713   2719   2729   2731   2741
   2749   2753   2767   2777   2789   2791   2797   2801   2803   2819
   2833   2837   2843   2851   2857   2861   2879   2887   2897   2903
   2909   2917   2927   2939   2953   2957   2963   2969   2971   2999
   3001   3011   3019   3023   3037   3041   3049   3061   3067   3079
   3083   3089   3109   3119   3121   3137   3163   3167   3169   3181
   3187   3191   3203   3209   3217   3221   3229   3251   3253   3257
   3259   3271   3299   3301   3307   3313   3319   3323   3329   3331
   3343   3347   3359   3361   3371   3373   3389   3391   3407   3413
   3433   3449   3457   3461   3463   3467   3469   3491   3499   3511
   3517   3527   3529   3533   3539   3541   3547   3557   3559   3571
   3581   3583   3593   3607   3613   3617   3623   3631   3637   3643
   3659   3671   3673   3677   3691   3697   3701   3709   3719   3727
   3733   3739   3761   3767   3769   3779   3793   3797   3803   3821
   3823   3833   3847   3851   3853   3863   3877   3881   3889   3907
   3911   3917   3919   3923   3929   3931   3943   3947   3967   3989
   4001   4003   4007   4013   4019   4021   4027   4049   4051   4057
   4073   4079   4091   4093   4099   4111   4127   4129   4133   4139
   4153   4157   4159   4177   4201   4211   4217   4219   4229   4231
   4241   4243   4253   4259   4261   4271   4273   4283   4289   4297
   4327   4337   4339   4349   4357   4363   4373   4391   4397   4409
   4421   4423   4441   4447   4451   4457   4463   4481   4483   4493
   4507   4513   4517   4519   4523   4547   4549   4561   4567   4583
   4591   4597   4603   4621   4637   4639   4643   4649   4651   4657
   4663   4673   4679   4691   4703   4721   4723   4729   4733   4751
   4759   4783   4787   4789   4793   4799   4801   4813   4817   4831
   4861   4871   4877   4889   4903   4909   4919   4931   4933   4937
   4943   4951   4957   4967   4969   4973   4987   4993   4999   5003
   5009   5011   5021   5023   5039   5051   5059   5077   5081   5087
   5099   5101   5107   5113   5119   5147   5153   5167   5171   5179
   5189   5197   5209   5227   5231   5233   5237   5261   5273   5279
   5281   5297   5303   5309   5323   5333   5347   5351   5381   5387
   5393   5399   5407   5413   5417   5419   5431   5437   5441   5443
   5449   5471   5477   5479   5483   5501   5503   5507   5519   5521
   5527   5531   5557   5563   5569   5573   5581   5591   5623   5639
   5641   5647   5651   5653   5657   5659   5669   5683   5689   5693
   5701   5711   5717   5737   5741   5743   5749   5779   5783   5791
   5801   5807   5813   5821   5827   5839   5843   5849   5851   5857
   5861   5867   5869   5879   5881   5897   5903   5923   5927   5939
   5953   5981   5987   6007   6011   6029   6037   6043   6047   6053
   6067   6073   6079   6089   6091   6101   6113   6121   6131   6133
   6143   6151   6163   6173   6197   6199   6203   6211   6217   6221
   6229   6247   6257   6263   6269   6271   6277   6287   6299   6301
   6311   6317   6323   6329   6337   6343   6353   6359   6361   6367
   6373   6379   6389   6397   6421   6427   6449   6451   6469   6473
   6481   6491   6521   6529   6547   6551   6553   6563   6569   6571
   6577   6581   6599   6607   6619   6637   6653   6659   6661   6673
   6679   6689   6691   6701   6703   6709   6719   6733   6737   6761
   6763   6779   6781   6791   6793   6803   6823   6827   6829   6833
   6841   6857   6863   6869   6871   6883   6899   6907   6911   6917
   6947   6949   6959   6961   6967   6971   6977   6983   6991   6997
   7001   7013   7019   7027   7039   7043   7057   7069   7079   7103
   7109   7121   7127   7129   7151   7159   7177   7187   7193   7207
   7211   7213   7219   7229   7237   7243   7247   7253   7283   7297
   7307   7309   7321   7331   7333   7349   7351   7369   7393   7411
   7417   7433   7451   7457   7459   7477   7481   7487   7489   7499
   7507   7517   7523   7529   7537   7541   7547   7549   7559   7561
   7573   7577   7583   7589   7591   7603   7607   7621   7639   7643
   7649   7669   7673   7681   7687   7691   7699   7703   7717   7723
   7727   7741   7753   7757   7759   7789   7793   7817   7823   7829
   7841   7853   7867   7873   7877   7879   7883   7901   7907   7919
   7927   7933   7937   7949   7951   7963   7993   8009   8011   8017
   8039   8053   8059   8069   8081   8087   8089   8093   8101   8111
   8117   8123   8147   8161   8167   8171   8179   8191   8209   8219
   8221   8231   8233   8237   8243   8263   8269   8273   8287   8291
   8293   8297   8311   8317   8329   8353   8363   8369   8377   8387
   8389   8419   8423   8429   8431   8443   8447   8461   8467   8501
   8513   8521   8527   8537   8539   8543   8563   8573   8581   8597
   8599   8609   8623   8627   8629   8641   8647   8663   8669   8677
   8681   8689   8693   8699   8707   8713   8719   8731   8737   8741
   8747   8753   8761   8779   8783   8803   8807   8819   8821   8831
   8837   8839   8849   8861   8863   8867   8887   8893   8923   8929
   8933   8941   8951   8963   8969   8971   8999   9001   9007   9011
   9013   9029   9041   9043   9049   9059   9067   9091   9103   9109
   9127   9133   9137   9151   9157   9161   9173   9181   9187   9199
   9203   9209   9221   9227   9239   9241   9257   9277   9281   9283
   9293   9311   9319   9323   9337   9341   9343   9349   9371   9377
   9391   9397   9403   9413   9419   9421   9431   9433   9437   9439
   9461   9463   9467   9473   9479   9491   9497   9511   9521   9533
   9539   9547   9551   9587   9601   9613   9619   9623   9629   9631
   9643   9649   9661   9677   9679   9689   9697   9719   9721   9733
   9739   9743   9749   9767   9769   9781   9787   9791   9803   9811
   9817   9829   9833   9839   9851   9857   9859   9871   9883   9887
   9901   9907   9923   9929   9931   9941   9949   9967   9973 ])

(def ^:private gcd-heuristic-trial-value-max 100000)
(def ^:private gcd-heuristic-trial-count 4)

(defn ^:private gcd-heuristic-random-value
  []
  (prime-collection (rand-int (count prime-collection)))
  #_(let [c (+ 1 (rand-int gcd-heuristic-trial-value-max))]
    (if (even? c) (+ c 1) c)))

(defn probabilistic-unit-gcd
  "Were a little skeptical of this: why doesn't GJS use prime numbers here?
  In our observations, enough even numbers generate spurious content in the
  results. For the present we aren't using it."
  [u v]
  (let [sw (Stopwatch/createStarted)
        a (:arity u)]
    (loop [n 0]
      (if (= n gcd-heuristic-trial-count)
        (do
          (when *poly-gcd-debug*
            (log/info (str "prob. test succeeded in " sw)))
          true)
        (let [xs (repeatedly a gcd-heuristic-random-value)
              u:xs (apply u xs)
              v:xs (apply v xs)
              g (euclid/gcd u:xs v:xs)]
          ;;(log/info (str "try " n " " u " " v " gcdc " " xs " (seq xs) " u@ " u:xs " v@ " v:xs " g " g))
          (if (= g 1)
            (recur (inc n))
            (do (when *poly-gcd-debug*
                  (log/info (str "prob. test failed in " sw)))
                false)))))))

(defn ^:private with-probabilistic-check
  [u v continue one]
  (if (and
       ;false  ;; XXX
       (> (:arity u) 1)
       (probabilistic-unit-gcd u v))
    (do (swap! gcd-probabilistic-unit inc) (one))
    (continue u v)))

(defn ^:private with-trivial-constant-gcd-check
  "We consider the maximum exponent found for each variable in
  any term of each polynomial. A nontrivial GCD would have to fit
  in this exponent limit for both polynomials. This is basically
  a test for a kind of disjointness of the variables. If it is
  impossible for the GCD to be nonconstant, the continuation one
  is invoked with no arguments; otherwise, continue is invoked
  with u and v.

  TODO: This is broken, and so temporarily short-circuited, b/c
  we didn't consider the case where the GCD is a non-one constant!"
  [u v continue one]
  (if true
    (continue u v)

    (let [umax (reduce #(mapv max %1 %2) (keys (:xs->c u)))
         vmax (reduce #(mapv max %1 %2) (keys (:xs->c v)))
         maxd (mapv min umax vmax)]
     (if (every? zero? maxd)
       (do (swap! gcd-trivial-constant inc) (one))
       (continue u v)))))

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

(defn ^:private monomial-gcd
  "TODO doc"
  [u v]
  (let [[uxs uc] (-> u :xs->c first)
        [vxs vc] (-> v :xs->c first)
        xs (mapv min uxs vxs)
        c (euclid/gcd uc vc)]
    (swap! gcd-monomials inc)
    (Polynomial. (:arity u) (assoc empty-coefficients xs c))))

(defn ^:private println-indented
  [level & args]
  (apply println (apply str (repeat level "  ")) args))

(defmacro ^:private dbg
  [level where & xs]
  `(when *poly-gcd-debug*
     (println-indented ~level ~where ~level ~@(map #(list 'str %) xs))))

(defn ^:private inner-gcd
  "gcd is just a wrapper for this function, which does the real work
  of computing a polynomial gcd. Delegates to gcd1 for univariate
  polynomials."
  [level u v]
  (when *poly-gcd-debug*
    (println-indented level "inner-gcd" level (str u) (str v)))
  (let [arity (check-same-arity u v)]
    (if-let [g (and *poly-gcd-cache-enable* (@gcd-memo [u v]))]
      (do (swap! gcd-cache-hit inc) g)
      (let [g (cond
                (= arity 1) (gcd1 u v)
                (v/nullity? u) v
                (v/nullity? v) u
                (v/unity? u) u
                (v/unity? v) v
                (= u v) u
                (and (monomial? u)
                     (monomial? v)) (monomial-gcd u v)
                :else (let [next-gcd #(inner-gcd (inc level) %1 %2)]
                        (*poly-gcd-bail-out*)
                        (with-lower-arity u v
                          (fn [u v]
                            (dbg level "LA" u v)
                            (with-content-removed u v next-gcd
                              (fn [u v] ;; TODO: remember to collapse, if we get rid of debugging
                                (dbg level "WCR" u v)
                                ((euclid-inner-loop next-gcd) u v)))))))]
        (when *poly-gcd-cache-enable*
          (swap! gcd-cache-miss inc)
          (swap! gcd-memo assoc [u v] g))
        (dbg level "<-" u)
        g))))

;; several observations. many of the gcds we find when attempting the troublesome
;; GCD are the case where we have two monomials. This can be done trivially
;; without lowering arity.
;;
;; secondly, we observe that lowering arity often produces constant polynomials.
;; can we get away with just dropping these to scalars, instead of constant
;; polynomials of lower degree?
;;
;; then we could special-case the question of GCD of a polynomial with a
;; basic number. That's just the gcd of the number and the polynomial's
;; integer coefficients.
;;
;; open question: why was dividing out the greatest monomial factor such
;; a lose? it's much cheaper to do that than to find a gcd by going down
;; the arity chain.


(defn abs
  [p]
  (if (-> p lead-term coefficient g/negative?)
    (negate p)
    p))

(defn gcd
  "Knuth's algorithm 4.6.1E.
  This can take a long time, unfortunately, and so we bail if it seems to
  be taking too long."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [clock (Stopwatch/createStarted)
        arity (check-same-arity u v)]
    (cond
      (v/nullity? u) v
      (v/nullity? v) u
      (v/unity? u) u
      (v/unity? v) v
      (= u v) u
      (= arity 1) (abs (gcd1 u v))
      :else (binding [*poly-gcd-bail-out* #(when (> (.elapsed clock (second *poly-gcd-time-limit*))
                                                    (first *poly-gcd-time-limit*))
                                             (println "too long" (str u) (str v))
                                             (throw (TimeoutException.
                                                     (str "Took too long to find multivariate polynomial GCD: "
                                                          clock))))]
              (let [continue (fn [u v] (abs (inner-gcd 0 u v)))
                    fail (fn [] (v/one-like u))]
                (with-trivial-constant-gcd-check u v
                  (fn [u v]
                    (with-probabilistic-check u v continue fail))
                  fail))))))


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

(defn variable-sort-key
  [v]
  (cond (symbol? v) [0 v]
        :else [1 v]))

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
  (let [expression-vars (sort-by variable-sort-key (set/difference (x/variables-in expr) operators-known))
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

(def operators-known (set (keys operator-table)))

(defmethod g/add [::polynomial ::polynomial] [a b] (add a b))
(defmethod g/add [Long ::polynomial] [n p] (add (make-constant (:arity p) n) p))
(defmethod g/add [::polynomial Long] [p n] (add p (make-constant (:arity p) n)))
(defmethod g/mul [::polynomial ::polynomial] [a b] (mul a b))
(defmethod g/mul [Long ::polynomial] [c p] (map-coefficients #(g/* c %) p))
(defmethod g/mul [BigInteger ::polynomial] [c p] (map-coefficients #(g/* c %) p))
(defmethod g/mul [::polynomial BigInteger] [p c] (map-coefficients #(g/* % c) p))
(defmethod g/mul [BigInt ::polynomial] [c p] (map-coefficients #(g/* c %) p))
(defmethod g/mul [::polynomial BigInt] [p c] (map-coefficients #(g/* % c) p))
(defmethod g/mul [::polynomial Long] [p c] (map-coefficients #(g/* % c) p))
(defmethod g/sub [::polynomial ::polynomial] [a b] (sub a b))
(defmethod g/sub [::polynomial Long] [p c] (sub p (make-constant (:arity p) c)))
(defmethod g/sub [Long ::polynomial] [c p] (sub (make-constant (:arity p) c) p))
(defmethod g/expt [::polynomial Integer] [b x] (expt b x))
(defmethod g/expt [::polynomial Long] [b x] (expt b x))
(defmethod g/exact-div [::polynomial ::polynomial] [p q] (evenly-divide p q))
(defmethod g/negate ::polynomial [a] (negate a))
