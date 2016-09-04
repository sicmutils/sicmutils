;
; Copyright (C) 2016 Colin Smith.
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

(ns sicmutils.polynomial-gcd
  (:import (com.google.common.base Stopwatch)
           (sicmutils.polynomial Polynomial)
           (java.util.concurrent TimeUnit TimeoutException))
  (:require [clojure.tools.logging :as log]
            [clojure.string]
            [sicmutils
             [value :as v]
             [generic :as g]
             [polynomial :refer :all]]))

(def ^:dynamic *poly-gcd-time-limit* [1000 TimeUnit/MILLISECONDS])
(def ^:dynamic *poly-gcd-cache-enable* true)
(def ^:dynamic *poly-gcd-debug* false)
(def ^:private ^:dynamic *poly-gcd-bail-out* (fn []))
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
                      (count @gcd-memo))))
  (log/info (format "GCD triv %d prob %d mono %d"
                    @gcd-trivial-constant
                    @gcd-probabilistic-unit
                    @gcd-monomials)))

(defn ^:private reduce-until
  "Returns a reducer over the function f which will exit early
  if done? becomes true."
  [done? f]
  (let [rf (fn [a b]
             (let [c (f a b)]
               (if (done? c) (reduced c) c)))]
    #(reduce rf %)))

(defn ^:private native-gcd
  [a b]
  (.gcd (biginteger a) (biginteger b)))

(def ^:private primitive-gcd
  "A function which will return the gcd of a sequence of numbers."
  (reduce-until #(= % 1) native-gcd))

(defn ^:private with-content-removed
  "For multivariate polynomials. u and v are considered here as
  univariate polynomials with polynomial coefficients. Using the
  supplied gcd function, the content of u and v is divided out and the
  primitive parts are supplied to the continuation, the result of which
  has the content reattached and is returned."
  [gcd u v continue]
  (let [gcd-reducer (reduce-until v/unity? gcd)
        content #(-> % coefficients gcd-reducer)
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
  (let [content #(->> % coefficients (reduce coefficient-gcd))]
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
  "We're a little skeptical of this: why doesn't GJS use prime numbers here?
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
              u:xs (evaluate u xs)
              v:xs (evaluate v xs)
              g (native-gcd u:xs v:xs)]
          (if (= g 1)
            (recur (inc n))
            (do (when *poly-gcd-debug*
                  (log/info (str "prob. test failed in " sw)))
                false)))))))

(defn ^:private with-probabilistic-check
  [u v continue]
  (if (and
                                        ;false  ;; XXX
       (> (:arity u) 1)
       (probabilistic-unit-gcd u v))
    (do (swap! gcd-probabilistic-unit inc) (v/one-like u))
    (continue u v)))

(defn ^:private joint-quotient
  "If d evenly divides both u and v, then [u/d, v/d, d], else nil."
  [u v d]
  (let [[q1 r1] (divide u d)
        [q2 r2] (divide v d)]
    (if (and (v/nullity? r1) (v/nullity? r2))
      [q1 q2 d])))

(defn ^:private with-easy-factors-removed
  "Try to depress the polynomials u, v by finding a common factor of
  the form x ± 1 for each variable. NB: this turned out to be a
  disastrous lose for some polynomials. Not obvious why, but I'm
  leaving the code here for the time being."
  [u v continue]
  (let [a (:arity u)
        candidates (for [i (range a)
                         k [-1 1]]
                     (make a [[(mapv #(if (= % i) 1 0) (range a)) k]]))
        joint-quotients (map #(joint-quotient u v %) candidates)]
    (let [[u' v' d] (some identity joint-quotients)]
      (if u'
        (mul d (with-easy-factors-removed u' v' continue))
        (continue u v)))))

(defn ^:private with-trivial-constant-gcd-check
  "We consider the maximum exponent found for each variable in any
  term of each polynomial. A nontrivial GCD would have to fit in this
  exponent limit for both polynomials. This is basically a test for a
  kind of disjointness of the variables. If this happens we just
  return the constant gcd and do not invoke the continuation."
  [u v continue]
  (let [umax (reduce #(mapv max %1 %2) (map exponents (:xs->c u)))
        vmax (reduce #(mapv max %1 %2) (map exponents (:xs->c v)))
        maxd (mapv min umax vmax)]
    (if (every? zero? maxd)
      (do
        (swap! gcd-trivial-constant inc)
        (->> (concat (coefficients u) (coefficients v))
             primitive-gcd
             (make-constant (:arity u))))
      (continue u v))))

(defn ^:private sort->permutations
  "Given a vector, returns a permutation function which would
  sort that vector, and the inverse permutation. Each of these
  functions expects a vector and returns one."
  [xs]
  (let [n (count xs)
        order (vec (sort-by xs (range n)))
        reverse-order (vec (sort-by order (range n)))]
    [#(mapv % order)
     #(mapv % reverse-order)]))

(defn ^:private with-optimized-variable-order
  "Rearrange the variables in u and v to make GCD go faster.
  Calls the continuation with the rearranged polynomials.  Undoes the
  rearrangement on return. Variables are sorted by increasing degree.
  Discussed in 'Evaluation of the Heuristic Polynomial GCD', by Liao
  and Fateman [1995]."
  [u v continue]
  (let [xs (reduce #(mapv max %1 %2) (concat (map exponents (:xs->c u))
                                             (map exponents (:xs->c v))))
        [sorter unsorter] (sort->permutations xs)]
    (map-exponents unsorter
                   (continue (map-exponents sorter u)
                             (map-exponents sorter v)))))

(def ^:private univariate-euclid-inner-loop
  (euclid-inner-loop native-gcd))

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
    :else (with-content-removed native-gcd u v univariate-euclid-inner-loop)))

(defn ^:private monomial-gcd
  "Computing the GCD is easy if one of the polynomials is a monomial.
  The monomial is the first argument."
  [m p]
  {:pre [(= (count (:xs->c m)) 1)]}
  (let [[mxs mc] (-> m :xs->c first)
        xs (reduce #(mapv min %1 %2) mxs (->> p :xs->c (map exponents)))
        c (primitive-gcd (cons mc (coefficients p)))]
    (swap! gcd-monomials inc)
    (make (:arity m) [[xs c]])))

(defn ^:private println-indented
  [level & args]
  (apply println (apply str (repeat level "  ")) args))

(defmacro ^:private dbg
  [level where & xs]
  `(when *poly-gcd-debug*
     (println-indented ~level ~where ~level ~@(map #(list 'str %) xs))))

(defmacro gcd-continuation-chain
  "Takes two polynomials and a chain of functions. Each function, except
  the last, should have signature [p q k] where p and q are polynomials
  and k is a continuation of the same type. The last function should have
  signature [p q], without a continuation argument, since there's nowhere
  to go from there."
  [u v & fs]
  (condp < (count fs)
    2 `(~(first fs) ~u ~v
        (fn [u# v#] (gcd-continuation-chain u# v# ~@(next fs))))
    1 `(~(first fs) ~u ~v ~(second fs))
    0 `(~(first fs) ~u ~v)))

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
                (monomial? u) (monomial-gcd u v)
                (monomial? v) (monomial-gcd v u)
                :else (let [next-gcd #(inner-gcd (inc level) %1 %2)
                            content-remover #(with-content-removed next-gcd %1 %2 %3)]
                        (*poly-gcd-bail-out*)
                        (gcd-continuation-chain u v
                                                with-lower-arity
                                                content-remover
                                                #((euclid-inner-loop next-gcd) %1 %2))))]
        (when *poly-gcd-cache-enable*
          (swap! gcd-cache-miss inc)
          (swap! gcd-memo assoc [u v] g))
        (dbg level "<-" g)
        g))))

(def ^:private integral?
  "A function returning true if the argument is exact but not a ratio.
  Polynomials must have such coefficients if we are to find GCDs for them.
  Note that a polynomial with integral coefficients is integral."
  #(and (v/exact? %) (not (ratio? %))))

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
      (not (and (every? integral? (coefficients u))
                (every? integral? (coefficients v)))) (v/one-like u)
      (v/nullity? u) v
      (v/nullity? v) u
      (v/unity? u) u
      (v/unity? v) v
      (= u v) u
      (= arity 1) (abs (gcd1 u v))
      :else (binding [*poly-gcd-bail-out* (fn [] (when (> (.elapsed clock (second *poly-gcd-time-limit*))
                                                          (first *poly-gcd-time-limit*))
                                                   (log/warn (format "long polynomial GCD: %s" clock))
                                                   (throw (TimeoutException.
                                                           (str "Took too long to find multivariate polynomial GCD: "
                                                                clock)))))]
              (abs (gcd-continuation-chain u v
                                           with-trivial-constant-gcd-check
                                           ;;with-easy-factors-removed (loses)
                                           with-probabilistic-check
                                           with-optimized-variable-order
                                           #(inner-gcd 0 %1 %2)))))))

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
