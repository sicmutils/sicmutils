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

(ns sicmutils.polynomial-gcd
  (:import (com.google.common.base Stopwatch)
           (sicmutils.polynomial Polynomial)
           (java.util.concurrent TimeUnit TimeoutException)
           (org.apache.commons.math3.fraction BigFraction BigFractionField)
           (org.apache.commons.math3.linear Array2DRowFieldMatrix
                                            ArrayFieldVector
                                            FieldDecompositionSolver
                                            FieldLUDecomposition
                                            FieldMatrixChangingVisitor))
  (:require [clojure.tools.logging :as log]
            [clojure.string]
            [clojure.math.numeric-tower :as nt]
            [sicmutils
             [generic :as g]
             [euclid :as euclid]
             [polynomial :refer :all]]))

(def ^:dynamic *poly-gcd-time-limit* [1000 TimeUnit/MILLISECONDS])
(def ^:dynamic *poly-gcd-cache-enable* true)
(def ^:dynamic *poly-gcd-debug* false)
(def ^:private ^:dynamic *poly-gcd-bail-out* (fn []))
(def ^:private gcd-memo (atom {}))
(def ^:private gcd-cache-hit (atom 0))
(def ^:private gcd-cache-miss (atom 0))
(def ^:private gcd-trivial-constant (atom 0))
(def ^:private gcd-monomials (atom 0))

(defn gcd-stats []
  (when (> (count @gcd-memo) 0)
    (log/info (format "GCD cache hit rate %.2f%% (%d entries)"
                      (* 100. (/ @gcd-cache-hit (+ @gcd-cache-hit @gcd-cache-miss)))
                      (count @gcd-memo))))
  (log/info (format "GCD triv %d mono %d"
                    @gcd-trivial-constant
                    @gcd-monomials)))


(defn ^:private reduce-until
  "Returns a reducer over the function f which will exit early
  if done? becomes true."
  [done? f]
  (let [rf #(let [c (f %1 %2)]
              (if (done? c) (reduced c) c))]
    (partial reduce rf)))

(defn ^:private native-gcd
  [a b]
  (if (zero? b) a
      (recur b (mod a b))))

(defn primitive-gcd
  [xs]
  (loop [x (first xs)
         xs (next xs)]
    (if (and xs (not= x 1))
      (recur (native-gcd x (first xs)) (next xs))
      x)))

(defn univariate-content
  [u]
  (primitive-gcd (coefficients u)))

(defn univariate-primitive-part
  [u]
  (let [content (univariate-content u)]
    (map-coefficients #(/ % content) u)))

(defn ^:private with-content-removed
  "For multivariate polynomials. u and v are considered here as
  univariate polynomials with polynomial coefficients. Using the
  supplied gcd function, the content of u and v is divided out and the
  primitive parts are supplied to the continuation, the result of which
  has the content reattached and is returned."
  [gcd u v continue]
  (let [gcd-reducer (reduce-until g/one? gcd)
        content #(-> % coefficients gcd-reducer)
        ku (content u)
        kv (content v)
        pu (map-coefficients #(g/exact-divide % ku) u)
        pv (map-coefficients #(g/exact-divide % kv) v)
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
          (if (polynomial-zero? r) v
              (let [kr (content r)]
                (recur v (map-coefficients #(g/exact-divide % kr) r)))))))))

(defn ^:private with-trivial-constant-gcd-check
  "We consider the maximum exponent found for each variable in any
  term of each polynomial. A nontrivial GCD would have to fit in this
  exponent limit for both polynomials. This is basically a test for a
  kind of disjointness of the variables. If this happens we just
  return the constant gcd and do not invoke the continuation."
  [^Polynomial u ^Polynomial v continue]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [umax (reduce #(mapv max %1 %2) (map exponents (.xs->c u)))
        vmax (reduce #(mapv max %1 %2) (map exponents (.xs->c v)))
        maxd (mapv min umax vmax)]
    (if (every? zero? maxd)
      (do
        (swap! gcd-trivial-constant inc)
        (->> (concat (coefficients u) (coefficients v))
             primitive-gcd
             (make-constant (.arity u))))
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
  [^Polynomial u ^Polynomial v continue]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (let [xs (reduce #(mapv max %1 %2) (concat (map exponents (.xs->c u))
                                             (map exponents (.xs->c v))))
        [sorter unsorter] (sort->permutations xs)]
    (map-exponents unsorter
                   (continue (map-exponents sorter u)
                             (map-exponents sorter v)))))

(defn ^:private monomial-gcd
  "Computing the GCD is easy if one of the polynomials is a monomial.
  The monomial is the first argument."
  [^Polynomial m ^Polynomial p]
  {:pre [(instance? Polynomial m)
         (instance? Polynomial p)
         (= (count (.xs->c m)) 1)]}
  (let [[mxs mc] (-> m .xs->c first)
        xs (reduce #(mapv min %1 %2) mxs (->> p .xs->c (map exponents)))
        c (primitive-gcd (cons mc (coefficients p)))]
    (swap! gcd-monomials inc)
    (make (.arity m) [[xs c]])))

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

(defn univariate-pseudo-remainder
  "Knuth Algorithm 4.6.1R (Pseudo-division of polynomials): Given polynomials
  u, v this algorithm finds polynomials q and r satisfying:
    lc(v)^(m-n-1)u = qv + r, deg(r) < deg(v)."
  [^Polynomial u ^Polynomial v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)
         (not (polynomial-zero? v))
         (= (.arity u) (.arity v) 1)]}
  (let [vn (lead-coefficient v)
        m-n+1 (inc (- (degree u) (degree v)))]
    (second (divide (scale (nt/expt vn m-n+1) u) v))))

(def ^:dynamic gcd-time-data #_(atom []) nil)

(defn ^:private height
  [p]
  (transduce (map nt/abs) max 0 (coefficients p)))

(defn univariate-gcd-subresultant
  [u v]
  (cond
    (polynomial-zero? u) v
    (polynomial-zero? v) u
    :else
    (let [sw (Stopwatch/createStarted)
          return (fn [g]
                   (if gcd-time-data
                     (let [t (.toNanos (.elapsed sw))]
                      (swap! gcd-time-data conj [(degree u) (degree v) (height u) (height v) t])
                      g)
                     g))
          cu (univariate-content u)
          cv (univariate-content v)
          d (primitive-gcd [cu cv])]
      (loop [g 1
             h 1
             u (map-coefficients #(/ % cu) u)
             v (map-coefficients #(/ % cv) v)]
        (let [delta (int (- (degree u) (degree v)))
              r ^Polynomial (univariate-pseudo-remainder u v)]
          (cond
            (polynomial-zero? r) (return (scale d (univariate-primitive-part v)))
            (zero? (degree r)) (return (make-constant 1 d))
            :else (let [q (*' g (nt/expt h delta))
                        u' v
                        v' (Polynomial. 1 (mapv #(vector (first %) (/ (second %) q)) (.xs->c r)))
                        g' (lead-coefficient u')
                        h' (let [gd (nt/expt g' delta)]
                             (case delta
                               0 (*' h gd)
                               1 gd
                               (/ gd (nt/expt h (dec delta)))))]
                    (recur g' h' u' v'))))))))

(defn univariate-modular-gcd
  [p u v]
  (loop [u (polynomial-reduce-mod p u)
         v (polynomial-reduce-mod p v)]
    (let [r (univariate-modular-remainder p u v)]
      (cond
        (polynomial-zero? r) (let [l (lead-coefficient v)]
                               (if (not= l 1)
                                 (let [l' (euclid/modular-inverse p l)]
                                   (map-coefficients #(mod (* % l') p) v))
                                 v))
        (zero? (degree r)) (make-constant 1 1)
        :else (recur v r)))))

(defonce ^:private primes
  [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
   67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
   139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
   223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283,
   293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379,
   383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461,
   463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563,
   569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643,
   647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739,
   743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829,
   839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937,
   941, 947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 1021,
   1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093,
   1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181,
   1187, 1193, 1201, 1213, 1217, 1223])

(defn ^:private random-prime-such-that
  "Returns one of the primes in the above list, such that (f p) is true.
  This is kind of hokey: so long as we have a finite list, there's a
  possibility that we will run out of primes. Would it be better to just
  take the primes in order, and trust that the product will build up
  quickly enough?"
  [f]
  (loop []
    (let [p (nth primes (rand-int (count primes)))]
      (if (f p) p (recur)))))

(defn univariate-gcd-zippel
  "Univariate GCD from R. Zippel, Effective Polynomial Computation, §15.2"
  [F G]
  (let [c (native-gcd (univariate-content F) (univariate-content G))
        F (univariate-primitive-part F)
        G (univariate-primitive-part G)
        u (lead-coefficient F)
        v (lead-coefficient G)
        h (native-gcd u v)
        l (/ (* u v) h)
        r (degree F)
        s (degree G)
        B (inc (* 2 (nt/abs h) (min (* (nt/expt 2 r) (nt/sqrt (inc r)) (height F))
                                    (* (nt/expt 2 s) (nt/sqrt (inc s)) (height G)))))]
    (loop [H (make [])
           primes-used #{}
           m 1]
      (if (>= B m)
        (let [H-degree (degree H)
              p (random-prime-such-that #(and (not= (mod l %) 0)
                                              (not (contains? primes-used %))))
              Hhat (univariate-modular-gcd p F G)
              Hhat-degree (degree Hhat)
              new-primes-used (conj primes-used p)]
          (cond
            (or (polynomial-zero? H) ;; first time through
                (< Hhat-degree H-degree)) (recur Hhat new-primes-used p) ;; had too many divisors
            (> Hhat-degree H-degree) (recur H new-primes-used m) ;;  bad prime
            ;; else use the Chinese Remainder Algorithm to combine the solutions (mod m) and
            ;; (mod p) into the solution (mod mp).
            :else (let [[_ rm rp] (euclid/extended-gcd m p)
                        m*p (* m p)
                        rp*p (* rp p)
                        rm*m (* rm m)
                        H (combine-like-terms H Hhat (fn [h hh] (mod (+ (*' rp*p h) (*' rm*m hh)) m*p)))]
                    (recur H new-primes-used m*p))))
        ;; we're done when m > B. First balance H, then reimpose the content.
        (let [Hb (map-coefficients #(if (> % (/ m 2)) (- % m) %) H)
              ch (univariate-content Hb)]
          (map-coefficients #(/ (*' c %) ch) Hb))))))

(defn ^:private inner-gcd
  "gcd is just a wrapper for this function, which does the real work
  of computing a multivariate polynomial gcd. Delegates for univariate
  polynomials."
  [level u v]
  (when *poly-gcd-debug*
    (println-indented level "inner-gcd" level (str u) (str v)))
  (let [arity (check-same-arity u v)]
    (if-let [g (and *poly-gcd-cache-enable* (@gcd-memo [u v]))]
      (do (swap! gcd-cache-hit inc) g)
      (let [g (cond
                (= arity 1) (univariate-gcd-subresultant u v)
                (polynomial-zero? u) v
                (polynomial-zero? v) u
                (g/one? u) u
                (g/one? v) v
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
  #(and (g/exact? %) (not (ratio? %))))

(defn ^:private maybe-bail-out
  "Returns a function that checks if clock has been running longer
  than timeout and if so throws an exception after logging the event.
  Timeout should be of the form [number TimeUnit]. "
  [description ^Stopwatch clock timeout]
  (fn []
    (when (> (.elapsed clock (second timeout))
             (first timeout))
      (let [s (format "Timed out: %s after %s" description clock)]
        (log/warn s)
        (throw (TimeoutException. s))))))

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
                (every? integral? (coefficients v)))) (g/one-like u)
      (polynomial-zero? u) v
      (polynomial-zero? v) u
      (g/one? u) u
      (g/one? v) v
      (= u v) u
      (= arity 1) (abs (univariate-gcd-subresultant u v))
      :else (binding [*poly-gcd-bail-out* (maybe-bail-out "polynomial GCD" clock *poly-gcd-time-limit*)]
              (abs
               (with-optimized-variable-order u v
                 (partial inner-gcd 0)
                 )

               )))))

(defn ^:private expand-poly
  "Let skeleton and ps be the same length. A new polynomial is generated
  in which each term of each p is augmented by the powers of the new
  indeterminates in the corresponding skeleton element. Or you could
  look at this as the skeleton being fleshed out, termwise, by
  additional indeterminates and coefficients provided by each p to
  make a new polynomial out of all the resulting terms."
  [skeleton ps]
  (make (+ (.arity ^Polynomial (first ps)) (count (first skeleton)))
        (mapcat (fn [s ^Polynomial p]
                  (for [[es c] (.xs->c p)]
                    (vector (into s es) c)))
                skeleton ps)))

(defn ^:private bigfraction->ratio
  [^BigFraction bf]
  (clojure.lang.Ratio. (.getNumerator bf)
                       (.getDenominator bf)))

(defn ^:private generate-sequence
  [s]
  (let [i (atom -1)]
    (fn []
      (nth s (swap! i inc)))))

;;
;; where we left off: consider regenerating new xs if the inner stage exceeds a certain
;; number of iterations. i.e., put limits on both types of restart.
;;

(defn ^:private acm-rational-matrix
  "Produce an Apache Commons Math matrix of dimension m × n over the
  field of arbitrary size rational numbers. (f i j) is called to
  supply the entries. That function should return BigFractions."
  [^long m ^long n f]
  (let [M (Array2DRowFieldMatrix. (BigFractionField/getInstance) m n)]
    (.walkInRowOrder M
                     (reify FieldMatrixChangingVisitor
                       (start [this r c r1 r2 c1 c2])
                       (visit [this i j v] (f i j))
                       (end [this])))
    M))

(def ^:dynamic *spmod-max-restart* 20)
(def ^:dynamic *spmod-max-stage-restart* 100)

(defn gcd-spmod
  [u v]
  (print 'gcd-spmod u v)
  (let [arity (check-same-arity u v)
        ds (reduce (partial map max) (mapcat ->skeleton [u v]))
        B 100000
        ;;arggen (fn [] (inc (rand-int B)))
        arggen #(random-prime-such-that (constantly true))]
    (loop [restart-count 0
           xs (repeatedly arity arggen)]
      (let [u0 (partial-evaluate u (next xs) :direction :right)
            v0 (partial-evaluate v (next xs) :direction :right)
            r (loop [stage-count 0
                     k 1
                     g (univariate-gcd-subresultant u0 v0)]
                (cond (= k arity) g
                      (> stage-count *spmod-max-stage-restart*) :restart
                      :else
                      (do
                        (when *poly-gcd-debug*
                          (println 'STAGE k '\# stage-count)
                          (println 'g g))
                        (let [S (->skeleton g)
                              nterms (count S)
                              arglists (repeatedly nterms #(repeatedly k arggen))
                              xsk+1 (drop (inc k) xs)
                              uk (partial-evaluate u xsk+1 :direction :right)
                              vk (partial-evaluate v xsk+1 :direction :right)
                              gks (for [a arglists]
                                    (univariate-gcd-subresultant (partial-evaluate uk a)
                                                                 (partial-evaluate vk a)))
                              skels (map ->skeleton gks)
                              nGkTerms (count (first skels))
                              maxGkTerms (inc (nth ds k))]
                          (when *poly-gcd-debug*
                            (println 'S S)
                            (println 'arglists arglists)
                            (println 'uk uk)
                            (println 'vk vk)
                            (println 'gks gks)
                            (println 'skels skels)
                            (println 'ds ds))
                          (cond
                            (or (not (reduce #(and %1 (= (first skels) %2)) true (next skels)))
                                (> nGkTerms maxGkTerms))
                            (do
                              (log/warn "bad skeletons")
                              :restart)

                            :else
                            (let [A (acm-rational-matrix
                                     (count arglists) (count S)
                                     (fn [i j]
                                       (BigFraction.
                                        (biginteger
                                         (reduce *' (map nt/expt (nth arglists i) (nth S j)))))))
                                  LU (FieldLUDecomposition. A)
                                  solver (.getSolver LU)]
                              (when *poly-gcd-debug*
                                (println 'A A)
                                (println 'L (str (.getL LU)))
                                (println 'U (str (.getU LU))))
                              (if (not (.isNonSingular solver))
                                (do
                                  (log/warn "singular matrix")
                                  :restart)
                                (let [newXs (loop [xs (repeatedly maxGkTerms arggen)]
                                              (if (distinct? xs) xs (recur (repeatedly maxGkTerms arggen))))
                                      values (for [newX newXs]
                                               (let [b (ArrayFieldVector. (BigFractionField/getInstance) (count gks))]
                                                 (doseq [j (range (count gks))]
                                                   (.setEntry b j (BigFraction.
                                                                   (biginteger
                                                                    (evaluate-1 (nth gks j) newX)))))
                                                 b))
                                      coeffs (map #(.solve solver ^ArrayFieldVector %) values)]
                                  (when *poly-gcd-debug*
                                    (println 'newXs newXs)
                                    (println 'values (map #(java.util.Arrays/toString (.toArray ^ArrayFieldVector %)) values))
                                    (println 'coeffs (map #(java.util.Arrays/toString (.toArray ^ArrayFieldVector %)) coeffs)))
                                  (let [things (try (doall  ;; because if an exception is to be thrown, we want to see it now
                                                     (for [j (range nterms)]
                                                       (lagrange-interpolating-polynomial
                                                        newXs
                                                        (map #(bigfraction->ratio (.getEntry ^ArrayFieldVector % j)) coeffs))))
                                                    (catch ArithmeticException e
                                                      (log/warn (str "unable to interpolate" (into [] newXs)))
                                                      nil))]
                                    (if things
                                      (let [gk (expand-poly S things)]
                                        (if (and (polynomial-zero? (second (divide uk gk)))
                                                 (polynomial-zero? (second (divide vk gk))))
                                          ;; Success! Start interpolating the next variable. Reset the stage
                                          ;; counter. If we're out of variables we're done.
                                          (recur 0 (inc k) gk)
                                          ;; If the stage trial-division fails, note that we have tried a step
                                          ;; by incrementing stage-count but do not advance k. Start over with the
                                          ;; smae g; new random interpolation points will be selected.
                                          (recur (inc stage-count) k g)))
                                      (recur (inc stage-count) k g)))))))))))]
        (if (= r :restart)
          (let [rc (inc restart-count)]
            (when (> rc *spmod-max-restart*)
              (throw (ArithmeticException. "unable to compute GCD with SPMOD")))
            (log/warn "spmod GCD restart")
            (recur rc (repeatedly arity arggen)))
          r)))))

(defn gcd-spmod-w
  [u v]
  (with-optimized-variable-order u v gcd-spmod))

(def gcd-seq
  "Compute the GCD of a sequence of polynomials (we take care to
  break early if the gcd of an initial segment is unity)"
  (reduce-until g/one? gcd))

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
