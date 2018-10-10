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
           (java.util.concurrent TimeUnit TimeoutException))
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
  (let [vn (coefficient (lead-term v))
        m-n+1 (inc (- (degree u) (degree v)))]
    (second (divide (g/mul (nt/expt vn m-n+1) u) v))))

(def ^:dynamic gcd-time-data #_(atom []) nil)

(defn ^:private height
  [p]
  (transduce (map nt/abs) max 0 (coefficients p)))

(defn univariate-subresultant-gcd
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
                        g' (coefficient (lead-term u'))
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
        (polynomial-zero? r) (let [l (coefficient (lead-term v))]
                               (if (not= l 1)
                                 (let [l' (euclid/modular-inverse p l)]
                                   (map-coefficients #(mod (* % l') p) v))
                                 v))
        (zero? (degree r)) (make-constant 1 1)
        :else (recur v r)))))

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
                (= arity 1) (univariate-subresultant-gcd u v)
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
      (= arity 1) (abs (univariate-subresultant-gcd u v))
      :else (binding [*poly-gcd-bail-out* (maybe-bail-out "polynomial GCD" clock *poly-gcd-time-limit*)]
              (abs (gcd-continuation-chain u v
                                           with-trivial-constant-gcd-check
                                           with-optimized-variable-order
                                           #(inner-gcd 0 %1 %2)))))))

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
