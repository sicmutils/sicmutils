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
  #?(:cljs (:require-macros
            [sicmutils.polynomial-gcd :refer [dbg gcd-continuation-chain]]))
  (:require #?(:cljs [goog.string :refer [format]])
            [sicmutils.generic :as g]
            [sicmutils.polynomial :as p
             #?@(:cljs [:refer [Polynomial]])]
            [sicmutils.util :as u]
            [sicmutils.util.stopwatch :as us]
            [sicmutils.value :as v]
            [taoensso.timbre :as log])
  #?(:clj
     (:import (sicmutils.polynomial Polynomial))))

(def ^:dynamic *poly-gcd-time-limit* [1000 :millis])
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

(defn ^:private native-gcd [l r]
  (g/gcd (u/biginteger l)
         (u/biginteger r)))

(defn primitive-gcd
  "A function which will return the gcd of a sequence of numbers."
  [xs]
  (g/abs ((reduce-until v/unity? native-gcd) xs)))

(defn ^:private with-content-removed
  "For multivariate polynomials. u and v are considered here as
  univariate polynomials with polynomial coefficients. Using the
  supplied gcd function, the content of u and v is divided out and the
  primitive parts are supplied to the continuation, the result of which
  has the content reattached and is returned."
  [gcd u v continue]
  (let [gcd-reducer (reduce-until v/unity? gcd)
        content #(-> % p/coefficients gcd-reducer)
        ku (content u)
        kv (content v)
        pu (p/map-coefficients #(g/exact-divide % ku) u)
        pv (p/map-coefficients #(g/exact-divide % kv) v)
        d (gcd ku kv)]
    (p/map-coefficients #(g/* d %) (continue pu pv))))

(defn ^:private with-lower-arity
  [u v continue]
  (p/raise-arity
   (continue (p/lower-arity u)
             (p/lower-arity v))))

(defn ^:private euclid-inner-loop
  [coefficient-gcd]
  (let [content #(->> % p/coefficients (reduce coefficient-gcd))]
    (fn [u v]
      (loop [u u v v]
        (*poly-gcd-bail-out*)
        (let [[r _] (p/pseudo-remainder u v)]
          (if (v/nullity? r) v
              (let [kr (content r)]
                (recur v (p/map-coefficients #(g/exact-divide % kr) r)))))))))

(defn ^:private joint-quotient
  "If d evenly divides both u and v, then [u/d, v/d, d], else nil."
  [u v d]
  (let [[q1 r1] (p/divide u d)
        [q2 r2] (p/divide v d)]
    (if (and (v/nullity? r1) (v/nullity? r2))
      [q1 q2 d])))

(defn ^:private with-trivial-constant-gcd-check
  "We consider the maximum exponent found for each variable in any
  term of each polynomial. A nontrivial GCD would have to fit in this
  exponent limit for both polynomials. This is basically a test for a
  kind of disjointness of the variables. If this happens we just
  return the constant gcd and do not invoke the continuation."
  [^Polynomial u ^Polynomial v continue]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)]}
  (let [umax (reduce #(mapv max %1 %2) (map p/exponents (.-xs->c u)))
        vmax (reduce #(mapv max %1 %2) (map p/exponents (.-xs->c v)))
        maxd (mapv min umax vmax)]
    (if (every? zero? maxd)
      (do
        (swap! gcd-trivial-constant inc)
        (->> (concat (p/coefficients u) (p/coefficients v))
             primitive-gcd
             (p/make-constant (.-arity u))))
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
  {:pre [(p/polynomial? u)
         (p/polynomial? v)]}
  (let [xs (reduce #(mapv max %1 %2)
                   (concat (map p/exponents (.-xs->c u))
                           (map p/exponents (.-xs->c v))))
        [sorter unsorter] (sort->permutations xs)]
    (p/map-exponents unsorter
                     (continue (p/map-exponents sorter u)
                               (p/map-exponents sorter v)))))

(def ^:private univariate-euclid-inner-loop
  (euclid-inner-loop native-gcd))

(defn ^:private gcd1
  "Knuth's algorithm 4.6.1E for UNIVARIATE polynomials."
  [u v]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)
         (= (.-arity u) 1)
         (= (.-arity v) 1)]}
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
  [^Polynomial m ^Polynomial p]
  {:pre [(p/polynomial? m)
         (p/polynomial? p)
         (= (count (.-xs->c m)) 1)]}
  (let [[mxs mc] (-> m .-xs->c first)
        xs (reduce #(mapv min %1 %2) mxs (->> p .-xs->c (map p/exponents)))
        c (primitive-gcd (cons mc (p/coefficients p)))]
    (swap! gcd-monomials inc)
    (p/make (.-arity m) [[xs c]])))

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
  (let [arity (p/check-same-arity u v)]
    (if-let [g (and *poly-gcd-cache-enable* (@gcd-memo [u v]))]
      (do (swap! gcd-cache-hit inc) g)
      (let [g (cond
                (= arity 1) (gcd1 u v)
                (v/nullity? u) v
                (v/nullity? v) u
                (v/unity? u) u
                (v/unity? v) v
                (= u v) u
                (p/monomial? u) (monomial-gcd u v)
                (p/monomial? v) (monomial-gcd v u)
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

(defn ^:private maybe-bail-out
  "Returns a function that checks if clock has been running longer than timeout
  and if so throws an exception after logging the event. Timeout should be of
  the form [number Keyword], where keyword is one of the supported units from
  sicmutils.util.stopwatch."
  [description clock timeout]
  (fn []
    (when (> (us/elapsed clock (second timeout))
             (first timeout))
      (let [s (format "Timed out: %s after %s" description (us/repr clock))]
        (log/warn s)
        (u/timeout-ex s)))))

(defn gcd
  "Knuth's algorithm 4.6.1E.
  This can take a long time, unfortunately, and so we bail if it seems to
  be taking too long."
  [u v]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)]}
  (let [clock (us/stopwatch)
        arity (p/check-same-arity u v)]
    (cond
      (not (and (every? v/integral? (p/coefficients u))
                (every? v/integral? (p/coefficients v)))) (v/one-like u)
      (v/nullity? u) v
      (v/nullity? v) u
      (v/unity? u) u
      (v/unity? v) v
      (= u v) u
      (= arity 1) (g/abs (gcd1 u v))
      :else (binding [*poly-gcd-bail-out* (maybe-bail-out "polynomial GCD" clock *poly-gcd-time-limit*)]
              (g/abs
               (gcd-continuation-chain u v
                                       with-trivial-constant-gcd-check
                                       with-optimized-variable-order
                                       #(inner-gcd 0 %1 %2)))))))

(defmethod g/gcd [::p/polynomial ::p/polynomial] [u v] (gcd u v))

(def gcd-seq
  "Compute the GCD of a sequence of polynomials (we take care to
  break early if the gcd of an initial segment is unity)"
  (reduce-until v/unity? gcd))

;; several observations. many of the gcds we find when attempting the
;; troublesome GCD are the case where we have two monomials. This can be done
;; trivially without lowering arity.
;;
;; secondly, we observe that lowering arity often produces constant polynomials.
;; can we get away with just dropping these to scalars, instead of constant
;; polynomials of lower degree?
;;
;; then we could special-case the question of GCD of a polynomial with a basic
;; number. That's just the gcd of the number and the polynomial's integer
;; coefficients.
;;
;; open question: why was dividing out the greatest monomial factor such a lose?
;; it's much cheaper to do that than to find a gcd by going down the arity
;; chain.
