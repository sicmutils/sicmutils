;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.polynomial.gcd
  #?(:cljs (:require-macros
            [sicmutils.polynomial.gcd :refer [dbg gcd-continuation-chain]]))
  (:require [clojure.set :as cs]
            #?(:cljs [goog.string :refer [format]])
            [sicmutils.generic :as g]
            [sicmutils.polynomial :as p]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stopwatch :as us]
            [sicmutils.value :as v]
            [taoensso.timbre :as log]))

;; TODO these are new ones.

(def ^:dynamic *poly-gcd-time-limit* [1000 :millis])
(def ^:dynamic *clock* nil)
(def ^:dynamic *euclid-breakpoint-arity* 3)
(def ^:dynamic *gcd-cut-losses* nil)

(def ^:dynamic *poly-gcd-cache-enable* true)
(def ^:dynamic *poly-gcd-debug* false)

;; TODO these make the memoization work. It is probably great.

(def ^:private gcd-memo (atom {}))
(def ^:private gcd-cache-hit (atom 0))
(def ^:private gcd-cache-miss (atom 0))
(def ^:private gcd-trivial-constant (atom 0))
(def ^:private gcd-monomials (atom 0))

;; ## Stats, Debugging

(defn gcd-stats []
  (let [memo-count (count @gcd-memo)]
    (when (> memo-count 0)
      (let [hits   @gcd-cache-hit
            misses @gcd-cache-miss]
        (log/info
         (format "GCD cache hit rate %.2f%% (%d entries)"
                 (* 100 (/ (float hits) (+ hits misses)))
                 memo-count)))))

  (log/info
   (format "GCD triv %d mono %d"
           @gcd-trivial-constant
           @gcd-monomials)))

(defn- println-indented
  [level & args]
  (apply println (apply str (repeat level "  ")) args))

(defmacro ^:private dbg
  [level where & xs]
  `(when *poly-gcd-debug*
     (println-indented
      ~level ~where ~level
      ~@(map #(list 'str %) xs))))

;; TODO move these somewhere better! To the stopwatch namespace is the best
;; place.

(defn time-expired? []
  (and *clock*
       (let [[ticks units] *poly-gcd-time-limit*]
         (> (us/elapsed *clock* units) ticks))))

(defn- maybe-bail-out
  "Returns a function that checks if clock has been running longer than timeout
  and if so throws an exception after logging the event. Timeout should be of
  the form [number Keyword], where keyword is one of the supported units from
  sicmutils.util.stopwatch."
  [description]
  (when (time-expired?)
    (let [s (format "Timed out: %s after %s" description (us/repr *clock*))]
      (log/warn s)
      (u/timeout-ex s))))

(defn with-limited-time [timeout thunk]
  (binding [*poly-gcd-time-limit* timeout
            *clock* (us/stopwatch)]
    (thunk)))

;; Continuation Helpers

;; Continuation Menu

;; TODO I THINK we have this very similar thing in `permute.cljc`, which we
;; should port over here first!! But not for sorted maps.

(defn sort->permutations [m]
  (def sort-m m)
  (let [indices (range (count m))
        order (into [] (sort-by m (keys m)))
        sort (fn [m']
               (into (sorted-map)
                     (mapcat (fn [i]
                               (when-let [v (m' (order i))]
                                 [[i v]])))
                     indices))
        unsort (fn [m']
                 (into (sorted-map)
                       (mapcat (fn [i]
                                 (when-let [v (m' i)]
                                   [[(order i) v]])))
                       indices))]
    [sort unsort]))

(comment
  (defn tester [m]
    (let [[sort unsort] (sort->permutations' m)]
      (and (= (vals (sort m))
              (clojure.core/sort (vals m)))
           (= m (unsort (sort m))))))

  (tester (sorted-map 1 2, 3 1, 5 4))
  (tester (sorted-map 1 3, 3 1, 5 4))
  (tester (sorted-map 1 8, 3 1, 5 4)))

(defn- terms->permutations
  "Returns a pair of functions that sort and unsort terms into the order of terms
  with max degree in ANY monomial."
  [terms]
  (if (<= (count terms) 1)
    [identity identity]
    (sort->permutations
     (transduce (map p/exponents)
                p/expt:max
                (p/exponents (first terms))
                (rest terms)))))

(defn- with-optimized-variable-order
  "Rearrange the variables in u and v to make GCD go faster.
  Calls the continuation with the rearranged polynomials. Undoes the
  rearrangement on return. Variables are sorted by increasing degree.

  Discussed in 'Evaluation of the Heuristic Polynomial GCD', by Liao and
  Fateman [1995]."
  [u v continue]
  (if (or (p/polynomial? u) (p/polynomial? v))
    (let [l-terms (if (p/polynomial? u) (p/bare-terms u) [])
          r-terms (if (p/polynomial? v) (p/bare-terms v) [])
          [sort unsort] (terms->permutations
                         (into l-terms r-terms))]
      (->> (continue
            (p/map-exponents sort u)
            (p/map-exponents sort v))
           (p/map-exponents unsort)))
    (continue u v)))

(defn ->gcd [binary-gcd]
  (fn [coefs]
    (reduce (fn
              ([] 0)
              ([x] x)
              ([x y]
               (if (v/one? x)
                 (reduced x)
                 (binary-gcd x y))))
            coefs)))

;; TODO fix these bullshits!

(defn- with-content-removed
  "For multivariate polynomials. u and v are considered here as univariate
  polynomials with polynomial coefficients. Using the supplied gcd function, the
  content of u and v is divided out and the primitive parts are supplied to the
  continuation, the result of which has the content reattached and is returned."
  [gcd u v continue]
  (let [content (fn [p]
                  (gcd (p/coefficients p)))
        ku (content u)
        kv (content v)
        pu (if (v/one? ku)
             u
             (p/map-coefficients #(g/exact-divide % ku) u))
        pv (if (v/one? kv)
             v
             (p/map-coefficients #(g/exact-divide % kv) v))
        d (gcd [ku kv])
        result (continue pu pv)
        result (if (p/polynomial? result)
                 result
                 (p/constant 1 result))]
    (p/scale-l d result)))

(defn- with-lower-arity
  [u v continue]
  (let [a (p/check-same-arity u v)
        result (continue
                (p/lower-arity u)
                (p/lower-arity v))]
    (if (p/polynomial? result)
      (p/raise-arity result a)
      result)))

(declare primitive-gcd)

(defn- with-trivial-constant-gcd-check
  "We consider the maximum exponent found for each variable in any term of each
  polynomial. A nontrivial GCD would have to fit in this exponent limit for both
  polynomials. This is basically a test for a kind of disjointness of the
  variables. If this happens we just return the constant gcd and do not invoke
  the continuation.

  TODO note that this will now return a constant, NOT a polynomial."
  [u v continue]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)]}
  (let [umax (reduce into (map (comp u/keyset p/exponents)
                               (p/bare-terms u)))
        vmax (reduce into (map (comp u/keyset p/exponents)
                               (p/bare-terms v)))]
    (if (empty? (cs/intersection umax vmax))
      (do (swap! gcd-trivial-constant inc)
          (primitive-gcd
           (concat (p/coefficients u)
                   (p/coefficients v))))
      (continue u v))))

;; ## Basic GCD for Coefficients, Monomials

(defn primitive-gcd
  "A function which will return the gcd of a sequence of numbers. TODO put this in
  `numbers`... NOT here."
  [xs]
  (g/abs
   (reduce (fn
             ([] 0)
             ([x] x)
             ([x y] (if (v/one? x)
                      (reduced x)
                      (g/gcd x y))))
           xs)))

;; Next simplest! We have a poly on one side, coeff on the other.

(defn- gcd-poly-number
  [p n]
  {:pre [(p/polynomial? p)
         (p/coeff? n)]}
  (primitive-gcd (cons n (p/coefficients p))))

(defn- monomial-gcd
  "Computing the GCD is easy if one of the polynomials is a monomial.
  The monomial is the first argument.

  Basically... take the GCD of the coeffs for the coefficient, and the MINIMUM
  exp of each variable across all, pinned at the top by the monomial."
  [m p]
  {:pre [(p/monomial? m)
         (p/polynomial? p)]}
  (let [[mono-expts mono-coeff] (nth (p/bare-terms m) 0)
        mono-keys (keys mono-expts)
        xs (transduce (map p/exponents)
                      p/expt:gcd
                      mono-expts
                      (p/bare-terms p))
        c (gcd-poly-number p mono-coeff)]
    (swap! gcd-monomials inc)
    (p/terms->polynomial (p/bare-arity m) [[xs c]])))

(comment
  (is (= (p/make 2 {[1 2] 3})
         (monomial-gcd (p/make 2 {[1 2] 12})
                       (p/make 2 {[4 3] 15})))))

;; The content of a polynomial is the GCD of its coefficients. The content of a
;; polynomial has the arity of its coefficients.

;; The primitive-part of a polynomial is the polynomial with the content
;; removed.

;; ## GCD Routines

(defn- euclid-inner-loop
  "TODO THIS is sort of messed up, since this damned thing does a RECUR and can
  potentially drop down to non... polynomial stuff. I think the whole codebase
  needs to get RID of this thing."
  [coeff-gcd]
  (letfn [(content [p]
            (coeff-gcd
             (p/coefficients p)))]
    (fn [u v]
      (maybe-bail-out "euclid inner loop")
      (cond (v/zero? u) v
            (v/zero? v) u
            (v/one? u) u
            (v/one? v) v
            (p/coeff? u) (if (p/coeff? v)
                           (g/gcd u v)
                           (gcd-poly-number v u))
            (p/coeff? v) (gcd-poly-number u v)
            (= u v) u
            :else
            (let [[r _] (p/pseudo-remainder u v)]
              (if (v/zero? r)
                v
                (let [kr (content r)]
                  (recur v (if (v/one? kr)
                             r
                             (p/map-coefficients
                              #(g/exact-divide % kr) r))))))))))

(def ^:private univariate-euclid-inner-loop
  (euclid-inner-loop primitive-gcd))

(defn- gcd1
  "Knuth's algorithm 4.6.1E for UNIVARIATE polynomials."
  [u v]
  {:pre [(p/univariate? u)
         (p/univariate? v)]}
  (with-content-removed primitive-gcd u v univariate-euclid-inner-loop))

;; Helpers

(defmacro gcd-continuation-chain
  "Takes two polynomials and a chain of functions. Each function, except
  the last, should have signature [p q k] where p and q are polynomials
  and k is a continuation of the same type. The last function should have
  signature [p q], without a continuation argument, since there's nowhere
  to go from there."
  [u v & fs]
  (condp < (count fs)
    2 `(~(first fs) ~u ~v
        (fn [u# v#]
          (gcd-continuation-chain u# v# ~@(next fs))))
    1 `(~(first fs) ~u ~v ~(second fs))
    0 `(~(first fs) ~u ~v)))

;; Continuations

(defn- inner-gcd
  "gcd is just a wrapper for this function, which does the real work
  of computing a polynomial gcd. Delegates to gcd1 for univariate
  polynomials."
  [level u v]
  (dbg level "inner-gcd" u v)
  (if-let [g (and *poly-gcd-cache-enable* (@gcd-memo [u v]))]
    (do (swap! gcd-cache-hit inc)
        g)
    ;; TODO SHARE these trivial conditions!
    ;; TODO do we need abs?
    (let [g (cond (v/zero? u) (g/abs v)
                  (v/zero? v) (g/abs u)
                  (p/coeff? u)(if (p/coeff? v)
                                (g/gcd u v)
                                (gcd-poly-number v u))
                  (p/coeff? v)(gcd-poly-number u v)
                  (v/one? u)  u
                  (v/one? v)  v
                  (= u v) u
                  :else
                  (let [arity (p/check-same-arity u v)]
                    (cond
                      (= arity 1) (gcd1 u v)
                      (p/monomial? u) (monomial-gcd u v)
                      (p/monomial? v) (monomial-gcd v u)
                      :else
                      (let [next-gcd (->gcd
                                      (fn [u v]
                                        (inner-gcd (inc level) u v)))
                            content-remover (fn [u v cont]
                                              (with-content-removed next-gcd u v cont))]
                        (maybe-bail-out "polynomial GCD")
                        (gcd-continuation-chain u v
                                                with-lower-arity
                                                content-remover
                                                (euclid-inner-loop next-gcd))))))]
      (when *poly-gcd-cache-enable*
        (swap! gcd-cache-miss inc)
        (swap! gcd-memo assoc [u v] g))
      (dbg level "<-" g)
      g)))

(defn gcd-euclid [u v]
  (g/abs
   (gcd-continuation-chain u v
                           with-trivial-constant-gcd-check
                           with-optimized-variable-order
                           #(inner-gcd 0 %1 %2))))

(defn- gcd-dispatch
  "Dispatcher for GCD routines.

  TODO isn't... the defmethod sort of the dispatcher??"
  ([] 0)
  ([u] u)
  ([u v]
   ;; TODO I... think we can kill ALL of these... since we do them inside `inner-gcd`??
   (cond (v/zero? u)(g/abs v)
         (v/zero? v)(g/abs u)
         (p/coeff? u)(if (p/coeff? v)
                       (g/gcd u v)
                       (gcd-poly-number v u))
         (p/coeff? v)(gcd-poly-number u v)
         (v/one? u)  u
         (v/one? v)  v
         (= u v) (g/abs u)
         :else
         (let [arity (p/check-same-arity u v)]
           (cond
             (not (and (every? v/exact? (p/coefficients u))
                       (every? v/exact? (p/coefficients v))))
             (v/one-like u)

             (= arity 1) (g/abs (gcd1 u v))

             :else
             (with-limited-time *poly-gcd-time-limit*
               (fn [] (gcd-euclid u v))))))))

(def ^{:doc "main GCD entrypoint."}
  gcd
  #'gcd-dispatch)

;; TODO test `gcd` between OTHER types and polynomials here...

(defmethod g/gcd [::p/polynomial ::p/polynomial] [u v]
  (gcd-dispatch u v))

(defmethod g/gcd [::p/polynomial ::p/coeff] [u v]
  (if (v/zero? v)
    u
    (gcd-poly-number u v)))

(defmethod g/gcd [::p/coeff ::p/polynomial] [u v]
  (if (v/zero? u)
    v
    (gcd-poly-number v u)))

(defn- gcd-seq
  "Compute the GCD of a sequence of polynomials (we take care to
  break early if the gcd of an initial segment is unity)"
  [items]
  (transduce (ua/halt-at v/one?)
             gcd-dispatch
             items))

(defn gcd-Dp
  "Compute the gcd of the all the partial derivatives of p."
  [p]
  (if (p/polynomial? p)
    (gcd-seq
     (p/partial-derivatives p))
    1))

;; several observations. many of the gcds we find when attempting the
;; troublesome GCD are the case where we have two monomials. This can be done
;; trivially without lowering arity.
;;
;; TODO we can totally do this! Lower should NOT raise back up properly if you
;; get down to a constant. But I don't know if I can do that if I still have
;; with-lowered around...
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
