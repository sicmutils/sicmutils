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
  (:require #?(:cljs [goog.string :refer [format]])
            [sicmutils.generic :as g]
            [sicmutils.polynomial :as p]
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



;; TODO these are old.
(def ^:private ^:dynamic *poly-gcd-bail-out* (fn []))

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
        (log/info (format "GCD cache hit rate %.2f%% (%d entries)"
                          (* 100 (/ hits (+ hits misses)))
                          memo-count)))))

  (log/info (format "GCD triv %d mono %d"
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

;; ## Coefficient GCD

(defn- native-gcd
  "TODO ONLY needed if we don't do the transform game!!"
  ([] 0)
  ([l] l)
  ([l r]
   (g/gcd (u/biginteger l)
          (u/biginteger r))))

(defn primitive-gcd
  "A function which will return the gcd of a sequence of numbers. TODO put this in
  `numbers`... NOT here."
  [xs]
  (g/abs
   (transduce
    (comp (ua/halt-at v/one?)
          (map u/biginteger))
    (fn
      ([] 0)
      ([x] x)
      ([x y] (g/gcd x y)))
    xs)))

;; ## GCD Helpers

;; TODO I THINK we have this exact same thing in `permute.cljc`, which we should
;; port over here first!!
(defn- sort->permutations
  "Given a vector, returns a permutation function which would sort that vector,
  and the inverse permutation. Each of these functions expects a vector and
  returns one."
  [xs]
  (let [n (count xs)
        order (into [] (sort-by xs (range n)))
        reverse-order (into [] (sort-by order (range n)))]
    [#(mapv % order)
     #(mapv % reverse-order)]))

(defn- terms->permutations
  "Returns a pair of functions that sort and unsort terms into the order of terms
  with max degree in ANY monomial."
  [terms]
  ;; AND remove it up here.
  (if (<= (count terms) 1)
    [identity identity]
    (sort->permutations
     (transduce (map p/exponents)
                (fn [l-expts r-expts]
                  (mapv max l-expts r-expts))
                (first terms)
                (rest terms)))))

(defn- with-optimized-variable-order
  "Rearrange the variables in u and v to make GCD go faster.
  Calls the continuation with the rearranged polynomials. Undoes the
  rearrangement on return. Variables are sorted by increasing degree. Discussed
  in 'Evaluation of the Heuristic Polynomial GCD', by Liao and Fateman [1995]."
  [u v continue]
  (if (and (p/coeff? u) (p/coeff? v))
    (continue u v)
    (let [l-terms (if (p/coeff? u) [] (p/bare-terms u))
          r-terms (if (p/coeff? v) [] (p/bare-terms v))
          [sort unsort] (terms->permutations
                         (into l-terms r-terms))]
      ;; TODO, HERE, check if the terms count is tiny, if so just do identity.
      (->> (continue (p/map-exponents sort u)
                     (p/map-exponents sort v))
           (p/map-exponents unsort)))))

;; TODO implement these!

;; The content of a polynomial is the GCD of its coefficients. The content of a
;;  polynomial has the arity of its coefficients.

;; The primitive-part of a polynomial is the polynomial with the content
;; removed.

(comment
  ;; Okay... so the only difference HERE is that this can tell us if it never
  ;; won! return nil if it failed. It's fine if we default to 1... AND in fact
  ;; they have some stuff to do that!
  (defn poly:content-maker [gcd]
    (fn content [u win lose]
      (let [coeffs (p/coefficients u)]
        (if (empty? coeffs)
	        (win 0)
	        (loop [c0 (first coeffs)
                 cs (rest coeffs)]
	          (if (empty? cs)
		          (win c0)
		          (gcd c0
                   (first cs)
		               (fn [g1]
		                 (if (v/one? g1)
			                 (win g1)
			                 (recur g1 (rest cs))))
		               lose))))))
    poly/content))

(defn ^:private with-content-removed
  "For multivariate polynomials. u and v are considered here as
  univariate polynomials with polynomial coefficients. Using the
  supplied gcd function, the content of u and v is divided out and the
  primitive parts are supplied to the continuation, the result of which
  has the content reattached and is returned."
  [gcd u v continue]
  (let [xform   (comp (map p/coefficients)
                      (ua/halt-at v/one?))
        content (fn [p]
                  (transduce xform gcd p))
        ku (content u)
        kv (content v)
        pu (p/map-coefficients #(g/exact-divide % ku) u)
        pv (p/map-coefficients #(g/exact-divide % kv) v)
        d (gcd ku kv)]
    (p/map-coefficients #(g/* d %) (continue pu pv))))

;; ## GCD Routines

(defn- euclid-inner-loop
  [coefficient-gcd]
  (let [xform   (comp (ua/halt-at v/one?)
                      (map p/coefficients))
        content (fn [p]
                  (transduce xform coefficient-gcd p))]
    (fn [u v]
      (*poly-gcd-bail-out*)
      (let [[r _] (p/pseudo-remainder u v)]
        (when (v/zero? r) v
              (let [kr (content r)]
                (recur v (p/map-coefficients
                          #(g/exact-divide % kr) r))))))))

(def ^:private univariate-euclid-inner-loop
  (euclid-inner-loop native-gcd))

(defn ^:private joint-quotient
  "If d evenly divides both u and v, then [u/d, v/d, d], else nil."
  [u v d]
  (let [[q1 r1] (p/divide u d)
        [q2 r2] (p/divide v d)]
    (when (and (v/zero? r1)
               (v/zero? r2))
      [q1 q2 d])))

(defn- with-trivial-constant-gcd-check
  "We consider the maximum exponent found for each variable in any term of each
  polynomial. A nontrivial GCD would have to fit in this exponent limit for both
  polynomials. This is basically a test for a kind of disjointness of the
  variables. If this happens we just return the constant gcd and do not invoke
  the continuation."
  [u v continue]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)]}
  (let [umax (reduce #(mapv max %1 %2) (map p/exponents (p/bare-terms u)))
        vmax (reduce #(mapv max %1 %2) (map p/exponents (p/bare-terms v)))
        maxd (mapv min umax vmax)]
    (if (every? zero? maxd)
      (do (swap! gcd-trivial-constant inc)
          (->> (concat (p/coefficients u) (p/coefficients v))
               (primitive-gcd)
               (p/make-constant (p/bare-arity u))))
      (continue u v))))

(defn ^:private gcd1
  "Knuth's algorithm 4.6.1E for UNIVARIATE polynomials."
  [u v]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)
         (= (p/arity u) 1)
         (= (p/arity v) 1)]}
  (cond
    (v/zero? u) v
    (v/zero? v) u
    (v/one? u) u
    (v/one? v) v
    (= u v) u
    :else (with-content-removed native-gcd u v univariate-euclid-inner-loop)))

(defn ^:private monomial-gcd
  "Computing the GCD is easy if one of the polynomials is a monomial.
  The monomial is the first argument."
  [m p]
  {:pre [(p/polynomial? m)
         (p/polynomial? p)
         (= (count (p/bare-terms m)) 1)]}
  (let [[mxs mc] (-> m p/bare-terms first)
        xs (reduce #(mapv min %1 %2) mxs (->> p p/bare-terms (map p/exponents)))
        c (primitive-gcd (cons mc (p/coefficients p)))]
    (swap! gcd-monomials inc)
    (p/make (p/bare-arity m) [[xs c]])))

(defn ^:private with-lower-arity
  [u v continue]
  (p/raise-arity
   (continue (p/lower-arity u)
             (p/lower-arity v))))

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
  (dbg level "inner-gcd" u v)
  (let [arity (p/check-same-arity u v)]
    (if-let [g (and *poly-gcd-cache-enable* (@gcd-memo [u v]))]
      (do (swap! gcd-cache-hit inc)
          g)
      (let [g (cond (= arity 1) (gcd1 u v)
                    ;; TODO this is repetitive, can we can this in the dispatcher??
                    (v/zero? u) v
                    (v/zero? v) u
                    (v/one? u)  u
                    (v/one? v)  v
                    (= u v)     u
                    (p/monomial? u) (monomial-gcd u v)
                    (p/monomial? v) (monomial-gcd v u)
                    :else (let [next-gcd        #(inner-gcd (inc level) %1 %2)
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

;; TODO we want this to die.

(defn- maybe-bail-out
  "Returns a function that checks if clock has been running longer than timeout
  and if so throws an exception after logging the event. Timeout should be of
  the form [number Keyword], where keyword is one of the supported units from
  sicmutils.util.stopwatch."
  [description clock [ticks units :as timeout]]
  (fn []
    (when (> (us/elapsed clock units)
             ticks)
      (let [s (format "Timed out: %s after %s" description (us/repr clock))]
        (log/warn s)
        (u/timeout-ex s)))))

;; TODO move these somewhere better! To the stopwatch namespace is the best
;; place.

(defn time-expired? []
  (and *clock*
       (let [[ticks units] *poly-gcd-time-limit*]
         (> (us/elapsed *clock* units) ticks))))

(defn with-limited-time [timeout thunk]
  (binding [*poly-gcd-time-limit* timeout
            *clock* (us/stopwatch)]
    (thunk)))

(declare sparse-base-content poly->sparse)

(defn gcd-sparse
  "TODO get this filled in."
  [u v]
  false)

(defn gcd-euclid [u v]
  (let [clock (us/stopwatch)]
    (binding [*poly-gcd-bail-out* (maybe-bail-out
                                   "polynomial GCD" clock *poly-gcd-time-limit*)]
      (g/abs
       (gcd-continuation-chain u v
                               with-trivial-constant-gcd-check
                               with-optimized-variable-order
                               #(inner-gcd 0 %1 %2))))))

(defn- gcd-dispatch
  "Dispatcher for GCD routines."
  ([] 0)
  ([u] u)
  ([u v]
   (cond (v/zero? u) (g/abs v)
         (v/zero? v) (g/abs u)
         (v/one? u)  u
         (v/one? v)  v
         (= u v)     u
         ;; TODO there is no way that this will work, the `sparse-base-content`
         ;; thing. isn't that u and a list?
         (p/coeff? u) (if (p/coeff? v)
                        (g/gcd u v)
                        (g/gcd u (sparse-base-content (poly->sparse v))))
         (p/coeff? v) (g/gcd (sparse-base-content (poly->sparse u)) v)

         :else
         (let [arity (p/check-same-arity u v)]
           (cond (not (and (every? v/integral? (p/coefficients u))
                           (every? v/integral? (p/coefficients v))))
                 (v/one-like u)

                 (= arity 1) (g/abs (gcd1 u v))

                 :else
                 (or (with-limited-time [1.0 :seconds]
		                   (fn [] (gcd-sparse u v)))

	                   (with-limited-time [1.0 :seconds]
		                   (fn [] (gcd-euclid u v)))

	                   (with-limited-time [100.0 :seconds]
		                   (fn [] (gcd-sparse u v)))

	                   (and (< arity *euclid-breakpoint-arity*)
		                      (with-limited-time [100.0 :seconds]
		                        (fn [] (gcd-euclid u v))))

	                   (gcd-sparse u v)

	                   (if *gcd-cut-losses*
		                   (or (with-limited-time *gcd-cut-losses*
			                       (fn [] (gcd-euclid u v)))
		                       1)
		                   (gcd-euclid u v))))))))

(def ^{:doc "main GCD entrypoint."}
  gcd
  gcd-dispatch)

;; TODO test `gcd` between OTHER types and polynomials here...

(defmethod g/gcd [::p/polynomial ::p/polynomial] [u v]
  (gcd-dispatch u v))

(defmethod g/gcd [::p/polynomial ::p/coeff] [u v]
  (primitive-gcd (cons v (p/coefficients u))))

(defmethod g/gcd [::p/coeff ::p/polynomial] [u v]
  (primitive-gcd (cons u (p/coefficients v))))

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
  (gcd-seq
   (p/partial-derivatives p)))

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
