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
  (:require [clojure.set :as cs]
            [clojure.string :as string]
            #?(:cljs [goog.string :refer [format]])
            [sicmutils.generic :as g]
            [sicmutils.polynomial :as p]
            [sicmutils.polynomial.exponent :as xpt]
            [sicmutils.polynomial.impl :as pi]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stopwatch :as us]
            [sicmutils.value :as v]
            [taoensso.timbre :as log]))

;; ## Multivariate Polynomial GCD
;;
;; This namespace contains functions for calculating the greatest common divisor
;; of multivariate `p/Polynomial` instances.
;;
;; This namespace will eventually dispatch between a sparse GCD algorithm and
;; Euclid's; for now it only contains a "classical" implementation of Euclid's
;; algorithm.

(def ^{:dynamic true
       :doc "Pair of the form [number
  Keyword], where keyword is one of the supported units from
  [[sicmutils.util.stopwatch]]. If Euclidean GCD takes longer than this time
  limit, the system will bail out by throwing an exception."}
  *poly-gcd-time-limit*
  [1000 :millis])

(def ^:dynamic *clock* nil)

(def ^{:dynamic true
       :doc "When true, multivariate GCD will cache each recursive step in the
  Euclidean GCD algorithm, and attempt to shortcut out on a successful cache
  hit. True by default."}
  *poly-gcd-cache-enable*
  true)

(def ^{:dynamic true
       :doc "When true, multivariate GCD will log each `u` and `v` input and the
  result of each step, along with the recursive level of the logged GCD
  computation. False by default."}
  *poly-gcd-debug*
  false)

;; Stateful instances required for GCD memoization and stats tracking.

(def ^:private gcd-memo (atom {}))
(def ^:private gcd-cache-hit (atom 0))
(def ^:private gcd-cache-miss (atom 0))
(def ^:private gcd-trivial-constant (atom 0))
(def ^:private gcd-monomials (atom 0))

;; ## Stats, Debugging
;;
;; This first block of functions provides utilities for logging statistics on
;; the GCD search, as well as for limiting the time of attempts with a time
;; limit and stopwatch.

(defn gcd-stats
  "When called, logs statistics about the GCD memoization cache, and the number of
  times the system has encountered monomial or other trivial GCDs. "
  []
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

(defn- dbg
  "Generates a DEBUG logging statement guarded by the [[*poly-gcd-debug*]] dynamic
  variable."
  [level where & xs]
  (when *poly-gcd-debug*
    (let [xs     (map str xs)
          xs'    (into [where level] xs)
          xs-s   (string/join " " xs')
          prefix (apply str (repeat level "  "))]
      (log/debug prefix xs-s))))

(defn time-expired?
  "Returns true if the [[*clock*]] dynamic variable contains a Stopwatch with an
  elapsed time that's passed the limit allowed by the
  dynamic [[*poly-gcd-time-limit*]], false otherwise."
  []
  (and *clock*
       (let [[ticks units] *poly-gcd-time-limit*]
         (> (us/elapsed *clock* units) ticks))))

(defn- maybe-bail-out!
  "When called, if [[time-expired?]] returns `true`, logs a warning and throws a
  TimeoutException, signaling that the GCD process has gone on past its allowed
  time limit."
  [description]
  (when (time-expired?)
    (let [s (format "Timed out: %s after %s" description (us/repr *clock*))]
      (log/warn s)
      (u/timeout-ex s))))

(defn with-limited-time
  "Given an explicit `timeout` and a no-argument function `thunk`, calls `thunk`
  in a context where [[*poly-gcd-time-limit*]] is dynamically bound to
  `timeout`. Calling [[time-expired?]] or [[maybe-bail-out!]] inside `thunk`
  will signal failure appropriately if `thunk` has taken longer than `timeout`."
  [timeout thunk]
  (binding [*poly-gcd-time-limit* timeout
            *clock* (us/stopwatch)]
    (thunk)))

(defn- cached
  "Attempts to call `f` with arguments `u` and `v`, but only after checking that
  `[u v]` is not present in the global GCD memoization cache. If not, calls `(f
  u v)` and registers the result in [[gcd-memo]] before returning the result.

  Use the [[*poly-gcd-cache-enable*]] dynamic variable to turn the cache on and
  off."
  [f u v]
  (if-let [g (and *poly-gcd-cache-enable*
                  (@gcd-memo [u v]))]
    (do (swap! gcd-cache-hit inc)
        g)
    (let [result (f u v)]
      (when *poly-gcd-cache-enable*
        (swap! gcd-cache-miss inc)
        (swap! gcd-memo assoc [u v] result))
      result)))

;; Continuations
;;
;; The GCD implementation below uses a continuation-passing style to apply
;; transformations to each polynomial that make the process more efficient.
;; First, a few helper functions, and then a number of continuations used to
;; compute GCDs.

(defn- cont->
  "Takes two polynomials `u` and `v` and any number of 'continuation' functions,
  and returns the result of threading `u` and `v` through all continuation
  functions.

  Each function, except the last, should have signature `[p q k]`, where `p` and
  `q` are polynomials and k is a continuation of the same type.

  The last function should have signature `[p q]` without a continuation
  argument.

  For example, the following forms are equivalent:

  ```clojure
  (cont-> u v f1 f2 f3)
  (f1 u v (fn [u' v']
            (f2 u' v' f3)))
  ```"
  ([[u v]] [u v])
  ([[u v] f]
   (f u v))
  ([[u v] f1 f2]
   (f1 u v f2))
  ([[u v] f1 f2 & more]
   (f1 u v
       (fn [u' v']
         (apply cont-> [u' v'] f2 more)))))

(defn- terms->sort+unsort
  "Given a sequence of polynomial terms, returns a pair of functions of one
  polynomial argument that respectively sort and unsort the variables in the
  polynomial by increasing degree."
  [terms]
  (if (<= (count terms) 1)
    [identity identity]
    (xpt/->sort+unsort
     (transduce (map pi/exponents)
                xpt/lcm
                terms))))

(defn- with-optimized-variable-order
  "Accepts two polynomials `u` and `v` and calls `continuation` with the variable
  indices in each polynomial rearranged to make GCD go faster. Undoes the
  rearrangement on return.

  When passed either non-polynomials or univariate polynomials,
  returns `(continue u v)` unchanged.

  Variables are sorted by increasing degree, where the degree is considered
  across terms of both `u` and `v`. Discussed in ['Evaluation of the Heuristic
  Polynomial
  GCD'](https://people.eecs.berkeley.edu/~fateman/282/readings/liao.pdf) by Liao
  and Fateman [1995]."
  [u v continue]
  (if (or (p/multivariate? u)
          (p/multivariate? v))
    (let [l-terms (if (p/polynomial? u) (p/bare-terms u) [])
          r-terms (if (p/polynomial? v) (p/bare-terms v) [])
          [sort unsort] (terms->sort+unsort
                         (into l-terms r-terms))]
      (->> (continue
            (p/map-exponents sort u)
            (p/map-exponents sort v))
           (p/map-exponents unsort)))
    (continue u v)))

(defn ->content+primitive
  "Given some polynomial `p`, and a multi-arity `gcd` function for its
  coefficients, returns a pair of the polynomial's content and primitive.

  The 'content' of a polynomial is the greatest common divisor of its
  coefficients. The 'primitive part' of a polynomial is the quotient of the
  polynomial by its content.

  See Wikipedia's ['Primitive Part and
  Content'](https://en.wikipedia.org/wiki/Primitive_part_and_content) page for
  more details. "
  [p gcd]
  (let [content (apply gcd (p/coefficients p))
        primitive (if (v/one? content)
                    p
                    (p/map-coefficients
                     #(g/exact-divide % content) p))]
    [content primitive]))

(defn- with-content-removed
  "Given a multi-arity `gcd` routine, returns a function of polynomials `u` and
  `v` and a continuation `continue`.

  The returned function calls the `continue` continuation with the [primitive
  parts](https://en.wikipedia.org/wiki/Primitive_part_and_content) of `u` and
  `v` respectively.

  On return, [[with-content-removed]]'s returned function scales the result back
  up by the `gcd` of the contents of `u` and `v` (ie, the greatest common
  divisor across the coefficients of both polynomials).

  [[with-content-removed]] is intended for use with multivariate polynomials. In
  this case, `u` and `v` are considered to be univariate polynomials with
  polynomial coefficients."
  [gcd]
  (fn [u v continue]
    (let [[ku pu] (->content+primitive u gcd)
          [kv pv] (->content+primitive v gcd)
          d       (gcd ku kv)
          result  (continue pu pv)
          result  (if (p/polynomial? result)
                    result
                    (p/constant 1 result))]
      (p/scale-l d result))))

(defn- with-trivial-constant-gcd-check
  "Given a multi-arity `gcd` routine, returns a function of polynomials `u` and
  `v` and a continuation `continue`.

  This function determines whether or not `u` and `v` have any variables in
  common. If they don't, then it's not possible for any common divisor to share
  variables; the function returns the `gcd` of the coefficients of `u` and `v`.

  If they do, the function returns `(continue u v)`."
  [gcd]
  (fn [u v continue]
    {:pre [(p/polynomial? u)
           (p/polynomial? v)]}
    (let [u-vars (reduce into (map (comp u/keyset pi/exponents)
                                   (p/bare-terms u)))
          v-vars (reduce into (map (comp u/keyset pi/exponents)
                                   (p/bare-terms v)))]
      (if (empty? (cs/intersection u-vars v-vars))
        (do (swap! gcd-trivial-constant inc)
            (apply gcd
                   (concat (p/coefficients u)
                           (p/coefficients v))))
        (continue u v)))))

;; ## Basic GCD for Coefficients, Monomials
;;
;; Now we come to the GCD routines. There are a few here, to handle simple cases
;; like dividing a monomial into a larger polynomial, or taking the GCD of
;; sequences of coefficients.

(defn- ->gcd
  "Given a `binary-gcd` function for computing greatest common divisors, returns a
  multi-arity function that returns `0` when called with no arguments, and
  reduces multiple arguments with `binary-gcd`, aborting if any `one?` is
  reached.

  NOTE: This is only appropriate if you don't expect rational coefficients; the
  GCD of 1 and a rational number IS that other number, so the `v/one?` guard is
  not appropriate."
  [binary-gcd]
  (ua/monoid binary-gcd 0 v/one?))

(def ^:no-doc primitive-gcd
  (->gcd g/gcd))

;; The GCD of a sequence of integers is the simplest case; simply reduce across
;; the sequence using `g/gcd`. The next-easiest case is the GCD of a coefficient
;; and a polynomial.

(defn- gcd-poly-number
  "Returns the GCD of some polynomial `p` and a non-polynomial `n`; this is simply
  the GCD of `n` and all coefficients of `p`."
  [p n]
  {:pre [(p/polynomial? p)
         (p/coeff? n)]}
  (apply primitive-gcd n (p/coefficients p)))

;; Wih these two in hand, there are a few trivial cases that are nice to catch
;; before dispatching more heavyweight routines.

(defn trivial-gcd
  "Given two polynomials `u` and `v`, attempts to return the greatest common
  divisor of `u` and `v` by testing for trivial cases. If no trivial case
  applies, returns `nil`."
  [u v]
  (cond (v/zero? u) (g/abs v)
        (v/zero? v) (g/abs u)
        (p/coeff? u) (if (p/coeff? v)
                       (primitive-gcd u v)
                       (gcd-poly-number v u))
        (p/coeff? v) (gcd-poly-number u v)
        (= u v) (p/abs u)
        :else nil))

;; Next, the case of the GCD of polynomials. If one of the sides is a monomial,
;; the GCD is easy.
;;
;; For example, $3xy$ divides $6xy^2 + 9x$ trivially; the GCD is $3x$.
;;
;; See the docstring below for a description.

(defn- monomial-gcd
  "Returns the greatest common divisor of some monomial `m` and a polynomial `p`.
  The GCD of these two inputs is a monomial (or bare coefficient) with:

  - coefficient portion equal to the GCD of the coefficient of both sides
  - power product equal to the GCD of the power products of all `p` terms with
    the power product of `m`"
  [m p]
  {:pre [(p/monomial? m)
         (p/polynomial? p)]}
  (let [[mono-expts mono-coeff] (nth (p/bare-terms m) 0)
        expts (transduce (map pi/exponents)
                         xpt/gcd
                         mono-expts
                         (p/bare-terms p))
        coeff (gcd-poly-number p mono-coeff)]
    (swap! gcd-monomials inc)
    (p/terms->polynomial (p/bare-arity m)
                         [(pi/make-term expts coeff)])))

;; The next-toughest case is the GCD of two univariate polynomials. The
;; 'classical' way to do this is with the [Euclidean
;; algorithm](https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclidean_algorithm)
;; for univariate polynomials. This method can be extended to multivariate
;; polynomials by using [[p/lower-arity]] to push

(defn- euclidean-gcd
  "Given some multivariate `gcd` function, returns a function of polynomials `u`
  and `v` that returns greatest common divisor of `u` and `v` using
  the [Euclidean algorithm for multivariate
  polynomials](https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclidean_algorithm).

  `u` and `v` are assumed to be either non-polynomial coefficients or univariate
  polynomials. To use [[euclidean-gcd]] for multivariate polynomials, convert
  the polynomial to univariate first using [[p/lower-arity]] recursively."
  [gcd]
  (fn [u v]
    (maybe-bail-out! "euclid inner loop")
    (or (trivial-gcd u v)
        (let [[r _] (p/pseudo-remainder u v)]
          (if (v/zero? r)
            (g/abs v)
            (let [[_ prim] (->content+primitive r gcd)]
              (recur v prim)))))))

(defn- univariate-gcd
  "Given two univariate polynomials `u` and `v`, returns the greatest common
  divisor of `u` and `v` calculated using Knuth's algorithm 4.6.1E."
  [u v]
  {:pre [(p/univariate? u)
         (p/univariate? v)]}
  (cont-> [u v]
          (with-content-removed primitive-gcd)
          (euclidean-gcd primitive-gcd)))

;; Multivariate GCD builds on the univariate case

(defn- full-gcd
  "gcd is just a wrapper for this function, which does the real work
  of computing a polynomial gcd. Delegates to [[univariate-gcd]] for univariate
  polynomials.

  TODO move the wrapping to with-wrapped or something."
  ([u v] (full-gcd 0 u v))
  ([level u v]
   (letfn [(attempt [u v]
             (or (trivial-gcd u v)
                 (let [arity (p/check-same-arity u v)]
                   (cond (p/monomial? u) (monomial-gcd u v)
                         (p/monomial? v) (monomial-gcd v u)
                         (= arity 1) (univariate-gcd u v)
                         :else
                         (let [rec (fn [u v]
                                     (full-gcd (inc level) u v))
                               next-gcd (->gcd rec)]
                           (maybe-bail-out! "polynomial GCD")
                           (cont-> [u v]
                                   p/with-lower-arity
                                   (with-content-removed next-gcd)
                                   (euclidean-gcd next-gcd)))))))]
     (dbg level "multivariate-gcd" u v)
     (let [result (cached attempt u v)]
       (dbg level "<-" result)
       result))))

(defn classical-gcd [u v]
  (cont-> [u v]
          (with-trivial-constant-gcd-check primitive-gcd)
          with-optimized-variable-order
          full-gcd))

(defn- gcd-dispatch
  "Dispatcher for GCD routines.

  TODO isn't... the defmethod sort of the dispatcher??"
  ([] 0)
  ([u] u)
  ([u v]
   (or (trivial-gcd u v)
       (cond
         (not (and (every? v/exact? (p/coefficients u))
                   (every? v/exact? (p/coefficients v))))
         (v/one-like u)

         :else
         (with-limited-time *poly-gcd-time-limit*
           (fn [] (classical-gcd u v))))))
  ([u v & more]
   (reduce gcd-dispatch u (cons v more))))

(def ^{:doc "main GCD entrypoint."}
  gcd
  #'gcd-dispatch)

(defn lcm [u v]
  (if (or (p/polynomial? u)
          (p/polynomial? v))
    (let [g (gcd u v)]
      (p/abs
       (p/mul (p/evenly-divide u g) v)))
    (g/lcm u v)))

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

(defmethod g/lcm [::p/polynomial ::p/polynomial] [u v] (lcm u v))
(defmethod g/lcm [::p/coeff ::p/polynomial] [u v] (lcm u v))
(defmethod g/lcm [::p/polynomial ::p/coeff] [u v] (lcm u v))

(defn- gcd-seq
  "Compute the GCD of a sequence of polynomials (we take care to break early if
  the gcd of an initial segment is [[sicmutils.value/one?]])"
  [items]
  (transduce (ua/halt-at v/one?)
             gcd-dispatch
             items))

(defn gcd-Dp
  "Compute the gcd of the all the partial derivatives of p."
  [p]
  (if (p/polynomial? p)
    (gcd-seq (p/partial-derivatives p))
    1))

;; NOTE from Colin:
;;
;; many of the gcds we find when attempting the troublesome GCD are the case
;; where we have two monomials. This can be done trivially without lowering
;; arity.
;;
;; open question: why was dividing out the greatest monomial factor such a lose?
;; it's much cheaper to do that than to find a gcd by going down the arity
;; chain.
