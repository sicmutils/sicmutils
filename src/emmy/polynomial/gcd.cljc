#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.polynomial.gcd
  (:require [clojure.set :as cs]
            [clojure.string :as string]
            #?(:cljs [goog.string :refer [format]])
            [emmy.generic :as g]
            [emmy.polynomial :as p]
            [emmy.polynomial.exponent :as xpt]
            [emmy.polynomial.impl :as pi]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.util.stopwatch :as us]
            [emmy.value :as v]
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
  [[emmy.util.stopwatch]]. If Euclidean GCD takes longer than this time
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
  (let [coeffs (p/coefficients p)]
    (if (= 1 (count coeffs))
      (let [content   (first coeffs)
            primitive (p/map-coefficients (fn [_] 1) p)]
        [content primitive])
      (let [content   (apply gcd coeffs)
            primitive (if (v/one? content)
                        p
                        (p/map-coefficients
                         #(g/exact-divide % content) p))]
        [content primitive]))))

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
  (->gcd (fn [l r]
           (if (and (v/number? l)
                    (v/number? r))
             (g/gcd l r)
             1))))

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

(defn ^:no-doc monomial-gcd
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
;; polynomials by using [[p/lower-arity]] to push all but the first variable
;; into the coefficients, and passing a `gcd` argument to [[euclidean-gcd]] that
;; will recursively do the same until we hit bottom, at a univariate polynomial
;; with non-polynomial coefficients.

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

;; The next function pairs [[euclidean-gcd]] with one of our continuations from
;; above; [[with-content-removed]] removes the initial greatest common divisor
;; of the coefficients of `u` and `v` before calling [[euclidean-gcd]], to keep
;; the size of the coefficients small.

(defn univariate-gcd
  "Given two univariate polynomials `u` and `v`, returns the greatest common
  divisor of `u` and `v` calculated using Knuth's algorithm 4.6.1E."
  [u v]
  {:pre [(p/univariate? u)
         (p/univariate? v)]}
  (cont-> [u v]
          (with-content-removed primitive-gcd)
          (euclidean-gcd primitive-gcd)))

;; [[full-gcd]] extends [[univariate-gcd]] by using [[p/with-lower-arity]]
;; and [[with-content-removed]] to recursively handle the first, principal
;; variable using [[euclidean-gcd]], but passing a recursive call
;; to [[full-gcd]] that the functions will use to handle their
;; coefficients (which are polynomials of one less arity!)

(defn full-gcd
  "Given two polynomials `u` and `v` (potentially multivariate) with
  non-polynomial coefficients, returns the greatest common divisor of `u` and
  `v` calculated using a multivariate extension of Knuth's algorithm 4.6.1E.

  Optionally takes a debugging `level`. To see the debugging logs generated over
  the course of the run, set [[*poly-gcd-debug*]] to true.

  NOTE: [[full-gcd]] Internally checks that it hasn't run out a stopwatch set
  with [[with-limited-time]]; you can wrap a call to [[full-gcd]] in this
  function to limit its execution time.

  For example, this form will throw a TimeoutException after 1 second:

  ```clojure
  (with-limited-time [1 :seconds]
    (fn [] (full-gcd u v)))
  ```"
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
                           (maybe-bail-out! "full-gcd")
                           (cont-> [u v]
                                   p/with-lower-arity
                                   (with-content-removed next-gcd)
                                   (euclidean-gcd next-gcd)))))))]
     (dbg level "full-gcd" u v)
     (let [result (cached attempt u v)]
       (dbg level "<-" result)
       result))))

(defn classical-gcd
  "Higher-level wrapper around [[full-gcd]] that:

  - optimizes the case where `u` and `v` share no variables
  - sorts the variables in `u` and `v` in order of increasing degree

  before attempting [[full-gcd]]. See [[full-gcd]] for a full description."
  [u v]
  (cont-> [u v]
          (with-trivial-constant-gcd-check primitive-gcd)
          with-optimized-variable-order
          full-gcd))

(defn- gcd-dispatch
  "Dispatches to [[classical-gcd]] with an enforced time limit
  of [[*poly-gcd-time-limit*]].

  NOTE this function is the place to add support for other GCD methods, like
  sparse polynomial GCD, that are coming down the pipe."
  [u v]
  (or (trivial-gcd u v)
      (with-limited-time *poly-gcd-time-limit*
        (fn [] (classical-gcd u v)))))

(def
  ^{:doc "Returns the greatest common divisor of `u` and `v`, calculated by a
  multivariate extension to the [Euclidean algorithm for multivariate
  polynomials](https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclidean_algorithm).

  `u` and `v` can be polynomials or non-polynomials coefficients."
    :arglists '([]
                [u]
                [u v]
                [u v & more])}
  gcd
  (ua/monoid gcd-dispatch 0))

(defn lcm
  "Returns the least common multiple of (possibly polynomial) arguments `u` and
  `v`, using [[gcd]] to calculate the gcd portion of

  ```
  (/ (g/abs (* u v))
     (gcd u v))
  ```"
  [u v]
  (if (or (p/polynomial? u)
          (p/polynomial? v))
    (let [g (gcd-dispatch u v)]
      (p/abs
       (p/mul (p/evenly-divide u g) v)))
    (g/lcm u v)))

(defn gcd-Dp
  "Returns the greatest common divisor of all partial derivatives of the
  polynomial `p` using binary applications of the [[gcd]] algorithm between each
  partial derivative.

  This algorithm assumes that all coefficients are integral, and halts when it
  encounters a result that responds true to [[emmy.value/one?]].

  If a non-[[p/Polynomial]] is supplied, returns 1."
  [p]
  (if (p/polynomial? p)
    (transduce (halt-when v/one?)
               gcd
               (p/partial-derivatives p))
    1))

;; ## Generic GCD Installation
;;
;; The following block installs appropriate GCD and LCM routines between
;; polynomial and coefficient instances.

(p/defbinary g/lcm lcm)

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
