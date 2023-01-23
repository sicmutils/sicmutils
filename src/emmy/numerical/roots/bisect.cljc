#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.roots.bisect
  "This namespace contains implementations of a number of methods for root-finding
  on real-valued functions of one argument.

  NOTE: This namespace is not yet stable: Expect these functions to change as
  new root-finding methods are added. "
  (:require [emmy.util :as u]
            [emmy.util.stream :as us]
            [emmy.value :as v]))

;; ## Root finding by successive bisection
;;
;; The [[bisect]] function below is really a combination of three methods of
;; root-finding; bisection search, the [Secant
;; method](https://en.wikipedia.org/wiki/Secant_method) and a mixed method found
;; in scmutils.
;;
;; NOTE: As we bring in methods like `zbrent` it could be that the shell below
;; should be shared for ALL root finding methods.

(def ^{:dynamic true
       :doc "Controls the default behavior of [[bisect]]'s search.
  See [[bisect]]'s docstring for more info."}
  *bisect-break* 60)

(def ^{:doc "Set of all methods allowed as `:method` options to [[bisect]]."}
  all-methods
  #{:bisection :secant :mixed})

;; ## Success / Failure Utilities

(defn- succeed
  "Given some point `x` and its value `fx`, the number of `iterations` of the
  root-finding algorithm, and the total number of calls `fncalls` of the
  function `f`, returns a data structure representing successful completion."
  [x fx iterations fncalls]
  {:result     x
   :value      fx
   :iterations iterations
   :converged? true
   :fncalls    fncalls})

(defn- fail
  "Generates a 'failure'-type message."
  [message a fa b fb iterations fncalls]
  {:error message
   :bounds
   {:lower a :f-lower fa
    :upper b :f-upper fb}
   :iterations iterations
   :converged? false
   :fncalls fncalls})

(defn ^:no-doc midpoint
  "Implements the midpoint lookup given endpoints `a` and `b` of a range. Used in
  the implementation of the [Bisection
  method](https://en.wikipedia.org/wiki/Bisection_method#Iteration_tasks)"
  [a b]
  (* 0.5 (+ a b)))

(defn ^:no-doc secant-root
  "Given two endpoints `[a fa]` and `[b fb]`, returns the root of a line drawn
  between the two endpoints. Used in the implementation of the
  [Secant
  method](https://en.wikipedia.org/wiki/Secant_method)

  NOTE that the signs of `fa` and `fb` must be opposite for the result to make
  sense in the context of the secant method."
  [a fa b fb]
  (/ (- (* fb a) (* fa b))
     (- fb fa)))

(defn- next-point-fn
  "Given a [[bisect]] options map, returns a function of `a, fa, b, fb,
  iterations` that will generate the next candidate point for a root-finding
  method search."
  [{:keys [method n-break]}]
  (case (or method :mixed)
    :bisection (fn [a _fa b _fb _iter]
                 (midpoint a b))
    :secant (fn [a fa b fb _iter]
              (secant-root a fa b fb))
    :mixed (let [n-break (or n-break *bisect-break*)]
             (fn [a fa b fb iter]
               (if (< iter n-break)
                 (midpoint a b)
                 (secant-root a fa b fb))))
    (u/illegal
     (str "Method not supported: " method))))

(defn bisect
  "Given some function `f` and (inclusive) lower and upper bounds `a` and `b` on
  the domain, attempts to find a root of `f` in that range, ie, a value `x` for
  which `(f x)` is equal to 0.

  Supports the following optional keyword arguments:

  `:method`: can be `:bisection`, `:secant` or `:mixed`. See the Methods section
  below for a description of each. Defaults to `:mixed`

  `:eps`: defaults to [[emmy.value/machine-epsilon]].

  `:callback`: if supplied, the supplied `f` will be invoked at each
  intermediate point with the iteration count and the values of x and f(x) at
  each search step.

  `:maxiter`: maximum number of iterations allowed for the minimizer. Defaults to
  1000.

  `:maxfun` maximum number of times the function can be evaluated before
  exiting. Defaults to `(inc maxiter)`.

  `:n-break` defaults to the dynamically bindable `*bisect-break*` (which
  defaults to 60). Bind `*bisect-break*` to modify the behavior of the `:mixed`
  method (see below) when it's used inside a nested routine. Ignored if method
  is not `:mixed`.

  ## Methods

  - `:bisection` causes [[bisect]] to use the [Bisection
    method](https://en.wikipedia.org/wiki/Bisection_method); at each iteration,
    the midpoint between the bounds is chosen as the next point.

  - `:secant` uses the [Secant
    method](https://en.wikipedia.org/wiki/Secant_method); each candidate point is
    chosen by taking the root of a line drawn between the two endpoints `[a (f
    a)]` and `[b (f b)]`. This method is most useful when the bounds are close to
    the root.

  - `:mixed` uses `:bisection` up until `:n-break` iterations and `:secant`
    beyond. This can be useful for narrowing down a very wide range close to the
    root, and then switching in to a faster search method."
  ([f a b] (bisect f a b {}))
  ([f a b {:keys [eps
                  maxiter
                  maxfun
                  callback]
           :or {eps     v/machine-epsilon
                maxiter 1000
                callback (constantly nil)}
           :as opts}]
   (let [close?        (us/close-enuf? eps)
         get-next-pt   (next-point-fn opts)
         maxfun        (or maxfun (inc maxiter))
         [a b]         [(min a b) (max a b)]
         [f-counter f] (u/counted f)]
     (loop [a a, fa (f a)
            b b, fb (f b)
            iteration 0]
       (cond (zero? fa) (succeed a fa iteration @f-counter)
             (zero? fb) (succeed b fb iteration @f-counter)

             (pos? (* fa fb))
             (fail "Root not bounded" a fa b fb iteration @f-counter)

             (or (> iteration maxiter)
                 (> @f-counter maxfun))
             (fail "Iteration bounds exceeded" a fa b fb iteration @f-counter)

             :else
             (let [mid  (get-next-pt a fa b fb iteration)
                   fmid (f mid)]
               (callback mid fmid iteration)
               (if (close? a b)
                 (succeed a fa iteration @f-counter)
                 (if (pos? (* fb fmid))
                   (recur a fa mid fmid (inc iteration))
                   (recur mid fmid b fb (inc iteration))))))))))

;; If we don't know anything, it is usually a good idea to break the interval
;; into dx-sized pieces and look for roots in each interval.

(defn search-for-roots
  "Given a smooth function `f` and (inclusive) lower and upper bounds `a` and
  `b` on the domain, attempts to find all roots of `f`, ie, a vector of values
  `x_n` such that each `(f x_n)` is equal to 0.

  [[search-for-roots]] first attempts to cut the (inclusive) range `[a, b]`
  into pieces at most `dx` wide; then [[bisect]] is used to search each segment
  for a root.

  All `opts` supplied are passed on to [[bisect]]."
  ([f a b dx]
   (search-for-roots f a b dx {}))
  ([f a b dx opts]
   (letfn [(find-roots [a b]
             (let [f1 (f b) f0 (f a)]
               (if (< (Math/abs (- b a)) dx)
                 (if (neg? (* f0 f1))
                   (let [result (bisect f a b opts)]
                     (if (:converged? result)
                       [(:result result)]
                       []))
                   [])
                 (let [m (midpoint a b)]
                   (into (find-roots a m)
                         (find-roots m b))))))]
     (find-roots a b))))
