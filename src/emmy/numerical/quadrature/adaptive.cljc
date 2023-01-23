#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.adaptive
  (:require [emmy.numerical.quadrature.common :as qc]
            [emmy.util.aggregate :as ua]))

;; ## Adaptive Quadrature
;;
;; This namespace holds an implementation of "adaptive quadrature" usable with
;; any quadrature method in the library.
;;
;; The implementation was inspired by the `numerics/quadrature/rational.scm`
;; file in
;; the [scmutils](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt)
;; package. In that library, adaptive quadrature was special-cased to the
;; Bulirsch-Stoer algorithm, ported in `bulirsch_stoer.cljc`.
;;
;; ## Overview
;;
;; Most of the integrators in `quadrature` work by successively refining an
;; integration interval $a, b$ down into evenly-spaced integration slices. Some
;; functions are very well behaved in some regions, and then oscillate wildly in
;; others.
;;
;; Adaptive quadrature methods partition $a, b$ into intervals of different
;; sizes, allowing the integrator to drill in more closely to areas that need
;; attention.
;;
;; The `adaptive` implementation below works like this:
;;
;; - use a wrapped `integrate` function on the full $a, b$, capping the
;;   iterations at some configurable limit (`*adaptive-maxterms*`below)
;;
;; - If `integrate` fails to converge, split $a, b$ into two intervals and
;;   attempt to converge both sides using `integrate`
;;
;; - Continue this process until convergence, or until the interval becomes
;;   small enough to fail the test in `common/narrow-slice?`, at which point
;;   `integrate` returns an estimate for a single slice.
;;
;; The `*adaptive-maxterms*` variable is dynamic, which means you can adjust the
;; behavior of `adaptive` by supplying the `:maxterms` option directly, or
;; wrapping your call in `(binding [*adaptive-maxterms* 8] ,,,)`.

(def ^:dynamic *adaptive-maxterms* 10)

;; ## Fuzzy Midpoints
;;
;; Symmetric intervals like $-1, 1$ often show up with integrands with
;; singularities right at the center of the midpoint. For this reason,
;; `adaptive` is able to customize its splitting behavior using the
;; `*neighborhood-width*` dynamic variable.
;;
;; By default, when partitioning an interval, `adaptive` will choose an interval
;; within 5% of the midpoint. Override this behavior with the
;; `:adaptive-neighborhood-width` key in the options dict, or by binding this
;; dynamic variable.

(def ^:dynamic *neighborhood-width* 0.05)

(defn- split-point
  "Returns a point within`fuzz-factor` of the midpoint of the interval $[a, b]$.
  `fuzz-factor` defaults to 0 (ie, `split-point` returns the midpoint)."
  ([a b] (split-point a b 0))
  ([a b fuzz-factor]
   {:pre [(>= fuzz-factor 0)
          (< fuzz-factor 1)]}
   (let [width  (- b a)
         offset (if (zero? fuzz-factor)
                  0.5
                  (+ 0.5 (* fuzz-factor (dec (rand 2.0)))))]
     (+ a (* offset width)))))

;; ## Main Implementation
;;
;; The implementation below takes /two/ integrate functions, not the one
;; described above. This allows us to handle open and closed intervals, instead
;; of introducing open endpoints at every subdivision. All internal intervals
;; that don't touch an open endpoint are considered closed.


(defn- fill-defaults
  "Populates the supplied `opts` dictionary with defaults required by `adaptive`.
  Two of these have values controlled by dynamic variables in `adaptive.cljc`."
  [opts]
  (merge {:maxterms *adaptive-maxterms*
          :adaptive-neighborhood-width *neighborhood-width*
          :interval qc/open}
         opts))

(defn adaptive
  "Accepts one or two 'integrators', ie, functions of:

  - `f`: some integrand
  - `a` and `b`: the lower and upper endpoints of integration
  - `opts`, a dictionary of configuration options

  And returns a new integrator that adaptively subdivides the region $a, b$ into
  intervals if integration fails to converge. If two integrators are supplied,
  the first is applied to any interval that's not explicitly closed on both
  ends, and the second integrator is applied to explicitly closed intervals.
  This behavior depends on the interval set in the supplied `opts` dictionary.

  All `opts` will be passed through to the supplied `integrate` functions.

  ### Optional arguments relevant to `adaptive`:

  `:maxterms`: defaults to `*adaptive-maxterms*`. This is passed to the
  underlying integrators, and determines how long each interval attempts to
  converge before a subdivision.

  `:adaptive-neighborhood-width`: When dividing an interval, the split point
  will be within this factor of the midpoint. Set `:adaptive-neighborhood-width`
  to 0 for deterministic splitting."
  ([integrator] (adaptive integrator integrator))
  ([open-integrator closed-integrator]
   (fn rec
     ([f a b] (rec f a b {}))
     ([f a b opts]
      (let [opts      (fill-defaults opts)
            integrate (fn [l r interval]
                        (if (qc/closed? interval)
                          (closed-integrator f l r opts)
                          (open-integrator f l r opts)))]
        (loop [stack [[a b (:interval opts)]]
               sum   (ua/*fold*)
               iteration 0]
          (if (empty? stack)
            {:converged? true
             :iterations iteration
             :result (ua/*fold* sum)}
            (let [[l r interval] (peek stack)
                  remaining      (pop stack)
                  {:keys [converged? result]} (integrate l r interval)]
              (if converged?
                (recur remaining
                       (ua/*fold* sum result)
                       (inc iteration))
                (let [midpoint (split-point l r (:adaptive-neighborhood-width opts))]
                  (recur (conj remaining
                               [midpoint r (qc/close-l interval)]
                               [l midpoint (qc/close-r interval)])
                         sum
                         (inc iteration))))))))))))

;; ## Suggestions for Improvement
;;
;; ### Iteration Limit
;;
;; `adaptive` runs until it completes, with no facility available to bail out of
;; computation. An iteration limit would be a great addition... but it won't be
;; efficient without some way of prioritizing high-error subintervals for
;; refinement, as discussed next.
;;
;; ### Priority Queue on Error
;;
;; Another nice upgrade would be a version of `adaptive` that is able to return
;; an actual sequence of successive refinements. to the estimate. The current
;; implementation uses an internal stack to track the subdivided intervals it
;; needs to evaluate. If the integrator was able to provide an error estimate,
;; we could instead use a priority queue, prioritized by error.
;;
;; `adaptive-sequence` could then return a sequence where every element
;; processes a single refinement. Even without this upgrade, the priority queue
;; idea would allow the estimate to converge quickly and be more accurate if we
;; bailed out at some max number of iterations.
;;
;; This article holds a related implementation:
;; http://www.learningclojure.com/2011/05/numerical-integration-better_29.html

;; ## References
;;
;; - SCMUtils Refman: https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt
;; - Wikipedia, "Adaptive Simpson's Method": https://en.wikipedia.org/wiki/Adaptive_Simpson%27s_method
