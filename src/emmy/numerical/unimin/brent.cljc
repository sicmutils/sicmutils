#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.unimin.brent
  "This namespace contains an implementation of Brent's method for finding the
  minimum of a real-valued function."
  (:require [emmy.generic :as g]
            [emmy.numbers]
            [emmy.numerical.unimin.bracket :as ub]
            [emmy.numerical.unimin.golden :as ug]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (org.apache.commons.math3.optim.univariate
               BrentOptimizer
               UnivariateObjectiveFunction
               SearchInterval
               UnivariatePointValuePair)
              (org.apache.commons.math3.analysis
               UnivariateFunction)
              (org.apache.commons.math3.optim.nonlinear.scalar
               GoalType)
              (org.apache.commons.math3.optim
               MaxEval
               MaxIter
               OptimizationData
               ConvergenceChecker))))

(defn- terminate?
  "Brent's method terminates (ie converges) when `a` and `b` are narrow enough
  that `x` falls within `2 * tolerance` of both `a` and `b`, ie:

  `max(x - a, b - x) <= 2tol`

  From Numerical Recipes:

  \"A typical ending configuration for Brent's method is that $a$ and $b$
  are `(* 2 midpoint tol)` apart, with $x$ (the best abscissa) at the midpoint
  of $a$ and $b$, and therefore fractionally accurate to +-tol.\" ~Numerical
  Recipes, 397.
  "
  [a x b tol2]
  (let [half-ab (* 0.5 (- b a))
        mid     (* 0.5 (+ a b))
        mid->x  (g/abs (- x mid))]
    (<= (+ mid->x half-ab) tol2)))

(defn- golden-section-step
  "Returns a pair of:

  - The interval width between the new `a` and `b` after the algorithm completes
    this golden step
  - The delta that needs to be applied to `x` to take it into the larger of the
    two gaps between `a` and `b,` ie, to `new_x`:

  `xa---------new_x<---xx------xb`

  NOTE that in Brent's algorithm, a golden section step wipes out the tracked
  history of `p/q` parabolic steps that the algorithm uses to decide between
  parabolic and golden section steps.

  The goal of the dance is to force a golden section step every $log_2((b - a) /
  tol1)$ steps by allowing a parabolic step as long as it halves the step taken
  two iterations ago. Without this reset the algorithm would attempt fewer
  beneficial parabolic steps."
  [a x b]
  (let [midpoint (* 0.5 (+ a b))
        new-width (if (>= x midpoint)
                    (- a x)
                    (- b x))
        step (* ug/inv-phi2 new-width)]
    [new-width step]))

(defn- parabola-valid?
  "The parabolic step `p/q` is valid if the step:

  - keeps the candidate point `x` inbounds, ie, `a < x + p/q < b`
  - is at least 1/2 of `target`.

  `target` is a slightly tricky quantity, and depends on the type of step taken
  in the previous two iterations:

  -  *, golden: `target` == the full `(a, b)` interval `b - a`
  -  golden, *: `target` == the size of the step taken by the previous golden
  - para, para: `target` == `p/q` from two steps ago

  The idea is to force a golden section step every so often by narrowing the
  allowed jump that a parabolic step is allowed to take, and then widening the
  band to the full range whenever a golden section step occurs.

  Why the step before last? Experimentally (according to Brent) it works better
  than considering only the previous step, and aesthetically it feels right to
  give parabolic interpolation more than one try to do a nice job."
  [a x b target p q]
  (let [inbounds? (and (> p (* q (- a x)))
                       (< p (* q (- b x))))
        lt-half-target? (< (g/abs p) (g/abs (* 0.5 q target)))]
    (and inbounds? lt-half-target?)))

(defn- apply-delta
  "Returns `x + delta`, guarding against any `delta` addition that would return a
  value outside of `(a, b)`. Specifically guards against these two cases:

  - If `x` + the supplied `delta` results in a point within 2*tol of either
    edge, `delta` is replaced with `tol1` pointing back toward the center of the
    interval `(a, b)`.

  - If `delta < tol1`, returns `x + tol1` in the direction of `delta`, to force
    a step of at least `tol1`.

  NOTE tol2 == 2*tol1."
  [a x b delta tol1 tol2]
  (let [x+delta    (+ x delta)
        near-edge? (or (< (- x+delta a) tol2)
                       (< (- b x+delta) tol2))]
    (cond near-edge?
          (let [middle (* 0.5 (+ a b))]
            (if (<= x middle)
              (+ x tol1)
              (- x tol1)))

          ;; tiny delta?
          (< (g/abs delta) tol1)
          (if (pos? delta)
            (+ x tol1)
            (- x tol1))

          :else x+delta)))

(defn- update-history
  "Brent's method tracks the two best (non-candidate) points, so they can be used
  to fit a candidate parabolic step.

  This function accepts:

  - `x2` and `x1`, the previous two best non-candidates;
  - `x`, the previous candidate
  - `new-pt` the current new point

  and returns the third- and second-best points, ie, the new `[x2, x1]`.

  NOTE on the implementation: the assumption is that `x2` and `x1` will be
  initialized to `x`, and that they'll be replaced by potentially WORSE values
  that appear for the first two steps.
  "
  [[xx2 fx2 :as x2] [xx1 fx1 :as x1] [xx fx :as x] [xnew fnew]]
  (cond (<= fnew fx)                              [x1 x]
        (or (<= fnew fx1) (= xx1 xx))             [x1 [xnew fnew]]
        (or (<= fnew fx2) (= xx2 xx) (= xx2 xx1)) [[xnew fnew] x1]
        :else [x2 x1]))

(defn brent-min
  "Find the minimum of the function f: R -> R in the interval [a,b] using Brent's
  Method, described by Richard Brent in [Algorithms for Minimization without
  Derivatives](https://books.google.com/books?id=AITCAgAAQBAJ&q=Brent%E2%80%99s#v=onepage&q=Parabolic&f=false).

  Brent's method is a combination of a golden section search with a parabolic
  interpolation step. Parabolic interpolation can go wild if the candidate point
  is close to colinear with the search bounds, or of the points are too close
  together.

  Brent's method prevents this by applying an internal test that forces a golden
  section step every so often. (If you want the details, see `parabola-valid?`
  above.)

  Supports the following optional keyword arguments:

  `:callback` if supplied, the supplied fn will be invoked at each intermediate
  point with the iteration count and the values of x and f(x) at each search
  step.

  `:relative-threshold` defaults to around 1.49e8, the sqrt of the machine
  tolerance. You won't gain any benefit attempting to set the value less than
  the default.

  `:absolute-threshold` a smaller absolute threshold that applies when the
  candidate minimum point is close to 0.

  `:maxiter` Maximum number of iterations allowed for the minimizer. Defaults to
  1000.

  `:maxfun` Maximum number of times the function can be evaluated before
  exiting. Defaults to `(inc maxiter)`.
  "
  ([f a b] (brent-min f a b {}))
  ([f a b {:keys [relative-threshold
                  absolute-threshold
                  maxiter
                  maxfun
                  callback]
           :or {relative-threshold (g/sqrt v/machine-epsilon)
                absolute-threshold 1.0e-11
                maxiter 1000
                callback (constantly nil)}}]
   (let [maxfun        (or maxfun (inc maxiter))
         [a b]         [(min a b) (max a b)]
         [f-counter f] (u/counted f)
         xmid          (* 0.5 (+ a b))
         mid           [xmid (f xmid)]]
     (loop [
            ;; a and b bound the interval in which the minimizer is searching.
            ;; `xx` is the current candidate point, and `fx` is its value.
            [a [xx fx :as x] b] [a mid b]

            ;; The second-best and best points considered prior to this
            ;; iteration and its candidate point.
            [x2 x1] [mid mid]

            ;; This value is used by `parabola-valid?` to decide whether or not
            ;; it's appropriate to proceed with the parabolic step, or fall back
            ;; to a golden section step. `parabola-valid?`'s docstring describes
            ;; the logic.
            target 0

            ;; step size taken by the previous iteration.
            delta  0
            iteration 0]
       (let [
             ;; The total tolerance for the algorithm, and also the minimum
             ;; possible step to take during each iteration.
             tol  (+ absolute-threshold (* relative-threshold (g/abs xx)))
             tol2 (* 2 tol)
             converged? (terminate? a xx b tol2)]
         (callback iteration xx fx)
         (if (or (> iteration maxiter)
                 (> @f-counter maxfun)
                 converged?)
           {:result     xx
            :value      fx
            :iterations iteration
            :converged? converged?
            :fncalls    @f-counter}
           (let [[new-target new-delta]
                 (if (<= (g/abs target) tol)
                   (golden-section-step a xx b)
                   ;; If the target value is > the combined tolerance, generate
                   ;; a parabolic interpolation using
                   ;;
                   ;; - the previous two best guesses WORSE than the candidate point, and
                   ;; - the current candidate point
                   ;;
                   ;; `p` and `q` are the numerator and denominator of the step
                   ;; required to move to the minimum of the interpolated
                   ;; parabola from the current candidate `x`; ie, `x + p/q`
                   ;; lies at the parabola's minimum.
                   (let [[p q] (ub/parabolic-pieces x1 x x2)]
                     ;; Only proceed with the parabolic step if the new point
                     ;; lies inside `(a, b)` and results in a step >= 1/2
                     ;; target. Otherwise, default to a golden section step.
                     (if (parabola-valid? a xx b target p q)
                       [delta (/ p q)]
                       (golden-section-step a xx b))))

                 xnew   (apply-delta a xx b new-delta tol tol2)
                 new-pt [xnew (f xnew)]

                 ;; `x` and `xnew` both lie within `(a, b)`; assign them to `l`,
                 ;; `r` based on their ordering within the interval...
                 [[xl fl :as l] [xr fr :as r]] (if (< xnew xx)
                                                 [new-pt x]
                                                 [x new-pt])]
             ;; and then tighten the search interval down around the new
             ;; lowest-valued point.
             (recur (if (<= fl fr)
                      [a l xr]
                      [xl r b])
                    (update-history x2 x1 x new-pt)
                    new-target
                    new-delta
                    (inc iteration)))))))))

(defn brent-max
  "For convenience, we also provide the sister-procedure for finding the maximum
  of a unimodal function using Brent's method.

  Negate the function, minimize, negate the result."
  [f a b opts]
  (let [-f (comp g/negate f)]
    (-> (brent-min -f a b opts)
        (update :value g/negate))))

#?(:clj
   (defn brent-min-commons
     "Find the minimum of the function f: R -> R in the interval [a,b] using
  Brent's Method, described by Richard Brent in [Algorithms for Minimization
  without
  Derivatives](https://books.google.com/books?id=AITCAgAAQBAJ&q=Brent%E2%80%99s#v=onepage&q=Parabolic&f=false).

   This method is identical to `brent-min` but uses the apache-commons
   implementation of Brent's method. See `brent-min` for more information."
     ([f a b] (brent-min-commons f a b {}))
     ([f a b {:keys [relative-threshold
                     absolute-threshold
                     maxiter
                     maxfun
                     callback]
              :or {relative-threshold (g/sqrt v/machine-epsilon)
                   absolute-threshold 1.0e-11
                   maxiter 1000
                   callback (constantly nil)}}]
      (let [maxfun (or maxfun (inc maxiter))
            [f-counter f] (u/counted f)
            o (BrentOptimizer.
               relative-threshold
               absolute-threshold
               (reify ConvergenceChecker
                 (converged [_ iter _ current]
                   (callback iter
                             (.getPoint ^UnivariatePointValuePair current)
                             (.getValue ^UnivariatePointValuePair current))
                   false)))
            args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
            (into-array OptimizationData
                        [(UnivariateObjectiveFunction.
                          (reify UnivariateFunction
                            (value [_ x]
                              (f x))))
                         (MaxEval. maxfun)
                         (MaxIter. maxiter)
                         (SearchInterval. a b)
                         GoalType/MINIMIZE])
            p  (.optimize o args)
            xx (.getPoint p)
            fx (.getValue p)]
        (callback (.getIterations o) xx fx)
        {:result xx
         :value fx
         :iterations (.getIterations o)
         :converged? true
         :fncalls @f-counter}))))

#?(:clj
   (defn brent-max-commons
     "For convenience, we also provide the sister-procedure for finding the
  maximum of a unimodal function using the apache commons implementation of
  Brent's method.

  Negate the function, minimize, negate the result."
     ([f a b] (brent-max-commons f a b {}))
     ([f a b opts]
      (let [-f (comp g/negate f)]
        (-> (brent-min-commons -f a b opts)
            (update :value g/negate))))))
