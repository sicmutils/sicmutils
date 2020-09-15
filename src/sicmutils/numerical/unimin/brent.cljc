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

(ns sicmutils.numerical.unimin.brent
  "This namespace contains an implementation of Brent's method for finding the
  minimum of a real-valued function."
  (:require [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            [sicmutils.numerical.unimin.bracket :as ub]
            [sicmutils.numerical.unimin.golden :as ug])
  #?(:clj
     (:import (org.apache.commons.math3.optim.univariate
               BrentOptimizer
               UnivariateObjectiveFunction
               SearchInterval
               UnivariatePointValuePair)
              (org.apache.commons.math3.analysis
               UnivariateFunction)
              (org.apache.commons.math3.optim.nonlinear.scalar
               GoalType
               ObjectiveFunction)
              (org.apache.commons.math3.optim
               MaxEval
               MaxIter
               OptimizationData
               ConvergenceChecker
               PointValuePair))))

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

  - If `x` is already within 2*tol of either edge, `delta` is replaced with
    `tol1` pointing back toward the center of the interval `(a, b)`.

  - If `delta < tol1`, returns `x + tol1` in the direction of `delta`, to force
    a step of at least `tol1`.

  NOTE tol2 == 2*tol1."
  [a x b delta tol1 tol2]
  (let [near-edge? (or (< (- x a) tol2)
                       (< (- b x) tol2))]
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

          :else (+ x delta))))

(defn- update-history
  "Updates the brent history. This basically tries to pick out the two previous
  NON-best candidates, equivalent to the pair $(b_{k-2}, b_{k-1})$ in Brent's
  method.

  There is some finicky stuff going on with equalities here. That is ONLY to
  deal with the fact that we initialize v and w to `x`. If that went away, this
  would get simpler.

  Returns $(b_{k-2}, b_{k-1})$.
  "
  [[xnew fnew] [xx2 fx2 :as x2] [xx1 fx1 :as x1] [xx fx :as x]]
  (cond (<= fnew fx)                              [x1 x]
        (or (<= fnew fx1) (= xx1 xx))             [x1 [xnew fnew]]
        (or (<= fnew fx2) (= xx2 xx) (= xx2 xx1)) [[xnew fnew] x1]
        :else [x2 x1]))

(defn brent-min
  "Find the minimum of the function f: R -> R in the interval [a,b].

  If observe is supplied, will be invoked with the iteration count and the
  values of x and f(x) at each search step.

  Scipy code for the brent optimizer:
  https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2193-L2269

  Actual code:
  https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2028

  The simplest way to think about this is that it's doing a golden section
  search, just like the simpler thing. But periodically it decides to do a
  parabolic jump to try and converge faster.

  TODO Threshold discussion.

  TODO: discuss how we initialize our bracket. Right now (as with the java
  version) we simply takes the cut in between them. This is what Fortran does too.

  The python version actually calls `bracket` and gets whatever the return value
  is there. The scipy version in `fminbound` starts with a golden section jump,
  which sort of makes more sense.

  And then the scipy version, `brent`, expands to search for a bound... but
  otherwise it's the same.

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
         xmid          (ug/golden-cut b a)#_(* 0.5 (+ a b))
         mid           [xmid (f xmid)]]
     (loop [[a [xx fx :as x] b] [a mid b]
            [x2 x1] [mid mid]
            target 0 ;; twice the target
            delta  0 ;; step size for the previous iteration.
            iteration 0]
       (let [;; `tol` is the minimum possible step you can take. If you take
             ;; this, you're guaranteed to be different from the previous value
             ;; by at least "relative threshold", even for tiny values of `x`.
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
                   (let [[p q] (ub/parabolic-pieces x1 x x2)]
                     (if (parabola-valid? a xx b target p q)
                       [delta (/ p q)]
                       (golden-section-step a xx b))))

                 xnew   (apply-delta a xx b new-delta tol tol2)
                 new-pt [xnew (f xnew)]

                 [[xl fl :as l] [xr fr :as r]] (if (< xnew xx)
                                                 [new-pt x]
                                                 [x new-pt])]
             (recur (if (<= fl fr)
                      [a l xr]
                      [xl r b])
                    (update-history new-pt x2 x1 x)
                    new-target
                    new-delta
                    (inc iteration)))))))))

(defn brent-max
  "For convenience, we also provide the sister-procedure for finding
  the maximum of a unimodal function.

  Negate the function, minimize, negate the result."
  [f a b opts]
  (let [-f (comp g/negate f)]
    (-> (brent-min -f a b opts)
        (update-in [:value] g/negate))))

#?(:clj
   (defn brent-min-commons
     [f a b {:keys [relative-threshold
                    absolute-threshold
                    maxiter
                    maxfun
                    callback
                    goaltype]
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
                (converged [_ iter previous current]
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
           p (.optimize o args)]
       (let [xx (.getPoint p)
             fx (.getValue p)]
         (callback (.getIterations o) xx fx)
         (println "#" @f-counter)
         {:result xx
          :value fx
          :iterations (.getIterations o)
          :converged? true
          :fncalls @f-counter}))))

#?(:clj
   (defn brent-max-commons
     "For convenience, we also provide the sister-procedure for finding
  the maximum of a unimodal function.

  Negate the function, minimize, negate the result."
     [f a b opts]
     (let [-f (comp g/negate f)]
       (-> (brent-min-commons -f a b opts)
           (update-in [:value] g/negate)))))
