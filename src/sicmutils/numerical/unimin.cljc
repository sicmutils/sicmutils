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

(ns sicmutils.numerical.unimin
  "`unimin` is a module of functions and methods designed to find minimal (or
  maximal) values of single variable functions."
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.stopwatch :as us]
            [sicmutils.value :as v]
            [taoensso.timbre :as log])
  #?(:clj
     (:import [org.apache.commons.math3.optim.univariate
               BrentOptimizer
               UnivariateObjectiveFunction
               SearchInterval
               UnivariatePointValuePair]
              [org.apache.commons.math3.analysis
               UnivariateFunction]
              [org.apache.commons.math3.optim.nonlinear.scalar
               GoalType
               ObjectiveFunction]
              [org.apache.commons.math3.optim
               MaxEval
               OptimizationData
               ConvergenceChecker
               PointValuePair])))

;; Okay, what did they actually have here...
;;
;; scipy version: https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2373
;;
;; # Golden Section Method
;;
;; What is this? Here's the algo, a way to find a minimum value of a single
;; variable function: https://en.wikipedia.org/wiki/Golden-section_search
;;
;; Start with the two endpoints, and choose two interior points that fall at the
;; two points you could cut the x axis so that the interval splits into
;; golden-ratioed pieces.
;;
;; If $upper - lower = h$, then the distance from either endpoint needs to be
;; $a$, where ${h \over a} = \phi$, or $a = h \over \phi$.
;;
;; One test point is $lower + h \over \phi$. The other is $upper - h \over
;; \phi$. A nice feature of the golden ratio is that $1 \over \phi = \phi - 1 =
;; {\sqrt{5} - 1} \over 2$.
;;
;; But you ALSO have the nice property

(def ^:private inv-phi  (/ (- (g/sqrt 5) 1) 2))
(def ^:private inv-phi2 (- 1 inv-phi))

(defn golden-cut
  "Returns the point between `from` and `to` that cuts the region between the two
  into two sections in golden-ratioed proportion to each other.

  For example, depending on the ordering of `from` and `to`, `x` would be
  either:

  `from------x1---to`

  `to---x2------from`

  Such that `from->x1 / from->to == to->x2 / from->x1`.
  "
  [from to]
  (+ (* inv-phi2 from)
     (* inv-phi to)))

(defn ^:private shrink-interval
  "Takes three pairs of test (x, f(x)) and returns the pair with the minimal value
  of f(x)."
  [[_ fa :as a]
   [_ fl :as l]
   [_ fr :as r]
   [_ fb :as b]]
  (if (< fl fr)
    [a ]
    (cond (< fa  (min fb fc)) a
          (<= fb (min fa fc)) b
          :else c)))

(defn golden-step [a fa x fx b fb]
  (if (< fl fr)
    (let [new-l (contract xr xa)]
      (recur a [new-l (f new-l)] l r (inc iteration)))
    (let [new-r (golden-cut xl xb)]
      (recur l r [new-r (f new-r)] b (inc iteration)))))



(defn golden-section-min
  "Golden Section search, with supplied convergence test procedure.

  good-enuf?, which is a predicate accepting four arguments:

  [a fa]
  [minx fminx]
  [b fb]
  steps

  - TODO clean up the interface.

  - TODO check against the scipy implementation
  https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2373:5

  - TODO get a sequence of steps, just like in the multi-minimize, and make a
  lazy seq."
  [f a b good-enuf?]
  (let [
        ;; This makes sense, just find the min.
        best-of   (fn [[_ fa :as a]
                      [_ fb :as b]
                      [_ fc :as c]]
                    (cond (< fa (min fb fc)) a
                          (<= fb (min fa fc)) b
                          :else c))
        x1 (golden-cut b a)
        x2 (golden-cut a b)]
    (loop [[xa :as a] [a (f a)]
           [xl fl :as l] [x1 (f x1)]
           [xr fr :as r] [x2 (f x2)]
           [xb :as b] [b (f b)]
           iteration 0]
      (if (< fl fr)
        (if (good-enuf? a l r iteration)
          ;; TODO, get both bounds in here.
          (let [[x v] (best-of a l r)]
            {:result x
             :value v
             :iterations iteration})
          (let [new-l (golden-cut xr xa)]
            (recur a [new-l (f new-l)] l r (inc iteration))))
        (if (good-enuf? l r b iteration)
          (let [[x v] (best-of l r b)]
            {:result x
             :value v
             :iterations iteration})
          (let [new-r (golden-cut xl xb)]
            (recur l r [new-r (f new-r)] b (inc iteration))))))))

(defn golden-section-max
  "For convenience, we also provide the sister-procedure for finding
  the maximum of a unimodal function.

  Negate the function, minimize, negate the result."
  [f a b good-enuf?]
  (let [-f (comp g/negate f)]
    (-> (golden-section-min -f a b good-enuf?)
        (update-in [:result 1] g/negate))))

(defn gsmin
  "allows keyword specification of typical convergence criteria.


  TODO the scheme section adds a default good-enuf of a function tolerance of
  10xmachine tolerance.

  Other possibilities are a counter check and an argument
  tolerance, checking that the args are close to some value.

  Making this better at reporting would be great.
  "
  [f a b & params])

(defn gsmax
  "allows keyword specification of typical convergence criteria.

  This duplicates, in the scheme, the exact args sent to gsmin. Not great!"
  [f a b & params]
  )

(defn brent-min
  "Find the minimum of the function f: R -> R in the interval [a,b]. If
  observe is supplied, will be invoked with the iteration count and the
  values of x and f(x) at each search step.

  Modern code for the brent optimizer: https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2193-L2269

  Actual code: https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2028"
  [f a b {:keys [rel abs observe]
          :or {rel 1e-5
               abs 1e-5
               observe (constantly nil)}}]
  (let []
    ))

;; Now here's what we have before.
#_(defn minimize*
    [f a b {:keys [rel abs maxiter maxfun observe]
            :or {rel 1e-5
                 abs 1e-5
                 observe (constantly nil)}}]
    (let [o (BrentOptimizer.
             rel
             abs
             (reify ConvergenceChecker
               (converged [_ _ _ current]
                 (observe (.getPoint ^UnivariatePointValuePair current)
                          (.getValue ^UnivariatePointValuePair current))
                 false)))
          args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
          (into-array OptimizationData
                      [(UnivariateObjectiveFunction.
                        (reify UnivariateFunction
                          (value [_ x]
                            (us/start evaluation-time)
                            (swap! evaluation-count inc)
                            (let [fx (f x)]
                              (us/stop evaluation-time)
                              fx))))
                       (MaxEval. 1000)
                       (SearchInterval. a b)
                       GoalType/MINIMIZE])
          p (.optimize o args)]
      (let [x (.getPoint p)
            y (.getValue p)]
        (when observe
          (observe (dec (.getEvaluations o)) x y))
        (us/stop total-time)
        (log/info "#" @evaluation-count "total" (us/repr total-time) "f" (us/repr evaluation-time))
        [x y @evaluation-count]))
    )

(defn brent-max [f a b eps])

(defn bracket-min
  " Given a function f, a starting point and a step size, try to bracket a local
  extremum for f.

  Return a list (retcode a b c fa fb fc iter-count) where a < b < c, and fa, fb,
  fc are the function values at these points. In the case of a minimum, fb
  <= (min fa fc); the opposite inequality holds in the case of a maximum.

  iter-count is the number of function evaluations required. retcode is 'okay if
  the search succeeded, or 'maxcount if it was abandoned.
  "
  [f start step max-tries])
(defn bracket-min [f start step max-tries])

(defn local-maxima
  "
  Given a function f on [a, b] and N > 0, examine f at the endpoints
  a, b, and at N equally-separated interior points. From this form a
  list of brackets (p q) in each of which a local maximum is trapped.
  Then apply Golden Section to all these brackets and return a list of
  pairs (x fx) representing the local maxima.

  "
  [f a b n ftol])

(defn local-minima [f a b n ftol])

(defn estimate-global-max
  "Refer to the previous two functions and find the max of all of those."
  [f a b n ftol])

(defn estimate-global-min
  "Refer to the previous two functions and find the min."
  [f a b n ftol])
