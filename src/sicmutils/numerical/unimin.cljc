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
            [sicmutils.value :as v]))

;; should this be machine eps?
(def ^:private very-small-num 1e-21)
(def ^:private phi      (/ (+ (g/sqrt 5) 1) 2))
(def ^:private inv-phi  (/ (- (g/sqrt 5) 1) 2))
(def ^:private inv-phi2 (- 1 inv-phi))

(defn- ascending-by
  "Returns the points ordered as f(a) < f(b)"
  [f a b]
  (let [fa (f a) fb (f b)]
    (if (< fa fb)
      [[a fa] [b fb]]
      [[b fb] [a fa]])))

(defn- lagrange-interpolating-polynomial
  "Generates a lagrange interpolating polynomial that fits all of the supplied
  points.

  TODO this should move to polynomial and become a real thing."
  [& points]
  (let [points (vec points)
        n (count points)]
    (fn [x]
      (let [build-term (fn [i [a fa]]
                         (let [others (for [j (range n) :when (not= i j)]
                                        (get-in points [j 0]))
                               num (reduce g/* (map #(g/- x %) others))
                               denom (reduce g/* (map #(g/- a %) others))]
                           (g// (g/* fa num) denom)))]
        (->> (map-indexed build-term points)
             (reduce g/+))))))

(defn parabolic-step
  "Fits a parabola through all three points, and returns the coordinate of the
  minimum of the parabola.

  If the parabola is totally flat, the denominator will be zero... defaults to a
  very small denom vs 0.

  Nice notes here on how we derived this method:
  http://fourier.eng.hmc.edu/e176/lectures/NM/node25.html"
  [[xa fa] [xb fb] [xc fc]]
  (let [tmp1  (* (- xb xa) (- fb fc))
        tmp2  (* (- xb xc) (- fb fa))
        v     (- tmp2 tmp1)
        num   (- (* (- xb xc) tmp2)
                 (* (- xb xa) tmp1))
        denom (if (< (g/abs v) very-small-num)
                (* 2.0 very-small-num)
                (* 2.0 v))]
    (- xb (/ num denom))))

(defn- between?
  "Returns true if the bound is `a` is strictly between `l` and `r` (ie, they act
  as non-inclusive bounds), false otherwise."
  [a l r]
  (> (* (- a r) (- l a)) 0))

(defn- extend-pt
  "generate a new point by extending x away from `away-from`. The invariant is
  that `x` sits between the new point and `away-from` at the golden ratio
  point."
  [x away-from]
  (+ x (* phi (- x away-from))))

(defn bracket-step-fn
  "Returns a function that performs steps of bracket extension."
  [f {:keys [grow-limit] :or {grow-limit 110.0}}]
  (fn [[xa fa :as a]
      [xb fb :as b]
      [xc fc :as c]]
    (let [;; If f(c) is < f(b) the minimum of the parabola will be far
          ;; outside the bounds. This is a bound on how far we're allowed to
          ;; jump in a single step.
          wlim (+ xb (* grow-limit (- xc xb)))
          w (parabolic-step a b c)]
      (cond
        ;; If the minimum is between b and c, we know that either b or w are
        ;; suitable minima, since f(b) < f(a).
        (<= xb w xc)
        (let [fw (f w)]
          (cond
            ;; if the parabolic minimum w evaluates to < f(c), shift the interval
            ;; to (b, w, c):
            (< fw fc) [b [w fw] c]

            ;; If f(b) < f(w) >= f(c), tighten the interval to (a, b, w):
            (> fw fb) [a b [w fw]]

            ;; If the points are in descending order - f(a) > f(b) >= f(w) >=
            ;; f(c) - stretch beyond `c` to attempt to find an increasing
            ;; region.
            :else (let [new-c (extend-pt xc xb)]
                    [b c [new-c (f new-c)]])))

        ;; This is the case where the parabolic minimum stretched beyond c but
        ;; hasn't reached its limit.
        (<= xc w wlim)
        (let [fw (f w)]
          (if (< fw fc)
            ;; If we're still descending, shift the interval fully right to (c,
            ;; w, stretched-c)
            (let [new-c (extend-pt w xc)]
              [c [w fw] [new-c (f new-c)]])
            ;; if the fn value starts to rise, tighten to (b, c, w).
            [b c [w fw]]))

        ;; If the parabolic interpolation jumps beyond the stretch limit, adjust
        ;; the range to the limit only.
        (<= xc wlim w) [b c [wlim (f wlim)]]

        ;; I don't this this branch can ever actually be reached.
        :else (let [new-c (extend-pt xc xb)]
                [b c [new-c (f new-c)]])))))

(defn bracket-min
  "Scipy version:  https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2450

  This works by growing the bounds first with either golden section steps or
  jumps using parabolic interpolation, bounded by a max.
  "
  ([f] (bracket-min f {}))
  ([f {:keys [xa xb max-tries]
       :or {xa 0.0
            xb 1.0
            max-tries 1000}
       :as opts}]
   (let [[f-counter f] (u/counted f)
         step (bracket-step-fn f opts)
         stop-fn (fn [[xa fa :as a] [xb fb :as b] [xc fc :as c] iteration]
                   ;; TODO - properly distinguish convergence from the other
                   ;; stopping conditions.
                   (or (> iteration max-tries)
                       (<= fb fc)))
         complete (fn [[xa fa :as a] b [xc fc :as c] iterations]
                    (let [m {:lo a
                             :mid b
                             :hi c
                             :fncalls @f-counter
                             :iterations iterations}]
                      (if (< xc xa)
                        (assoc m :lo c :hi a)
                        m)))
         ;; Massage starting values into descending order by f; f(b) < f(a).
         [[xb :as b] [xa :as a]] (ascending-by f xa xb)

         ;; Generate the first value of c by stretching b away from a with
         ;; golden-ratio amount, so that b ends up at the golden ratio
         ;; point (with short segment leading to `a`).
         xc (extend-pt xb xa)
         fc (f xc)]
     (loop [[a b c] [a b [xc fc]]
            iteration 0]
       (if (stop-fn a b c iteration)
         (complete a b c iteration)
         (recur (step a b c)
                (inc iteration)))))))

(defn bracket-max
  ([f] (bracket-max f {}))
  ([f opts]
   (let [-f (comp g/negate f)]
     (bracket-min -f opts))))

(defn bracket-min-from-scmutils
  " Given a function f, a starting point and a step size, try to bracket a local
  extremum for f.

  Return a list (retcode a b c fa fb fc iter-count) where a < b < c, and fa, fb,
  fc are the function values at these points. In the case of a minimum, fb
  <= (min fa fc); the opposite inequality holds in the case of a maximum.

  iter-count is the number of function evaluations required. retcode is 'okay if
  the search succeeded, or 'maxcount if it was abandoned.
  "
  [f {:keys [start step max-tries]
      :or {start 0
           step 10
           max-tries 1000}}]
  (let [[f-counter f] (u/counted f)
        stop-fn (fn [[_ fa :as a] [_ fb :as b] [_ fc :as c] iteration]
                  (or (> iteration max-tries)
                      (<= fb (min fa fc))))
        complete (fn [[xa fa :as a] b [xc fc :as c] iterations]
                   (let [m {:lo a
                            :mid b
                            :hi c
                            :fncalls @f-counter
                            :converged? (<= iterations max-tries)
                            :iterations iterations}]
                     (if (< xc xa)
                       (assoc m :lo c :hi a)
                       m)))
        run (fn [[xa fa :as a] [xb fb :as b] [xc fc :as c] iter]
              (if (stop-fn a b c iter)
                (complete a b c iter)
                (let [xd (+ xc (- xc xa))]
                  (recur b c [xd (f xd)] (inc iter)))))
        [[xa :as a] [xb :as b]] (ascending-by f start (+ start step))]
    (let [xc (+ xb (- xb xa))]
      (run a b [xc (f xc)] 0))))

(defn bracket-max-from-scmutils
  ([f] (bracket-max-from-scmutils f {}))
  ([f opts]
   (let [-f (comp g/negate f)]
     (bracket-min -f opts))))

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
;; But you ALSO have the nice property...

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
  "Takes four pairs of test (x, f(x)) and returns the new interval"
  [f [xa :as a] [xl fl :as l] [xr fr :as r] [xb :as b]]
  (if (< fl fr)
    (let [new-l (golden-cut xr xa)]
      [a [new-l (f new-l)] l r])
    (let [new-r (golden-cut xl xb)]
      [l r [new-r (f new-r)] b])))

(defn ^:private best-of
  "Default selection for the best possible point. You could also return, say, the
  average value between the bounds."
  [& pairs]
  (apply min-key second pairs))

(defn ^:private fn-tolerance-fn [epsilon]
  (let [close? (v/within epsilon)]
    (fn [[_ fa] [_ fl] [_ fr] [_ fb] _]
      (close? (max fa fb)
              (min fl fr)))))

(defn ^:private arg-tolerance-fn [epsilon]
  (let [close? (v/within epsilon)]
    (fn [[xa _] _ [xb _] _]
      (close? xa xb))))

(defn ^:private counter-fn [max-count]
  (fn [_ _ _ _ iterations]
    (< max-count iterations)))

(defn stop?
  "TODO make a single function that will return the big stop fn."
  [{:keys []}]
  )

(defn golden-section-min
  "Golden Section search, with supplied convergence test procedure.

  stop?, which is a predicate accepting five arguments:

  [a fa]
  [l fl]
  [r fr]
  [b fb]
  steps

    - TODO check against the scipy implementation
  https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2373:5

  The big difference there is that they allow auto-bracketing, if you don't want
  to provide an interval.

  TODO add auto-bracketing using the new interval stuff above."
  [f a b {:keys [
                 ;; stop-fn override.
                 stop?

                 ;; How do we select the final result from the spread?
                 choose

                 ;; call this on every iteration.
                 callback

                 ;; max number of times we can call the fn.
                 maxfun

                 ;; max number of times we can loop.
                 maxiter

                 ;; check that the minimal value of any of the checked points is
                 ;; within this of f(a) or f(b).
                 fn-tolerance

                 arg-tolerance ;; check that a, b are close enough.
                 ]
          :or {choose best-of
               fn-tolerance 1e-4
               callback identity}}]
  (let [[f-counter f] (u/counted f)
        xl            (golden-cut b a)
        xr            (golden-cut a b)

        ;; TODO - change this to a thing that nicely composes the various
        ;; stopping conditions.
        stop-fn stop?]
    (loop [[a l r b :as state] [[a (f a)] [xl (f xl)] [xr (f xr)] [b (f b)]]
           iteration 0]
      (callback state)
      (if (stop-fn a l r b iteration)
        (let [[x fx] (choose a l r b)]
          {:result     x
           :value      fx
           :iterations iteration
           :fncalls    @f-counter})
        (recur (shrink-interval f a l r b)
               (inc iteration))))))

(defn golden-section-max
  "For convenience, we also provide the sister-procedure for finding
  the maximum of a unimodal function.

  Negate the function, minimize, negate the result."
  [f a b opts]
  (let [-f (comp g/negate f)]
    (-> (golden-section-min -f a b opts)
        (update-in [:result 1] g/negate))))

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
