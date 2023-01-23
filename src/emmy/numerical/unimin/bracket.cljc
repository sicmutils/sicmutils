#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.unimin.bracket
  (:require [emmy.generic :as g]
            [emmy.numerical.unimin.golden :as ug]
            [emmy.util :as u]))

(def ^:private epsilon 1e-21)

(defn ascending-by
  "Returns the points ordered as f(a) < f(b)"
  [f a b]
  (let [fa (f a) fb (f b)]
    (if (< fa fb)
      [[a fa] [b fb]]
      [[b fb] [a fa]])))

(defn parabolic-pieces
  "Accepts three pairs of `[x, (f x)]`, fits a quadratic function to all three
  points and returns the step from `xb` (the coordinate of the second argument)
  to the minimum of the fitted quadratic.

  Returns the numerator and denominator `p` and `q` of the required step. If `q`
  is 0, then the supplied points were colinear.

  `q` is guaranteed to be `>= 0`, while `p` might be negative.

  See these notes for the derivation of this method:
  http://fourier.eng.hmc.edu/e176/lectures/NM/node25.html"
  [[xa fa] [xb fb] [xc fc]]
  {:post [#(>= (second %) 0)]}
  (let [tmp1  (* (- xb xa) (- fb fc))
        tmp2  (* (- xb xc) (- fb fa))
        v     (- tmp2 tmp1)
        p (- (* (- xb xc) tmp2)
             (* (- xb xa) tmp1))
        q (* 2.0 v)]
    (if (pos? q)
      [(g/negate p) q]
      [p (g/abs q)])))

(defn parabolic-step
  "Fits a parabola through all three points, and returns the coordinate of the
  minimum of the parabola.

  If the supplied points are colinear, returns a point that takes a large jump
  in the direction of the downward slope of the line."
  [a [xb :as b] c]
  (let [two-eps (* 2.0 epsilon)
        [p q] (parabolic-pieces a b c)
        q (if (< q two-eps)
            two-eps
            q)]
    (+ xb (/ p q))))

(defn bracket-step-fn
  "Returns a function that performs steps of bracket extension.

  :grow-limit is the maximum factor that the parabolic interpolation can jump
  the function."
  [f {:keys [grow-limit] :or {grow-limit 110.0}}]
  (fn [a
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
            :else (let [new-c (ug/extend-pt xc xb)]
                    [b c [new-c (f new-c)]])))

        ;; This is the case where the parabolic minimum stretched beyond c but
        ;; hasn't reached its limit.
        (<= xc w wlim)
        (let [fw (f w)]
          (if (< fw fc)
            ;; If we're still descending, shift the interval fully right to (c,
            ;; w, stretched-c)
            (let [new-c (ug/extend-pt w xc)]
              [c [w fw] [new-c (f new-c)]])
            ;; if the fn value starts to rise, tighten to (b, c, w).
            [b c [w fw]]))

        ;; If the parabolic interpolation jumps beyond the stretch limit, adjust
        ;; the range to the limit only.
        (<= xc wlim w) [b c [wlim (f wlim)]]

        ;; I don't this this branch can ever actually be reached.
        :else (let [new-c (ug/extend-pt xc xb)]
                [b c [new-c (f new-c)]])))))

(defn bracket-min
  "Generates an interval `[lo, hi]` that is guaranteed to contain a minimum of the
  function `f`, along with a candidate point `[mid, (f mid)]` that the user can
  use to start a minimum search.

  Returns a dictionary of the form:

  {:lo `lower end of the bracket`
   :mid `candidate point`
   :hi `upper end of the bracket`
   :fncalls `# of fn evaluations so far`
   :iterations `total iterations`}

  `:lo`, `:mid` and `:hi` are each pairs of the form `[x, (f x)]`.

  The implementation works by growing the bounds using either:

  - a step outside the bounds that places one bound at the golden-ratio cut
  point between the new bounds, or
  - a parabola with a minimum interpolated outside the current bounds, bounded b
  a max.

  This implementation was ported from `scipy.optimize.optimize.bracket`:
  https://github.com/scipy/scipy/blob/v1.5.2/scipy/optimize/optimize.py#L2450

  `bracket-min` supports the following optional keyword arguments:

  `:xa` the initial guess for the lower end of the bracket. Defaults to 0.0.

  `:xb` the initial guess for the upper end of the bracket. Defaults to 1.0. (If
  these points aren't supplied in sorted order they'll be switched.)

  `:grow-limit` The maximum factor that the parabolic interpolation can jump the
  function. Defaults to 110.0.

  `:maxiter` Maximum number of iterations allowed for the minimizer. Defaults to
  1000.

  `:maxfun` Maximum number of times the function can be evaluated before exiting.
  Defaults to 1000.
  "
  ([f] (bracket-min f {}))
  ([f {:keys [xa xb maxiter maxfun]
       :or {xa 0.0
            xb 1.0
            maxiter 1000
            maxfun 1000}
       :as opts}]
   (let [[f-counter f] (u/counted f)
         step (bracket-step-fn f opts)
         stop-fn (fn [_ [_ fb] [_ fc] iteration]
                   (or (> iteration maxiter)
                       (> @f-counter maxfun)
                       (<= fb fc)))
         complete (fn [[xa :as a] b [xc :as c] iterations]
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
         xc (ug/extend-pt xb xa)
         fc (f xc)]
     (loop [[a b c] [a b [xc fc]]
            iteration 0]
       (if (stop-fn a b c iteration)
         (complete a b c iteration)
         (recur (step a b c)
                (inc iteration)))))))

(defn bracket-max
  "Identical to bracket-min, except brackets a maximum of the supplied fn."
  ([f] (bracket-max f {}))
  ([f opts]
   (let [-f (comp g/negate f)]
     (bracket-min -f opts))))

(defn bracket-min-scmutils
  " Given a function f, a starting point and a step size, try to bracket a local
  extremum for f.

  Return a list (retcode a b c fa fb fc iter-count) where a < b < c, and fa, fb,
  fc are the function values at these points. In the case of a minimum, fb
  <= (min fa fc); the opposite inequality holds in the case of a maximum.

  iter-count is the number of function evaluations required. retcode is 'okay if
  the search succeeded, or 'maxcount if it was abandoned.
  "
  ([f] (bracket-min-scmutils f {}))
  ([f {:keys [start step maxiter]
       :or {start 0
            step 10
            maxiter 1000}}]
   (let [[f-counter f] (u/counted f)
         stop-fn (fn [[_ fa] [_ fb] [_ fc] iteration]
                   (or (> iteration maxiter)
                       (<= fb (min fa fc))))
         complete (fn [[xa :as a] b [xc :as c] iterations]
                    (let [m {:lo a
                             :mid b
                             :hi c
                             :fncalls @f-counter
                             :converged? (<= iterations maxiter)
                             :iterations iterations}]
                      (if (< xc xa)
                        (assoc m :lo c :hi a)
                        m)))
         run (fn [[xa :as a] b [xc :as c] iter]
               (if (stop-fn a b c iter)
                 (complete a b c iter)
                 (let [xd (+ xc (- xc xa))]
                   (recur b c [xd (f xd)] (inc iter)))))
         [[xb :as b] [xa :as a]] (ascending-by f start (+ start step))
         xc (+ xb (- xb xa))]
     (run a b [xc (f xc)] 0))))

(defn bracket-max-scmutils
  "Identical to bracket-min-scmutils, except brackets a maximum of the supplied
  fn."
  ([f] (bracket-max-scmutils f {}))
  ([f opts]
   (let [-f (comp g/negate f)]
     (bracket-min -f opts))))
