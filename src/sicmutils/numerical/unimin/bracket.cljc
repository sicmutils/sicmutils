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

(ns sicmutils.numerical.unimin.bracket
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.numerical.unimin.golden :as ug]))

(def ^:private epsilon 1e-21)

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

  g(x) =  (f(a) * [(x-b)(x-c)...] / [(a-b)(a-c)...])
        + (f(b) * [(x-a)(x-c)...] / [(b-a)(b-c)...])
        + ...

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

(defn parabolic-pieces
  "Nice notes here on how we derived this method:
  http://fourier.eng.hmc.edu/e176/lectures/NM/node25.html"
  [[xa fa] [xb fb] [xc fc]]
  (let [tmp1  (* (- xb xa) (- fb fc))
        tmp2  (* (- xb xc) (- fb fa))
        v     (- tmp2 tmp1)
        num   (- (* (- xb xc) tmp2)
                 (* (- xb xa) tmp1))
        denom (* 2.0 v)]
    (if (pos? denom)
      [(g/negate num) denom]
      [num (g/abs denom)])))

(defn parabolic-step
  "Fits a parabola through all three points, and returns the coordinate of the
  minimum of the parabola.

  If the parabola is totally flat, the denominator will be zero... defaults to a
  very small denom vs 0.

  Nice notes here on how we derived this method:
  http://fourier.eng.hmc.edu/e176/lectures/NM/node25.html

  TODO note the assumption here... that fb < fa. Bake this in!"
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
         xc (ug/extend-pt xb xa)
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
        [[xb :as b] [xa :as a]] (ascending-by f start (+ start step))]
    (let [xc (+ xb (- xb xa))]
      (run a b [xc (f xc)] 0))))

(defn bracket-max-from-scmutils
  ([f] (bracket-max-from-scmutils f {}))
  ([f opts]
   (let [-f (comp g/negate f)]
     (bracket-min -f opts))))
