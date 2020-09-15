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

(ns sicmutils.numerical.unimin.golden
  (:require [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(def phi      (/ (+ (g/sqrt 5) 1) 2))
(def inv-phi  (/ (- (g/sqrt 5) 1) 2))
(def inv-phi2 (- 1 inv-phi))

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

(defn extend-pt
  "generate a new point by extending x away from `away-from`. The invariant is
  that `x` sits between the new point and `away-from` at the golden ratio
  point."
  [x away-from]
  (+ x (* phi (- x away-from))))

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
  [f {:keys [
             xa
             xb

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
        xl            (golden-cut xb xa)
        xr            (golden-cut xa xb)

        ;; TODO - change this to a thing that nicely composes the various
        ;; stopping conditions.
        stop-fn stop?]
    (loop [[a l r b :as state] [[xa (f xa)] [xl (f xl)] [xr (f xr)] [xb (f xb)]]
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
  [f opts]
  (let [-f (comp g/negate f)]
    (-> (golden-section-min -f opts)
        (update-in [:result 1] g/negate))))
