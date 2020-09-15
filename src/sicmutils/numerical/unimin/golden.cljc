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

(def phi      (/ (+ (g/sqrt 5) 1) 2))
(def inv-phi  (/ (- (g/sqrt 5) 1) 2))
(def inv-phi2 (- 1 inv-phi))

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

(defn- shrink-interval
  "Takes four pairs of test (x, f(x)) and narrows the interval down by choosing
  the minimum of `l` or `r` and bracketing around that.

  NOTE there's a guard internally against the items getting out of order; over
  many repeated evaluations, the points can get out of whack with the golden
  ratio. The slight guard is that we check internally that the interior points
  can never get past each other."
  [f [xa :as a] [xl fl :as l] [xr fr :as r] [xb :as b]]
  {:pre  [(< xa xl xr xb)]
   :post [#(apply < %&)]}
  (if (< fl fr)
    (let [new-l (golden-cut xr xa)]
      (if (< new-l xl)
        [a [new-l (f new-l)] l r]
        [a l [new-l (f new-l)] r]))
    (let [new-r (golden-cut xl xb)]
      (if (< xr new-r)
        [l r [new-r (f new-r)] b]
        [l [new-r (f new-r)] r b]))))

(defn best-of
  "Default selection function for the best possible point. This function chooses
  the point out of (a, l, r, b) with the minimum function value."
  [& pairs]
  (apply min-key second pairs))

(defn- fn-tolerance-fn
  "Returns a function that returns true if the max interior value is within
  `epsilon` of the smallest bound, false otherwise."
  [epsilon]
  (let [close? (v/within epsilon)]
    (fn [[_ fa] [_ fl] [_ fr] [_ fb] _]
      (close? (max fa fb)
              (min fl fr)))))

(defn- arg-tolerance-fn
  "Returns a fn that returns true if the coordinates of the outer bounds are
  within `epsilon` absolute distance, false otherwise."
  [epsilon]
  (let [close? (v/within epsilon)]
    (fn [[xa _] l r [xb _] _]
      (close? xa xb))))

(defn ^:private counter-fn
  "Returns a fn that returns true if the number of iterations has exceeded
  `max-count`, false otherwise. "
  [max-count]
  (fn [_ _ _ _ iterations]
    (< max-count iterations)))

(defn convergence-fn
  "Returns a fn that returns true if any of the following are true:

  - the max interior value is within `fn-tolerance` of the smallest bound,
  - `convergence?` (if supplied) returns true
  - the bounds are within `arg-tolerance` absolute distance,

  false otherwise."
  [{:keys [converged? fn-tolerance arg-tolerance]}]
  (fn [& args]
    (some #(apply % args)
          [(or converged? (constantly false))
           (arg-tolerance-fn arg-tolerance)
           (fn-tolerance-fn fn-tolerance)])))

(defn stop-fn
  "Returns a fn that returns true if any of the following are true::

  - the supplied `fn-counter` atom contains a value > `maxfun`
  - the loop has exceeded `maxiter` iterations

  false otherwise.
  "
  [{:keys [maxiter maxfun fn-counter]}]
  (fn [& args]
    (some #(apply % args)
          [(fn [& _] (> @fn-counter maxfun))
           (counter-fn maxiter)])))

(defn golden-section-min
  "Golden Section search, with supplied convergence test procedure.

  :converged? is a predicate accepting five arguments:

  [a fa]
  [l fl]
  [r fr]
  [b fb]
  steps

  :choose accepts 4 points and returns the final choice.

  :callback gets all 5.

  :maxfun

  :maxiter

  :fn-tolerance check that the minimal value of any of the checked points is
  within this of f(a) or f(b).

  :arg-tolerance check that a, b are close enough."
  ([f xa xb] (golden-section-min f xa xb {}))
  ([f xa xb {:keys [choose callback]
             :or {choose best-of
                  callback (constantly nil)}
             :as opts}]
   (let [[fn-counter f] (u/counted f)
         [xa fa :as a] (if (vector? xa) xa [xa (f xa)])
         [xb fb :as b] (if (vector? xb) xb [xb (f xb)])
         opts (merge {:maxfun 1000
                      :maxiter 1000
                      :fn-tolerance 1e-8
                      :arg-tolerance 1e-8
                      :fn-counter fn-counter}
                     opts)
         xl           (golden-cut xb xa)
         xr           (golden-cut xa xb)
         convergence? (convergence-fn opts)
         stop?        (stop-fn (assoc opts :fn-counter fn-counter))]
     (loop [[a l r b :as state] [a [xl (f xl)] [xr (f xr)] b]
            iteration 0]
       (callback a l r b iteration)
       (let [converged? (convergence? a l r b iteration)]
         (if (or converged? (stop? a l r b iteration))
           (let [[x fx] (choose a l r b)]
             {:result x
              :value fx
              :converged? (boolean converged?)
              :iterations iteration
              :fncalls @fn-counter})
           (recur (shrink-interval f a l r b)
                  (inc iteration))))))))

(defn golden-section-max
  "For convenience, we also provide the sister-procedure for finding
  the maximum of a unimodal function using the golden section method.

  Negate the function, minimize, negate the result."
  ([f xa xb] (golden-section-max f xa xb {}))
  ([f xa xb opts]
   (let [-f (comp g/negate f)]
     (-> (golden-section-min -f xa xb opts)
         (update-in [:value] g/negate)))))
