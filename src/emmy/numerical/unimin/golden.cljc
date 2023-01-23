#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.unimin.golden
  (:require [emmy.generic :as g]
            [emmy.numbers]
            [emmy.util :as u]
            [emmy.value :as v]))

;; # Golden Section Method
;;
;; The golden section method is an algorithm for locating the minimum of a
;; single variable function without access to its derivative.

;; Wikipedia page: https://en.wikipedia.org/wiki/Golden-section_search
;;
;; Start with the two endpoints `a` and `b` that you know bound a minimum, and
;; choose two interior points to test. (The goal is to progressively narrow your
;; search window; you need two points to know where to zoom in.)
;;
;; If the left point `l` has a lower function value, zoom in to `(a, r)`. Else,
;; zoom to `(l, b)` for the next round.
;;
;; How do you pick the points? Use the golden ratio. There are two ways to cut
;; an interval using the golden ratio. Each results in a larger and smaller piece.
;;
;; The "golden ratio" is the interval such that the ratio of the LARGER piece to
;; the whole is the same as the ratio of the smaller piece to the larger piece.
;; By choosing your interior points using the golden ratio, you guarantee that
;; whether you zoom in to `(a, r)` or `(l, b)`, both pieces will cover the same
;; ratio of the the total interval, every time. Any ratio other than the golden
;; ratio could degenerate into you selecting the larger of the two pieces every
;; time, slowing your search.

(def ^{:doc "$\\phi$, the golden ratio."}
  phi
  (/ (+ (g/sqrt 5) 1) 2))


(def ^{:doc "$1 \\over \\phi$. Multiply by this to scale some distance down to the
larger-sized golden ratio piece."}
  inv-phi
  (/ (- (g/sqrt 5) 1) 2))


(def ^{:doc "$1 \\over \\phi^2$. Scales down twice, compared to [[inv-phi]]."}
  inv-phi2
  (- 1 inv-phi))

(defn golden-cut
  "Returns the point between `from` and `to` that cuts the region between the two
  into two sections in golden-ratioed proportion to each other.

  For example, depending on the ordering of `from` and `to`, `x` would be
  either:

  ```
  from------x1---to
  to---x2------from
  ```

  Such that `from->x1 / from->to == to->x2 / from->x1`."
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
    (fn [[xa _] _ _ [xb _] _]
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
  "Golden Section search attempts to locate the minimum of the supplied function
  `f` by evaluating points located at golden-ratioed intervals between the two
  starting endpoints `a` and `b`. This method is slow, steady and reliable.

  Supports the following optional keyword arguments:

  `:converged?` is an optional predicate accepting five arguments:

  - `[a fa]`
  - `[l fl]`
  - `[r fr]`
  - `[b fb]`
  - `current-iteration`

  If the supplied `fn` returns true, it will signal convergence and the
  optimizer will return. Returning false will continue.

  `:choose` is called at the final step of optimization with all 4 points and
  their fn values (see the first four arguments to `:converged?`), and returns
  the final choice.

  `:callback` receives all 5 arguments on every iteration.

  `:maxiter` Maximum number of iterations allowed for the minimizer. Defaults to
  1000.

  `:maxfun` Maximum number of times the function can be evaluated before exiting.
  Defaults to 1000.

  `:fn-tolerance` check that the minimal value of any of the checked points is
  within the maximum of f(a) or f(b).

  `:arg-tolerance` check that `a` and `b` are within this supplied absolute
  distance."
  ([f xa xb] (golden-section-min f xa xb {}))
  ([f xa xb {:keys [choose callback]
             :or {choose best-of
                  callback (constantly nil)}
             :as opts}]
   (let [[fn-counter f] (u/counted f)
         [xa :as a] (if (vector? xa) xa [xa (f xa)])
         [xb :as b] (if (vector? xb) xb [xb (f xb)])
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
     (loop [[a l r b] [a [xl (f xl)] [xr (f xr)] b]
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
         (update :value g/negate)))))
