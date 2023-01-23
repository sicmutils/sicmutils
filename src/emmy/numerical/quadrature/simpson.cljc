#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.simpson
  (:require [emmy.numerical.quadrature.common :as qc]
            [emmy.numerical.quadrature.trapezoid :as qt]
            [emmy.polynomial.richardson :as pr]))

;; ## Simpson's Rule
;;
;; This numerical integration method is a [closed Newton-Cotes
;; formula](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas);
;; for each integral slice, Simpson's rule samples each endpoint and the
;; midpoint and combines them into an area estimate for this slice using the
;; following formula:
;;
;; $${{h} \over 3} (f_0 + 4f_1 + f_2)$$
;;
;; Given a window of $[a, b]$ and a "step size" of $h = {{b - a} \over 2}$. The
;; point $f_i$ is the point $i$ steps into the window.
;;
;; There are a few simpler ways to understand this:
;;
;; - Simpson's rule is simply the trapezoid method (see `trapezoid.cljc`),
;;   subject to a single refinement of "Richardson extrapolation".
;;
;; - The trapezoid method fits a line to each integration slice. Simpson's rule
;;   fits a quadratic to each slice.
;;
;; - Simpson's rule $S$ is the weighted average of the Midpoint rule $M$ and the
;;   trapezoid rule $T$:
;;
;; $$S = {{2M + T} \over 3}$$
;;
;; The test namespace contains a symbolic proof that the Richardson-extrapolated
;; Trapezoid method is equivalent to using the formula above to calculate
;; Simpson's rule directly.

(defn simpson-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed interval $[a, b]$ using Simpson's rule.

  Simpson's rule is equivalent to the trapezoid method subject to one refinement
  of Richardson extrapolation. The trapezoid method fits a line to each
  integration slice. Simpson's rule fits a quadratic to each slice.

  Returns estimates with $n, 2n, 4n, ...$ slices, geometrically increasing by a
  factor of 2 with each estimate.

  ### Optional arguments:

  If supplied, `:n` (default 1) specifies the initial number of slices to use."
  ([f a b] (simpson-sequence f a b {:n 1}))
  ([f a b {:keys [n] :or {n 1}}]
   {:pre [(number? n)]}
   (-> (qt/trapezoid-sequence f a b n)
       (pr/richardson-column 1 2 2 2))))

(qc/defintegrator integral
  "Returns an estimate of the integral of `f` over the closed interval $[a, b]$
  using Simpson's rule with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See [[simpson-sequence]] for more information about Simpson's rule, caveats that
  might apply when using this integration method and information on the optional
  args in `opts` that customize this function's behavior."
  :area-fn (comp first simpson-sequence)
  :seq-fn simpson-sequence)
