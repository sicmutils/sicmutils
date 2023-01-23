#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.boole
  (:require [emmy.numerical.quadrature.common :as qc]
            [emmy.numerical.quadrature.trapezoid :as qt]
            [emmy.polynomial.richardson :as pr]))

;; ## Boole's Rule
;;
;; NOTE - Boole's Rule is commonly mis-spelled as "Bode's Rule"!
;;
;; This numerical integration method is a [closed Newton-Cotes
;; formula](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas);
;; for each integral slice, Boole's rule samples:
;;
;; - each endpoint
;; - three interior points
;;
;; and combines them into an area estimate for this slice using the following
;; formula:
;;
;; $${{2h} \over 45} (7f_0 + 32f_1 + 12f_2 + 32f_3 + 7f_4)$$
;;
;; Given a window of $[a, b]$ and a "step size" of $h = {{b - a} \over 4}$. The
;; point $f_i$ is the point $i$ steps into the window.
;;
;; There are a few simpler ways to understand this:
;;
;; - Boole's rule is simply the trapezoid method (see `trapezoid.cljc`),
;;   subject to /two/ refinements of "Richardson extrapolation".
;;
;; - The trapezoid method fits a line to each integration slice. Boole's rule
;;   fits a quartic (4th-order) polynomial to each slice.
;;
;; The test namespace contains a symbolic proof that the Richardson-extrapolated
;; Trapezoid method is equivalent to using the formula above to calculate
;; Boole's rule directly.

(defn boole-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed interval $[a, b]$ using Boole's rule.

  Boole's rule is equivalent to the trapezoid method subject to two refinements
  of Richardson extrapolation. The trapezoid method fits a line to each
  integration slice. Boole's rule fits a quartic to each slice.

  Returns estimates with $n, 2n, 4n, ...$ slices, geometrically increasing by a
  factor of 2 with each estimate.

  ### Optional arguments:

  If supplied, `:n` (default 1) specifies the initial number of slices to use."
  ([f a b] (boole-sequence f a b {:n 1}))
  ([f a b {:keys [n] :or {n 1}}]
   {:pre [(number? n)]}
   (-> (qt/trapezoid-sequence f a b n)
       (pr/richardson-column 2 2 2 2))))

(qc/defintegrator integral
  "Returns an estimate of the integral of `f` over the closed interval $[a, b]$
  using Boole's rule with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `boole-sequence` for more information about Boole's rule, caveats that
  might apply when using this integration method and information on the optional
  args in `opts` that customize this function's behavior."
  :area-fn (comp first boole-sequence)
  :seq-fn boole-sequence)
