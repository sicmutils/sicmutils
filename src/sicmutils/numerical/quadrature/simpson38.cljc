;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.quadrature.simpson38
  (:require [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.util.stream :as us]))

;; ## Simpson's 3/8 Rule
;;
;; This numerical integration method is a [closed Newton-Cotes
;; formula](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas);
;; for each integral slice, Simpson's 3/8 rule samples each endpoint and TWO
;; interior, equally spaced points, and combines them into an area estimate for
;; this slice using the following formula:
;;
;; $${{3h} \over 8} (f_0 + 3f_1 + 3f_2 + f_3)$$
;;
;; Given a window of $[a, b]$ and a "step size" of $h = {{b - a} \over 3}$. The
;; point $f_i$ is the point $i$ steps into the window.
;;
;; There are a few simpler ways to understand this:
;;
;; - Simpson's 3/8 rule is simply the trapezoid method (see `trapezoid.cljc`),
;;   subject to a single refinement of "Richardson extrapolation", with an
;;   threefold-increase of integration slices at each step, from $n \to 3n$.
;;
;; - The trapezoid method fits a line to each integration slice. Simpson's 3/8
;;   rule fits a cubic to each slice.
;;
;; The test namespace contains a symbolic proof that the Richardson-extrapolated
;; Trapezoid method is equivalent to using the formula above to calculate
;; Simpson's 3/8 rule directly.

(defn simpson38-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the open interval $(a, b)$ using Simpson's 3/8 rule.

  Simpson's 3/8 rule is equivalent to the trapezoid method subject to:

  - one refinement of Richardson extrapolation, and

  - a geometric increase of integration slices by a factor of 3 for each
  sequence element. (the Trapezoid method increases by a factor of 2 by
  default.)

  The trapezoid method fits a line to each integration slice. Simpson's 3/8 rule
  fits a cubic to each slice.

  Returns estimates with $n, 3n, 9n, ...n3^i$ slices, geometrically increasing by a
  factor of 3 with each estimate.

  If supplied, `n` (defaults 1) specifies the initial number of slices to use.

  NOTE: the Trapezoid method is able to reuse function evaluations as its
  windows narrow /only/ when increasing the number of integration slices by 2.
  Simpson's 3/8 rule increases the number of slices geometrically by a factor of
  2 each time, so it will never hit the incremental path. You may want to
  memoize your function before calling `simpson38-sequence`."
  ([f a b] (simpson38-sequence f a b 1))
  ([f a b n]
   {:pre [(number? n)]}
   (-> (qt/trapezoid-sequence f a b (us/powers 3 n))
       (ir/richardson-column 1 3 2 2))))

(defn integral
  "Returns an estimate of the integral of `f` over the open interval $(a, b)$
  using Simpson's 3/8 rule with $1, 3, 9 ... 3^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `simpson38-sequence` for more information about Simpson's 3/8 rule, and
  caveats that might apply when using this integration method."
  ([f a b] (integral f a b {}))
  ([f a b opts]
   (-> (simpson38-sequence f a b)
       (us/seq-limit opts))))
