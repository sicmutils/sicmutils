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

(ns sicmutils.numerical.quadrature.milne
  (:require [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.util.stream :as us]))

;; ## Milne's Rule
;;
;; This numerical integration method is an [open Newton-Cotes
;; formula](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Open_Newton%E2%80%93Cotes_formulas);
;; for each integral slice, Milne's rule samples three interior points (not the
;; endpoints!) and combines them into an area estimate for this slice using the
;; following formula:
;;
;; $${{4h} \over 3} (2f_1 - f_2 + 2f_3)$$
;;
;; Given a window of $(a, b)$ and a "step size" of $h = {{b - a} \over 3}$. The
;; point $f_i$ is the point $i$ steps into the window.
;;
;; There is a simpler way to understand this! Milne's method is, in fact, just
;; the midpoint method (see `midpoint.cljc`), subject to a single refinement
;; of "Richardson extrapolation".
;;
;; The test namespace contains a symbolic proof that the Richardson-extrapolated
;; Midpoint method is equivalent to using the formula above to calculate
;; Milne's rule directly.

(defn milne-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the open interval $(a, b)$ using Milne's rule.

  Milne's rule is equivalent to the midpoint method subject to one refinement of
  Richardson extrapolation.

  Returns estimates with $n, 2n, 4n, ...$ slices, geometrically increasing by a
  factor of 2 with each estimate.

  If supplied, `n` (defaults 1) specifies the initial number of slices to use.

  NOTE: the Midpoint method is able to reuse function evaluations as its windows
  narrow /only/ when increasing the number of integration slices by 3. Milne's
  method increases the number of slices geometrically by a factor of 2 each
  time, so it will never hit the incremental path. You may want to memoize your
  function before calling `milne-sequence`."
  ([f a b] (milne-sequence f a b {:n 1}))
  ([f a b {:keys [n] :or {n 1} :as opts}]
   {:pre [(number? n)]}
   (-> (qm/midpoint-sequence f a b (assoc opts :n (us/powers 2 n)))
       (ir/richardson-column 1 2 2 2))))

(def ^{:doc "Returns an estimate of the integral of `f` over the open interval $(a, b)$
  using Milne's rule with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `milne-sequence` for more information about Milne's rule, and caveats that
  might apply when using this integration method."}
  integral
  (qc/make-integrator-fn
   (comp first milne-sequence)
   milne-sequence))
