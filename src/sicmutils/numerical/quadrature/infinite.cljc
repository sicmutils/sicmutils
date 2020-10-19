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

(ns sicmutils.numerical.quadrature.infinite
  (:require [clojure.core.match :refer [match]]
            [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.numerical.quadrature.substitute :as qs]))

;; ## Improper (infinite) Integrals
;;
;; This namespace holds an implementation of an "improper" integral
;; combinator (for infinite endpoints) usable with any quadrature method in the
;; library.
;;
;; The implementation was inspired by `evaluate-improper-integral` in
;; `numerics/quadrature/quadrature.scm` file in
;; the [scmutils](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt)
;; package.
;;
;; ## Overview
;;
;; To evaluate an [improper
;; integral](https://en.wikipedia.org/wiki/Improper_integral) with an infinite
;; endpoint, the `improper` combinator applies an internal change of variables.
;;
;; $$u(t) = {1 \over t}$$ $$du = {-1 \over t^2}$$
;;
;; This has the effect of mapping the endpoints from $a, b$ to ${1 \over b}, {1
;; \over a}$. Here's the identity we implement:
;;
;; $$\int_{a}^{b} f(x) d x=\int_{1 / b}^{1 / a} \frac{1}{t^{2}} f\left(\frac{1}{t}\right) dt$$
;;
;; This is implemented by `substitute/infinitize`.
;;
;; The variable change only works as written when both endpoints are of the same
;; sign; so, internally, `improper` only applies the variable change to the
;; segment of $a, b$ from `##-Inf => (- :infinite-breakpoint)` and
;; `:infinite-breakpoint -> ##Inf`, where `:infinite-breakpoint` is an argument
;; the user can specify in the returned integrator's options map.
;;
;; Any segment of $a, b$ /not/ in those regions is evaluated normally.
;;
;; NOTE: The ideas in this namespace could be implemented for other variable
;; substitutions (see `substitute.cljc`) that only need to apply to certain
;; integration intervals. The code below automatically cuts the range $(a, b)$
;; to accomodate this for the particular variable change we've baked in, but
;; there is a more general abstraction lurking.
;;
;; If you find it, please submit an issue!
;;
;; ## Implementation

(defn- fill-defaults
  "Populates the supplied `opts` dictionary with defaults required by
  `evaluate-infinite-integral`."
  [opts]
  (assoc opts :infinite-breakpoint 1))

(defn improper
  "Accepts:

  - An `integrator` (function of `f`, `a`, `b` and `opts`)
  - `a` and `b`, the endpoints of an integration interval, and
  - (optionally) `opts`, a dict of integrator-configuring options

  And returns a new integrator that's able to handle infinite endpoints. (If you
  don't specify `##-Inf` or `##Inf`, the returned integrator will fall through
  to the original `integrator` implementation.)

  All `opts` will be passed through to the supplied `integrator`.

  ## Optional arguments relevant to `improper`:

  `:infinite-breakpoint`: If either `a` or `b` is equal to `##Inf` or `##-Inf`,
  this function will internally perform a change of variables on the regions
  from:

  `(:infinite-breakpoint opts) => ##Inf`

  or

  `##-Inf => (- (:infinite-breakpoint opts))`

  using $u(t) = {1 \\over t}$, as described in the `infinitize` method of
  `substitute.cljc`. This has the effect of mapping the infinite endpoint to an
  open interval endpoint of 0.

  Where should you choose the breakpoint? According to Press in Numerical
  Recipes, section 4.4: \"At a sufficiently large positive value so that the
  function funk is at least beginning to approach its asymptotic decrease to
  zero value at infinity.\"

  References:

  - Press, Numerical Recipes (p138), Section 4.4: http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf"
  [integrator]
  (fn rec
    ([f a b] (rec f a b {}))
    ([f a b opts]
     (let [{:keys [infinite-breakpoint] :as opts} (fill-defaults opts)
           call (fn [integrate l r interval]
                  (let [m (qc/with-interval opts interval)]
                    (let [result (integrate f l r m)]
                      (:result result))))
           ab-interval   (qc/interval opts)
           integrate     (partial call integrator)
           inf-integrate (partial call (qs/infinitize integrator))
           r-break       (Math/abs infinite-breakpoint)
           l-break       (- r-break)]
       (match [[a b]]
              [(:or [##-Inf ##-Inf] [##Inf ##Inf])]
              {:converged? true
               :terms-checked 0
               :result 0.0}

              [(:or [_ ##-Inf] [##Inf _])]
              (-> (rec f b a opts)
                  (update-in [:result] -))

              ;; Break the region up into three pieces: a central closed core and
              ;; two open endpoints where we create a change of variables, letting
              ;; the boundary go to infinity. We use an OPEN interval on the
              ;; infinite side.
              [(:or [##-Inf ##Inf]
                    [##Inf ##-Inf])]
              (let [-inf->l (inf-integrate a l-break qc/open-closed)
                    l->r    (integrate     l-break r-break qc/closed)
                    r->+inf (inf-integrate r-break b qc/closed-open)]
                {:converged? true
                 :result (+ -inf->l l->r r->+inf)})

              ;; If `b` lies to the left of the negative breakpoint, don't cut.
              ;; Else, cut the integral into two pieces at the negative breakpoint
              ;; and variable-change the left piece.
              [[##-Inf _]]
              (if (<= b l-break)
                (inf-integrate a b ab-interval)
                (let [-inf->l (inf-integrate a l-break qc/open-closed)
                      l->b    (integrate     l-break b (qc/close-l ab-interval))]
                  {:converged? true
                   :result (+ -inf->l l->b)}))

              ;; If `a` lies to the right of the positive breakpoint, don't cut.
              ;; Else, cut the integral into two pieces at the positive breakpoint
              ;; and variable-change the right piece.
              [[_ ##Inf]]
              (if (>= a r-break)
                (inf-integrate a b ab-interval)
                (let [a->r    (integrate     a r-break (qc/close-r ab-interval))
                      r->+inf (inf-integrate r-break b qc/closed-open)]
                  {:converged? true
                   :result (+ a->r r->+inf)}))

              ;; This is a lot of machinery to use with NON-infinite endpoints;
              ;; but for completeness, if you reach this level the fn will attempt
              ;; to integrate the full range directly using the original
              ;; integrator.
              :else (integrator f a b opts))))))

;; ## Suggestions for Improvement
;;
;; The current implementation does not pass convergence information back up the
;; line! Ideally we would merge results by:
;;
;; - Adding results
;; - combining `:converged?` entries with `and`
;; - retaining all other keys
