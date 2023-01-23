#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.bulirsch-stoer
  (:require [emmy.numerical.quadrature.common :as qc]
            [emmy.numerical.quadrature.midpoint :as mid]
            [emmy.numerical.quadrature.trapezoid :as trap]
            [emmy.polynomial.interpolate :as poly]
            [emmy.rational-function.interpolate :as rat]
            [emmy.util.stream :as us]))

;; ## Bulirsch-Stoer Integration
;;
;; This quadrature method comes from
;; the [scmutils](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt)
;; package that inspired this library.
;;
;; The idea is similar to Romberg integration:

;; - use some simpler quadrature method like the Midpoint or Trapezoid method to
;;   approximate an integral with a successively larger number of integration
;;   slices
;;
;; - Fit a curve to the pairs $(h, f(h))$, where $h$ is the width of an
;;   integration slice and $f$ is the integral estimator
;;
;; - Use the curve to extrapolate forward to $h=0$.
;;
;; Romberg integration does this by fitting a polynomial to a geometric series
;; of slices - $1, 2, 4...$, for example - using Richardson extrapolation.
;;
;; The Bulirsch-Stoer algorithm is exactly this, but:
;;
;; - using rational function approximation instead of polynomial
;; - the step sizes increase like $2, 3, 4, 6, 8... 2n_{i-2}$ by default
;;
;; Here are the default step sizes:

(def ^{:doc "Default step sizes used by the Bulirsch-Stoer quadrature algorithm:

  ```
2, 3, 4, 6, 8, 12, 16, 24, 32, ...
  ```"}
  bulirsch-stoer-steps
  (interleave
   (us/powers 2 2)
   (us/powers 2 3)))

;; The more familiar algorithm named "Bulirsch-Stoer" applies the same ideas to
;; the solution of ODes, as described
;; on [Wikipedia](https://en.wikipedia.org/wiki/Bulirsch%E2%80%93Stoer_algorithm).
;; scmutils adapted this into the methods you see here.
;;
;; NOTE - The Wikipedia page states that "Hairer, Nørsett & Wanner (1993, p.
;; 228), in their discussion of the method, say that rational extrapolation in
;; this case is nearly never an improvement over polynomial
;; interpolation (Deuflhard 1983)."
;;
;; We can do this too! Passing `{:bs-extrapolator :polynomial}` enables
;; polynomial extrapolation in the sequence and integration functions
;; implemented below.
;;
;; ## Even Power Series
;;
;; One more detail is important to understand. You could apply the ideas above
;; to any function that approximates an integral, but this namespace focuses on
;; accelerating the midpoint and trapezoid methods.
;;
;; As discussed in `midpoint.cljc` and `trapezoid.cljc`, the error series for
;; these methods has terms that fall off as even powers of the integration slice
;; width:
;;
;; $$1/h^2, 1/h^4, ...$$
;;
;; $$1/(h^2) 1/(h^2)^2, ...$$
;;
;; This means that the rational function approximation needs to fit the function
;; to points of the form
;;
;; $$(h^2, f(h))$$
;;
;; to take advantage of the acceleration. This trick is baked into Richardson
;; extrapolation through the ability to specify a geometric series.
;; `richardson_test.cljc` shows that Richardson extrapolation is indeed
;; equivalent to a polynomial fit using $h^2$... the idea here is the same.
;;
;; The following two functions generate a sequence of NON-squared $h$ slice
;; widths. `bs-sequence-fn` below squares each entry.

(defn- slice-width [a b]
  (let [width (- b a)]
    (fn [n] (/ width n))))

(defn- h-sequence
  "Defines the sequence of slice widths, given a sequence of `n` (number of
  slices) in the interval $(a, b)$."
  ([a b] (h-sequence a b bulirsch-stoer-steps))
  ([a b n-seq]
   (map (slice-width a b) n-seq)))

;; ## Bulirsch-Stoer Estimate Sequences
;;
;; The next group of functions generates `open-sequence` and `closed-sequence`
;; methods, analagous to all other quadrature methods in the library.

(defn- extrapolator-fn
  "Allows the user to specify polynomial or rational function extrapolation via
  the `:bs-extrapolator` option."
  [opts]
  (if (= :polynomial (:bs-extrapolator opts))
    (poly/modified-neville-scan 0)
    (rat/modified-bulirsch-stoer-scan 0)))

;; This function exists because we wanted to provide an `open-sequence` and
;; `closed-sequence` option below. The logic for both is the same, other than
;; the underlying approximation sequence generator.

(defn- bs-sequence-fn
  "Accepts some function (like `mid/midpoint-sequence`) that returns a sequence of
  successively better estimates to the integral, and returns a new function with
  interface `(f a b opts)` that accelerates the sequence with either

  - polynomial extrapolation
  - rational function extrapolation

  By default, The `:n` in `opts` (passed on to `integrator-seq-fn`) is set to
  the sequence of step sizes suggested by Bulirsch-Stoer,
  `bulirsch-stoer-steps`."
  [integrator-seq-fn]
  (fn call
    ([f a b]
     (call f a b {:n bulirsch-stoer-steps}))
    ([f a b opts]
     {:pre [(not (number? (:n opts)))]}
     (let [{:keys [n] :as opts} (-> {:n bulirsch-stoer-steps}
                                    (merge opts))
           extrapolate (extrapolator-fn opts)
           square      (fn [x] (* x x))
           xs          (map square (h-sequence a b n))
           ys          (integrator-seq-fn f a b opts)]
       (extrapolate
        (map vector xs ys))))))

(def ^{:doc "Returns a (lazy) sequence of successively refined estimates of the
  integral of `f` over the closed interval $[a, b]$ by applying rational
  polynomial extrapolation to successive integral estimates from the Midpoint
  rule.

  Returns estimates formed from the same estimates used by the Bulirsch-Stoer
  ODE solver, stored in `bulirsch-stoer-steps`.

  ### Optional arguments:

  `:n`: If supplied, `n` (sequence) overrides the sequence of steps to use.

  `:bs-extrapolator`: Pass `:polynomial` to override the default rational
  function extrapolation and enable polynomial extrapolation using the modified
  Neville's algorithm implemented in `poly/modified-neville`."}
  open-sequence
  (bs-sequence-fn mid/midpoint-sequence))

(def ^{:doc "Returns a (lazy) sequence of successively refined estimates of the
  integral of `f` over the closed interval $[a, b]$ by applying rational
  polynomial extrapolation to successive integral estimates from the Trapezoid
  rule.

  Returns estimates formed from the same estimates used by the Bulirsch-Stoer
  ODE solver, stored in `bulirsch-stoer-steps`.

  ### Optional arguments:

  `:n`: If supplied, `:n` (sequence) overrides the sequence of steps to use.

 `:bs-extrapolator`: Pass `:polynomial` to override the default rational
  function extrapolation and enable polynomial extrapolation using the modified
  Neville's algorithm implemented in `poly/modified-neville`."}
  closed-sequence
  (bs-sequence-fn trap/trapezoid-sequence))

;; ## Integration API
;;
;; Finally, two separate functions that use the sequence functions above to
;; converge quadrature estimates.

(qc/defintegrator open-integral
  "Returns an estimate of the integral of `f` over the open interval $(a, b)$
  generated by applying rational polynomial extrapolation to successive integral
  estimates from the Midpoint rule.

  Considers successive numbers of windows into $(a, b)$ specified by
  `bulirsch-stoer-steps`.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See [[open-sequence]] for more information about Bulirsch-Stoer quadrature,
  caveats that might apply when using this integration method and information on
  the optional args in `opts` that customize this function's behavior."
  :area-fn mid/single-midpoint
  :seq-fn open-sequence)

(qc/defintegrator closed-integral
  "Returns an estimate of the integral of `f` over the closed interval $[a, b]$
  generated by applying rational polynomial extrapolation to successive integral
  estimates from the Trapezoid rule.

  Considers successive numbers of windows into $[a, b]$ specified by
  `bulirsch-stoer-steps`.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See [[closed-sequence]] for more information about Bulirsch-Stoer quadrature,
  caveats that might apply when using this integration method and information on
  the optional args in `opts` that customize this function's behavior."
  :area-fn trap/single-trapezoid
  :seq-fn closed-sequence)

;; ## References:
;;
;; - Press, Numerical Recipes, section 16.4: http://phys.uri.edu/nigh/NumRec/bookfpdf/f16-4.pdf
;; - Wikipedia: https://en.wikipedia.org/wiki/Bulirsch%E2%80%93Stoer_algorithm
