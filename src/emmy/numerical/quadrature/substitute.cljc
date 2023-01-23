#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.substitute
  "### U Substitution and Variable Changes

  This namespace provides implementations of functions that accept an
  `integrator` and perform a variable change to address some singularity, like
  an infinite endpoint, in the definite integral.

  The strategies currently implemented were each described by Press, et al. in
  section 4.4 of ['Numerical
  Recipes'](http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf)."
  (:require [emmy.generic :as g]
            [emmy.numerical.quadrature.common :as qc]))

;; ## Infinite Endpoints
;;
;; This first function, `infinitize`, transforms some integrator into a new
;; integrator with the same interface that can handle an infinite endpoint.
;;
;; This implementation can only handle one endpoint at a time, and, the way it's
;; written, both endpoints have to have the same sign. For an easier interface
;; to this transformation, see `infinite/evaluate-infinite-integral` in
;; `infinite.cljc`.

(defn infinitize
  "Performs a variable substitution targeted at converting a single infinite
  endpoint of an improper integral evaluation into an (open) endpoint at 0 by
  applying the following substitution:

  $$u(t) = {1 \\over t}$$ $$du = {-1 \\over t^2}$$

  This works when the integrand `f` falls off at least as fast as $1 \\over t^2$
  as it approaches the infinite limit.

  The returned function requires that `a` and `b` have the same sign, ie:

  $$ab > 0$$

  Transform the bounds with $u(t)$, and cancel the negative sign by changing
  their order:

  $$\\int_{a}^{b} f(x) d x=\\int_{1 / b}^{1 / a} \\frac{1}{t^{2}} f\\left(\\frac{1}{t}\\right) dt$$

  References:

  - Mathworld, [\"Improper Integral\"](https://mathworld.wolfram.com/ImproperIntegral.html)
  - Press, Numerical Recipes, [Section 4.4](http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf)"
  [integrate]
  (fn call
    ([f a b] (call f a b {}))
    ([f a b opts]
     {:pre [(not
             (and (g/infinite? a)
                  (g/infinite? b)))]}
     (let [f' (fn [t]
                (/ (f (/ 1.0 t))
                   (* t t)))
           a' (if (g/infinite? b) 0.0 (/ 1.0 b))
           b' (if (g/infinite? a) 0.0 (/ 1.0 a))
           opts (qc/update-interval opts qc/flip)]
       (integrate f' a' b' opts)))))

;; ## Power Law Singularities
;;
;; "To deal with an integral that has an integrable power-law singularity at its
;; lower limit, one also makes a change of variable." (Press, p138)
;;
;; A "power-law singularity" means that the integrand diverges as $(x -
;; a)^{-\gamma}$ near $x=a$.
;;
;; We implement the following identity (from Press) if the singularity occurs at
;; the lower limit:
;;
;; $$\int_{a}^{b} f(x) d x=\frac{1}{1-\gamma} \int_{0}^{(b-a)^{1-\gamma}} t^{\frac{\gamma}{1-\gamma}} f\left(t^{\frac{1}{1-\gamma}}+a\right) d t \quad(b>a)$$
;;
;; And this similar identity if the singularity occurs at the upper limit:
;;
;;$$\int_{a}^{b} f(x) d x=\frac{1}{1-\gamma} \int_{0}^{(b-a)^{1-\gamma}} t^{\frac{\gamma}{1-\gamma}} f\left(b-t^{\frac{1}{1-\gamma}}\right) d t \quad(b>a)$$
;;
;; If you have singularities at both sides, divide the interval at some interior
;; breakpoint, take separate integrals for both sides and add the values back
;; together.

(defn- inverse-power-law
  "Implements a change of variables to address a power law singularity at the
  lower or upper integration endpoint.

  An \"inverse power law singularity\" means that the integrand diverges as

  $$(x - a)^{-\\gamma}$$

  near $x=a$. Passing true for `lower?` to specify a singularity at the lower
  endpoint, false to signal an upper-endpoint singularity.

  References:

  - Mathworld, [\"Improper Integral\"](https://mathworld.wolfram.com/ImproperIntegral.html)
  - Press, Numerical Recipes, [Section 4.4](http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf)
  - Wikipedia, [\"Finite-time Singularity\"](https://en.wikipedia.org/wiki/Singularity_(mathematics)#Finite-time_singularity)"
  [integrate gamma lower?]
  {:pre [(<= 0 gamma 1)]}
  (fn call
    ([f a b] (call f a b {}))
    ([f a b opts]
     (let [inner-pow (/ 1 (- 1 gamma))
           gamma-pow (* gamma inner-pow)
           a' 0
           b' (Math/pow (- b a) (- 1 gamma))
           t->t' (if lower?
                   (fn [t] (+ a (Math/pow t inner-pow)))
                   (fn [t] (- b (Math/pow t inner-pow))))
           f' (fn [t] (* (Math/pow t gamma-pow)
                        (f (t->t' t))))]
       (-> (integrate f' a' b' opts)
           (update :result (partial * inner-pow)))))))

(defn inverse-power-law-lower
  "Implements a change of variables to address a power law singularity at the
  lower integration endpoint.

  An \"inverse power law singularity\" means that the integrand diverges as

  $$(x - a)^{-\\gamma}$$

  near $x=a$.

  References:

  - Mathworld, [\"Improper Integral\"](https://mathworld.wolfram.com/ImproperIntegral.html)
  - Press, Numerical Recipes, [Section 4.4](http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf)
  - Wikipedia, [\"Finite-time Singularity\"](https://en.wikipedia.org/wiki/Singularity_(mathematics)#Finite-time_singularity)"
  [integrate gamma]
  (inverse-power-law integrate gamma true))

(defn inverse-power-law-upper
  "Implements a change of variables to address a power law singularity at the
  upper integration endpoint.

  An \"inverse power law singularity\" means that the integrand diverges as

  $$(x - a)^{-\\gamma}$$

  near $x=a$.

  References:

  - Mathworld, [\"Improper Integral\"](https://mathworld.wolfram.com/ImproperIntegral.html)
  - Press, Numerical Recipes, [Section 4.4](http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf)
  - Wikipedia, [\"Finite-time Singularity\"](https://en.wikipedia.org/wiki/Singularity_(mathematics)#Finite-time_singularity)"
  [integrate gamma]
  (inverse-power-law integrate gamma false))

;; ## Inverse Square Root singularities
;;
;; The next two functions specialize the `inverse-power-law-*` functions to the
;; common situation of an inverse power law singularity.

(defn inverse-sqrt-lower
  "Implements a change of variables to address an inverse square root singularity
  at the lower integration endpoint. Use this when the integrand diverges as

  $$1 \\over {\\sqrt{x - a}}$$

  near the lower endpoint $a$."
  [integrate]
  (fn call
    ([f a b] (call f a b {}))
    ([f a b opts]
     (let [f' (fn [t] (* t (f (+ a (* t t)))))]
       (-> (integrate f' 0 (Math/sqrt (- b a)) opts)
           (update :result (partial * 2)))))))

(defn inverse-sqrt-upper
  "Implements a change of variables to address an inverse square root singularity
  at the upper integration endpoint. Use this when the integrand diverges as

  $$1 \\over {\\sqrt{x - b}}$$

  near the upper endpoint $b$."
  [integrate]
  (fn call
    ([f a b] (call f a b {}))
    ([f a b opts]
     (let [f' (fn [t] (* t (f (- b (* t t)))))]
       (-> (integrate f' 0 (Math/sqrt (- b a)) opts)
           (update :result (partial * 2)))))))

;; ## Exponentially Diverging Endpoints

;; From Press, section 4.4: "Suppose the upper limit of integration is infinite,
;; and the integrand falls off exponentially. Then we want a change of variable
;; that maps
;;
;; $$\exp{-x} dx$$
;;
;; into $\pm dt$ (with the sign chosen to keep the upper limit of the new
;; variable larger than the lower limit)."
;;
;; The required identity is:
;;
;; $$\int_{x=a}^{x=\infty} f(x) d x=\int_{t=0}^{t=e^{-a}} f(-\log t) \frac{d t}{t}$$

(defn exponential-upper
  "Implements a change of variables to address an exponentially diverging upper
  integration endpoint. Use this when the integrand diverges as $\\exp{x}$ near
  the upper endpoint $b$."
  [integrate]
  (fn call
    ([f a b] (call f a b {}))
    ([f a b opts]
     {:pre [(g/infinite? b)]}
     (let [f' (fn [t] (* (f (- (Math/log t)))
                        (/ 1 t)))
           opts (qc/update-interval opts qc/flip)]
       (integrate f' 0 (Math/exp (- a)) opts)))))
