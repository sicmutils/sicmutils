#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.minimize
  "Entrypoint for univariate and multivariate minimization routines."
  (:require [emmy.numerical.multimin.nelder-mead :as nm]
            [emmy.numerical.unimin.brent :as b]
            [emmy.util :as u]))

(defn minimize
  "Find the minimum of the function `f: R -> R` in the interval `[a, b]`.

  If an `observe` function is supplied, it will be invoked with the iteration
  count and the values of x and f(x) at each search step."
  ([f a b] (minimize f a b (constantly nil)))
  ([f a b observe]
   (b/brent-min f a b {:callback observe})))

(defn multidimensional-minimize
  "Entrypoint for multidimensional minimization routines.

  See [[emmy.numerical.multimin.nelder-mead/nelder-mead]] for the only
  supported option."
  [func x0 & {:keys [info?] :as opts}]
  (let [result (nm/nelder-mead func x0 opts)]
    (if (:converged? result)
      (if info?
        result
        (:result result))
      (u/failure-to-converge (str "multidimensional-minimize failed to converge: " result)))))
