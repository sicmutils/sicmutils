#_
"Copyright © 2017 Colin Smith.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

(ns sicmutils.numerical.minimize
  "Entrypoint for univariate and multivariate minimization routines."
  (:require [sicmutils.numerical.multimin.nelder-mead :as nm]
            [sicmutils.numerical.unimin.brent :as b]
            [sicmutils.util :as u]))

(defn minimize
  "Find the minimum of the function `f: R -> R` in the interval `[a, b]`.

  If an `observe` function is supplied, it will be invoked with the iteration
  count and the values of x and f(x) at each search step."
  ([f a b] (minimize f a b (constantly nil)))
  ([f a b observe]
   (b/brent-min f a b {:callback observe})))

(defn multidimensional-minimize
  "Entrypoint for multidimensional minimization routines.

  See [[sicmutils.numerical.multimin.nelder-mead/nelder-mead]] for the only
  supported option."
  [func x0 & {:keys [info?] :as opts}]
  (let [result (nm/nelder-mead func x0 opts)]
    (if (:converged? result)
      (if info?
        result
        (:result result))
      (u/failure-to-converge (str "multidimensional-minimize failed to converge: " result)))))
