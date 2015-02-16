;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.numerical.minimize
  (:import (org.apache.commons.math3.optim.univariate BrentOptimizer UnivariateObjectiveFunction SearchInterval)
           (org.apache.commons.math3.analysis UnivariateFunction MultivariateFunction)
           (org.apache.commons.math3.optim.nonlinear.scalar GoalType ObjectiveFunction)
           (org.apache.commons.math3.optim MaxEval OptimizationData InitialGuess)
           (org.apache.commons.math3.optim.nonlinear.scalar.noderiv SimplexOptimizer NelderMeadSimplex)))

(defn minimize
  [f a b]
  (let [o (BrentOptimizer. 1e-5 1e-5)
        args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
             (into-array OptimizationData
                         [(UnivariateObjectiveFunction.
                            (proxy [UnivariateFunction] []
                              (value [x] (f x))))
                          (MaxEval. 1000)
                          (SearchInterval. a b)
                          GoalType/MINIMIZE])
        p (.optimize o args)]
    #_(prn "brent steps" (.getEvaluations o))
    [(.getPoint p) (.getValue p)]))

(defn multidimensional-minimize
  [f qs]
  (let [o (SimplexOptimizer. 1e-10 1e-10)
        args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
             (into-array OptimizationData
                         [(NelderMeadSimplex. (count qs))
                          (ObjectiveFunction.
                            (proxy [MultivariateFunction] []
                              (value [xs]
                                (f xs))))
                          (MaxEval. 1000)
                          (InitialGuess. (double-array qs))
                          GoalType/MINIMIZE])
        p (.optimize o args)]
    [(.getPoint p) (.getValue p)]))