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
           (org.apache.commons.math3.analysis UnivariateFunction)
           (org.apache.commons.math3.optim.nonlinear.scalar GoalType)
           (org.apache.commons.math3.optim MaxEval OptimizationData)))

(defn minimize
  [f a b]
  (let [p (.optimize (BrentOptimizer. 1e-5 1e-5)
                     (into-array OptimizationData
                                 [(UnivariateObjectiveFunction.
                                    (proxy [UnivariateFunction] []
                                      (value [x] (f x))))
                                  (MaxEval. 1000)
                                  (SearchInterval. a b)
                                  GoalType/MINIMIZE]))]
    [(.getPoint p) (.getValue p)]))