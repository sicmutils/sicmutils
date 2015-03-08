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
           (org.apache.commons.math3.optim MaxEval OptimizationData InitialGuess ConvergenceChecker SimpleValueChecker PointValuePair)
           (org.apache.commons.math3.optim.nonlinear.scalar.noderiv SimplexOptimizer NelderMeadSimplex)))

(defn minimize
  "Find the minimum of the function f: R -> R in the interval [a,b]. If
  observe is supplied, will be invoked with the iteration count and the
  values of x and f(x) at each search step."
  ([f a b observe]
   (let [rel 1e-5
         abs 1e-5
         o (BrentOptimizer.
            rel abs
            (proxy [ConvergenceChecker] []
              (converged [_ _ ^PointValuePair current]
                (when observe
                  (observe (.getPoint current) (.getValue current)))
                false)))
         args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
         (into-array OptimizationData
                     [(UnivariateObjectiveFunction.
                       (proxy [UnivariateFunction] []
                         (value [x] (f x))))
                      (MaxEval. 1000)
                      (SearchInterval. a b)
                      GoalType/MINIMIZE])
         p (.optimize o args)]
     (let [x (.getPoint p)
           y (.getValue p)]
       (when observe
         (observe (dec (.getEvaluations o)) x y))
       [x y])))
  ([f a b]
   (minimize f a b nil)))

(defn multidimensional-minimize
  "Find the minimum of the function f: R^n -> R, given an initial point q ∈ R^n.
  If observe is supplied, will be invoked with the iteration cound and the values
  of X and f(X) at each search step."
  ([f q observe]
   (let [rel 1e-10
         abs 1e-10
         convergence-checker (SimpleValueChecker. rel abs)
         o (SimplexOptimizer.
            (proxy [ConvergenceChecker] []
              (converged [iteration ^PointValuePair previous ^PointValuePair current]
                (when observe
                  (observe (vec (.getPoint current)) (.getValue current)))
                (.converged convergence-checker iteration previous current))))
         args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
         (into-array OptimizationData
                     [(NelderMeadSimplex. (count q))
                      (ObjectiveFunction.
                       (proxy [MultivariateFunction] []
                         (value [xs]
                           (f xs))))
                      (MaxEval. 1000)
                      (InitialGuess. (double-array q))
                      GoalType/MINIMIZE])
         p (.optimize o args)]
     [(.getPoint p) (.getValue p)]))
  ([f q]
   (multidimensional-minimize f q nil)))
