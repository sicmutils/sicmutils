;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.numerical.minimize
  (:require [sicmutils.util :as u]
            [sicmutils.util.stopwatch :as us]
            [taoensso.timbre :as log])
  #?(:clj
     (:import (org.apache.commons.math3.optim.univariate
               BrentOptimizer
               UnivariateObjectiveFunction
               SearchInterval
               UnivariatePointValuePair)
              (org.apache.commons.math3.analysis
               UnivariateFunction
               MultivariateFunction)
              (org.apache.commons.math3.optim.nonlinear.scalar
               GoalType
               ObjectiveFunction)
              (org.apache.commons.math3.optim
               MaxEval
               OptimizationData
               ConvergenceChecker
               PointValuePair))))

(defn minimize
  "Find the minimum of the function f: R -> R in the interval [a,b]. If
  observe is supplied, will be invoked with the iteration count and the
  values of x and f(x) at each search step."
  ([f a b observe]
   #?(:cljs
      (u/unsupported "minimize isn't yet implemented in Clojurescript.")

      :clj
      (let [total-time (us/stopwatch :started? true)
            evaluation-time (us/stopwatch :started? false)
            evaluation-count (atom 0)
            rel 1e-5
            abs 1e-5
            o (BrentOptimizer.
               rel abs
               (reify ConvergenceChecker
                 (converged [_ _ _ current]
                   (when observe
                     (observe (.getPoint ^UnivariatePointValuePair current)
                              (.getValue ^UnivariatePointValuePair current)))
                   false)))
            args ^"[Lorg.apache.commons.math3.optim.OptimizationData;"
            (into-array OptimizationData
                        [(UnivariateObjectiveFunction.
                          (reify UnivariateFunction
                            (value [_ x]
                              (us/start evaluation-time)
                              (swap! evaluation-count inc)
                              (let [fx (f x)]
                                (us/stop evaluation-time)
                                fx))))
                         (MaxEval. 1000)
                         (SearchInterval. a b)
                         GoalType/MINIMIZE])
            p (.optimize o args)]
        (let [x (.getPoint p)
              y (.getValue p)]
          (when observe
            (observe (dec (.getEvaluations o)) x y))
          (us/stop total-time)
          (log/info "#" @evaluation-count "total" (us/repr total-time) "f" (us/repr evaluation-time))
          [x y @evaluation-count]))))
  ([f a b]
   (minimize f a b nil)))

(defn ^:private make-initial-simplex
  "Takes an n-vector x0 and returns a list of n+1 n-vectors, of which x0 is the
  first, and the remainder are formed by perturbing each coordinate in turn"
  [x0]
  (let [nonzdelt 0.05
        zdelt 0.00025
        N (count x0)]
    (into [x0]
          (for [i (range N)
                :let [xi (nth x0 i)]]
            (assoc x0 i (if (zero? xi) zdelt (* (+ 1 nonzdelt) xi)))))))

(defn ^:private sup-norm
  "Subtracts the first point in the simplex from all the others, and
  finds the largest remaining coordinate absolute value."
  [simplex]
  (reduce max (map u/compute-abs (flatten (map (fn [v] (map - v (first simplex))) simplex)))))

(defn multidimensional-minimize
  "Find the minimum of the function f: R^n -> R, given an initial point q ∈ R^n.
  If :callback is supplied, will be invoked with the intermediate points of evaluation.
  If :info is true, wraps the result with evaluation information.
  TODO: document other options

  See Gao, F. and Han, L.
      Implementing the Nelder-Mead simplex algorithm with adaptive
      parameters. 2012. Computational Optimization and Applications.
      51:1, pp. 259-277

  I gratefully acknowledge the Python implementation in SciPy, which I have imitated here.
  "
  [func x0 & {:keys [adaptive callback maxiter maxfun xatol fatol info]
              :or {xatol 1e-4, fatol 1e-4}}]

  (let [v+ #(mapv + %1 %2)                ;; add two vectors elementwise
        v- #(mapv - %1 %2)                ;; subtract two vectors elementwise
        v* (fn [s v] (mapv #(* s %) v))   ;; multiply vector v by scalar s
        ;; should we rely on something for the vector math, or keep this dependency-free?
        fncall_count (atom 0)
        f (fn [x]
            (swap! fncall_count inc)
            (func x))
        N (count x0)
        rho 1.0
        chi (if adaptive (+ 1.0 (/ 2.0 N)) 2)
        psi (if adaptive (- 0.75 (/ (* 2.0 N))) 0.5)
        sigma (if adaptive (- 1 (/ N)) 0.5)
        maxiter (or maxiter (* N 200))
        maxfun (or maxfun (* N 200))
        shrink (fn [s] (let [s0 (first s)]
                         (into [s0] (map #(v+ s0 (v* sigma (v- % s0))) (next s)))))]
    (loop [simplex (make-initial-simplex x0)
           fsimplex (mapv f simplex)
           iterations 0]
      (let [indices-by-f-value (sort #(compare (nth fsimplex %1) (nth fsimplex %2)) (range 0 (inc N)))
            ;; sort simplex and f(simplex) in ascending order of function value
            simplex (mapv simplex indices-by-f-value)
            fsimplex (mapv fsimplex indices-by-f-value)]

        (when callback (callback (first simplex)))

        (if (or (> iterations maxiter)
                (> @fncall_count maxfun)
                (and (<= (sup-norm simplex) xatol)
                     (<= (reduce max (map u/compute-abs (map #(- % (first fsimplex)) fsimplex))) fatol)))
          ;; RETURN
          (if info
            {:result (first simplex)
             :value (first fsimplex)
             :iterations iterations
             :fncalls @fncall_count}
            (first simplex))
          ;; CARRY OUT A STEP
          (let [xbar (v* (/ N) (reduce v+ (butlast simplex)))
                xr (v- (v* (+ 1 rho) xbar) (v* rho (last simplex)))
                fxr (f xr)]
            (if (< fxr (first fsimplex))
              (let [xe (v- (v* (+ 1 (* rho chi)) xbar) (v* (* rho chi) (last simplex)))
                    fxe (f xe)]
                (if (< fxe fxr)
                  (recur (assoc simplex N xe)
                         (assoc fsimplex N fxe)
                         (inc iterations))
                  (recur (assoc simplex N xr)
                         (assoc fsimplex N fxr)
                         (inc iterations))))

              ;; fsim[0] <= fxr
              (if (< fxr (nth fsimplex (dec N)))
                (recur (assoc simplex N xr)
                       (assoc fsimplex N fxr)
                       (inc iterations))
                ;; fxr >= fsim[-2]
                (if (< fxr (nth fsimplex N))
                  ;; perform contraction
                  (let [xc (v- (v* (+ 1 (* psi rho)) xbar) (v* (* psi rho) (last simplex)))
                        fxc (f xc)]
                    (if (<= fxc fxr)
                      (recur (assoc simplex N xc)
                             (assoc fsimplex N fxc)
                             (inc iterations))
                      (let [s (shrink simplex)
                            fs (mapv f s)]
                        (recur s fs (inc iterations)))))
                  ;; perform inside contraction
                  (let [xcc (v+ (v* (- 1 psi) xbar) (v* psi (last simplex)))
                        fxcc (f xcc)]
                    (if (< fxcc (nth fsimplex N))
                      (recur (assoc simplex N xcc)
                             (assoc fsimplex N fxcc)
                             (inc iterations))
                      (let [s (shrink simplex)
                            fs (mapv f s)]
                        (recur s fs (inc iterations))))))))))))))
