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

(ns math.numerical.ode
  (:require [math.structure :as s])
  (:import (org.apache.commons.math3.ode.nonstiff GraggBulirschStoerIntegrator)
           (org.apache.commons.math3.ode FirstOrderDifferentialEquations)
           (org.apache.commons.math3.ode.sampling StepHandler StepInterpolator)))

(defn- make-integrator
  [d:dt]
  (fn [initial-state observe step-size t ε]
    (let [state->array #(-> % flatten double-array)
          array->state #(s/unflatten % initial-state)
          initial-state-array (state->array initial-state)
          dimension (alength initial-state-array)
          integrator (GraggBulirschStoerIntegrator. 0. 1. (double ε) (double ε))
          equations (proxy [FirstOrderDifferentialEquations] []
                      (computeDerivatives
                        [t y out]
                        (let [y' (-> y array->state d:dt state->array)]
                          (System/arraycopy y' 0 out 0 (alength y'))))
                      (getDimension [] dimension))
          out (double-array dimension)]
      (when observe
        (.addStepHandler
         integrator
         (proxy [StepHandler] []
           (handleStep
             [^StepInterpolator interpolator is-last]
             (let [it0 (.getPreviousTime interpolator)
                   it1 (.getCurrentTime interpolator)
                   adjust (mod it0 step-size)
                   t0 (if (> adjust 0) (+ (- it0 adjust) step-size) it0)]
               (doseq [t (range t0 it1 step-size)]
                 (.setInterpolatedTime interpolator t)
                 (observe t (-> interpolator .getInterpolatedState array->state)))
               (when is-last
                 (.setInterpolatedTime interpolator it1)
                 (observe it1 (-> interpolator .getInterpolatedState array->state)))))
           (init [_ _ _]))))
      (.integrate integrator equations 0 initial-state-array t out)
      (-> out array->state))))

(defn state-advancer
  [state-derivative & state-derivative-args]
  (let [d:dt (apply state-derivative state-derivative-args)
        I (make-integrator d:dt)]
    (fn [initial-state t ε]
      (I initial-state nil 0 t ε))))

(defn evolve
  [state-derivative & state-derivative-args]
  (let [d:dt (apply state-derivative state-derivative-args)]
    (make-integrator d:dt)))
