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
           (org.apache.commons.math3.ode FirstOrderDifferentialEquations)))

(println "hello")

(defn state-advancer
  [state-derivative & state-derivative-args]
  (let [d:dt (apply state-derivative state-derivative-args)]
    (fn [initial-state t epsilon]
      (let [initial-state-array (-> initial-state flatten double-array)
            dimension (alength initial-state-array)
            integrator (GraggBulirschStoerIntegrator. 0. 1. epsilon epsilon)
            equations (proxy [FirstOrderDifferentialEquations] []
                        (computeDerivatives
                          [t y out]
                          (let [y' (-> y
                                       seq
                                       (s/unflatten initial-state)
                                       d:dt
                                       flatten
                                       double-array)]
                            (prn "ode-step" (seq y'))
                            (System/arraycopy y' 0 out 0 (alength y'))))
                        (getDimension [] dimension))
            y1 (double-array dimension)]
        (.integrate integrator equations 0 initial-state-array t y1)
        (-> y1 seq (s/unflatten initial-state))))))
