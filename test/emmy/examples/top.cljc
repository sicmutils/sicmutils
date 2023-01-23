#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.top
  (:refer-clojure :exclude [- *])
  (:require [emmy.env :as e :refer [cos up - *]]
            [emmy.mechanics.rigid :as rigid]))

(defn L
  [A B C gMR]
  (let [T (comp e/simplify (rigid/T-rigid-body A B C))
        V (fn [[_ [theta _ _]]]
            (* gMR (cos theta)))]
    (- T V)))

(defn L-axisymmetric
  [A C gMR]
  (L A A C gMR))

(defn state-derivative
  [A B C gMR]
  (e/Lagrangian->state-derivative
   (L A B C gMR)))

(defn equations []
  (e/simplify
   ((state-derivative 'A 'B 'C 'gMR)
    (up 't (up 'theta 'phi 'psi) (up 'thetadot 'phidot 'psidot)))))
