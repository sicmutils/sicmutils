(ns sicmutils.examples.top
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [sicmutils.env :refer :all]
            [sicmutils.mechanics.rigid :as rigid]))

(defn L
  [A B C gMR]
  (let [T (rigid/T-rigid-body A B C)
        V (fn [[t [theta _ _] qdot]]
            (* gMR (cos theta)))]
    (- T V)))

(defn L-axisymmetric
  [A C gMR]
  (L A A C gMR))

(defn state-derivative
  [A B C gMR]
  (Lagrangian->state-derivative
   (L A B C gMR)))

(defn equations
  []
  (simplify ((state-derivative 'A 'B 'C 'gMR)
             (up 't (up 'theta 'phi 'psi) (up 'thetadot 'phidot 'psidot)))))
