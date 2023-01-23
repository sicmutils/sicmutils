#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.double-pendulum
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.env :as e :refer [up square cos + - * /]]))

(defn V
  [m1 m2 l1 l2 g]
  (fn [[_ [θ φ] _]]
    (let [y1 (- (* l1 (cos θ)))
          y2 (- y1 (* l2 (cos φ)))]
      (+ (* m1 g y1)
         (* m2 g y2)))))

(defn T
  [m1 m2 l1 l2 _]
  (fn [[_ [θ φ] [θdot φdot]]]
    (let [v1sq (* (square l1) (square θdot))
          v2sq (* (square l2) (square φdot))]
      (+ (* (/ 1 2) m1 v1sq)
         (* (/ 1 2) m2 (+ v1sq
                          v2sq
                          (* 2 l1 l2 θdot φdot (cos (- θ φ)))))))))

(def L
  (- T V))

(defn state-derivative [m1 m2 l1 l2 g]
  (e/Lagrangian->state-derivative
   (L m1 m2 l1 l2 g)))

(defn evolver
  [{:keys [t dt g m1 l1 theta_0 thetadot_0 m2 l2 phi_0 phidot_0 observe]
    :or {t 1
         dt (/ 1 60)
         g 9.8
         m1 1
         l1 0.5
         theta_0 (/ Math/PI 2)
         thetadot_0 0
         m2 1
         l2 0.5
         phi_0 0
         phidot_0 0}}]
  ((e/evolve state-derivative
             m1 ;; mass of bob1
             m2 ;; mass of bob2
             l1 ;; length of rod1
             l2 ;; length of rod2
             g  ;; acceleration due to gravity
             )
   (up 0.0
       (up theta_0 phi_0)
       (up thetadot_0 phidot_0))
   dt
   t
   {:compile? true
    :epsilon 1.0e-6
    :observe observe}))

(defn equations []
  (e/simplify
   ((state-derivative 'm_1 'm_2 'l_1 'l_2 'g)
    (up 't
        (up 'θ_0 'φ_0)
        (up 'θdot_0 'φdot_0)))))
