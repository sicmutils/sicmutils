;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.examples.double-pendulum
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [sicmutils.env :refer :all]
            [sicmutils.mechanics.lagrange :refer :all]))

(defn- coords
  [l1 l2 θ φ]
  (let [x1 (* l1 (sin θ))
        y1 (- (* l1 (cos θ)))
        x2 (+ x1 (* l2 (sin φ)))
        y2 (- y1 (* l2 (cos φ)))]
    [x1 y1 x2 y2]))

(defn V-double-pend
  [m1 m2 l1 l2 g]
  (fn [[_ [θ φ] _]]
    (let [[_ y1 _ y2] (coords l1 l2 θ φ)]
      (+ (* m1 g y1)
         (* m2 g y2)))))

(defn T-double-pend
  [m1 m2 l1 l2 _]
  (fn [[_ [θ φ] [θdot φdot]]]
    (let [v1sq (* (square l1) (square θdot))
          v2sq (* (square l2) (square φdot))]
      (+ (* 1/2 m1 v1sq)
         (* 1/2 m2 (+ v1sq
                      v2sq
                      (* 2 l1 l2 θdot φdot (cos (- θ φ)))))))))

(def L-double-pend
  (- T-double-pend V-double-pend))

(defn- pend-state-derivative  [m1 m2 l1 l2 g]
  (Lagrangian->state-derivative
    (L-double-pend m1 m2 l1 l2 g)))

(defn evolver
  [t dt g m1 l1 θ0 θdot0 m2 l2 φ0 φdot0]
  (let [state-history (atom [])]
    ((evolve pend-state-derivative
             m1 ;; mass of bob1
             m2 ;; mass of bob2
             l1 ;; length of rod1
             l2 ;; length of rod2
             g  ;; acceleration due to gravity
             )
     (up 0.0
          (up θ0 φ0)
          (up θdot0 φdot0))
     (fn [t [_ q _]] (swap! state-history conj (into [t] q)))
     dt
     t
     1.0e-6
     :compile true)
    @state-history))

;(def equations
;  (simplify ((pend-state-derivative 'm_1 'm_2 'l_1 'l_2 'g)
;              (up 't
;                  (up 'θ_0 'φ_0)
;                  (up 'θdot_0 'φdot_0)))))
