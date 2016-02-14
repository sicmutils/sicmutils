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

(ns sicmutils.examples.driven-pendulum
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [sicmutils.env :refer :all]
            [sicmutils.mechanics.lagrange :refer :all]))

(defn T-pend
  [m l _ y]
  (let [y' (D y)]
    (fn [[t θ θdot]]
      (* 1/2 m
         (+ (square (* l θdot))
            (square (y' t))
            (* 2 l (y' t) θdot (sin θ)))))))

(defn V-pend
  [m l g y]
  (fn [[t θ _]]
    (* m g (- (y t) (* l (cos θ))))))

(def L-pend (- T-pend V-pend))

(defn periodic-drive
  [A ω φ]
  #(-> % (* ω) (+ φ) cos (* A)))

(defn pend-state-derivative
  [m l g A ω φ]
  (Lagrangian->state-derivative
    (L-pend m l g (periodic-drive A ω φ))))

;(def equations
;  (simplify ((pend-state-derivative 'm 'l 'g 'A 'ω 'φ)
;              (up 't 'θ_0 'θdot_0))))

(defn evolver
  [& {:keys [t dt A omega g theta0 thetadot0 observe]
      :or {t 1
           dt 1/60
           A 0
           omega 0
           g 9.8
           theta0 (/ 2 pi)
           thetadot0 0}}]
  (let [drive (periodic-drive A omega 0)]
    ((evolve pend-state-derivative
             1.0   ;; mass of bob
             1.0   ;; length of rod
             g     ;; acceleration due to gravity
             A     ;; amplitude of drive
             omega ;; frequency of drive
             0     ;; phase of drive
             )
     (up 0.0
         theta0
         thetadot0)
     observe
     dt
     t
     1.0e-6
     :compile true)))
