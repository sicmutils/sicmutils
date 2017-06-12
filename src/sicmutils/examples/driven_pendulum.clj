;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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
            [sicmutils.examples.pendulum :as pendulum]))

(defn vertical-periodic-drive
  [amplitude frequency phase]
  (fn [t]
    (up 0 (* amplitude (cos (+ (* frequency t) phase))))))

(defn L
  [m l g a ω]
  (pendulum/L m l g (vertical-periodic-drive a ω 0)))

(defn state-derivative
  [m l g a ω]
  (Lagrangian->state-derivative
    (L m l g a ω)))

(defn equations
  []
  (simplify ((state-derivative 'm 'l 'g 'a 'ω)
             (up 't 'θ_0 'θdot_0))))

(defn evolver
  [{:keys [t dt a omega g theta_0 thetadot_0 observe]
    :or {t 1
         dt 1/60
         a 0
         omega 0
         g 9.8
         theta_0 1
         thetadot_0 0}}]
  ((evolve state-derivative
           1.0    ;; mass of bob
           1.0    ;; length of rod
           g      ;; acceleration due to gravity
           a      ;; amplitude of drive
           omega) ;; frequency of drive
   (up 0.0
       theta_0
       thetadot_0)
   observe
   dt
   t
   1.0e-6
   :compile true))
