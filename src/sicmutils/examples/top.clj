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
