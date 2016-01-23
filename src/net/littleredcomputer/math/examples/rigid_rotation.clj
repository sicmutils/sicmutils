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

(ns net.littleredcomputer.math.examples.rigid-rotation
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [net.littleredcomputer.math.env :refer :all]
            [net.littleredcomputer.math.mechanics.rigid :refer :all]))

(defn evolver
  [t dt A B C θ0 φ0 ψ0 θdot0 φdot0 ψdot0]
  (let [state-history (atom [])
        L (Euler-state->L-space A B C)]
    ((evolve rigid-sysder
             A B C                                                ;; moments of inertia
             )
     (up 0.0
         (up θ0 φ0 ψ0)
         (up θdot0 φdot0 ψdot0))
     (fn [t [_ [θ φ ψ] _ :as local]]
       (swap! state-history conj [t θ φ ψ (seq (L local))]))
     dt
     t
     1.0e-6
     {:compile true})
    @state-history))

;; (def equations
;;   (simplify ((rigid-sysder 'A 'B 'C)
;;              (up 't
;;                  (up 'θ 'φ 'ψ)
;;                  (up 'θdot 'φdot 'ψdot)))))
