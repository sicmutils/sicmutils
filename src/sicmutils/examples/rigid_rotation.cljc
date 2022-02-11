;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.examples.rigid-rotation
  (:require [sicmutils.env :as e :refer [up]]
            [sicmutils.mechanics.rigid :as r]))

(defn evolver
  [t dt A B C θ0 φ0 ψ0 θdot0 φdot0 ψdot0]
  (let [state-history (atom [])
        L (r/Euler-state->L-space A B C)]
    ((e/evolve r/rigid-sysder
               ;; moments of inertia
               A B C)
     (up 0.0
         (up θ0 φ0 ψ0)
         (up θdot0 φdot0 ψdot0))
     dt
     t
     {:compile? true
      :epsilon 1.0e-6
      :observe (fn [t [_ [θ φ ψ] _ :as local]]
                 (swap! state-history conj [t θ φ ψ (seq (L local))]))})
    @state-history))
