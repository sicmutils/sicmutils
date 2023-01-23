#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.rigid-rotation
  (:require [emmy.env :as e :refer [up]]
            [emmy.mechanics.rigid :as r]))

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
