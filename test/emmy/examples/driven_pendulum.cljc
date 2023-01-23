#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.driven-pendulum
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.env :as e :refer [cos up + * /]]
            [emmy.examples.pendulum :as pendulum]))

(defn vertical-periodic-drive
  [amplitude frequency phase]
  (fn [t]
    (up 0 (* amplitude (cos (+ (* frequency t) phase))))))

(defn L
  [m l g a ω]
  (pendulum/L m l g (vertical-periodic-drive a ω 0)))

(defn state-derivative
  [m l g a ω]
  (e/Lagrangian->state-derivative
   (L m l g a ω)))

(defn equations
  []
  (e/simplify ((state-derivative 'm 'l 'g 'a 'ω)
               (up 't 'θ_0 'θdot_0))))

(defn evolver
  [{:keys [t dt a omega g theta_0 thetadot_0 observe]
    :or {t 1
         dt (/ 1 60)
         a 0
         omega 0
         g 9.8
         theta_0 1
         thetadot_0 0}}]
  ((e/evolve state-derivative
             1.0    ;; mass of bob
             1.0    ;; length of rod
             g      ;; acceleration due to gravity
             a      ;; amplitude of drive
             omega) ;; frequency of drive
   (up 0.0 theta_0 thetadot_0)
   dt
   t
   {:compile? true
    :epsilon 1.0e-6
    :observe observe}))
