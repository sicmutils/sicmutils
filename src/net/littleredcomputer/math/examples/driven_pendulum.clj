(ns net.littleredcomputer.math.examples.driven-pendulum
  (:refer-clojure :exclude [+ - * /])
  (:require [net.littleredcomputer.math.env :refer :all]
            [net.littleredcomputer.math.mechanics.lagrange :refer :all]))

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
  [t dt A ω g θ0 θdot0]
  (let [drive (periodic-drive A ω 0)
        state-history (atom [])]
    ((evolve pend-state-derivative
             1.0 ;; mass of bob
             1.0 ;; length of rod
             g   ;; acceleration due to gravity
             A   ;; amplitude of drive
             ω   ;; frequency of drive
             0   ;; phase of drive
             )
      (up 0.0
          θ0
          θdot0)
      (fn [t [_ q _]] (swap! state-history conj [t q (drive t)]))
      dt
      t
      1.0e-6
      {:compile true})
    @state-history))
