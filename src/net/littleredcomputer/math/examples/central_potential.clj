(ns net.littleredcomputer.math.examples.central-potential
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.tools.logging :as log]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [net.littleredcomputer.math.env :refer :all]
            [net.littleredcomputer.math.mechanics.lagrange :refer :all]))

(defn- pairs
  "Return a sequence of pairs of different elements from the given sequence."
  [[x & xs]]
  (when xs
    (concat
     (for [y xs] [x y])
     (pairs xs))))

(defn V
  [& masses]
  ;; for V we want each distinct pair
  (fn [[t x v]]
    (let [mass-position-pairs (->> x
                                   (partition 2)
                                   (apply up)
                                   (map (fn [m [x y]] [m (up x y)]) masses)
                                   pairs)]
      (reduce - 0
              (map (fn [[[m1 p1] [m2 p2]]]
                     (/ (* m1 m2) (sqrt (square (- p1 p2)))))
                   mass-position-pairs)))))

(defn T
  [& masses]
  (fn [[t x v]]
    (let [velocities (->> v (partition 2) (map #(apply up %)))]
      (reduce + (map #(* 1/2 %1 (square %2)) masses velocities)))))

(def L (- T V))

(defn state-derivative
  [m M]
  (Lagrangian->state-derivative (L m M)))

(defn evolver
  [t dt m M x0 y0 xDot0 yDot0]
  (let [state-history (atom [])
        initial-state (up 0.0
                          (up x0    y0    0 0)
                          (up xDot0 yDot0 0 0))]
    ((evolve state-derivative m M)
     initial-state
     (fn [t [_ q _]]
       (swap! state-history conj (into [t] q)))
     dt
     t
     1.0e-6
     {:compile true})
    @state-history))

(defn- to-svg
  [evolution]
  [:svg {:width 480 :height 480}
   [:rect {:width 480 :height 480 :fill "#330033"}]
   [:g {:transform "translate(240,240)"}
    ;;[:circle {:fill "green" :stroke "none" :r 5 :cx 0 :cy 0}]
    ;;[:circle {:fill "green" :stroke "none" :r 5 :cx 20 :cy 0}]
    ;;[:circle {:fill "green" :stroke "none" :r 5 :cx 0 :cy 20}]
    (for [[t x y X Y] evolution]
      [:circle {:fill "orange" :stroke "none" :r 1 :cx x :cy y}]
      )
    (for [[t x y X Y] evolution]
      [:circle {:fill "green" :stroke "none" :r 1 :cx X :cy Y}]
      )]])

;; Simó's initial data
;; x1=−x2=0.97000436−0.24308753i,x3=0; V~ = ˙x3=−2 ˙x1=−2 ˙x2=−0.93240737−0.86473146i
;; T =12T =6.32591398, I(0)=2, m1=m2=m3=1

(defn -main
  [& args]
  (let [head [:head {:title "foo"}]
        counter (atom 0)
        body [:body
              (for [dy (range -10 -1 1/10)]
                (let [svg (to-svg (evolver 100 1/3 500 500 50 50 0 dy))]
                  (log/info (str "dy " dy))
                  (spit (format "%03d.svg" @counter) (html svg))
                  (swap! counter inc)
                  svg))]]
    (println (html5 head body))))
