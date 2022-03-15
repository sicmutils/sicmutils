#_
"Copyright © 2017 Colin Smith.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

(ns sicmutils.examples.central-potential
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.env :as e :refer [abs square up + - * /]]
            #?@(:clj
                [[taoensso.timbre :as log]
                 [hiccup.core :refer [html]]
                 [hiccup.page :refer [html5]]])))

(defn- pairs
  "Return a sequence of pairs of different elements from the given sequence."
  [[x & xs]]
  (when xs
    (concat
     (for [y xs] [x y])
     (pairs xs))))

(defn V [& masses]
  ;; for V we want each distinct pair
  (fn [[_ x _]]
    (let [mass-position-pairs (->> (partition 2 x)
                                   (map (fn [m [x y]] [m (up x y)])
                                        masses)
                                   (pairs))]
      (reduce - 0 (map (fn [[[m1 p1] [m2 p2]]]
                         (/ (* m1 m2)
                            (abs (- p1 p2))))
                       mass-position-pairs)))))

(defn T
  [& masses]
  (fn [[_ _ v]]
    (let [velocities (->> (partition 2 v)
                          (map (fn [[vx vy]]
                                 (up vx vy))))]
      (reduce + (map (fn [m v]
                       (* (/ 1 2) m (square v)))
                     masses
                     velocities)))))

(def L (- T V))

(defn state-derivative
  [m M]
  (e/Lagrangian->state-derivative (L m M)))

(defn evolver
  [{:keys [t dt m M x_0 y_0 xdot_0 ydot_0 observe]
    :or {t 1
         dt 1
         m 1
         M 1
         x_0 1
         y_0 1
         xdot_0 -0.05
         ydot_0 0}}]
  (let [initial-state (up 0.0
                          (up x_0    y_0    0 0)
                          (up xdot_0 ydot_0 0 0))]
    ((e/evolve state-derivative m M)
     initial-state
     dt
     t
     {:compile? true
      :epsilon 1.0e-6
      :observe observe})))

(defn equations
  []
  (e/simplify ((state-derivative 'm 'M)
               (up 't
                   (up 'x_0 'y_0 'x_1 'y_1)
                   (up 'xdot_0 'ydot_0 'xdot_1 'ydot_1)))))

(defn ^:no-doc to-svg
  [evolution]
  [:svg {:width 480 :height 480}
   [:rect {:width 480 :height 480 :fill "#330033"}]
   [:g {:transform "translate(240,240)"}
    ;;[:circle {:fill "green" :stroke "none" :r 5 :cx 0 :cy 0}]
    ;;[:circle {:fill "green" :stroke "none" :r 5 :cx 20 :cy 0}]
    ;;[:circle {:fill "green" :stroke "none" :r 5 :cx 0 :cy 20}]
    (for [[_ x y _ _] evolution]
      [:circle {:fill "orange" :stroke "none" :r 1 :cx x :cy y}]
      )
    (for [[_ _ _ X Y] evolution]
      [:circle {:fill "green" :stroke "none" :r 1 :cx X :cy Y}])]])

;; Simó's initial data
;; x_1=−x2=0.97000436−0.24308753i,x3=0; V~ = ˙x3=−2 ˙x_1=−2 ˙x2=−0.93240737−0.86473146i
;; T =12T =6.32591398, I(0)=2, m1=m2=m3=1

#?(:clj
   (defn ^:no-doc -main
     [& _]
     (let [head [:head {:title "foo"}]
           counter (atom 0)
           body [:body
                 (for [dy (range -10 -1 1/10)]
                   (let [svg (to-svg (evolver {:t 100 :dt 1/3 :M 500 :m 500 :x_0 50 :y_0 50 :xdot_0 0 :ydot_0 dy}))]
                     (log/info (str "dy " dy))
                     (spit (format "%03d.svg" @counter) (html svg))
                     (swap! counter inc)
                     svg))]]
       (println (html5 head body)))))
