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

^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns sicmutils.examples.central-potential
  (:refer-clojure :exclude [+ - * /])
  (:require [nextjournal.clerk :as clerk]
            [sicmutils.env :as e :refer [abs square up + - * /]]
            [taoensso.timbre :as log]))

;; ## Central Potential!!
;;
;; This notebook implements this paper: https://arxiv.org/pdf/math/0011268.pdf
;;
;; Using a variational method, we exhibit a surprisingly simple periodic orbit
;; for the newtonian problem of three equal masses in the plane.

;; ### Simó's initial data

;; $$
;; x_1= −x_2 = 0.97000436−0.24308753i, \\
;; x3=0; \\
;; \vec{V} = \dot{x}_3 = −2\dot{x}_1 = −2\dot{x}_2 = −0.93240737−0.86473146i \\
;; \bar{T} =12T =6.32591398, I(0)=2, m_1 = m_2 = m_3 = 1
;; $$

;; Note that we could have used [[sicmutils.util.permute/combinations]].
;;
;; TODO get this running, actually animating, in MathBox. Can we freaking do
;; that? How awesome would that be? I think it will be possible... by opting in
;; to the JS stuff.

(defn- pairs
  "Return a sequence of each distinct pairing of different elements from the given
  sequence."
  [[x & xs]]
  (when xs
    (concat (for [y xs] [x y])
            (pairs xs))))

;; for V we want each distinct pair, funky way to do it:

(defn V [& masses]
  (fn [[_ x _]]
    (let [mass-position-pairs
          (->> (partition 2 x)
               (map (fn [m [x y]] [m (up x y)])
                    masses)
               (pairs))]
      (reduce - 0 (map (fn [[[m1 p1] [m2 p2]]]
                         (/ (* m1 m2)
                            (abs (- p1 p2))))
                       mass-position-pairs)))))

(defn T [& masses]
  (fn [[_ _ v]]
    (let [velocities (->> (partition 2 v)
                          (map (fn [[vx vy]]
                                 (up vx vy))))]
      (apply + (map (fn [m v]
                      (* (/ 1 2) m (square v)))
                    masses
                    velocities)))))

(def L (- T V))

(def state-derivative
  (comp e/Lagrangian->state-derivative L))

(defn my-evolver
  [kick {:keys [t dt]
         :or {t 1 dt 1}}]
  (let [initial-state (up 0.0
                          (+ kick (up 0.97000436
                                      -0.24308753
                                      -0.97000436
                                      0.24308753
                                      0
                                      0))
                          (up (/ -0.93240737 -2)
                              (/ -0.86473146 -2)
                              (/ -0.93240737 -2)
                              (/ -0.86473146 -2)
                              -0.93240737
                              -0.86473146))]
    (e/integrate-state-derivative
     state-derivative [1 1 1] initial-state t dt)))

(defn equations
  [m M]
  (e/->TeX
   (e/simplify
    ((state-derivative m M)
     (up 't
         (up 'x_0 'y_0 'x_1 'y_1)
         (up 'xdot_0 'ydot_0 'xdot_1 'ydot_1))))))

#_
(clerk/tex
 (equations 'm 'M))

(defn ^:no-doc to-svg
  ([evolution]
   (to-svg evolution {}))
  ([evolution {:keys [scale] :or {scale 1}}]
   [:svg {:width 700 :height 480}
    [:rect {:width 700 :height 480 :fill "#330033"}]
    [:g {:transform
         (str "translate(350,240) scale(" scale ")")}
     (mapcat
      (fn [[_ q]]
        (map (fn [[x y] color]
               [:circle {:fill color :stroke "none"
                         :r (/ 1 scale)
                         :cx x
                         :cy y}])
             (partition 2 q)
             ["red" "green" "blue" "indigo" "violet"]))
      evolution)]]))

(clerk/html
 (to-svg
  (my-evolver (up 0.0 0.0 0.0 0.0 0.0 0.0) {:t 20 :dt 0.01})
  {:scale 200}))

(clerk/html
 (to-svg
  (my-evolver (up 0.002 0.0 0.0 0.0 0.0 0.0) {:t 20 :dt 0.01})
  {:scale 200}))

(clerk/html
 (to-svg
  (my-evolver (up 0.010 0.0 0.0 0.0 0.0 0.0) {:t 20 :dt 0.01})
  {:scale 200}))

(clerk/html
 (to-svg
  (my-evolver (up 0.010 0.0 0.0 0.01 0.01 0.0) {:t 20 :dt 0.01})
  {:scale 200}))

(clerk/html
 (to-svg
  (my-evolver (up 0.010 0.01 0.0 0.01 0.01 0.0) {:t 20 :dt 0.01})
  {:scale 200}))

(defn evolver
  [{:keys [t dt m M x_0 y_0 xdot_0 ydot_0]
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
    (e/integrate-state-derivative
     state-derivative
     [m M] initial-state t dt)))

(defn ^:no-doc generate-svgs []
  [:div
   (for [dy (take 2 (range -10 -1 (/ 1 10)))]
     (let [svg (to-svg
                (evolver
                 {:t 100
                  :dt (/ 1 3)
                  :M 500
                  :m 500
                  :x_0 50
                  :y_0 50
                  :xdot_0 0
                  :ydot_0 dy}))]
       (log/info (str "dy " dy))
       svg))])

(def runner
  (clerk/html
   (generate-svgs)))
