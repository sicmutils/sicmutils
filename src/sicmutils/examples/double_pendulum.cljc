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
(ns sicmutils.examples.double-pendulum
  (:refer-clojure :exclude [+ - * /])
  (:require [nextjournal.clerk :as clerk]
            [sicmutils.env :as e :refer [up square cos + - * /]]))

;; ## Double Pendulum

(def ->tex (comp clerk/tex e/->TeX))

(defn V
  [m1 m2 l1 l2 g]
  (fn [[_ [θ φ] _]]
    (let [y1 (- (* l1 (cos θ)))
          y2 (- y1 (* l2 (cos φ)))]
      (+ (* m1 g y1)
         (* m2 g y2)))))

(defn T
  [m1 m2 l1 l2 _]
  (fn [[_ [θ φ] [θdot φdot]]]
    (let [v1sq (* (square l1) (square θdot))
          v2sq (* (square l2) (square φdot))]
      (+ (* (/ 1 2) m1 v1sq)
         (* (/ 1 2) m2 (+ v1sq
                          v2sq
                          (* 2 l1 l2 θdot φdot (cos (- θ φ)))))))))

(def L (- T V))

(defn state-derivative [m1 m2 l1 l2 g]
  (e/Lagrangian->state-derivative
   (L m1 m2 l1 l2 g)))

(defn evolver
  [{:keys [t dt g m1 l1 theta_0 thetadot_0 m2 l2 phi_0 phidot_0 observe]
    :or {t 1
         dt (/ 1 60)
         g 9.8
         m1 1
         l1 0.5
         theta_0 (/ Math/PI 2)
         thetadot_0 0
         m2 1
         l2 0.5
         phi_0 0
         phidot_0 0}}]
  ((e/evolve state-derivative
             m1 ;; mass of bob1
             m2 ;; mass of bob2
             l1 ;; length of rod1
             l2 ;; length of rod2
             g  ;; acceleration due to gravity
             )
   (up 0.0
       (up theta_0 phi_0)
       (up thetadot_0 phidot_0))
   dt
   t
   {:compile? true
    :epsilon 1.0e-6
    :observe observe}))

(defn equations []
  (e/simplify
   ((state-derivative 'm_1 'm_2 'l_1 'l_2 'g)
    (up 't
        (up 'θ_0 'φ_0)
        (up 'θdot_0 'φdot_0)))))


;; Equations of motion:

(->tex (equations))

(defn ^:no-doc to-svg
  ([evolution]
   (to-svg evolution {}))
  ([evolution {:keys [t-scale scale] :or {t-scale 1 scale 1}}]
   [:svg {:width 700 :height 480}
    [:rect {:width 700 :height 480 :fill "#330033"}]
    [:g {:transform "translate(0,240)"}
     (mapcat (fn [[t [theta phi]]]
               [[:circle {:fill "red" :stroke "none"
                          :r 1
                          :cx (* t-scale t)
                          :cy (* scale (- theta))}]
                [:circle {:fill "blue" :stroke "none"
                          :r 1
                          :cx (* t-scale t)
                          :cy (* scale (- phi))}]])
             evolution)]]))

(defn to-view []
  (let [o (atom (transient []))
        observe (fn [_ q] (swap! o conj! q))]
    (evolver {:t 100 :dt (/ 1 60) :observe observe})
    (let [result (persistent! @o)]
      (to-svg result {:t-scale 10
                      :scale 40}))))

;; TODO: instead of generating an SVG, OBVIOUSLY what I want to do is use
;; vega-lite to view this stuff. Generate the dataset once. In fact I already
;; did that a while back.

(clerk/html
 (to-view))
