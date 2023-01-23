#_"SPDX-License-Identifier: GPL-3.0"

;; The simple pendulum supported at the point x (in the plane; x should
;; be a function of t returning an up-tuple (up xt yt).

(ns emmy.examples.pendulum
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [emmy.env :as e :refer [D sin cos square + - * / ref]]))

(defn T
  [m l _ x]
  (let [v (D x)]
    (fn [[t θ θdot]]
      (let [[vx vy :as vt] (v t)]
        (* (/ 1 2) m
           (+ (square vt)
              (* 2 l θdot (+ (* vy (sin θ)) (* vx (cos θ))))
              (square (* l θdot))))))))

(defn V
  [m l g x]
  (fn [[t θ _]]
    (* m g (- (ref (x t) 1) (* l (cos θ))))))

(def L (- T V))
