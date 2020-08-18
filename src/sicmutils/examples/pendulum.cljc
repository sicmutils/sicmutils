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

;; The simple pendulum supported at the point x (in the plane; x should
;; be a function of t returning an up-tuple (up xt yt).

(ns sicmutils.examples.pendulum
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-env!)

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
