;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.mechanics.rotation
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :refer :all]
            [math.structure :refer :all]
            [math.function :refer :all]))


(defn Rx
  [angle]
  (fn [[x y z]]
    (let [ca (cos angle)
          sa (sin angle)]
      (up x
          (- (* ca y) (* sa z))
          (+ (* sa y) (* ca z))))))

(defn Ry
  [angle]
  (fn [[x y z]]
    (let [ca (cos angle)
          sa (sin angle)]
      (up (+ (* ca x) (* sa z))
          y
          (- (* ca z) (* sa x))))))

(defn Rz
  [angle]
  (fn [[x y z]]
    (let [ca (cos angle)
          sa (sin angle)]
      (up (- (* ca x) (* sa y))
          (+ (* sa x) (* ca y))
          z))))

(defn rotate-x-matrix
  [α]
  (let [c (cos α)
        s (sin α)]
    (down (up 1 0 0) (up 0 c s) (up 0 (- s) c))))

(defn rotate-y-matrix
  [α]
  (let [c (cos α)
        s (sin α)]
    (down (up c 0 (- s)) (up 0 1 0) (up s 0 c))))

(defn rotate-z-matrix
  [α]
  (let [c (cos α)
        s (sin α)]
    (down (up c s 0) (up (- s) c 0) (up 0 0 1))))

(defn Euler->M
  [[θ φ ψ]]
  (* (rotate-z-matrix φ)
     (rotate-x-matrix θ)
     (rotate-z-matrix ψ)))
