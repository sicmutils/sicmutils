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
            [math.calculus.derivative :refer :all]
            [math.function :refer :all]))


;; XXX: R[xyz] should not return an up; they should return a struct
;; of the same shape they were given. But do rotations of covectors
;; work that way? Maybe we should assert up-ness here rather than
;; promise to be more general than we are.

(defn Rx
  "Returns a function which rotates a vector α radians about the x axis."
  [α]
  (fn [[x y z :as v]]
    (let [c (cos α)
          s (sin α)]
      (up x
          (- (* c y) (* s z))
          (+ (* s y) (* c z))))))

(defn Ry
  "Returns a function which rotates a vector α radians about the y axis."
  [α]
  (fn [[x y z]]
    (let [c (cos α)
          s (sin α)]
      (up (+ (* c x) (* s z))
          y
          (- (* c z) (* s x))))))

(defn Rz
  "Returns a function which rotates a vector α radians about the z axis."
  [α]
  (fn [[x y z]]
    (let [c (cos α)
          s (sin α)]
      (up (- (* c x) (* s y))
          (+ (* s x) (* c y))
          z))))

(defn rotate-x-matrix
  "Produce the matrix of a rotation of α radians about the x axis."
  [α]
  (let [c (cos α)
        s (sin α)]
    (down (up 1 0 0) (up 0 c s) (up 0 (- s) c))))

(defn rotate-y-matrix
  "Produce the matrix of a rotation of α radians about the y axis."
  [α]
  (let [c (cos α)
        s (sin α)]
    (down (up c 0 (- s)) (up 0 1 0) (up s 0 c))))

(defn rotate-z-matrix
  "Produce the matrix of a rotation of α radians about the z axis."
  [α]
  (let [c (cos α)
        s (sin α)]
    (down (up c s 0) (up (- s) c 0) (up 0 0 1))))

(defn Euler->M
  "Compute the rotation matrix from a set of Euler angles."
  [[θ φ ψ]]
  (* (rotate-z-matrix φ)
     (rotate-x-matrix θ)
     (rotate-z-matrix ψ)))
