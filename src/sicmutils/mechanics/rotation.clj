;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.mechanics.rotation
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [sicmutils
             [generic :refer :all]
             [structure :refer :all]
             [matrix :as matrix]
             [function :refer :all]]
            [sicmutils.calculus.derivative :refer :all]))

;; XXX: R[xyz] should not return an up; they should return a struct
;; of the same shape they were given. But do rotations of covectors
;; work that way? Maybe we should assert up-ness here rather than
;; promise to be more general than we are.

(defn Rx
  "Returns a function which rotates a vector α radians about the x axis."
  [α]
  (fn [[x y z]]
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

(defn ^:private rotate-x-matrix-2
  [c s]
  (matrix/by-rows [1 0 0]
                  [0 c (- s)]
                  [0 s c]))

(defn rotate-x-matrix
  "Produce the matrix of a rotation of α radians about the x axis."
  [α]
  (rotate-x-matrix-2 (cos α) (sin α)))

(defn ^:private rotate-y-matrix-2
  [c s]
  (matrix/by-rows  [c 0 s]
                   [0 1 0]
                   [(- s) 0 c]))

(defn rotate-y-matrix
  "Produce the matrix of a rotation of α radians about the y axis."
  [α]
  (rotate-y-matrix-2 (cos α) (sin α)))

(defn rotate-y-tuple-2
  [c s]
  (matrix/m->s (down 'ignore 'ignore 'ignore)
               (rotate-y-matrix-2 c s)
               (up 'ignore 'ignore 'ignore)))

(defn rotate-y-tuple
  [α]
  (rotate-y-tuple-2 (cos α) (sin α)))

(defn ^:private rotate-z-matrix-2
  "Produce the matrix of a rotation of α radians about the z axis."
  [c s]
  (matrix/by-rows [c (- s) 0]
                  [s c 0]
                  [0 0 1]))

(defn rotate-z-matrix
  "Produce the matrix of a rotation of α radians about the z axis."
  [α]
  (rotate-z-matrix-2 (cos α) (sin α)))

(defn ^:private rotate-x-tuple-2
  [c s]
  (matrix/m->s (down 'ignore 'ignore 'ignore)
               (rotate-x-matrix-2 c s)
               (up 'ignore 'ignore 'ignore)))

(defn rotate-x-tuple
  [α]
  (rotate-x-tuple-2 (cos α) (sin α)))

(defn ^:private rotate-z-tuple-2
  [c s]
  (matrix/m->s (down 'ignore 'ignore 'ignore)
               (rotate-z-matrix-2 c s)
               (up 'ignore 'ignore 'ignore)))

(defn rotate-z-tuple
  [α]
  (rotate-z-tuple-2 (cos α) (sin α)))

(defn Euler->M
  "Compute the rotation matrix from a set of Euler angles."
  [[θ φ ψ]]
  (* (rotate-z-matrix φ)
     (rotate-x-matrix θ)
     (rotate-z-matrix ψ)))
