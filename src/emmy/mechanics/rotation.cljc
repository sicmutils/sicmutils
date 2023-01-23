#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.rotation
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.generic :as g :refer [cos sin + - * /]]
            [emmy.matrix :as matrix]
            [emmy.structure :as s :refer [up]]
            [emmy.util.stream :as us]
            [emmy.value :as v]))

(defn- rotate-x-matrix-2 [c s]
  (matrix/by-rows [1 0 0]
                  [0 c (- s)]
                  [0 s c]))

(defn rotate-x-matrix
  "Produce the matrix of a rotation of α radians about the x axis."
  [α]
  (rotate-x-matrix-2 (cos α) (sin α)))

(def Rx-matrix rotate-x-matrix)

(defn- rotate-y-matrix-2 [c s]
  (matrix/by-rows  [c 0 s]
                   [0 1 0]
                   [(- s) 0 c]))

(defn rotate-y-matrix
  "Produce the matrix of a rotation of α radians about the y axis."
  [α]
  (rotate-y-matrix-2 (cos α) (sin α)))

(def Ry-matrix rotate-y-matrix)

(defn- rotate-z-matrix-2
  "Produce the matrix of a rotation of α radians about the z axis."
  [c s]
  (matrix/by-rows [c (- s) 0]
                  [s c 0]
                  [0 0 1]))

(defn rotate-z-matrix
  "Produce the matrix of a rotation of α radians about the z axis."
  [α]
  (rotate-z-matrix-2 (cos α) (sin α)))

(def Rz-matrix rotate-z-matrix)

(defn angle-axis->rotation-matrix [theta [x y z]]
  (let [colatitude (g/acos z)
        longitude (g/atan y x)]
    (* (rotate-z-matrix longitude)
       (rotate-y-matrix colatitude)
       (rotate-z-matrix theta)
       (matrix/transpose (rotate-y-matrix colatitude))
       (matrix/transpose (rotate-z-matrix longitude)))))

;; ## Rotation Tuples

(defn ^:no-doc rotate-x-tuple-2 [c s]
  (matrix/m->s
   (s/literal-down 'l 3)
   (rotate-x-matrix-2 c s)
   (s/literal-up 'r 3)))

(defn rotate-x-tuple [α]
  (rotate-x-tuple-2 (cos α)
                    (sin α)))

(defn ^:no-doc rotate-y-tuple-2 [c s]
  (matrix/m->s
   (s/literal-down 'l 3)
   (rotate-y-matrix-2 c s)
   (s/literal-up 'r 3)))

(defn rotate-y-tuple [α]
  (rotate-y-tuple-2 (cos α)
                    (sin α)))

(defn ^:no-doc rotate-z-tuple-2 [c s]
  (matrix/m->s
   (s/literal-down 'l 3)
   (rotate-z-matrix-2 c s)
   (s/literal-up 'r 3)))

(defn rotate-z-tuple [α]
  (rotate-z-tuple-2 (cos α)
                    (sin α)))

;; ## Rotation procedures

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

;; Aliases to match scmutils.

(def rotate-x Rx)
(def rotate-y Ry)
(def rotate-z Rz)

(defn wcross->w [A]
  (up (get-in A [1 2])
      (get-in A [2 0])
      (get-in A [0 1])))

;; ## Rotation Matrix to Euler Angles

(defn Euler->M
  "Compute the rotation matrix from a 3-vector of Euler angles.

  Our Euler Angle convention:

  M(theta, phi, psi) = R_z(phi)*R_x(theta)*R_z(psi)"
  [[theta phi psi]]
  (* (rotate-z-matrix phi)
     (rotate-x-matrix theta)
     (rotate-z-matrix psi)))

;; Ported from code added to scmutils by GJS, 28 Sept 2020.

(defn M->Euler
  "Given a 3x3 rotation matrix, returns a [[emmy.structure/up]] of the
  corresponding Euler angles.

  Our Euler Angle convention:

  M(theta, phi, psi) = R_z(phi)*R_x(theta)*R_z(psi)"
  ([M]
   (M->Euler M nil))
  ([M tolerance-in-ulps]
   (let [tolerance (if (nil? tolerance-in-ulps)
                     v/machine-epsilon
                     (* tolerance-in-ulps v/machine-epsilon))
         close? (us/close-enuf? tolerance)
         cx (get-in M [2 2])
         cx-number? (v/number? cx)]
     (cond (and cx-number? (close? cx -1)) ;; Nonunique
           (let [theta Math/PI
                 phi (- (g/atan
                         (- (get-in M [0 1]))
                         (get-in M [0 0])))
                 psi 0]
             (up theta phi psi))

           (and cx-number? (close? cx +1)) ;; Nonunique
           (let [theta 0
                 phi (g/atan
                      (- (get-in M [0 1]))
                      (get-in M [0 0]))
                 psi 0]
             (up theta phi psi))

           :else
           (let [theta (g/acos cx)
                 sx (sin theta)
                 phi (g/atan (/ (get-in M [0 2]) sx)
                             (- (/ (get-in M [1 2]) sx)))
                 psi (g/atan (/ (get-in M [2 0]) sx)
                             (/ (get-in M [2 1]) sx))]
             (up theta phi psi))))))
