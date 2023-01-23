#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.curvature
  (:require [emmy.calculus.basis :as b]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.indexed :as ci]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.generic :as g]
            [emmy.operator :as o]
            [emmy.structure :as s]))

;; Riemann curvature "tensor" is pretty easy

;; Hawking and Ellis equation 2.18, page 35.

(defn Riemann-curvature [nabla]
  (fn [u v]
    (g/- (o/commutator (nabla u) (nabla v))
         (nabla (o/commutator u v)))))

;; The traditional Riemann tensor R^i_jkl:

(defn Riemann [nabla]
  (letfn [(Riemann-tensor [w x u v]
            (w (((Riemann-curvature nabla) u v) x)))]
    (ci/with-argument-types
      Riemann-tensor
      [::ff/oneform-field
       ::vf/vector-field
       ::vf/vector-field
       ::vf/vector-field])))

(defn Ricci [nabla basis]
  (letfn [(Ricci-tensor [u v]
            (b/contract
             (fn [ei wi]
               ((Riemann nabla) wi u ei v))
             basis))]
    (ci/with-argument-types
      Ricci-tensor
      [::vf/vector-field
       ::vf/vector-field])))

;; Hawking and Ellis page 34.

(defn torsion-vector [nabla]
  (fn [X Y]
    (g/+ ((nabla X) Y)
         (g/* -1 ((nabla Y) X))
         (g/* -1 (o/commutator X Y)))))

;; The torsion tensor T^i_jk

(defn torsion [nabla]
  (letfn [(the-torsion [w x y]
            (w ((torsion-vector nabla) x y)))]
    (ci/with-argument-types
      the-torsion
      [::ff/oneform-field
       ::vf/vector-field
       ::vf/vector-field])))

;; Components of the curvature tensor R^i_{jkl}

(defn curvature-components [nabla coord-sys]
  (let [d:dxs (vf/coordinate-system->vector-basis coord-sys)
        dxs   (ff/coordinate-system->oneform-basis coord-sys)
        point ((m/point coord-sys) (s/up 'x 'y 'z))]
    ((s/mapr
      (fn [dx]
        (s/mapr
         (fn [d:dx]
           (s/mapr
            (fn [d:dy]
              (s/mapr
               (fn [d:dz]
                 (dx (((Riemann-curvature nabla) d:dy d:dz)
                      d:dx)))
               d:dxs))
            d:dxs))
         d:dxs))
      dxs)
     point)))
