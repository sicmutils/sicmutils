#_
"Copyright © 2021 Sam Ritchie.
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

(ns sicmutils.calculus.curvature
  (:require [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.indexed :as ci]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]))

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
