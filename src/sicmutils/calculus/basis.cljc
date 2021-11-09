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

(ns sicmutils.calculus.basis
  (:require [sicmutils.generic :as g]
            [sicmutils.structure :as s]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.matrix :as matrix]
            [sicmutils.value :as v]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(defn coordinate-system->basis
  "Returns the standard basis object for `coordinate-system`."
  [coordinate-system]
  (let [vector-basis  (vf/coordinate-system->vector-basis coordinate-system)
        oneform-basis (ff/coordinate-system->oneform-basis coordinate-system)]
    {:type ::coordinate-basis
     :dimension (:dimension (m/manifold coordinate-system))
     :vector-basis vector-basis
     :oneform-basis oneform-basis
     :coordinate-system coordinate-system}))

(defn coordinate-basis?
  "Returns true if `x` is a coordinate basis, false otherwise."
  [x]
  (isa? (v/kind x) ::coordinate-basis))

(derive ::coordinate-basis ::basis)

(defn basis->coordinate-system [b]
  {:pre [(coordinate-basis? b)]}
  (:coordinate-system b))

(defn make-basis
  "Make a basis object out of a vector and dual basis.

  The dimensions of `vector-basis` and `dual-basis` must agree."
  [vector-basis dual-basis]
  (let [d (count (flatten vector-basis))]
    (assert (= (count (flatten dual-basis)) d))
    {:type ::basis
     :dimension d
     :vector-basis vector-basis
     :oneform-basis dual-basis}))

(defn basis?
  "Returns true if `x` is a basis, false otherwise."
  [x]
  (isa? (v/kind x) ::basis))

(defn basis->oneform-basis
  "Extract the dual basis from the given basis object `b`."
  [b]
  {:pre [(basis? b)]}
  (:oneform-basis b))

(defn basis->vector-basis
  "Extract the vector basis from the given basis object `b`."
  [b]
  {:pre [(basis? b)]}
  (:vector-basis b))

(defn basis->dimension
  "Extract the dimension from the given basis object `b`."
  [b]
  {:pre [(basis? b)]}
  (:dimension b))

(defn contract [f basis]
  (let [vector-basis  (basis->vector-basis basis)
        oneform-basis (basis->oneform-basis basis)]
    (p :basis/contract
       (s/sumr f vector-basis oneform-basis))))

(defn vector-basis->dual [vector-basis coordinate-system]
  (let [prototype (m/coordinate-prototype coordinate-system)

        vector-basis-coefficient-functions
        (s/mapr #(vf/vector-field->components % coordinate-system)
                vector-basis)

        guts (fn [coords]
               (let [shape (s/compatible-shape prototype)
                     invert #(matrix/s:inverse shape % prototype)
                     transpose #(matrix/s:transpose shape % prototype)]
                 (-> (s/mapr #(% coords) vector-basis-coefficient-functions)
                     (invert)
                     (transpose))))
        oneform-basis-coefficient-functions (m/c:generate
                                             (:dimension
                                              (m/manifold coordinate-system))
                                             ::s/up
                                             #(comp (s/component %) guts))
        oneform-basis (s/mapr #(ff/components->oneform-field % coordinate-system)
                              oneform-basis-coefficient-functions)]
    oneform-basis))

(defn make-constant-vector-field [basis m0]
  (fn [v]
    (fn [f]
      (let [vector-basis  (basis->vector-basis basis)
            oneform-basis (basis->oneform-basis basis)]
        (g/* (vector-basis f)
             (s/mapr (fn [onefb] (fn [m] ((onefb v) m0)))
                     oneform-basis))))))

(defn Jacobian
  "Returns the Jacobian of transition from `from-basis` to `to-basis`.

  The Jacobian is a structure of manifold functions. The outer index is the
  from-basis index, so this structure can be multiplied by tuple of component
  functions of a vector field relative to `from-basis` to get component
  functions for a vector field in `to-basis`."
  [to-basis from-basis]
  (s/mapr (basis->oneform-basis to-basis)
          (basis->vector-basis from-basis)))
