#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.basis
  (:require [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.structure :as s]
            [emmy.value :as v]))

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
    (s/sumr f vector-basis oneform-basis)))

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
             (s/mapr (fn [onefb] (fn [_] ((onefb v) m0)))
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
