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

(ns sicmutils.calculus.vector-field
  (:require [sicmutils
             [generic :as g]
             [operator :as o]
             [function :as f]
             [structure :as s]
             [series :as series]]
            [sicmutils.calculus
             [manifold :as m]
             [derivative :refer [D ∂]]]
            [sicmutils.function :as f]))

;; A vector field is an operator that takes a smooth real-valued
;; function of a manifold and produces a new function on the manifold
;; which computes the directional derivative of the given function at
;; each point of the manifold.

(derive ::vector-field ::o/operator)

(defn procedure->vector-field
  [vfp & name]
  (let [name (if name (first name) 'unnamed-vector-field)]
    (o/make-operator vfp name
                     :subtype ::vector-field
                     :arguments [::f/function])))

(defn vector-field?
  [vf]
  (and (o/operator? vf)
       (-> vf :context :subtype (= ::vector-field))))

(defn vector-field-procedure
  [components coordinate-system]
  (fn [f]
    (f/compose (g/* (D (comp f (m/point coordinate-system)))
                    components)
               (m/chart coordinate-system))))

(defn components->vector-field
  [components coordinate-system & [name]]
  (let [name (or name `(~'vector-field ~components))]
    (procedure->vector-field (vector-field-procedure components coordinate-system) name)))

(defn vector-field->components
  [vf coordinate-system]
  (fn [coords]
    (assert (vector-field? vf))
    ((vf (m/chart coordinate-system))
     (m/coords->point coordinate-system coords))))

(defn literal-vector-field
  [name coordinate-system]
  (let [n (:dimension (m/manifold coordinate-system))
        domain (apply s/up (repeat n 0))
        range 0
        components (s/generate n ::s/up #(f/literal-function
                                           (symbol (str name \↑ %))
                                           domain
                                           range))]
    (components->vector-field components coordinate-system name)))

(defn ^:private coordinate-basis-vector-field-procedure
  [coordinate-system & i]
  (fn [f]
    (f/compose ((apply ∂ i) (f/compose f (m/point coordinate-system)))
               (m/chart coordinate-system))))

(defn coordinate-name->vf-name
  "From the name of a coordinate, produce the name of the coordinate basis
  vector field (as a symbol)"
  [n]
  (symbol (str "d:d" n)))

(defn coordinate-basis-vector-field
  [coordinate-system name & i]
  (procedure->vector-field
    (apply coordinate-basis-vector-field-procedure coordinate-system i)
    name))

(defn coordinate-basis-vector-fields
  [coordinate-system]
  (let [prototype (s/mapr coordinate-name->vf-name (m/coordinate-prototype coordinate-system))]
    (s/flip-indices
     (s/mapr #(apply coordinate-basis-vector-field coordinate-system %1 %2)
             prototype
             (s/structure->access-chains prototype)))))

(defn coordinatize
  [v coordinate-system]
  (let [coordinatized-v (fn [f]
                          (fn [x]
                            (let [b (f/compose (v (m/chart coordinate-system))
                                               (m/point coordinate-system))]
                              (g/* ((D f) x) (b x)))))]
    (o/make-operator coordinatized-v `(~'coordinatized ~v))))

(defn evolution
  "We can use the coordinatized vector field to build an evolution along an
  integral curve."
  [order]
  (fn [delta-t vector-field]
    (fn [manifold-function]
      (fn [manifold-point]
        (series/sum
          (((g/exp (g/* delta-t vector-field))
             manifold-function)
            manifold-point)
          order)))))
