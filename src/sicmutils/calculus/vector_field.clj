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

(defn procedure->vector-field
  [vfp & name]
  (let [name (if name (first name) 'unnamed-vector-field)]
    (o/make-operator vfp name :subtype ::vector-field)))

(defn vector-field?
  [vf]
  (and (o/operator? vf)
       (= (:subtype vf) ::vector-field)))

;; TODO(colin.smith): GJS supplies the allowed argument list here; but we think
;; this is probably already covered by the defmethod operator applicability rules.

(defn vector-field-procedure
  [components coordinate-system]
  (fn [f]
    (f/compose (g/* (D (comp f #(m/coords->point coordinate-system %)))
                    components)
               #(m/point->coords coordinate-system %))))

(defn components->vector-field
  [components coordinate-system name]
  (procedure->vector-field (vector-field-procedure components coordinate-system) name))

(defn vector-field->components
  [vf coordinate-system]
  (fn [coords]
    (assert (vector-field? vf))
    ((vf #(m/point->coords coordinate-system %))
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
    (f/compose ((apply ∂ i) (f/compose f #(m/coords->point coordinate-system %)))
               #(m/point->coords coordinate-system %))))

(defn coordinate-basis-vector-field
  [coordinate-system name & i]
  (procedure->vector-field
    (apply coordinate-basis-vector-field-procedure coordinate-system i)
    name))

(defn coordinate-basis-vector-fields
  [coordinate-system prototype]
  (s/mapr #(apply coordinate-basis-vector-field coordinate-system %1 %2)
          prototype
          (s/structure->access-chains prototype)))

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
