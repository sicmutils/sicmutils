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

(ns sicmutils.calculus.vector-field
  "A vector field is an operator that takes a smooth real-valued function of a
  manifold and produces a new function on the manifold which computes the
  directional derivative of the given function at each point of the manifold."
  (:refer-clojure :exclude [partial])
  (:require [sicmutils.abstract.function :as af]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.operator :as o]
            [sicmutils.series :as series]
            [sicmutils.structure :as s]
            [sicmutils.value :as v]))

(derive ::vector-field ::o/operator)

;; A vector field is an operator that takes a smooth real-valued function of a
;; manifold and produces a new function on the manifold which computes the
;; directional derivative of the given function at each point of the manifold.
;;
;; As with other differential operators such as D, a vector-field operator
;; multiplies by composition. Like D it takes the given function to another
;; function of a point.

(defn procedure->vector-field
  ([vfp]
   (procedure->vector-field vfp 'unnamed-vector-field))
  ([vfp name]
   (o/make-operator vfp name
                    {:subtype ::vector-field
                     :arguments [::v/function]})))

(defn vector-field?
  [vf]
  (and (o/operator? vf)
       (-> (o/context vf)
           (:subtype)
           (= ::vector-field))))

(defn vector-field-procedure
  [components coordinate-system]
  (fn [f]
    (f/compose (g/* (D (comp f (m/point coordinate-system)))
                    components)
               (m/chart coordinate-system))))

(defn components->vector-field
  ([components coordinate-system]
   (let [name `(~'vector-field ~components)]
     (components->vector-field
      components coordinate-system name)))
  ([components coordinate-system name]
   (-> (vector-field-procedure components coordinate-system)
       (procedure->vector-field name))))

;; We can extract the components function for a vector field, given a coordinate
;; system.

(defn vector-field->components
  [vf coordinate-system]
  (fn [coords]
    (assert (vector-field? vf))
    ((vf (m/chart coordinate-system))
     (m/coords->point coordinate-system coords))))

(comment
  (define (vf:zero f) zero-manifold-function)

  (define (vf:zero-like op)
    (assert (vector-field? op) "vf:zero-like")
    (make-op vf:zero
	           'vf:zero
	           (operator-subtype op)
	           (operator-arity op)
	           (operator-optionals op)))

  (define (vf:zero? vf)
    (assert (vector-field? vf) "vf:zero?")
    (eq? (operator-procedure vf) vf:zero))

  (assign-operation 'zero-like vf:zero-like vector-field?)
  (assign-operation 'zero? vf:zero? vector-field?))

(defn literal-vector-field
  [name coordinate-system]
  (let [n      (:dimension (m/manifold coordinate-system))
        domain (apply s/up (repeat n 0))
        range  domain]
    (-> (af/literal-function name domain range)
        (components->vector-field coordinate-system name))))

;; For any coordinate system we can make a coordinate basis.

(defn ^:private coordinate-basis-vector-field-procedure
  [coordinate-system & i]
  (fn [f]
    (f/compose ((apply partial i) (f/compose f (m/point coordinate-system)))
               (m/chart coordinate-system))))

(defn coordinate-basis-vector-field
  [coordinate-system name & i]
  (procedure->vector-field
   (apply coordinate-basis-vector-field-procedure coordinate-system i)
   name))

(defn coordinate-name->vf-name
  "From the name `n` of a coordinate, produce the name of the coordinate basis
  vector field (as a symbol)"
  [n]
  (symbol (str "d:d" n)))

(defn coordinate-system->vector-basis [coordinate-system]
  (let [prototype (s/mapr coordinate-name->vf-name
                          (m/coordinate-prototype coordinate-system))]
    (s/transpose
     (s/mapr
      #(apply coordinate-basis-vector-field coordinate-system %1 %2)
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

(comment
  ;;; Given a vector basis, can make a vector field as a linear
;;; combination.  This is for any basis, not just a coordinate basis.
;;; The components are evaluated at the point, not the coordinates.

  (define (basis-components->vector-field components vector-basis)
    (procedure->vector-field
     (lambda (f)
             (lambda (point)
                     (* ((vector-basis f) point)
	                      (components point))))
     `(+ ,@(map (lambda (component basis-element)
		                    `(* ,(diffop-name component)
		                         ,(diffop-name basis-element)))
	              (s:fringe components)
	              (s:fringe vector-basis)))))


;;; And the inverse

  (define (vector-field->basis-components v dual-basis)
    (s:map/r (lambda (w) (w v)) dual-basis)))

(defn coordinatize [sfv coordsys]
  (let [v (fn [f]
            (fn [x]
              (let [b (f/compose (sfv (m/chart coordsys))
                                 (m/point coordsys))]
                (* ((D f) x) (b x)))))]
    (o/make-operator v 'coordinatize)))

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
