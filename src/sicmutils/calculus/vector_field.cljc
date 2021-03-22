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

;; ## Vector Fields
;;
;; A vector field is an operator that takes a smooth real-valued function of a
;; manifold and produces a new function on the manifold which computes the
;; directional derivative of the given function at each point of the manifold.
;;
;; As with other differential operators such as D, a vector-field operator
;; multiplies by composition. Like D it takes the given function to another
;; function of a point.

(defn procedure->vector-field
  "Accepts a function `f` and an optional symbolic `name`, and returns a vector
  field, ie, a subtype of [[sicmutils.operator/Operator]].

  `f` should be a function from a smooth real-valued function `g` of a manifold
  to a new function on the manifold which computes the directional derivative of
  `g` at each point of the manifold."
  ([f]
   (procedure->vector-field f 'unnamed-vector-field))
  ([f name]
   (let [context {:subtype ::vector-field
                  :arguments [::v/function]}]
     (o/make-operator f name context))))

(defn vector-field?
  "Returns true if the supplied argument `vf` is a vector field operator, false
  otherwise."
  [vf]
  (and (o/operator? vf)
       (-> (o/context vf)
           (:subtype)
           (= ::vector-field))))

;; TODO fix this docstring... what is going on?

(defn- vector-field-procedure
  "A vector field is specified by a function that gives components, as an up
  tuple, relative to a coordinate system, for each point, specified in the given
  coordinate system."
  [components coordinate-system]
  (fn [f]
    (f/compose
     (g/* (D (f/compose f (m/point coordinate-system)))
          components)
     (m/chart coordinate-system))))

(defn components->vector-field
  "TODO fix...

  takes components of the vector field relative to a coordinate system AND the
  coordinate system... and returns the vector field."
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
  "TODO this goes backwards; takes a vector field and a coordinate system, then
  applies the vector field to the chart function (which, remember, takes points
  to coordinate representations).

  So the returned function is from coordinates => the components of the
  directional derivative in each component."
  [vf coordinate-system]
  {:pre [(vector-field? vf)]}
  (f/compose (vf (m/chart coordinate-system))
             (m/point coordinate-system)))

(defn vf:zero [f]
  m/zero-manifold-function)

(defn vf:zero-like [op]
  {:pre [(vector-field? op)]}
  (o/make-operator vf:zero
                   'vf:zero
	                 (o/context op)))

(defn vf:zero? [vf]
  {:pre [(vector-field? vf)]}
  (= (o/procedure vf) vf:zero))

(comment
  ;; TODO install these for vector fields! We can't do it until they have their
  ;; own types...
  (assign-operation 'zero-like vf:zero-like vector-field?)
  (assign-operation 'zero? vf:zero? vector-field?))

;; TODO test case of n==1 with Real input, not an up.

(defn literal-vector-field [name coordinate-system]
  (let [n      (:dimension
                (m/manifold coordinate-system))
        domain (if (= n 1) 0   (s/up* (repeat n 0)))
        range  (if (= n 1) [0] domain)]
    (-> (af/literal-function name domain range)
        (components->vector-field coordinate-system name))))

;; For any coordinate system we can make a coordinate basis.

(defn- coordinate-basis-vector-field-procedure
  [coordinate-system & i]
  (fn [f]
    (f/compose
     ((apply partial i) (f/compose f (m/point coordinate-system)))
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

(defn coordinate-system->vector-basis
  "TODO simplify the heck out of this with FEWER mapr instances... document what
  is going on now, and use this intel to get the stuff above figured out."
  [coordinate-system]
  (s/transpose
   (s/map-chain
    (fn [c-name chain _]
      (let [vf-name (coordinate-name->vf-name c-name)]
        (apply coordinate-basis-vector-field
               coordinate-system vf-name chain)))
    (m/coordinate-prototype coordinate-system))))

;; Given a vector basis, can make a vector field as a linear combination. This
;; is for any basis, not just a coordinate basis. The components are evaluated
;; at the point, not the coordinates.

;; TODO this is clearly busted... how can we do this in a way that we STAY
;; operator?
;;
;; Is the problem that multiplication makes the thing NOT an operator anymore? Fix this and give it a test!

(comment
  ;; TODO maybe we could do it like this?
  (defn basis-components->vector-field
    [components vector-basis]
    (g/* vector-basis
         (fn [_] components))))

(defn basis-components->vector-field
  [components vector-basis]
  (let [op (fn [f]
             (fn [point]
               (g/* ((vector-basis f) point)
	                  (components point))))
        name `(~'+ ~@(map (fn [component basis-element]
		                        `(~'*
                              ~(v/freeze component)
		                          ~(v/freeze basis-element)))
	                        (flatten components)
	                        (flatten vector-basis)))]
    (procedure->vector-field op name)))

;;; And the inverse

(defn vector-field->basis-components [v dual-basis]
  (s/mapr (fn [w] (w v)) dual-basis))

(defn coordinatize
  "Returns an operator that acts as a coordinate version of the supplied vector
  field `v`."
  [vf coordinate-system]
  (letfn [(coordinatized-v [f]
            (fn [x]
              (let [b (f/compose (vf (m/chart coordinate-system))
                                 (m/point coordinate-system))]
                (g/* ((D f) x) (b x)))))]
    (o/make-operator
     coordinatized-v
     `(~'coordinatized ~(o/name vf)))))

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
