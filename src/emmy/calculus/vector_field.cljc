#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.vector-field
  "This namespace implements a vector field operator and a number of functions for
  creating and working with vector fields.

  A vector field is an operator that takes a smooth real-valued function of a
  manifold and produces a new function on the manifold which computes the
  directional derivative of the given function at each point of the manifold."
  (:require [emmy.abstract.function :as af]
            [emmy.calculus.derivative :as deriv :refer [D]]
            [emmy.calculus.manifold :as m]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.operator :as o]
            [emmy.series :as series]
            [emmy.structure :as s]
            [emmy.value :as v]))

;; ## Vector Fields
;;
;; A vector field is an operator that takes a smooth real-valued function of a
;; manifold and produces a new function on the manifold which computes the
;; directional derivative of the given function at each point of the manifold.
;;
;; As with other differential operators such as D, a vector-field operator
;; multiplies by composition. Like D it takes the given function to another
;; function of a point.

(derive ::vector-field ::o/operator)

(declare vf:zero? vf:zero-like)

(defn vector-field?
  "Returns true if the supplied argument `vf` is a vector field operator, false
  otherwise."
  [vf]
  (and (o/operator? vf)
       (-> (o/context vf)
           (:subtype)
           (= ::vector-field))))

(defn ^:no-doc procedure->vector-field
  "Accepts a function `f` and an optional symbolic `name`, and returns a vector
  field, ie, a subtype of [[emmy.operator/Operator]].

  `f` should be a function from a smooth real-valued function `g` of a manifold
  to a new function on the manifold which computes the directional derivative of
  `g` at each point of the manifold."
  ([f]
   (procedure->vector-field f 'unnamed-vector-field))
  ([f name]
   (let [context {:subtype ::vector-field
                  :zero? vf:zero?
                  :zero-like vf:zero-like
                  :arguments [::v/function]}]
     (o/make-operator f name context))))

(defn ^:no-doc vector-field-procedure
  "Takes:

  - an `up` tuple, `components`, of the functions that each return the
    corresponding component of the vector field relative to `coordinate-system`
  - the `coordinate-system`

  And returns a procedure (not yet an operator!) that takes a smooth real-valued
  function of manifold points and produces a NEW function that computes the
  directional derivative of the given function at each point of the manifold."
  [component-fns coordinate-system]
  (fn [f]
    (f/compose
     (g/* (D (f/compose f (m/point coordinate-system)))
          component-fns)
     (m/chart coordinate-system))))

(defn components->vector-field
  "Takes:

  - an `up` tuple of the functions that each return the corresponding component
  of the vector field relative `coordinate-system`
  - the `coordinate-system`
  - optionally, a symbolic name for the vector field operator

  And returns a vector field.

  A vector field is an operator that takes a smooth real-valued function of
  manifold points and produces a NEW function that computes the directional
  derivative of the given function at each point of the manifold."
  ([components coordinate-system]
   (let [name `(~'vector-field ~components)]
     (components->vector-field
      components coordinate-system name)))
  ([components coordinate-system name]
   (let [vfp (vector-field-procedure components coordinate-system)]
     (-> (f/memoize
          (comp f/memoize vfp))
         (procedure->vector-field name)))))

;; We can extract the components function for a vector field, given a coordinate
;; system.

(defn vector-field->components
  "Given a vector field `vf` and a `coordinate-system`, returns a function from
  the coordinate representation of a manifold point to a coordinate
  representation of the coordinatized components of the vector field at that
  point.

  For example:

  ```clojure
  (let-coordinates [[x y] R2-rect]
    (let [f (literal-vector-field 'f R2-rect)]
        ((vector-field->components f R2-rect)
         (up 'x0 'y0))))

  ;;=> (up (f↑0 (up x0 y0))
  ;;       (f↑1 (up x0 y0)))
  ```"
  [vf coordinate-system]
  {:pre [(vector-field? vf)]}
  (f/compose (vf (m/chart coordinate-system))
             (m/point coordinate-system)))

;; ## API

(defn vf:zero
  "Returns a vector field that returns, for any supplied function `f`, a manifold
  function [[manifold/zero-manifold-function]] that maps every input manifold
  `point` to the scalar value 0."
  [_]
  m/zero-manifold-function)

(defn- vf:zero-like
  "Given some vector field `vf`, returns a vector field with the same context and
  its procedure replaced by `vf:zero`.

  The returned vector field responds `true` to `v/zero?`."
  [vf]
  {:pre [(vector-field? vf)]}
  (o/make-operator vf:zero
                   'vf:zero
	                 (o/context vf)))

(defn- vf:zero?
  "Returns true if the supplied vector field `vf` is a vector field with a
  procedure equal to `vf:zero`, false otherwise."
  [op]
  (and (vector-field? op)
       (= (o/procedure op) vf:zero)))

(defn literal-vector-field
  "Given a symbolic name `sym` and a `coordinate-system`, returns a vector field
  consisting of literal real-valued functions from the coordinate system's
  dimension for each coordinate component.

  These functions are passed to [[components->vector-field]], along with the
  supplied `coordinate-system` and symbolic name `sym`.

  For coordinate systems of dimension 1, `literal-vector-field`'s component
  functions will accept a single non-structural argument."
  [sym coordinate-system]
  (let [n      (:dimension
                (m/manifold coordinate-system))
        domain (if (= n 1) 0 (s/up* (repeat n 0)))
        range  (if (= n 1) [0] domain)]
    (-> (af/literal-function sym domain range)
        (components->vector-field coordinate-system sym))))

;; For any coordinate system we can make a coordinate basis.

(defn- coordinate-basis-vector-field-procedure
  [coordinate-system & indices]
  (fn [f]
    (f/compose
     ((apply deriv/partial indices)
      (f/compose f (m/point coordinate-system)))
     (m/chart coordinate-system))))

(defn coordinate-basis-vector-field
  "Given some `coordinate-system`, a symbolic `name` and a sequence of indices
  into the structure of the coordinate system's representation,

  returns a vector field that takes a function and returns a new function that
  computes the partial derivative of that function with respect to the supplied
  `indices` into `coordinate-system`.

  To compute the full Jacobian, pass no indices."
  [coordinate-system name & indices]
  (let [vfp (apply coordinate-basis-vector-field-procedure
                   coordinate-system indices)]
    (-> (f/memoize
         (comp f/memoize vfp))
        (procedure->vector-field name))))

(defn ^:no-doc coordinate-name->vf-name
  "From the name `n` of a coordinate, produce the name of the coordinate basis
  vector field (as a symbol)"
  [n]
  (symbol (str "d:d" n)))

(defn coordinate-system->vector-basis
  "Given some `coordinate-system`, returns a structure of
  `coordinate-basis-vector-field` instances. The vector field at each structural
  spot takes a function and computes its directional derivative with respect to
  that coordinate.

  When applied as a function, the structure behaves equivalently to

  ```clojure
  (coordinate-basis-vector-field <coordinate-system> 'ignored-name)
  ```

  With no indices supplied."
  [coordinate-system]
  (s/transpose
   (s/map-chain
    (fn [c-name chain _]
      (let [vf-name (coordinate-name->vf-name c-name)]
        (apply coordinate-basis-vector-field
               coordinate-system vf-name chain)))
    (m/coordinate-prototype coordinate-system))))

(defn basis-components->vector-field
  "Given a structure of `components` and and a matching `vector-basis` (of
  identical structure with orientations flipped), returns a new vector field
  generated contracting by these two structures together.

  The returned vector field passes its input function to the operator generated
  by this contraction.

  For example:

  ```clojure
  (let-coordinates [[x y] R2-rect]
    (basis-components->vector-field
     (up x y)
     (coordinate-system->vector-basis R2-rect)))
  ;; => (+ (* x d:dx) (* y d:dy))
  ```

  NOTE:
  - This is for any basis, not just a coordinate basis
  - The `components` are evaluated at a manifold point, not its coordinates
  - Given a dual basis, you can retrieve the original components
    with [[vector-field->basis-components]]"
  [components vector-basis]
  {:pre [(s/compatible-for-contraction? components
                                        vector-basis)]}
  (let [op (fn [f]
             (let [applied (vector-basis f)]
               (fn [point]
                 (g/* (applied point)
	                    (components point)))))
        name `(~'+ ~@(map (fn [component basis-element]
		                        `(~'*
                              ~(v/freeze component)
		                          ~(v/freeze basis-element)))
	                        (flatten components)
	                        (flatten vector-basis)))]
    (procedure->vector-field op name)))

;; And the inverse:

(defn vector-field->basis-components
  "Given a vector field `vf` generated from [[basis-components->vector-field]] and
  a dual basis, returns the original basis components.

  NOTE: You can generate a dual basis with [[basis/vector-basis->dual-basis]].

  Here's an example of how to use this function to round trip a structure of
  basis components:

  ```clojure
  (let [basis (coordinate-system->vector-basis coordsys)
        dual  (basis/vector-basis->dual basis coordsys)]
    (= basis-components
       (-> basis-components
           (basis-components->vector-field basis)
           (vector-field->basis-components dual))))
  ```"
  [vf dual-basis]
  (s/mapr (fn [w] (w vf))
          dual-basis))

(defn coordinatize
  "Returns an operator that acts as a coordinate version of the supplied vector
  field `vf` with respect to `coordinate-system`.

  The returned operator takes a function and returns a new function that takes
  directional derivatives of coordinate representations of manifold points, with
  respect to `coordinate-system`."
  [vf coordinate-system]
  (letfn [(coordinatized-v [f]
            (fn [x]
              (let [b (f/compose
                       (vf (m/chart coordinate-system))
                       (m/point coordinate-system))]
                (g/* ((D f) x) (b x)))))]
    (o/make-operator
     coordinatized-v
     `(~'coordinatized ~(o/name vf)))))

(defn evolution
  "We can use the coordinatized vector field to build an evolution along an
  integral curve.

  NOTE: I don't see how this has anything to do with [[coordinatize]]!"
  [order]
  (fn [delta-t vector-field]
    (fn [manifold-fn]
      (fn [manifold-point]
        (-> (((g/exp (g/* delta-t vector-field))
              manifold-fn)
             manifold-point)
            (series/sum order))))))
