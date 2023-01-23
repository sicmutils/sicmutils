#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.form-field
  "This namespace implements a form field operator and a number of functions for
  creating and working with form fields.

  A form-field of rank n is an operator that takes n vector fields to a
  real-valued function on the manifold. A one-form field takes a single vector
  field.

  The namespace also contains two definitions of the [[wedge]]
  product ([[alt-wedge]] is the second), plus the [[exterior-derivative]]."
  (:require [emmy.abstract.function :as af]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.operator :as o]
            [emmy.special.factorial :refer [factorial]]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.util.permute :as permute]
            [emmy.value :as v]))

;; ## Form fields
;;
;; A form-field of rank n is an operator that takes n vector fields to a
;; real-valued function on the manifold. A one-form field takes a single vector
;; field.

(derive ::oneform-field ::form-field)
(derive ::form-field ::o/operator)

(defn ff:zero
  "Returns a form field that returns, for any supplied vector field `vf`, a
  manifold function [[manifold/zero-manifold-function]] that maps every input
  manifold `point` to the scalar value 0."
  [_]
  m/zero-manifold-function)

(defn get-rank
  "Returns the rank of the supplied differential form `f`. Functions are treated
  as differential forms of rank 0.

  Throws for any non differential form supplied."
  [f]
  (cond (o/operator? f)
        (or (:rank (o/context f))
            (u/illegal
             (str "operator, but not a differential form: " f)))

        (f/function? f) 0
        :else (u/illegal
               (str "not a differential form: " f))))

(defn form-field?
  "Returns true if the supplied `f` is a form field operator, false otherwise."
  [ff]
  (and (o/operator? ff)
       (let [subtype (:subtype
                      (o/context ff))]
         (isa? subtype ::form-field))))

(defn nform-field?
  "Returns true if the supplied `f` is an [form field of rank
  n](https://en.wikipedia.org/wiki/Differential_form), false otherwise.

  A form-field of rank n is an operator that takes n vector fields to a
  real-valued function on the manifold."
  [f n]
  (and (form-field? f)
       (= n (get-rank f))))

(defn oneform-field?
  "Returns true if the supplied `f` is
  a [One-form](https://en.wikipedia.org/wiki/One-form), false
  otherwise.

  A [One-form](https://en.wikipedia.org/wiki/One-form) takes a single vector
  field to a real-valued function on the manifold."
  [f]
  (nform-field? f 1))

(defn- ff:zero-like
  "Given some form field `op`, returns a form field with the same context and
  its procedure replaced by `ff:zero`.

  The returned form field responds `true` to `v/zero?`."
  [op]
  {:pre [(form-field? op)]}
  (o/make-operator ff:zero
                   'ff:zero
                   (o/context op)))

(defn- ff:zero?
  "Returns true if the supplied form field `op` is a form field with a procedure
  equal to `ff:zero`, false otherwise."
  [op]
  (and (form-field? op)
       (= (o/procedure op) ff:zero)))

(letfn [(one-like [_]
          (u/unsupported
           "form fields don't have an identity."))
        (id-like [_]
          (u/unsupported
           "form fields don't have a multiplicative identity."))
        (identity? [_] false)]
  (let [defaults {:zero? ff:zero?
                  :zero-like ff:zero-like
                  :one-like one-like
                  :identity? identity?
                  :identity-like id-like}]
    (defn- ff-context [m]
      (merge defaults m))))

(defn ^:no-doc procedure->nform-field
  "Accepts a function `f` and an optional symbolic `name`, and returns an n-form
  field, ie, a subtype of [[emmy.operator/Operator]].

  `f` should be a function from n vector field arguments to a smooth real-valued
  function `g` of a manifold.

  If `n` is 0, the function will be called immediately and its return value
  returned."
  ([f n]
   (procedure->nform-field f n 'unnamed-nform-field))
  ([f n name]
   (if (= n 0)
     (f)
     (let [args (into [] (repeat n ::vf/vector-field))]
       (o/make-operator f name
                        (ff-context
                         {:subtype ::form-field
                          :arity [:exactly n]
                          :rank n
                          :arguments args}))))))

(defn ^:no-doc procedure->oneform-field
  "Accepts a function `f` and an optional symbolic `name`, and returns a one-form
  field, ie, a subtype of [[emmy.operator/Operator]].

  `f` should be a function from a vector field to a smooth real-valued function
  `g` of a manifold."
  ([f]
   (let [name 'unnamed-1form-field]
     (procedure->oneform-field f name)))
  ([f name]
   (o/make-operator f name
                    (ff-context
                     {:subtype ::oneform-field
                      :arity [:exactly 1]
                      :rank 1
                      :arguments [::vf/vector-field]}))))

(defn ^:no-doc oneform-field-procedure
  "Takes:

  - a `down` tuple of `components` of the one-form field relative to
    `coordinate-system`
  - the `coordinate-system`

  And returns a procedure (not yet an operator!) that takes a structure of vector fields
  and produces a new structure of functions of manifold points."
  [components coordinate-system]
  (fn [vf-components]
    (s/mapr (fn [vf]
              {:pre [(vf/vector-field? vf)]}
              (f/compose
               (g/* components
                    (vf/vector-field->components vf coordinate-system))
               (m/chart coordinate-system)))
            vf-components)))

(defn components->oneform-field
  "Takes:

  - a `down` tuple of `components` of the one-form field relative to
    `coordinate-system`
  - the `coordinate-system`

  And returns a full one-form field.

  A one-field field is an operator that takes a vector field to a real-valued
  function on the manifold."
  ([components coordinate-system]
   (let [name `(~'oneform-field
                ~(v/freeze components))]
     (components->oneform-field
      components coordinate-system name)))
  ([components coordinate-system name]
   (-> (oneform-field-procedure components coordinate-system)
       (procedure->oneform-field name))))

;; We can extract the components function for a form, given a
;; coordinate system.

(defn oneform-field->components
  "Given a one-form field `form` and a `coordinate-system`, returns a function
  from the coordinate representation of a manifold point to a coordinate
  representation of the coordinatized components of the form field at that
  point.

  For example:

  ```clojure
  (let-coordinates [[x y] R2-rect]
    (let [f (literal-oneform-field 'f R2-rect)]
      ((oneform-field->components f R2-rect)
       (up 'x0 'y0))))

  ;;=> (down (f_0 (up x0 y0))
  ;;         (f_1 (up x0 y0)))
  ```"
  [form coordinate-system]
  {:pre [(form-field? form)]}
  (let [basis (vf/coordinate-system->vector-basis coordinate-system)]
    (f/compose (form basis)
               (m/point coordinate-system))))

;; ### API

(defn literal-oneform-field
  "Given a symbolic name `sym` and a `coordinate-system`, returns a one-form field
  consisting of literal real-valued functions from the coordinate system's
  dimension for each coordinate component.

  These functions are passed to [[components->oneform-field]], along with the
  supplied `coordinate-system` and symbolic name `sym`.

  For coordinate systems of dimension 1, `literal-form-field`'s component
  functions will accept a single non-structural argument."
  [name coordinate-system]
  (let [n (:dimension (m/manifold coordinate-system))
        domain (if (= n 1) 0 (s/up* (repeat n 0)))
        range  (s/down* (repeat n 0))]
    (-> (af/literal-function name domain range)
        (components->oneform-field coordinate-system name))))

;; To get the elements of a coordinate basis for the 1-form fields:

(defn- coordinate-basis-oneform-field-procedure
  [coordinate-system & indices]
  (fn [vf-structure]
    (letfn [(internal [vf]
              {:pre [(vf/vector-field? vf)]}
              (vf
               (f/compose (apply s/component indices)
                          (m/chart coordinate-system))))]
      (s/mapr internal vf-structure))))

(defn coordinate-basis-oneform-field
  "Given some `coordinate-system`, a symbolic `name` and a sequence of indices
  into the structure of the coordinate system's representation, returns a
  one-form field.

  The returned one-form field at each structural spot takes a vector field and
  returns a function that takes the directional derivative in that coordinate's
  direction using the vector field."
  [coordinate-system name & indices]
  (let [ofp (apply coordinate-basis-oneform-field-procedure
                   coordinate-system indices)]
    (-> (f/memoize
         (comp f/memoize ofp))
        (procedure->oneform-field name))))

(defn ^:no-doc coordinate-name->ff-name
  "From the name of a coordinate, produce the name of the coordinate basis
  one-form field (as a symbol)"
  [n]
  (symbol (str \d n)))

(defn coordinate-system->oneform-basis
  "Given some `coordinate-system`, returns a structure of
  `coordinate-basis-oneform-field` instances.

  The one-form field at each structural spot takes a vector field and returns a
  function that takes the directional derivative in that coordinate's direction
  using the vector field.

  When applied as a function, the structure behaves equivalently to

  ```clojure
  (coordinate-basis-oneform-field <coordinate-system> 'ignored-name)
  ```

  With no indices supplied."
  [coordinate-system]
  (s/map-chain
   (fn [c-name chain _]
     (let [ff-name (coordinate-name->ff-name c-name)]
       (apply coordinate-basis-oneform-field
              coordinate-system ff-name chain)))
   (m/coordinate-prototype coordinate-system)))

;; Given component functions defined on manifold points and a 1-form basis, to
;; produce the 1-form field as a linear combination.

(defn basis-components->oneform-field
  "Given a structure of `components` functions defined on manifold points and and
  a matching `oneform-basis` (of identical structure),

  Returns a new one-form field that

  - passes its vector-field argument to `oneform-basis`, returning a new
    equivalent structure with each slot populated by functions from a manifold
    point to the directional derivative (using the vector field) in that
    coordinate direction

  - contracts the result of that operation with the result of applying each
    component in `components` to the manifold point.

  NOTE:
  - This is for any basis, not just a coordinate basis
  - The `components` are evaluated at a manifold point, not its coordinates
  - Given a dual basis, you can retrieve the original components
    with [[oneform-field->basis-components]]"
  [components oneform-basis]
  (procedure->oneform-field
   (fn [vf]
     (g/* components (oneform-basis vf)))))

(defn oneform-field->basis-components
  "Given a structure `w` of and a vector field basis `vector-basis`, returns a new
  structure generated by applying the full vector basis to each element of `w`.

  Here's an example of how to use this function to round trip a structure of
  basis components:

  ```clojure
  (let [vb    (vf/coordinate-system->vector-basis coordsys)
        basis (coordinate-system->oneform-basis coordsys)
        components (down d:dx d:dy)]
    (= components
       (-> components
           (basis-components->oneform-field basis)
           (oneform-field->basis-components vb))))
  ```"
  [w vector-basis]
  (s/mapr w vector-basis))

(defn function->oneform-field
  "One of the two incompatible definitions of differential.

  This differential is a special case of exterior derivative. The other one
  lives at [[map/differential]]."
  [f]
  {:pre [(f/function? f)]}
  (let [op (fn [vf-structure]
             (s/mapr (fn [vf]
                       {:pre [(vf/vector-field? vf)]}
                       (fn [m] ((vf f) m)))
                     vf-structure))
        name `(~'d ~(v/freeze f))]
    (procedure->oneform-field op name)))

(def ^{:doc "Alias for [[function->oneform-field]].
  One of the two incompatible definitions of differential.

  This differential is a special case of exterior derivative. The other one
  lives at [[map/differential]]."}
  differential-of-function
  function->oneform-field)

;; ## Wedge Product (from Wedge.scm)
;;
;; Now we transition to wedge.
;;
;; Higher rank forms can be constructed from 1forms by wedging them together.
;; This antisymmetric tensor product is computed as a determinant. The purpose
;; of this is to allow us to use the construction dx^dy to compute the area
;; described by the vectors that are given to it.

(defn- wedge2
  "Binary and unary cases of the wedge product."
  ([form1] form1)
  ([form1 form2]
   (let [n1 (get-rank form1)
         n2 (get-rank form2)]
     (if (or (zero? n1) (zero? n2))
       (g/* form1 form2)
       (let [n (+ n1 n2)
             k (/ 1
                  (* (factorial n1)
                     (factorial n2)))
             w (fn [& args]
                 (assert (= (count args) n)
                         (str "Wrong number of args to wedge product: "
                              (count args) " vs required " n))
                 ;; "Error in Singer" comment from GJS.
                 (g/* k (apply
                         g/+ (map (fn [permutation parity]
                                    (let [[a1 a2] (split-at n1 permutation)]
                                      (g/* parity
                                           (apply form1 a1)
                                           (apply form2 a2))))
                                  (permute/permutation-sequence args)
                                  (cycle [1 -1])))))
             name `(~'wedge
                    ~(v/freeze form1)
                    ~(v/freeze form2))]
         (procedure->nform-field w n name))))))

(defn wedge
  "Computes the wedge product of the sequence `fs` of one-forms.

  Higher rank forms can be constructed from one-forms by wedging them together.
  This antisymmetric tensor product is computed as a determinant. The purpose of
  this is to allow us to use the construction dx^dy to compute the area
  described by the vectors that are given to it.

  See Spivak p275 v1 of 'Differential Geometry' to see the correct definition.
  The key is that the wedge of the coordinate basis forms had better be the
  volume element."
  ([] (constantly 1))
  ([f] f)
  ([f & fs]
   (reduce (fn [r l]
             (wedge2 l r))
           (reverse (cons f fs)))))

;; One-form fields multiply by [[wedge]].

(defmethod g/mul [::form-field ::form-field] [a b]
  (wedge2 a b))

;; Alternative definition in terms of alternation.

(defn Alt
  "Returns the alternation of the supplied differential `form`."
  [form]
  (let [n (get-rank form)]
    (if (zero? n)
      form
      (letfn [(alternation [& args]
                (assert (= (count args) n)
                        "Wrong number of args to alternation")
                (g/* (/ 1 (factorial n))
                     (ua/generic-sum
                      (map (fn [permutation parity]
                             (g/* parity (apply form permutation)))
                           (permute/permutation-sequence args)
                           (cycle [1 -1])))))]
        (procedure->nform-field
         alternation n `(~'Alt ~(v/freeze form)))))))

(defn- tensor-product2
  ([t1] t1)
  ([t1 t2]
   (let [n1 (get-rank t1)
         n2 (get-rank t2)]
     (if (or (zero? n1) (zero? n2))
       (g/* t1 t2)
       (let [n  (+ n1 n2)
             tp (fn [& args]
                  (assert (= (count args) n)
                          "Wrong number of args to tensor product")
                  (let [[a1 a2] (split-at n1 args)]
                    (g/* (apply t1 a1)
                         (apply t2 a2))))]
         (procedure->nform-field
          tp n `(~'tensor-product
                 ~(v/freeze t1)
                 ~(v/freeze t2))))))))

(defn- w2
  ([form1] form1)
  ([form1 form2]
   (let [n1 (get-rank form1)
         n2 (get-rank form2)]
     (g/* (/ (factorial (+ n1 n2))
             (* (factorial n1)
                (factorial n2)))
          (Alt (tensor-product2 form1 form2))))))

(defn alt-wedge
  "Alternative definition of [[wedge]] in terms of alternation."
  [& args]
  (reduce w2 (constantly 1) args))

;; ## Exterior Derivative
;;
;; A form field is a function of a place and a number of vector fields. The
;; exterior derivative of this form field is a derivative of the form with
;; respect to the place, removing any dependence on place of the vector fields.

;; Consider w(v)(x), where b is the coefficient function for w in coordinates X:
;;
;; v1(w(v2))(x) - v2(w(v1))(x)
;; = v1(b v2(X))(x) - v2(b v1(X))(x)
;; = v1(b)(x) v2(X)(x) + b(x) v1(v2(X))(x)
;; - v2(b)(x) v1(X)(x) - b(x) v2(v1(X))(x)
;; = v1(b)(x) v2(X)(x) - v2(b)(x) v1(X)(x) + b(x)[v1, v2](X)(x)
;; = v1(b)(x) v2(X)(x) - v2(b)(x) v1(X)(x) + w([v1, v2])(x)
;;
;;
;; We define exterior derivative as follows:
;;
;; dw(v1, v2)(x)
;; = v1(b)(x) v2(X)(x) - v2(b)(x) v1(X)(x)
;; = v1(w(v2))(x) - v2(w(v1))(x) - w([v1, v2])(x)
;;
;; It is not obvious that this is equivalent to the standard definition. See
;; page 91 in Spivak.
;;
;; Another way to think about this is that if we were able to define constant
;; vector fields (v1_bar, v2_bar) that have constant coefficient functions equal
;; to the value of the coefficient function at the point, then dw(v1, v2)(x)
;; would be just v1_bar(w(v2_bar))(x) - v2_bar(w(v1_bar))(x).

;; This definition is a generalization to k-forms, by recursion on the vector
;; argument list.

;; The test for k<n is best if the n is the dimension of the manifold under
;; study. However, if the manifold is embedded in a higher dimensional manifold
;; n will be the dimension of the bigger manifold, making this test less
;; effective (cutting off fewer branches).

;; Formula is from Spivak Vol. 1 p289.
;;
;; NOTE: This is an excessively complicated program. Another, more elementary
;; program would, for a k-form, extract the cofficient functions relative to a
;; literal basis, by applying it to the basis vectors, do the derivatives of the
;; coefficients, to make one forms, and form the sum of the wedges of the new
;; 1-forms with the wedges of the corresponding dual basis elements.

(defn- exterior-derivative-procedure [kform]
  (let [k (get-rank kform)]
    (if (= k 0)
      (differential-of-function kform)
      (let [without #(concat (take %1 %2)
                             (drop (inc %1) %2))
            k+1form (fn [& vectors]
                      (assert (= (count vectors) (inc k)))
                      (fn [point]
                        (let [n (:dimension
                                 (m/point->manifold point))]
                          (if (< k n)
                            (ua/generic-sum
                             (fn [i]
                               (let [rest (without i vectors)]
                                 (g/+ (g/* (if (even? i) 1 -1)
                                           (((nth vectors i) (apply kform rest))
                                            point))
                                      (ua/generic-sum
                                       (fn [j]
                                         (g/* (if (even? (+ i j)) 1 -1)
                                              ((apply kform
                                                      (cons
                                                       (o/commutator (nth vectors i)
                                                                     (nth vectors j))
                                                       ;; (dec j) because
                                                       ;; already deleted i.
                                                       (without (dec j) rest)))
                                               point)))
                                       (inc i)
                                       (inc k)))))
                             0 (inc k))
                            0))))]
        (procedure->nform-field
         k+1form (inc k) `(~'d ~(v/freeze kform)))))))

(def exterior-derivative
  (o/make-operator
   exterior-derivative-procedure 'd))

(def d exterior-derivative)
