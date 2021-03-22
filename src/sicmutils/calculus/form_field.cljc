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

(ns sicmutils.calculus.form-field
  (:require [sicmutils.abstract.function :as af]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v]))

;; this has stuff from form-fields.scm, wedge.scm and exterior-derivative.scm.

(derive ::form-field ::o/operator)

;; ## 1Form fields
;;
;; A form-field of rank n is an operator that takes n vector fields to a
;; real-valued function on the manifold. A 1form field takes a single vector
;; field.

(defn form-field?
  "Returns true if the supplied `f` is a form field operator, false otherwise.

  A form-field of rank n is an operator that takes n vector fields to a
  real-valued function on the manifold."
  [f]
  (and (o/operator? f)
       (-> (o/context f)
           (:subtype)
           (= ::form-field))))

(defn oneform-field?
  "Returns true if the supplied `f` is
  a [One-form](https://en.wikipedia.org/wiki/One-form) field operator, false
  otherwise.

  A form-field of rank n is an operator that takes n vector fields to a
  real-valued function on the manifold.
  A [One-form](https://en.wikipedia.org/wiki/One-form) field takes a single
  vector field."
  [f]
  (and (form-field? f)
       (= 1 (:rank
             (o/context f)))))

(defn procedure->oneform-field
  "Accepts a function `f` and an optional symbolic `name`, and returns
  a [One-form](https://en.wikipedia.org/wiki/One-form) field, ie, a subtype
  of [[sicmutils.operator/Operator]].

  `f` should be a function from a single vector field to a real-valued function
  on the manifold."
  ([fp]
   (let [name 'unnamed-1form-field]
     (procedure->oneform-field fp name)))
  ([fp name]
   (o/make-operator fp name
                    {:subtype ::form-field
                     :rank 1
                     :arguments [::vf/vector-field]})))

(defn procedure->nform-field
  "TODO note how we just... CALL it if `n` is zero, since there are no arguments
  to supply."
  ([proc n]
   (procedure->nform-field proc n 'unnamed-nform-field))
  ([proc n name]
   (if (= n 0)
     (proc)
     (o/make-operator proc name
                      {:subtype ::form-field
                       :arity [:exactly n]
                       :rank n
                       :arguments (repeat n ::vf/vector-field)}))))

(defn get-rank [f]
  (cond (o/operator? f)
        (or (:rank (o/context f))
            (u/illegal (str "operator, but not a differential form: " f)))

        (f/function? f) 0
        :else (u/illegal (str "not a differential form: " f))))

(defn oneform-field-procedure
  "A One-form is specified by a function that gives `components`, in a down tuple,
  relative to a given `coordinate-system`."
  [components coordinate-system]
  (fn [f]
    (s/mapr (fn [f]
              {:pre [(vf/vector-field? f)]}
              (f/compose
               (g/* components
                    (vf/vector-field->components f coordinate-system))
               (m/chart coordinate-system)))
            f)))

(defn components->oneform-field
  "TODO fix...

  takes components of the One-form field relative to a coordinate system AND the
  coordinate system... and returns the One-form field."
  ([components coordinate-system]
   (let [name `(~'oneform-field ~(v/freeze components))]
     (components->oneform-field
      components coordinate-system name)))
  ([components coordinate-system name]
   (-> (oneform-field-procedure components coordinate-system)
       (procedure->oneform-field name))))

;; We can extract the components function for a form, given a
;; coordinate system.

(defn oneform-field->components
  "TODO this goes backwards; takes a One-form field `form` and a
  `coordinate-system`... describe!

  The returned function is from coordinates => something! TODO describe!"
  [form coordinate-system]
  {:pre [(form-field? form)]}
  (let [basis (vf/coordinate-system->vector-basis coordinate-system)]
    (f/compose (form basis)
               (m/point coordinate-system))))

;; ### API

(defn ff:zero [f]
  m/zero-manifold-function)

(defn ff:zero-like [op]
  {:pre [(form-field? op)]}
  (o/make-operator ff:zero
                   'ff:zero
                   (o/context op)))

(defn ff:zero? [ff]
  {:pre [(form-field? ff)]}
  (= (o/procedure ff) ff:zero))

(comment
  ;; TODO install these for form fields! We can't do it until they have their
  ;; own types...
  (assign-operation 'zero-like ff:zero-like form-field?)
  (assign-operation 'zero? ff:zero? form-field?))

(defn literal-oneform-field
  [name coordinate-system]
  (let [n (:dimension (m/manifold coordinate-system))
        domain (if (= n 1) 0 (s/up* (repeat n 0)))
        range  (s/down* (repeat n 0))]
    (-> (af/literal-function name domain range)
        (components->oneform-field coordinate-system name))))

;; To get the elements of a coordinate basis for the 1-form fields

(defn coordinate-basis-oneform-field-procedure
  [coordinate-system & indices]
  (fn [vf]
    (letfn [(internal [vf]
              {:pre [(vf/vector-field? vf)]}
              (vf (f/compose (apply s/component indices)
                             (m/chart coordinate-system))))]
      (s/mapr internal vf))))

(defn coordinate-basis-oneform-field
  [coordinate-system name & indices]
  (-> (apply coordinate-basis-oneform-field-procedure coordinate-system indices)
      (procedure->oneform-field name)))

(defn coordinate-name->ff-name
  "From the name of a coordinate, produce the name of the coordinate basis
  one-form field (as a symbol)"
  [n]
  (symbol (str \d n)))

(defn coordinate-system->oneform-basis
  [coordinate-system]
  (s/map-chain
   (fn [c-name chain _]
     (let [ff-name (coordinate-name->ff-name c-name)]
       (apply coordinate-basis-oneform-field
              coordinate-system ff-name chain)))
   (m/coordinate-prototype coordinate-system)))

;; Given component functions defined on manifold points and a 1-form basis, to
;; produce the 1-form field as a linear combination.

(defn basis-components->oneform-field [components oneform-basis]
  (procedure->oneform-field
   (fn [vf]
     (g/* components (oneform-basis vf)))))

(defn oneform-field->basis-components [w vector-basis]
  (s/mapr w vector-basis))

;; This is one of the two incompatible definitions of "differential". This
;; differential is a special case of exterior derivative. The other one appears
;; in map.cljc.

(defn function->oneform-field [f]
  {:pre [(f/function? f)]}
  (procedure->oneform-field
   (fn [vf]
     (s/mapr (fn [v]
               {:pre [(vf/vector-field? v)]}
               (fn [m] ((v f) m)))
             vf))
   `(~'d ~(v/freeze f))))

(def differential-of-function
  function->oneform-field)

;; ## Wedge.scm
;;
;; Now we transition to wedge.

(defn permutation-sequence
  "Produces an iterable sequence developing the permutations of the input sequence
  of objects (which are considered distinct) in church-bell-changes order - that
  is, each permutation differs from the previous by a transposition of adjacent
  elements (Algorithm P from §7.2.1.2 of Knuth).

  This is an unusual way to go about this in a functional language, but it's
  fun.

  This approach has the side-effect of arranging for the parity of the generated
  permutations to alternate; the first permutation yielded is the identity
  permutation (which of course is even).

  Inside, there is a great deal of mutable state, but this cannot be observed by
  the user."
  [as]
  (let [n (count as)
        a (object-array as)
        c (int-array n (repeat 0)) ;; P1. [Initialize.]
        o (int-array n (repeat 1))
        return #(into [] %)
        the-next (atom (return a))
        has-next (atom true)
        ;; step implements one-through of algorithm P up to step P2,
        ;; at which point we return false if we have terminated, true
        ;; if a has been set to a new permutation. Knuth's code is
        ;; one-based; this is zero-based.
        step (fn [j s]
               (let [q (int (+ (aget c j) (aget o j)))] ;; P4. [Ready to change?]
                 (cond (< q 0)
                       (do ;; P7. [Switch direction.]
                         (aset o j (int (- (aget o j))))
                         (recur (dec j) s))

                       (= q (inc j))
                       (if (zero? j)
                         false ;; All permutations have been delivered.
                         (do (aset o j (int (- (aget o j)))) ;; P6. [Increase s.]
                             (recur (dec j) (inc s)))) ;; P7. [Switch direction.]

                       :else ;; P5. [Change.]
                       (let [i1 (+ s (- j (aget c j)))
                             i2 (+ s (- j q))
                             t (aget a i1)
                             ]
                         (aset a i1 (aget a i2))
                         (aset a i2 t)
                         (aset c j q)
                         true ;; More permutations are forthcoming.
                         ))))]
    (#?(:clj iterator-seq :cljs #'cljs.core/chunkIteratorSeq)
     (reify #?(:clj java.util.Iterator :cljs Object)
       (hasNext [_] @has-next)
       (next [_]  ;; P2. [Visit.]
         (let [prev @the-next]
           (reset! has-next (step (dec n) 0))
           (reset! the-next (return a))
           prev))

       #?@(:cljs
           [IIterable
            (-iterator [this] this)])))))

;; Higher rank forms can be constructed from 1forms by wedging them together.
;; This antisymmetric tensor product is computed as a determinant. The purpose
;; of this is to allow us to use the construction dx^dy to compute the area
;; described by the vectors that are given to it.

(defn- wedge2 [form1 form2]
  (let [n1 (get-rank form1)
        n2 (get-rank form2)]
    (if (or (zero? n1) (zero? n2))
      (g/* form1 form2)
      (let [n (+ n1 n2)
            k (/ 1
                 (* (g/factorial n1)
                    (g/factorial n2)))
            w (fn [& args]
                (assert (= (count args) n)
                        "Wrong number of args to wedge product")
                ;; "Error in Singer" comment from GJS.
                (g/* k (apply
                        g/+ (map (fn [permutation parity]
                                   (let [[a1 a2] (split-at n1 permutation)]
                                     (g/* parity
                                          (apply form1 a1)
                                          (apply form2 a2))))
                                 (permutation-sequence args)
                                 (cycle [1 -1])))))
            name `(~'wedge
                   ~(v/freeze form1)
                   ~(v/freeze form2))]
        (procedure->nform-field w n name)))))

;; See Spivak p275 v1 "Differential Geometry" to see the correct
;; definition.  The key is that the wedge of the coordinate basis
;; forms had better be the volume element.

(defn wedge [& fs]
  (reduce wedge2 fs))

;; TODO document, figure out WHERE this happened in scmutils.

;; One-form fields multiply by [[wedge]].

(defmethod g/mul [::form-field ::form-field] [a b]
  (wedge2 a b))

;; Alternative definition in terms of alternation.

(defn Alt [form]
  (let [n (get-rank form)]
    (if (zero? n)
      form
      (letfn [(alternation [& args]
                (assert (= (count args) n)
                        "Wrong number of args to alternation")
                (g/* (/ 1 (g/factorial n))
                     (apply g/+
                            (map (fn [permutation parity]
                                   (g/* parity (apply form permutation)))
                                 (permutation-sequence args)
                                 (cycle [1 -1])))))]
        (procedure->nform-field
         alternation n `(~'Alt ~(v/freeze form)))))))

(defn tensor-product2 [t1 t2]
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
                ~(v/freeze t2)))))))

(defn w2 [form1 form2]
  (let [n1 (get-rank form1)
        n2 (get-rank form2)]
    (g/* (/ (g/factorial (+ n1 n2))
            (* (g/factorial n1)
               (g/factorial n2)))
         (Alt (tensor-product2 form1 form2)))))

(defn alt-wedge [& args]
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

(def d exterior-derivative-procedure)
