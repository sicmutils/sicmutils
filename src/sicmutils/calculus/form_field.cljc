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
            [sicmutils.value :as v]))

;; this has stuff from form-fields.scm, wedge.scm and exterior-derivative.scm.
;;
;; TODO:
;; - get all tests ported, or ID old tests
;; - get all functions ported from the three files above.

(derive ::form-field ::o/operator)

;; ## 1Form fields
;;
;; A form-field of rank n is an operator that takes n vector fields to a
;; real-valued function on the manifold. A 1form field takes a single vector
;; field.

(defn form-field?
  [f]
  (and (o/operator? f)
       (-> (o/context f)
           (:subtype)
           (= ::form-field))))

(defn oneform-field?
  [f]
  (and (form-field? f)
       (= 1 (:rank
             (o/context f)))))

(defn procedure->oneform-field
  ([fp]
   (let [name 'unnamed-1form-field]
     (procedure->oneform-field fp name)))
  ([fp name]
   (o/make-operator fp name
                    {:subtype ::form-field
                     :rank 1
                     :arguments [::vf/vector-field]})))

(comment
  (define (ff:zero vf) zero-manifold-function)

  (define (ff:zero-like op)
    (assert (form-field? op) "ff:zero-like")
    (make-op ff:zero
	           'ff:zero
	           (operator-subtype op)
	           (operator-arity op)
	           (operator-optionals op)))

  (assign-operation 'zero-like ff:zero-like form-field?)


  (define (ff:zero? ff)
    (assert (form-field? ff) "ff:zero?")
    (eq? (operator-procedure ff) ff:zero))

  (assign-operation 'zero? ff:zero? form-field?))

(defn oneform-field-procedure [components coordinate-system]
  (fn [f]
    (s/mapr (fn [f]
              (assert (vf/vector-field? f))
              (f/compose (g/* components
                              (vf/vector-field->components f coordinate-system))
                         (m/chart coordinate-system)))
            f)))

(defn components->oneform-field
  ([components coordinate-system]
   (let [name `(~'oneform-field ~(v/freeze components))]
     (components->oneform-field
      components coordinate-system name)))
  ([components coordinate-system name]
   (procedure->oneform-field
    (oneform-field-procedure components coordinate-system) name)))

;; We can extract the components function for a form, given a
;; coordinate system.

(defn oneform-field->components
  [form coordinate-system]
  {:pre [(form-field? form)]}
  (let [X (vf/coordinate-system->vector-basis coordinate-system)]
    (f/compose (form X) #(m/point coordinate-system))))

(defn literal-oneform-field
  [name coordinate-system]
  (let [n (:dimension (m/manifold coordinate-system))
        domain (apply s/up   (repeat n 0))
        range  (apply s/down (repeat n 0))]
    (-> (af/literal-function name domain range)
        (components->oneform-field coordinate-system name))))

;; To get the elements of a coordinate basis for the 1-form fields

(defn coordinate-basis-oneform-field-procedure [coordinate-system & i]
  (fn [vf]
    (let [internal (fn [vf]
                     (assert (vf/vector-field? vf))
                     (vf (f/compose (apply s/component i) (m/chart coordinate-system))))]
      (s/mapr internal vf))))

(defn coordinate-basis-oneform-field [coordinate-system name & i]
  (procedure->oneform-field
   (apply coordinate-basis-oneform-field-procedure coordinate-system i)
   name))

(defn coordinate-name->ff-name
  "From the name of a coordinate, produce the name of the coordinate basis
  one-form field (as a symbol)"
  [n]
  (symbol (str \d n)))

(defn coordinate-system->oneform-basis [coordinate-system]
  (let [prototype (s/mapr coordinate-name->ff-name
                          (m/coordinate-prototype coordinate-system))]
    (s/mapr
     #(apply coordinate-basis-oneform-field coordinate-system %1 %2)
     prototype
     (s/structure->access-chains prototype))))

;; Given component functions defined on manifold points and a 1-form basis, to
;; produce the 1-form field as a linear combination.

(defn basis-components->oneform-field [components oneform-basis]
  (procedure->oneform-field
   (fn [v]
     (g/* components (oneform-basis v)))))

(defn oneform-field->basis-components [w vector-basis]
  (s/mapr w vector-basis))

;; This is one of the two incompatible definitions of "differential".
;; This differential is a special case of exterior derivative.
;; The other one appears in maps.scm.

(defn function->oneform-field [f]
  {:pre [(fn? f)]}
  (procedure->oneform-field
   (fn [v] (s/mapr (fn [v]
                    (assert (vf/vector-field? v))
                    (fn [m] ((v f) m)))
                  v))
   `(~'d ~(v/freeze f))))

(def differential-of-function
  function->oneform-field)

;; ## Wedge.scm
;;
;; Now we transition to wedge.

(defn procedure->nform-field
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

;; See Spivak p275 v1 "Differential Geometry" to see the correct
;; definition.  The key is that the wedge of the coordinate basis
;; forms had better be the volume element.

(defn get-rank [f]
  (cond (o/operator? f) (or (:rank (o/context f))
                            (u/illegal (str "operator, but not a differential form: " f)))
        (fn? f) 0
        :else (u/illegal "not a differential form")))

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

;; Higher rank forms can be constructed from 1forms by wedging them
;; together.  This antisymmetric tensor product is computed as a
;; determinant.  The purpose of this is to allow us to use the
;; construction dx^dy to compute the area described by the vectors
;; that are given to it.

(defn ^:private wedge2
  [form1 form2]
  (let [n1 (get-rank form1)
        n2 (get-rank form2)]
    (if (or (zero? n1) (zero? n2))
      (g/* form1 form2)
      (let [n (+ n1 n2)
            w (fn [& args]
                (assert (= (count args) n) "Wrong number of args to wedge product")
                (g/* (/ 1 (g/factorial n1) (g/factorial n2))
                     (reduce g/+ (map (fn [permutation parity]
                                        (let [a1 (take n1 permutation)
                                              a2 (drop n1 permutation)]
                                          (g/* parity (apply form1 a1) (apply form2 a2))))
                                      (permutation-sequence args)
                                      (cycle [1 -1])))))]
        (procedure->nform-field w n `(~'wedge
                                      ~(v/freeze form1)
                                      ~(v/freeze form2)))))))

(defn wedge [& fs]
  (reduce wedge2 fs))

(comment
  (define (Alt form)
    (let ((n (get-rank form)))
      (if (zero? n)
	      form
	      (let ()
	        (define (the-alternation . args)
	          (assert (fix:= (length args) n)
		                "Wrong number of args to alternation")
	          (let ((perms (permutations (iota n))))
	            (g:* (/ 1 (factorial n))
		               (apply g:+
			                    (map (lambda (p)
				                               (let ((pargs (permute p args)))
				                                 (let ((order (permutation-interchanges p)))
				                                   (g:* (if (even? order) 1 -1)
					                                      (apply form pargs)))))
			                         perms)))))
	        (procedure->nform-field the-alternation
				                          n
				                          `(Alt ,(diffop-name form)))))))

  (define (tensor-product2 t1 t2)
    (let ((n1 (get-rank t1)) (n2 (get-rank t2)))
      (if (or (zero? n1) (zero? n2))
	      (* form1 form2)
	      (let ((n (fix:+ n1 n2)))
	        (define (the-product . args)
	          (assert (fix:= (length args) n)
		                "Wrong number of args to tensor product")
	          (* (apply t1 (list-head args n1))
	             (apply t2 (list-tail args n1))))
	        (procedure->nform-field the-product
				                          n
				                          `(tensor-product ,(diffop-name t1)
						                                        ,(diffop-name t2)))))))

  (define (w2 form1 form2)
    (let ((n1 (get-rank form1)) (n2 (get-rank form2)))
      (* (/ (factorial (+ n1 n2))
	          (* (factorial n1) (factorial n2)))
         (Alt (tensor-product2 form1 form2)))))

  (define (alt-wedge . args)
    (reduce w2 (constant 1) args)))

;; ## Exterior Derivative

;; This definition is a generalization to k-forms, by recursion on the vector
;; argument list.

;; The test for k<n is best if the n is the dimension of the manifold under
;; study. However, if the manifold is embedded in a higher dimensional manifold
;; n will be the dimension of the bigger manifold, making this test less
;; effective (cutting off fewer branches).

;; Formula is from Spivak Vol. 1 p289.

(defn- exterior-derivative-procedure [kform]
  (let [k (get-rank kform)]
    (if (= k 0)
      (function->oneform-field kform)
      (let [without #(concat (take %1 %2) (drop (inc %1) %2))
            k+1form (fn [& vectors]
                      (assert (= (count vectors) (inc k)))
                      (fn [point]
                        (let [n (:dimension
                                 (m/point->manifold point))]
                          (if (< k n)
                            (reduce g/+ (for [i (range 0 (inc k))]
                                          (let [rest (without i vectors)]
                                            (g/+ (g/* (if (even? i) 1 -1)
                                                      (((nth vectors i) (apply kform rest))
                                                       point))
                                                 (reduce g/+ (for [j (range (inc i) (inc k))]
                                                               (g/* (if (even? (+ i j)) 1 -1)
                                                                    ((apply kform
                                                                            (cons
                                                                             (o/commutator (nth vectors i)
                                                                                           (nth vectors j))
                                                                             (without (dec j) rest)))
                                                                     point))))))))
                            0))))]
        (procedure->nform-field k+1form (inc k) `(~'d ~(v/freeze kform)))))))

(def d
  (o/make-operator
   exterior-derivative-procedure 'd))
