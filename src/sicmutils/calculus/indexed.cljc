;;
;; Copyright © 2021 Sam Ritchie.
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

(ns sicmutils.calculus.indexed
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.structure :as s]
            [sicmutils.util.permute :as permute]))

;; ## Minimal support for Indexed Objects
;;
;; e.g. the components of tensors relative to a basis.
;;
;; TODO maybe move arg types here, if we don't need them in covariant. For sure!

(defn index-types [f]
  (::index-types (meta f)))

(defn has-index-types? [f]
  (boolean
   (index-types f)))

(defn with-index-types [f types]
  {:pre [(f/function? f)]}
  (vary-meta f assoc ::index-types types))

;; argument-types are, for example
;; (list up down down), for a Christoffel-2: it takes one oneform field and two
;; vector fields.

;; An argument-typed function of type (n . m) takes n oneform fields and m
;; vector-fields, in that order, and produces a function on a manifold point. An
;; indexed function of type (n . m) takes n+m indices and gives a function on a
;; manifold point.

;; For each argument-typed function and basis there is an indexed function that
;; gives the function resulting from applying the argument-typed function to the
;; basis elements specified by the corresponding indices.

(defn valid-arg-types?
  "Validates that:

  - The sequence is not empty
  - every entry is either a vector field or a form field
  - form fields come before vector fields"
  [ts]
  (letfn [(one-ff? [t] (isa? t ::ff/oneform-field))
          (vf? [t] (isa? t ::vf/vector-field))]
    (and (seq ts)
		     (every? (some-fn one-ff? vf?) ts)
         (every? vf? (drop-while one-ff? ts)))))

(defn valid-index-types?
  "Validates that:

  - The sequence is not empty
  - every entry is either 'up or 'down
  - all 'up entries come before 'down"
  [ts]
  (boolean
   (and (seq ts)
		    (every? #{'up 'down} ts)
        (every? #{'down} (drop-while #{'up} ts)))))

(defn typed->indexed [f basis]
  (let [arg-types (cov/argument-types f)]
    (assert (valid-arg-types? arg-types)
	          (str "Bad arg types: " arg-types))
    (let [vector-basis (b/basis->vector-basis basis)
          oneform-basis (b/basis->oneform-basis basis)
          idx-types (map (fn [t]
	                         (if (isa? t ::vf/vector-field)
                             'down
                             'up))
	                       arg-types)]
      (-> (fn indexed [indices]
            (assert (= (count indices)
                       (count arg-types)))
            (let [args (mapv (fn [t idx]
		                           (if (isa? t ::vf/vector-field)
			                           (get vector-basis idx)
                                 (get oneform-basis idx)))
		                         arg-types
                             indices)]
              (apply f args)))
          (with-index-types idx-types)))))

(defn indexed->typed [indexed basis]
  (let [index-types (index-types indexed)]
    (assert (valid-index-types? index-types)
            (str "Bad index types: " index-types))
    (let [vector-basis (b/basis->vector-basis basis)
          oneform-basis (b/basis->oneform-basis basis)
          n (b/basis->dimension basis)
          arg-types (mapv (fn [t]
	                          (if (= t 'up)
                              ::vf/vector-field
                              ::ff/oneform-field))
	                        index-types)]
      (-> (fn typed [& args]
            (assert (= (count index-types)
                       (count args)))
            (assert (every?
                     (map (fn [index-type arg]
		                        (or (and (= index-type 'up)
                                     (ff/oneform-field? arg))
			                          (and (= index-type 'down)
                                     (vf/vector-field? arg))))
		                      index-types
                          args))
	                  "Args do not match indices")

            ;; TODO what the heck is this sum doing???
            (let [sum (atom 0)]
              (letfn [(aloop [args term indices]
                        (if (empty? args)
	                        (swap! sum (fn [v] (+ (* (indexed indices) term) v)))
	                        (let [arg (first args)]
		                        (let dloop ((i 0))
		                             (if (< i n)
		                               (begin
			                              (aloop (rest args)
			                                     (g:* (if (vf/vector-field? arg)
					                                        ((get oneform-basis i) arg)
                                                  (arg (get vector-basis i)))
				                                        term)
			                                     (conj indices i))
			                              (dloop (inc i))))))))
                      (dloop [i])])
	            (aloop args 1 [])
	            sum))
          (with-meta {:arguments arg-types})))))


(declare outer-product contract)

(defn count-occurrences [x xs]
  (count
   (filter #{x} xs)))

(defn outer-product [T1 T2]
  (let [i1 (index-types T1)
        i2 (index-types T2)]
    (assert i1 "T1 not index typed")
    (assert i2 "T2 not index typed")
    (let [nu1 (count-occurrences 'up i1)
          nd1 (count-occurrences 'down i1)
          nu2 (count-occurrences 'up i2)
          nd2 (count-occurrences 'down i2)
          nup (+ nu1 nu2)
          ndp (+ nd1 nd2)
          np (+ nup ndp)
          n1 (+ nup nd1)]
      (letfn [(product [& args]
                (assert (= (count args) np)
                        (str "Wrong number of args to outer-product: " (count args) " vs " np))
                (let [argv (into [] args)]
                  (* (T1 (into (subvec argv 0 nu1)
			                         (subvec argv nup n1)))
		                 (T2 (into (subvec argv nu1 nup)
			                         (subvec argv n1 np))))))]
	      (with-index-types product
	        (concat (repeat nup 'up)
                  (repeat ndp 'down)))))))



(defn contract [T u d n]
  #_(let ((i-types (index-types T)))
      (assert i-types "T not index typed")
      (let ((nu (count-occurrences up i-types))
	          (nd (count-occurrences down i-types)))
        (assert (and (fix:<= 0 u) (fix:< u nu)
		                 (fix:<= 0 d) (fix:< d nd))
	              "Contraction indices not in range")
        (let ((nuc (fix:- nu 1)) (ndc (fix:- nd 1)))
	        (define (contraction args)
	          (sigma (lambda (i)
		                       (T (append
		                           (list-with-inserted-coord (list-head args nuc) u i)
		                           (list-with-inserted-coord (list-tail args nuc) d i))))
		               0 (fix:- n 1)))
	        (declare-index-types! contraction
                                (append (make-list nuc up) (make-list ndc down)))
	        contraction))))

(declare typed->structure structure->typed)

(defn typed->structure [T basis]
  (let [vector-basis (b/basis->vector-basis basis)
	      oneform-basis (b/basis->oneform-basis basis)]
    #_#_(define (iterate arg-types argl)
          (if (null? arg-types)
	          (apply T (reverse argl))
	          (s:map/r (lambda (e) (iterate (cdr arg-types) (cons e argl)))
		                 (cond ((eq? (car arg-types) vector-field?) vector-basis)
			                     ((eq? (car arg-types) oneform-field?) oneform-basis)
			                     (else (error "Bad arg-type"))))))
    (iterate (argument-types T) '())))

(defn structure->typed [coeff-functions basis]
  #_(let [vector-basis (b/basis->vector-basis basis)
	        oneform-basis (b/basis->oneform-basis basis)
	        arg-types (let lp ((cf coeff-functions))
	                       (if (structure? cf)
	                         (cons (let ((shape (s:opposite cf)))
		                               (cond ((eq? shape 'up) vector-field?)
			                                   ((eq? shape 'down) oneform-field?)
			                                   (else (error "Bad Shape"))))
		                             (lp (ref cf 0)))
	                         '()))]
      (define (indexed-function . args)
        (assert (fix:= (length args) (length arg-types)))
        (for-each (lambda (arg-type arg) (assert (arg-type arg)))
		              arg-types args)
        (g:* (let lp ((args args) (arg-types arg-types))
	                (if (null? args)
		                one-manifold-function
		                (let ((arg (car args)) (arg-type (car arg-types)))
		                  (cond ((eq? arg-type vector-field?)
			                       (s:map/r (lambda (etilde)
				                                      (g:* (etilde arg)
					                                         (lp (cdr args)
					                                             (cdr arg-types))))
				                              oneform-basis))
			                      ((eq? arg-type oneform-field?)
			                       (s:map/r (lambda (e)
				                                      (g:* (arg e)
					                                         (lp (cdr args)
					                                             (cdr arg-types))))
				                              vector-basis))))))
	           coeff-functions))
      (declare-argument-types! indexed-function arg-types)
      indexed-function))

(comment
  (define (manifold-function-cofunction? x)
    ;; f-quant returns true for functions AND procedures.
    (or (function-quantity? x)
        (numerical-quantity? x)))

  ;; TODO get this one!
  (defhandler '* (lambda (x y) zero-manifold-function) zero-manifold-function? manifold-function-cofunction?)
  (defhandler '* (lambda (x y) zero-manifold-function) manifold-function-cofunction? zero-manifold-function?)
  )
