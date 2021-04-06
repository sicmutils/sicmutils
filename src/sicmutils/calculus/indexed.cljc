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
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.operator :as o]
            [sicmutils.matrix :as matrix]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.permute :as permute]
            [sicmutils.value :as v]))

;; ## Minimal support for Indexed Objects
;;
;; Indices here are the components of tensors relative to a basis.

(defn- meta-k
  ([f k]
   (meta-k f k nil))
  ([f k default]
   (if (o/operator? f)
     (k (o/context f) default)
     (k (meta f) default))))

(defn- with-kv [f k v]
  (if (o/operator? f)
    (o/with-context f (assoc (o/context f) k v))
    (vary-meta f assoc k v)))

;; argument-types are, for example,
;;
;;  [::ff/oneform-field ::vf/vector-field ::vf/vector-field]
;;
;; for a Christoffel-2: it takes one oneform field and two vector fields.

(defn argument-types [f]
  (meta-k f :arguments []))

(defn ^:no-doc has-argument-types? [op]
  (boolean
   (seq (argument-types op))))

(defn with-argument-types [f types]
  (with-kv f :arguments (into [] types)))

(defn index-types [f]
  (meta-k f :index-types []))

(defn ^:no-doc has-index-types? [f]
  (boolean
   (seq (index-types f))))

(defn with-index-types [f types]
  (with-kv f :index-types (into [] types)))

;; index types are, for example:
;;
;;  ['up 'down 'down]
;;
;; for a Christoffel-2: it takes one oneform field and two vector fields.
;;
;; An argument-typed function of type (n . m) takes n oneform fields and m
;; vector-fields, in that order, and produces a function on a manifold point.
;;
;; An indexed function of type (n . m) takes n+m indices and gives a function on
;; a manifold point.

;; For each argument-typed function and basis, there is an indexed function that
;; gives the function resulting from applying the argument-typed function to the
;; basis elements specified by the corresponding indices.

(defn- valid-arg-types?
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

(defn typed->indexed [f basis]
  (let [arg-types (argument-types f)]
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
                       (count arg-types))
                    (str "Wrong counts: indices - "
                         (count indices)
                         ", arg-types:  " (count arg-types)))
            (let [args (mapv (fn [t idx]
		                           (if (isa? t ::vf/vector-field)
			                           (get vector-basis idx)
                                 (get oneform-basis idx)))
		                         arg-types
                             indices)]
              (apply f args)))
          (with-index-types idx-types)))))

(defn- valid-index-types?
  "Validates that:

  - The sequence is not empty
  - every entry is either 'up or 'down
  - all 'up entries come before 'down"
  [ts]
  (boolean
   (and (seq ts)
		    (every? #{'up 'down} ts)
        (every? #{'down} (drop-while #{'up} ts)))))

(defn- validate-typed-args! [index-types args]
  (assert (= (count index-types)
             (count args)))
  (assert (every? true?
                  (map (fn [index-type arg]
		                     (or (and (= index-type 'up)
                                  (ff/oneform-field? arg))
			                       (and (= index-type 'down)
                                  (vf/vector-field? arg))))
		                   index-types
                       args))
	        "Args do not match indices"))

(defn indexed->typed [indexed basis]
  (let [index-types (index-types indexed)]
    (assert (valid-index-types? index-types)
            (str "Bad index types: " index-types))

    (let [vector-basis  (b/basis->vector-basis basis)
          oneform-basis (b/basis->oneform-basis basis)
          n              (b/basis->dimension basis)
          arg-types (mapv {'up ::ff/oneform-field
                           'down ::vf/vector-field}
	                        index-types)]
      (-> (fn typed [& args]
            (validate-typed-args! index-types args)
            (let [n-seq  (reverse (range n))
                  combos (permute/cartesian-product
                          (for [x args]
                            (map (fn [i arg]
                                   [i (if (vf/vector-field? arg)
					                              ((get oneform-basis i) arg)
                                        (arg (get vector-basis i)))])
                                 n-seq
                                 (repeat x))))]
              (ua/generic-sum
               (for [combo combos
                     :let [indices      (map first combo)
                           product-args (map peek combo)]]
                 (apply *
                        (indexed indices)
                        (reverse product-args))))))
          (with-meta {:arguments arg-types})))))

(defn outer-product [T1 T2]
  (let [i1 (index-types T1)
        i2 (index-types T2)]
    (assert i1 "T1 not index typed!")
    (assert i2 "T2 not index typed!")
    (let [{nu1 'up nd1 'down} (frequencies i1)
          {nu2 'up nd2 'down} (frequencies i2)
          nup (+ nu1 nu2)
          ndp (+ nd1 nd2)
          np  (+ nup ndp)
          n1  (+ nup nd1)]
      (letfn [(product [args]
                (assert (= (count args) np)
                        (str "Wrong number of args to outer-product: "
                             (count args) " vs " np))
                (let [argv (into [] args)]
                  (* (T1 (into (subvec argv 0 nu1)
			                         (subvec argv nup n1)))
		                 (T2 (into (subvec argv nu1 nup)
			                         (subvec argv n1 np))))))]
	      (with-index-types product
	        (concat (repeat nup 'up)
                  (repeat ndp 'down)))))))

(letfn [(insertv [coll i v]
          (let [l (subvec coll 0 i)
                r (subvec coll i)]
            (apply conj l v r)))]

  (defn contract [T u d n]
    (let [i-types (index-types T)]
      (assert i-types "T not index typed!")
      (let [{nu 'up nd 'down} (frequencies i-types)]
        (assert (and (<= 0 u) (< u nu)
		                 (<= 0 d) (< d nd))
	              "Contraction indices not in range")
        (let [nuc (dec nu)
              ndc (dec nd)]
          (-> (fn contraction [args]
	              (let [argv (into [] args)]
                  (ua/generic-sum
                   (fn [i]
		                 (T (concat
                         (insertv (subvec argv 0 nuc) u i)
                         (insertv (subvec argv nuc) d i))))
		               0 n)))
	            (with-index-types
                (concat (repeat nuc 'up)
                        (repeat ndc 'down)))))))))

(defn typed->structure [T basis]
  (let [vector-basis  (b/basis->vector-basis basis)
	      oneform-basis (b/basis->oneform-basis basis)]
    (letfn [(lp [arg-types argv]
              (if (empty? arg-types)
	              (apply T argv)
	              (s/mapr (fn [e]
                          (lp (rest arg-types)
                              (conj argv e)))
		                    (cond (isa? (first arg-types) ::vf/vector-field)
                              vector-basis

                              (isa? (first arg-types) ::ff/oneform-field)
                              oneform-basis

			                        :else (u/illegal "Bad arg-type!")))))]
      (lp (argument-types T) []))))

(defn structure->typed [coeff-functions basis]
  (let [vector-basis  (b/basis->vector-basis basis)
	      oneform-basis (b/basis->oneform-basis basis)
	      arg-types     (loop [cf  coeff-functions
                             acc []]
	                      (if-not (s/structure? cf)
                          acc
                          (let [shape (s/opposite-orientation
                                       (s/orientation cf))
                                t (case shape
                                    ::s/up ::vf/vector-field
			                              ::s/down ::ff/oneform-field)]
                            (recur (get cf 0)
                                   (conj acc t)))))]
    (-> (fn indexed-fn [& args]
          (assert (= (count args) (count arg-types)))
          (assert (every? true? (map (fn [arg arg-type]
                                       (isa? (v/kind arg) arg-type))
		                                 args arg-types)))
          (letfn [(lp [args arg-types]
                    (if (empty? args)
		                  m/one-manifold-function
		                  (let [arg (first args)
                            arg-type (first arg-types)]
		                    (cond (isa? arg-type ::vf/vector-field)
			                        (s/mapr (fn [etilde]
				                                (* (etilde arg)
					                                 (lp (rest args)
					                                     (rest arg-types))))
				                              oneform-basis)

                              (isa? arg-type ::ff/oneform-field)
			                        (s/mapr (fn [e]
				                                (* (arg e)
					                                 (lp (rest args)
					                                     (rest arg-types))))
				                              vector-basis)))))]
            (* (lp args arg-types)
	             coeff-functions)))
        (with-argument-types arg-types))))
