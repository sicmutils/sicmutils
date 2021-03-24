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

(ns sicmutils.calculus.covariant
  (:require [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.map :as cm]
            [sicmutils.calculus.manifold :as manifold]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v]))

;; This comes from `Lie.scm`.

(defn- vector-field-Lie-derivative [X]
  (o/make-operator
   (fn [Y]
     (cond (f/function? Y)      (X Y)
           (vf/vector-field? Y) (o/commutator X Y)
           (ff/form-field? Y)
           (let [k (ff/get-rank Y)
                 op (fn [& vectors]
                      (assert (= k (count vectors))
                              `(~'≠ ~k ~(count vectors)
                                ~@vectors
                                ~@(map meta vectors)))
                      (g/- ((g/Lie-derivative X) (apply Y vectors))
                           (ua/generic-sum
                            (fn [i]
                              (apply Y (map-indexed (fn [j v]
                                                      (if (= j i)
                                                        ((g/Lie-derivative X) v)
                                                        v))
                                                    vectors)))
                            0 k)))
                 name `((~'Lie-derivative ~(v/freeze X))
                        ~(v/freeze Y))]
             (ff/procedure->nform-field op k name))
           (s/structure? Y)
	         (s/mapr (vector-field-Lie-derivative X) Y)

           :else (u/unsupported "Can't take the Lie derivative of that yet")))
   `(~'Lie-derivative
     ~(v/freeze X))))

(defmethod g/Lie-derivative [::vf/vector-field] [V]
  (vector-field-Lie-derivative V))

;; ## Interior Product, from interior-product.scm

(defn interior-product [V]
  (assert (vf/vector-field? V))
  (fn [omega]
    (assert (ff/form-field? omega))
    (let [k (ff/get-rank omega)]
      (ff/procedure->nform-field
       (fn [& vectors]
         (assert (= (dec k) (count vectors)))
         (apply omega V vectors))
       (dec k)
       `((~'interior-product ~(v/freeze V)) ~(v/freeze omega))))))

;; ## Covariant Derivative, from covariant-derivative.scm

(defn make-Cartan
  [forms basis]
  {:type ::Cartan
   :forms forms
   :basis basis})

(defn Cartan? [m]
  (= (v/kind m) ::Cartan))

(def Cartan->forms :forms)
(def Cartan->basis :basis)

(defn make-Christoffel
  [symbols basis]
  {:type ::Christoffel
   :symbols symbols
   :basis basis})

(defn Christoffel? [m]
  (= (v/kind m) ::Christoffel))

(def Christoffel->symbols :symbols)
(def Christoffel->basis :basis)

(defn Cartan->Christoffel [Cartan]
  {:pre [(Cartan? Cartan)]}
  (let [basis (Cartan->basis Cartan)
	      forms (Cartan->forms Cartan)]
    (make-Christoffel
     (s/mapr forms (b/basis->vector-basis basis))
     basis)))

(defn Christoffel->Cartan [Christoffel]
  {:pre [(Christoffel? Christoffel)]}
  (let [basis   (Christoffel->basis Christoffel)
        symbols (Christoffel->symbols Christoffel)]
    (make-Cartan (g/* symbols
                      (b/basis->oneform-basis basis))
                 basis)))

(defn symmetrize-Christoffel [G]
  (let [s (Christoffel->symbols G)]
    (make-Christoffel
     (g/* (g// 1 2)
          (g/+ s (s/transpose-outer s)))
     (Christoffel->basis G))))

(defn symmetrize-Cartan [Cartan]
  (Christoffel->Cartan
   (symmetrize-Christoffel
    (Cartan->Christoffel Cartan))))

(defn Cartan-transform
  [cartan basis-prime]
  (let [basis (Cartan->basis cartan) ;; tuple of basis vectors
        forms (Cartan->forms cartan)
        prime-dual-basis (b/basis->oneform-basis basis-prime)
        prime-vector-basis (b/basis->vector-basis basis-prime)
        vector-basis (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)
        J-inv (s/mapr oneform-basis prime-vector-basis)
        J (s/mapr prime-dual-basis vector-basis)
        omega-prime-forms (ff/procedure->oneform-field
                           (fn [u]
                             (g/+ (g/* J (u J-inv))
                                  (g/* J (g/* (forms u) J-inv))))
                           'omega-prime-forms)]
    (make-Cartan omega-prime-forms basis-prime)))

(defn Cartan->Cartan-over-map [Cartan map]
  (let [basis (cm/basis->basis-over-map map (Cartan->basis Cartan))
	      Cartan-forms
	      (s/mapr (cm/form-field->form-field-over-map map)
		            (Cartan->forms Cartan))]
    (make-Cartan (f/compose Cartan-forms (cm/differential map))
                 basis)))

;; ### Covariant Vector Definition

(defn- argument-types [t]
  (if (o/operator? t)
    (:arguments (o/context t) [])
    (:arguments (meta t) [])))

(defn- has-argument-types? [op]
  (boolean
   (seq (argument-types op))))

(defn- covariant-derivative-vector [Cartan]
  (let [basis (Cartan->basis Cartan)
        Cartan-forms (Cartan->forms Cartan)
        vector-basis (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)]
    (fn [V]
      (let [CV (Cartan-forms V)]
        (fn [U]
          (let [u-components (oneform-basis U)
                deriv-components (g/+ (V u-components)
                                      (g/* CV u-components))]
            (vf/procedure->vector-field
             (fn [f]
               (g/* (vector-basis f) deriv-components))
             `((~'nabla ~(v/freeze V))
               ~(v/freeze U)))))))))

(defn- covariant-derivative-form [Cartan]
  (fn [V]
    (fn [tau]
      (let [k (ff/get-rank tau)
            nabla_V ((covariant-derivative-vector Cartan) V)
            op (fn [& vectors]
                 (assert (= k (count vectors)))
                 (g/- (V (apply tau vectors))
                      (ua/generic-sum
                       (fn [i]
                         (apply tau (map-indexed
                                     (fn [j v]
                                       (if (= j i)
                                         (nabla_V v)
                                         v))
                                     vectors)))
                       0 k)))
            name `((~'nabla ~(v/freeze V))
                   ~(v/freeze tau))]
        (ff/procedure->nform-field op k name)))))

(defn- covariant-derivative-argument-types
  [Cartan]
  (let [basis (Cartan->basis Cartan)
	      vector-basis (b/basis->vector-basis basis)
	      oneform-basis (b/basis->oneform-basis basis)
	      Cartan-forms (Cartan->forms Cartan)]
    (fn [V]
      (let [CV (Cartan-forms V)]
	      (fn [T]
          (let [arg-types (argument-types T)]
            ;; TODO verify now that all args are the correct type, either one-ff
            ;; or vf.
            (letfn [(the-derivative [& args]
                      (assert (= (count args) (count arg-types)))
                      (let [VT (letfn [(lp [types arg-types
                                            args args
                                            targs []
                                            factors []]
		                                     (if (empty? types)
			                                     (g/* (V (apply T targs))
				                                        (apply g/* factors))

			                                     (b/contract
			                                      (fn [e w]
			                                        (cond (isa? (first types) ::vf/vector-field)
				                                            (do (assert (vf/vector-field? (first args)))
				                                                (lp (rest types)
					                                                  (rest args)
					                                                  (conj targs e)
					                                                  (conj factors (w (first args)))))

				                                            (isa? (first types) ::ff/oneform-field)
				                                            (do (assert (ff/oneform-field? (first args)))
				                                                (lp (rest types)
					                                                  (rest args)
					                                                  (conj targs w)
					                                                  (conj factors ((first args) e))))
				                                            :else (u/illegal "Bad arg types")))
			                                      basis)))])
		                        corrections (apply g/+
		                                           (map-indexed
                                                (fn [i type]
			                                            (cond (isa? type ::ff/oneform-field) ;; positive
				                                                (g/*
				                                                 (g/* (s/mapr (fn [e]
						                                                            ((nth args i) e))
						                                                          vector-basis)
					                                                    CV)
				                                                 (s/mapr
				                                                  (fn [w]
                                                            ;; TODO more efficient...
					                                                  (apply T (assoc (into [] args) i w)))
				                                                  oneform-basis))

				                                                (isa? type ::vf/vector-field) ;; negative
				                                                (g/negate
				                                                 (g/*
				                                                  (s/mapr
				                                                   (fn [e]
					                                                   (apply T (assoc (into [] args) i e)))
				                                                   vector-basis)
				                                                  (g/* CV
					                                                     (s/mapr (fn [w]
						                                                             (w (nth args i)))
						                                                           oneform-basis))))))
			                                          arg-types))]
		                    (g/+ VT corrections)))]
              ;; TODO: note: Then we pass along the derivative with the SAME
              ;; argument types.
              (with-meta the-derivative
                {:arguments arg-types}))))))))

(defn- covariant-derivative-function
  [Cartan]
  (fn [X]
    (fn [f]
      (fn [& args]
        (let [types (map (fn [arg]
                           ;; TODO map v/kind, do (apply v/argument-kind args)
		                       (cond (vf/vector-field? arg) ::vf/vector-field
		                             (ff/oneform-field? arg) ::ff/oneform-field
		                             (manifold/manifold-point? arg) ::manifold/manifold-point
		                             :else false))
	                       args)]
          (cond (and (= (count types) 1)
                     ;; TODO check if first arg is a manifold point.
		                 (isa? (first types) ::manifold/manifold-point))
	              (let [f (with-meta f {:arguments types})]
                  ((X f) (first args)))

	              (some (fn [type]
                        ;; TODO check the actual args here! Goal is to check
                        ;; whether we have any arg that's not either of these.
                        ;; SO, actually we want to check that every arg is
                        ;; either a vector field or oneform.
		                    (not (or (isa? type ::vf/vector-field)
			                           (isa? type ::ff/oneform-field))))
		                  types)
	              (u/illegal "Bad function or arguments to covariant derivative")

	              :else
                ;; Now we know that they are all either a single-arg function of
                ;; a point or multi-arg functions of vf or one-ff.
	              (let [f (with-meta f {:arguments types})]
	                (apply (((covariant-derivative-argument-types Cartan) X) f)
		                     args))))))))

(defn- covariant-derivative-ordinary [Cartan]
  {:pre [(Cartan? Cartan)]}
  (fn [X]
    (let [op (fn nabla_X [V]
               (cond (vf/vector-field? V)
                     (((covariant-derivative-vector Cartan) X) V)

                     (ff/form-field? V)
                     (((covariant-derivative-form Cartan) X) V)

                     (has-argument-types? V)
	                   (((covariant-derivative-argument-types Cartan) X) V)

                     (f/function? V)
                     (((covariant-derivative-function Cartan) X) V)

                     (s/structure? V)
                     (s/mapr nabla_X V)

                     :else
                     (u/unsupported
                      (str "Can't do this kind of covariant derivative yet "
                           (v/freeze X) " @ " (v/freeze V)))))
          name `(~'nabla
                 ~(v/freeze X))]
      (o/make-operator op name))))

(defn covariant-derivative
  ([Cartan]
   (covariant-derivative-ordinary Cartan))
  ([Cartan map]
   (covariant-derivative-ordinary
    (Cartan->Cartan-over-map Cartan map))))

(defn geodesic-equation
  [source-coordsys target-coordsys Cartan-on-target]
  (fn [gamma]
    (fn [source-m]
      {:pre [(= 1 (:dimension
                   (manifold/manifold source-coordsys)))]}
      (let [e (vf/coordinate-system->vector-basis source-coordsys)]
        (((((covariant-derivative Cartan-on-target gamma)
	          e)
           ((cm/differential gamma) e))
          (manifold/chart target-coordsys))
         source-m)))))

(defn parallel-transport-equation
  [source-coordsys target-coordsys Cartan-on-target]
  (fn [gamma]
    (fn [vector-over-gamma]
      (fn [source-m]
        {:pre [(= 1 (:dimension
                     (manifold/manifold source-coordsys)))]}
        (let [e (vf/coordinate-system->vector-basis source-coordsys)]
          (((((covariant-derivative Cartan-on-target gamma)
	            e);; d/dt
             vector-over-gamma)
            (manifold/chart target-coordsys))
           source-m))))))
