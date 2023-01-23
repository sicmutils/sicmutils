#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.covariant
  (:require [emmy.calculus.basis :as b]
            [emmy.calculus.derivative :refer [D]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.indexed :as ci]
            [emmy.calculus.manifold :as manifold]
            [emmy.calculus.map :as cm]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.operator :as o]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v]))

;; This comes from `Lie.scm`.

(defn- vector-field-Lie-derivative [X]
  (let [freeze-X (v/freeze X)
        op-name `(~'Lie-derivative ~freeze-X)]
    (-> (fn rec [Y]
          (cond (f/function? Y)      (X Y)
                (vf/vector-field? Y) (o/commutator X Y)

                (ff/form-field? Y)
                (let [k (ff/get-rank Y)
                      op (fn [& vectors]
                           (let [vectors (into [] vectors)]
                             (assert (= k (count vectors))
                                     `(~'≠ ~k ~(count vectors)
                                       ~@vectors
                                       ~@(map meta vectors)))
                             (g/- ((g/Lie-derivative X) (apply Y vectors))
                                  (ua/generic-sum
                                   (fn [i]
                                     (let [xs (update vectors i (g/Lie-derivative X))]
                                       (apply Y xs)))
                                   0 k))))
                      name `((~'Lie-derivative ~freeze-X) ~(v/freeze Y))]
                  (ff/procedure->nform-field op k name))

                (s/structure? Y)
                (s/mapr (vector-field-Lie-derivative X) Y)

                :else (u/unsupported "Bad argument: Lie Derivative")))
        (o/make-operator op-name))))

(defmethod g/Lie-derivative [::vf/vector-field] [V]
  (vector-field-Lie-derivative V))

;; ## From ODE.scm:
;;
;; Let (sigma t) be the state of a system at time t.  Let the
;; (first-order) system of differential equations governing the evolution of
;; this state be:

;; ((D sigma) t) = (R (sigma t))
;; or  (D sigma) = (compose R sigma)

;; i.e. R is a system derivative.

;; Let F be any function of state, then a differential equation for the
;; evolution of F, as it is dragged along the integral curve sigma is:

;; (D (compose F sigma)) = (* (compose (D F) sigma) (D sigma))
;; = (compose (* (D F) R) sigma)

;; Let's call this operation Lie-D (the Lie derivative for coordinates):

(defn Lie-D
  "Takes a system derivative `R` and returns a operator that takes a function `F`
  of coordinatized state and performs the operation described below, from
  ODE.scm in scmutils:

  Let `(sigma t)` be the state of a system at time `t`. Let the
  (first-order) system of differential equations governing the evolution of
  this state be:

  ```clojure
  ((D sigma) t) = (R (sigma t))
  ```

  ```clojure
  (D sigma) = (compose R sigma)
  ```

  i.e. `R` is a system derivative.

  Let `F` be any function of state, then a differential equation for the
  evolution of `F`, as it is dragged along the integral curve sigma is:

  ```clojure
  (D (compose F sigma)) = (* (compose (D F) sigma) (D sigma))
  = (compose (* (D F) R) sigma)
  ```

  Let's call this operation `Lie-D` (the Lie derivative for coordinates)."
  [R]
  (-> (fn [F]
        (g/* (D F) R))
      (o/make-operator
       (list 'Lie-D (v/freeze R)))))

;; ## Interior Product, from interior-product.scm

(defn interior-product [X]
  (assert (vf/vector-field? X))
  (fn ix [alpha]
    (assert (ff/form-field? alpha))
    (let [p (ff/get-rank alpha)]
      (assert (> p 0)
              "Rank of form not greater than zero: interior-product")
      (ff/procedure->nform-field
       (fn the-product [& vectors]
         (assert (= (dec p) (count vectors)))
         (apply alpha X vectors))
       (dec p)
       `((~'interior-product ~(v/freeze X))
         ~(v/freeze alpha))))))

;; ## Covariant Derivative, from covariant-derivative.scm

(defn make-Cartan
  [forms basis]
  {:type ::Cartan
   :forms forms
   :basis basis})

(defn Cartan? [x]
  (= (v/kind x) ::Cartan))

(defn Cartan->forms [C]
  (:forms C))

(defn Cartan->basis [C]
  (:basis C))

(defn make-Christoffel
  "Returns a data structure representing [Christoffel symbols of the second
  kind](https://en.wikipedia.org/wiki/Christoffel_symbols#Christoffel_symbols_of_the_second_kind_(symmetric_definition))."
  [symbols basis]
  {:type ::Christoffel
   :symbols symbols
   :basis basis})

(defn Christoffel? [x]
  (= (v/kind x) ::Christoffel))

(defn Christoffel->symbols [C]
  (:symbols C))

(defn Christoffel->basis [C]
  (:basis C))

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
    (make-Cartan
     (g/* symbols (b/basis->oneform-basis basis))
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
                             (g/+
                              (g/* J (u J-inv))
                              (g/* J (g/* (forms u) J-inv))))
                           'omega-prime-forms)]
    (make-Cartan omega-prime-forms basis-prime)))

(defn Cartan->Cartan-over-map [Cartan map]
  (let [basis (cm/basis->basis-over-map
               map (Cartan->basis Cartan))
        forms (s/mapr (cm/form-field->form-field-over-map map)
                      (Cartan->forms Cartan))]
    (make-Cartan forms basis)))

;; ### Covariant Vector Definition

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
             (fn the-derivative [f]
               (g/* (vector-basis f) deriv-components))
             `((~'nabla ~(v/freeze V))
               ~(v/freeze U)))))))))

(defn- covariant-derivative-form [Cartan]
  (fn [V]
    (fn [tau]
      (let [k (ff/get-rank tau)
            nabla_V ((covariant-derivative-vector Cartan) V)
            op (fn [& vectors]
                 (let [vectors (into [] vectors)]
                   (assert (= k (count vectors)))
                   (g/- (V (apply tau vectors))
                        (ua/generic-sum
                         (fn [i]
                           (let [xs (update vectors i nabla_V)]
                             (apply tau xs)))
                         0 k))))
            name `((~'nabla ~(v/freeze V))
                   ~(v/freeze tau))]
        (ff/procedure->nform-field op k name)))))

(defn- covariant-derivative-argument-types
  "NOTE: Returns a derivative with the same argument types as the original input
  function."
  [Cartan]
  (let [basis (Cartan->basis Cartan)
        vector-basis (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)
        Cartan-forms (Cartan->forms Cartan)]
    (fn [V]
      (let [CV (Cartan-forms V)]
        (fn [T]
          (let [arg-types (ci/argument-types T)]
            (assert
             (every? (fn [t]
                       (or (isa? t ::vf/vector-field)
                           (isa? t ::ff/oneform-field)))
                     arg-types))
            (letfn [(lp [types args targs factors]
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
                                         (conj factors ((first args) e))))))
                         basis)))
                    (the-derivative [& args]
                      (assert (= (count args)
                                 (count arg-types)))
                      (let [argv (into [] args)
                            VT (lp arg-types argv [] [])
                            corrections (ua/generic-sum
                                         (map-indexed
                                          (fn [i type]
                                            (cond
                                              ;; positive
                                              (isa? type ::ff/oneform-field)
                                              (g/*
                                               (g/* (s/mapr (fn [e]
                                                              ((nth argv i) e))
                                                            vector-basis)
                                                    CV)
                                               (s/mapr
                                                (fn [w]
                                                  (apply T (assoc argv i w)))
                                                oneform-basis))

                                              ;; negative
                                              (isa? type ::vf/vector-field)
                                              (g/negate
                                               (g/*
                                                (s/mapr
                                                 (fn [e]
                                                   (apply T (assoc argv i e)))
                                                 vector-basis)
                                                (g/* CV (s/mapr
                                                         (fn [w]
                                                           (w (nth argv i)))
                                                         oneform-basis))))))
                                          arg-types))]
                        (g/+ VT corrections)))]
              (ci/with-argument-types
                the-derivative
                arg-types))))))))

(defn- covariant-derivative-function [Cartan]
  (fn [X]
    (fn [f]
      (fn [& args]
        (let [types (apply v/argument-kind args)]
          (cond (and (= (count args) 1)
                     (manifold/manifold-point? (first args)))
                (let [f (ci/with-argument-types f types)]
                  ((X f) (first args)))

                (every? (fn [arg] ;; either a vector field or oneform.
                          (or (vf/vector-field? arg)
                              (ff/oneform-field? arg)))
                        args)
                (let [f (ci/with-argument-types f types)]
                  (apply (((covariant-derivative-argument-types Cartan) X) f)
                         args))

                :else
                (u/illegal "Bad function or arguments to covariant derivative")))))))

(defn- covariant-derivative-ordinary [Cartan]
  {:pre [(Cartan? Cartan)]}
  (fn [X]
    (let [op (fn nabla_X [V]
               (cond (vf/vector-field? V)
                     (((covariant-derivative-vector Cartan) X) V)

                     (ff/form-field? V)
                     (((covariant-derivative-form Cartan) X) V)

                     (ci/has-argument-types? V)
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
   (let [mapped (Cartan->Cartan-over-map Cartan map)]
     (covariant-derivative-ordinary
      (make-Cartan (f/compose (Cartan->forms mapped)
                              (cm/differential map))
                   (Cartan->basis mapped))))))

(defn covariant-differential [Cartan]
  (fn [V]
    (fn [X]
      (((covariant-derivative Cartan) X) V))))

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
              e) ;; d/dt
             vector-over-gamma)
            (manifold/chart target-coordsys))
           source-m))))))
