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

(ns sicmutils.calculus.tensor-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.coordinate :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.form-field :as vf]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(comment
  ;; ## Testing a function for being a tensor field.

  ;; To be a tensor field a function must be linear
  ;; over the scalar function field in each of its
  ;; arguments.
  ;;
  ;; Each argument of a tensor field must be either
  ;; a one-form field or a vector field.
  ;;
  ;; The test is done with respect to some coordinate
  ;; system.  The types of the arguments are specified
  ;; in a list.

  (define ((literal-field coordsys) type)
    (case type
      (scalar function)
      (m/literal-manifold-function (gensym 'g)
                                   coordsys)
      (up vector)
      (vf/literal-vector-field (gensym 'v)
                               coordsys)
      (down oneform one-form)
      (ff/literal-oneform-field (gensym 'omega)
                                coordsys)
      :else
      (error "Bad type list" types)))

  (defn tensor-test [T types coordsys]
    (let [args (mapv (literal-field coordsys) types)
          f ((literal-field coordsys) 'scalar)]
      (map (fn [i]
             (let [thing ((literal-field coordsys) (nth types i))]
               ((- (apply T (assoc args i
                                   (+ (* f (ref args i))
                                      thing)))
                   (+ (* f (apply T args))
                      (apply T (assoc args i thing))))
                (m/typical-point coordsys))))
           (range (count types)))))


  (tensor-test
   (Riemann (cov/covariant-derivative (literal-Cartan 'G m/R3-rect)))
   '(oneform vector vector vector)
   m/R3-rect)

  (0 0 0 0)

  (defn F [nabla]
    (fn [omega u v]
      (omega ((nabla u) v))))

  (tensor-test
   (F (cov/covariant-derivative (literal-Cartan 'G m/R3-rect)))
   '(oneform vector vector)
   m/R3-rect)

  (0 0 <Mess>)


  (define ((G nabla) omega u v)
    (omega ((torsion-vector nabla) u v)))

  (tensor-test
   (G (cov/covariant-derivative (literal-Cartan 'G m/R3-rect)))
   '(oneform vector vector)
   m/R3-rect)

  (0 0 0)

  )
