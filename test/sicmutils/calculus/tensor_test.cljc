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
            [sicmutils.calculus.connection :as conn]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.curvature :as curv]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

;; ## Testing a function for being a tensor field.

;; To be a tensor field a function must be linear over the scalar function field
;; in each of its arguments.
;;
;; Each argument of a tensor field must be either a one-form field or a vector
;; field.
;;
;; The test is done with respect to some coordinate system. The types of the
;; arguments are specified in a list.

(defn literal-field [coordsys]
  (fn [t]
    (case t
      (:scalar :function)
      (m/literal-manifold-function (gensym 'g)
                                   coordsys)
      (:up :vector)
      (vf/literal-vector-field (gensym 'v)
                               coordsys)
      (:down :oneform :one-form)
      (ff/literal-oneform-field (gensym 'omega)
                                coordsys)
      :else (u/illegal (str "Bad type: " t)))))

(defn run-tensor-test [T types coordsys]
  (let [args (mapv (literal-field coordsys) types)
        f    ((literal-field coordsys) :scalar)]
    (doall
     (for [i (range (count types))
           :let [thing ((literal-field coordsys) (nth types i))]]
       (simplify
        ((- (apply T (assoc args i
                            (+ (* f (get args i))
                               thing)))
            (+ (* f (apply T args))
               (apply T (assoc args i thing))))
         (m/typical-point coordsys)))))))

(comment
  ;; Commenting this out for our big test suite; this takes about 30 seconds to
  ;; run, and it would be nice to remove it until we can properly mark long
  ;; tests for non-inclusion.
  (deftest ^:long long-tensor-tests
    (is (= [0 0 0 0]
           (run-tensor-test
            (curv/Riemann
             (cov/covariant-derivative
              (conn/literal-Cartan 'G m/R3-rect)))
            [:oneform :vector :vector :vector]
            m/R3-rect)))))

(deftest tensor-tests
  (letfn [(F [nabla]
            (fn [omega u v]
              (omega ((nabla u) v))))]
    (is (= [0 0]
           (take 2 (run-tensor-test
                    (F (cov/covariant-derivative (conn/literal-Cartan 'G m/R3-rect)))
                    [:oneform :vector :vector]
                    m/R3-rect)))
        "The first two are zero, the third entry is a mess, as GJS notes."))

  (letfn [(G [nabla]
            (fn [omega u v]
              (omega ((curv/torsion-vector nabla) u v))))]
    (is (= [0 0 0]
           (run-tensor-test
            (G (cov/covariant-derivative (conn/literal-Cartan 'G m/R3-rect)))
            [:oneform :vector :vector]
            m/R3-rect)))))
