#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.tensor-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [emmy.calculus.connection :as conn]
            [emmy.calculus.covariant :as cov]
            [emmy.calculus.curvature :as curv]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.generic :as g :refer [+ - *]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.util :as u]
            [emmy.value :as v]))

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
    (for [i (range (count types))
          :let [thing ((literal-field coordsys) (nth types i))]]
      (simplify
       ((- (apply T (assoc args i
                           (+ (* f (get args i))
                              thing)))
           (+ (* f (apply T args))
              (apply T (assoc args i thing))))
        (m/typical-point coordsys))))))

(deftest ^:long long-tensor-tests
  (is (= [0 0 0 0]
         (run-tensor-test
          (curv/Riemann
           (cov/covariant-derivative
            (conn/literal-Cartan 'G m/R3-rect)))
          [:oneform :vector :vector :vector]
          m/R3-rect))))

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
