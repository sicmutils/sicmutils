#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [up /]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest equations
  (e/with-literal-functions [θ]
    (is (= '(+ (* -1 a l m (expt ω 2) (sin (θ t)) (cos (* t ω)))
               (* g l m (sin (θ t)))
               (* (expt l 2) m (((expt D 2) θ) t)))
           (e/freeze
            (e/simplify (((e/Lagrange-equations
                           (driven/L 'm 'l 'g 'a 'ω))
                          θ)
                         't)))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (driven/evolver {:t (/ 3 60) :dt (/ 1 60) :observe observe})
      (is (= 4 (count @o))))))

(deftest as-javascript
  (let [eq (e/simplify
            ((driven/state-derivative 'm 'l 'g 'a 'omega)
             (up 't 'theta 'thetadot)))]
    (is (= (str "function(t, theta, thetadot) {\n"
                "  var _0001 = Math.sin(theta);\n"
                "  return [1, thetadot, (a * Math.pow(omega, 2) * _0001 * Math.cos(omega * t) - g * _0001) / l];\n"
                "}")
           (e/->JavaScript eq
                           :parameter-order '[t theta thetadot]
                           :deterministic? true))))
  (let [eq (e/simplify
            ((e/Hamiltonian->state-derivative
              (e/Lagrangian->Hamiltonian
               (driven/L 'm 'l 'g 'a 'omega)))
             (e/->H-state 't 'theta 'p_theta)))]
    (is (= (str "function(t, theta, p_theta) {\n"
                "  var _0001 = omega * t;\n"
                "  var _0002 = Math.cos(theta);\n"
                "  var _0003 = Math.pow(l, 2);\n"
                "  var _0005 = Math.sin(theta);\n"
                "  var _0006 = Math.sin(_0001);\n"
                "  return [1, (a * l * m * omega * _0005 * _0006 + p_theta) / (_0003 * m), (- Math.pow(a, 2) * l * m * Math.pow(omega, 2) * _0005 * Math.pow(_0006, 2) * _0002 - a * omega * p_theta * _0006 * _0002 - g * _0003 * m * _0005) / l];\n"
                "}")
           (e/->JavaScript eq
                           :parameter-order '[t theta p_theta]
                           :deterministic? true)))))
