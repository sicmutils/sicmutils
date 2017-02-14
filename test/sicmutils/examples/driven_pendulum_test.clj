;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.examples.driven-pendulum :as driven]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest equations
  (with-literal-functions
    [θ y]
    (is (= '(+ (* -1 (cos (* t ω)) (sin (θ t)) a l m (expt ω 2))
               (* (sin (θ t)) g l m)
               (* (((expt D 2) θ) t) (expt l 2) m))
           (simplify (((Lagrange-equations
                         (driven/L 'm 'l 'g 'a 'ω))
                        θ)
                       't))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (driven/evolver {:t 3/60 :dt 1/60 :observe observe})
      (is (= 4 (count @o))))))

(deftest as-javascript
  (let [eq (simplify
            ((driven/state-derivative 'm 'l 'g 'a 'omega)
             (up 't 'theta 'thetadot)))]
    (is (= (str "function(t, theta, thetadot) {\n"
                "  var _0001 = Math.sin(theta);\n"
                "  return [1, thetadot, (_0001 * Math.cos(omega * t) * a * Math.pow(omega, 2) - _0001 * g) / l];\n"
                "}")
           (->JavaScript eq :parameter-order '[t theta thetadot]))))
  (let [eq (simplify
            ((Hamiltonian->state-derivative
              (Lagrangian->Hamiltonian
               (driven/L 'm 'l 'g 'a 'omega)))
             (->H-state 't 'theta 'p_theta)))]
    (is (= (str "function(t, theta, p_theta) {\n"
                "  var _0002 = Math.pow(l, 2);\n"
                "  var _0003 = omega * t;\n"
                "  var _0004 = Math.sin(theta);\n"
                "  var _0005 = Math.cos(theta);\n"
                "  var _0006 = Math.sin(_0003);\n"
                "  return [1, (_0006 * _0004 * a * l * m * omega + p_theta) / (_0002 * m), (- Math.pow(_0006, 2) * _0005 * _0004 * Math.pow(a, 2) * l * m * Math.pow(omega, 2) - _0006 * _0005 * a * omega * p_theta - _0004 * g * _0002 * m) / l];\n"
                "}")
           (->JavaScript eq :parameter-order '[t theta p_theta])))))
