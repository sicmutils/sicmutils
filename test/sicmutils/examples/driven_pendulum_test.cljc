;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sicmutils.env :as e :refer [up + - * /]]
            [sicmutils.examples.driven-pendulum :as driven]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest equations
  (e/with-literal-functions [θ y]
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
