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

(ns sicmutils.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.examples.double-pendulum :as double]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.env :as e :refer [up down expt cos sin + - * /]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest equations
  (let [state (up 't (up 'θ 'φ) (up 'θdot 'φdot))
        V (double/V 'm1 'm2 'l1 'l2 'g)
        T (double/T 'm1 'm2 'l1 'l2 'g)
        L (double/L 'm1 'm2 'l1 'l2 'g)]
    (is (= '(+ (* -1 g l1 m1 (cos θ))
               (* -1 g l1 m2 (cos θ))
               (* -1 g l2 m2 (cos φ)))
           (e/simplify (V state))))
    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* #?(:clj 1/2 :cljs 0.5) (expt l1 2) m1 (expt θdot 2))
               (* #?(:clj 1/2 :cljs 0.5) (expt l1 2) m2 (expt θdot 2))
               (* #?(:clj 1/2 :cljs 0.5) (expt l2 2) m2 (expt φdot 2)))
           (e/simplify (T state))))
    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* #?(:clj 1/2 :cljs 0.5) (expt l1 2) m1 (expt θdot 2))
               (* #?(:clj 1/2 :cljs 0.5) (expt l1 2) m2 (expt θdot 2))
               (* #?(:clj 1/2 :cljs 0.5) (expt l2 2) m2 (expt φdot 2))
               (* g l1 m1 (cos θ))
               (* g l1 m2 (cos θ))
               (* g l2 m2 (cos φ)))
           (e/simplify (L state))))
    (e/with-literal-functions [θ φ]
      (is (= '(down (+ (* l1 l2 m2 (expt ((D φ) t) 2) (sin (+ (θ t) (* -1 (φ t)))))
                       (* l1 l2 m2 (((expt D 2) φ) t) (cos (+ (θ t) (* -1 (φ t)))))
                       (* g l1 m1 (sin (θ t)))
                       (* g l1 m2 (sin (θ t)))
                       (* (expt l1 2) m1 (((expt D 2) θ) t))
                       (* (expt l1 2) m2 (((expt D 2) θ) t)))
                    (+ (* -1 l1 l2 m2 (sin (+ (θ t) (* -1 (φ t)))) (expt ((D θ) t) 2))
                       (* l1 l2 m2 (((expt D 2) θ) t) (cos (+ (θ t) (* -1 (φ t)))))
                       (* g l2 m2 (sin (φ t)))
                       (* (expt l2 2) m2 (((expt D 2) φ) t))))
             (e/simplify (((e/Lagrange-equations
                            (double/L 'm1 'm2 'l1 'l2 'g))
                           (up θ φ))
                          't)))))
    ;; Integrator doesn't work yet in cljs.
    #?(:clj
       (let [o (atom [])
             observe (fn [t q] (swap! o conj [t q]))]
         (do
           (double/evolver {:t (/ 3 60) :dt (/ 1 60) :observe observe})
           (is (= 4 (count @o))))))))

(deftest infix-forms
  (let [eq (e/simplify
            ((double/state-derivative 'm1 'm2 'l1 'l2 'g)
             (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))]
    (is (= (str "function(t, theta, phi, thetadot, phidot) {\n"
                "  var _0001 = - phi;\n"
                "  var _0005 = Math.pow(phidot, 2);\n"
                "  var _0006 = Math.pow(thetadot, 2);\n"
                "  var _0008 = Math.sin(phi);\n"
                "  var _0009 = Math.sin(theta);\n"
                "  var _000a = _0001 + theta;\n"
                "  var _000e = Math.cos(_000a);\n"
                "  var _0010 = Math.sin(_000a);\n"
                "  var _0011 = Math.pow(_0010, 2);\n"
                "  return [1, [thetadot, phidot], [(- l1 * m2 * _0006 * _0010 * _000e - l2 * m2 * _0005 * _0010 + g * m2 * _000e * _0008 - g * m1 * _0009 - g * m2 * _0009) / (l1 * m2 * _0011 + l1 * m1), (l2 * m2 * _0005 * _0010 * _000e + l1 * m1 * _0006 * _0010 + l1 * m2 * _0006 * _0010 + g * m1 * _0009 * _000e + g * m2 * _0009 * _000e - g * m1 * _0008 - g * m2 * _0008) / (l2 * m2 * _0011 + l2 * m1)]];\n"
                "}")
           (e/->JavaScript eq
                           :parameter-order '[t theta phi thetadot phidot]
                           :deterministic? true))))

  (let [eq (e/simplify
            ((double/state-derivative 1 1 1 1 'g)
             (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))]
    (is (= (str "function(g, phi, phidot, theta, thetadot) {\n"
                "  var _0001 = - phi;\n"
                "  var _0006 = Math.pow(phidot, 2);\n"
                "  var _0007 = Math.pow(thetadot, 2);\n"
                "  var _0009 = Math.sin(phi);\n"
                "  var _000a = Math.sin(theta);\n"
                "  var _000c = _0001 + theta;\n"
                "  var _0011 = Math.cos(_000c);\n"
                "  var _0013 = Math.sin(_000c);\n"
                "  var _0015 = Math.pow(_0011, 2);\n"
                "  var _0016 = _0015 + -2;\n"
                "  return [1, [thetadot, phidot], [(_0007 * _0013 * _0011 - g * _0011 * _0009 + _0006 * _0013 + 2 * g * _000a) / _0016, (- _0006 * _0013 * _0011 -2 * g * _000a * _0011 -2 * _0007 * _0013 + 2 * g * _0009) / _0016]];\n"
                "}")
           (e/->JavaScript eq :deterministic? true))))

  (let [eq (e/simplify
            ((e/Hamiltonian->state-derivative
              (e/Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
             (e/->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))))]
    (is (= "up(1, up((- l₁ p_psi cos(- psi + theta) + l₂ p_theta) / (l₁² l₂ m₂ sin²(- psi + theta) + l₁² l₂ m₁), (- l₂ m₂ p_theta cos(- psi + theta) + l₁ m₁ p_psi + l₁ m₂ p_psi) / (l₁ l₂² m₂² sin²(- psi + theta) + l₁ l₂² m₁ m₂)), down((- g l₁³ l₂² m₁ m₂² sin(theta) (cos(- psi + theta))⁴ - g l₁³ l₂² m₂³ sin(theta) (cos(- psi + theta))⁴ + 2 g l₁³ l₂² m₁² m₂ sin(theta) cos²(- psi + theta) + 4 g l₁³ l₂² m₁ m₂² sin(theta) cos²(- psi + theta) + 2 g l₁³ l₂² m₂³ sin(theta) cos²(- psi + theta) - g l₁³ l₂² m₁³ sin(theta) -3 g l₁³ l₂² m₁² m₂ sin(theta) -3 g l₁³ l₂² m₁ m₂² sin(theta) - g l₁³ l₂² m₂³ sin(theta) - l₁ l₂ m₂ p_psi p_theta cos²(- psi + theta) sin(- psi + theta) + l₁² m₁ p_psi² cos(- psi + theta) sin(- psi + theta) + l₁² m₂ p_psi² cos(- psi + theta) sin(- psi + theta) + l₂² m₂ p_theta² cos(- psi + theta) sin(- psi + theta) - l₁ l₂ m₁ p_psi p_theta sin(- psi + theta) - l₁ l₂ m₂ p_psi p_theta sin(- psi + theta)) / (l₁² l₂² m₂² (cos(- psi + theta))⁴ + 2 l₁² l₂² m₁ m₂ sin²(- psi + theta) -2 l₁² l₂² m₂² cos²(- psi + theta) + l₁² l₂² m₁² + l₁² l₂² m₂²), (- g l₁² l₂³ m₂³ (cos(- psi + theta))⁴ sin(psi) -2 g l₁² l₂³ m₁ m₂² sin²(- psi + theta) sin(psi) + 2 g l₁² l₂³ m₂³ cos²(- psi + theta) sin(psi) - g l₁² l₂³ m₁² m₂ sin(psi) - g l₁² l₂³ m₂³ sin(psi) + l₁ l₂ m₂ p_psi p_theta cos²(- psi + theta) sin(- psi + theta) - l₁² m₁ p_psi² cos(- psi + theta) sin(- psi + theta) - l₁² m₂ p_psi² cos(- psi + theta) sin(- psi + theta) - l₂² m₂ p_theta² cos(- psi + theta) sin(- psi + theta) + l₁ l₂ m₁ p_psi p_theta sin(- psi + theta) + l₁ l₂ m₂ p_psi p_theta sin(- psi + theta)) / (l₁² l₂² m₂² (cos(- psi + theta))⁴ + 2 l₁² l₂² m₁ m₂ sin²(- psi + theta) -2 l₁² l₂² m₂² cos²(- psi + theta) + l₁² l₂² m₁² + l₁² l₂² m₂²)))"
           (e/->infix eq)))))
