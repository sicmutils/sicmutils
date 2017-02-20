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

(ns sicmutils.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.examples.double-pendulum :as double]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest equations
  (let [state (up 't (up 'θ 'φ) (up 'θdot 'φdot))
        V (double/V 'm1 'm2 'l1 'l2 'g)
        T (double/T 'm1 'm2 'l1 'l2 'g)
        L (double/L 'm1 'm2 'l1 'l2 'g)]
    (is (= '(+ (* -1 g l1 m1 (cos θ))
               (* -1 g l1 m2 (cos θ))
               (* -1 g l2 m2 (cos φ)))
           (simplify (V state))))
    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* 1/2 (expt l1 2) m1 (expt θdot 2))
               (* 1/2 (expt l1 2) m2 (expt θdot 2))
               (* 1/2 (expt l2 2) m2 (expt φdot 2)))
           (simplify (T state))))
    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* 1/2 (expt l1 2) m1 (expt θdot 2))
               (* 1/2 (expt l1 2) m2 (expt θdot 2))
               (* 1/2 (expt l2 2) m2 (expt φdot 2))
               (* g l1 m1 (cos θ))
               (* g l1 m2 (cos θ))
               (* g l2 m2 (cos φ)))
           (simplify (L state))))
    (with-literal-functions [θ φ]
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
             (simplify (((Lagrange-equations
                          (double/L 'm1 'm2 'l1 'l2 'g))
                         (up θ φ))
                        't)))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (do
        (double/evolver {:t 3/60 :dt 1/60 :observe observe})
        (is (= 4 (count @o)))))))

(deftest infix-forms
  (let [eq (simplify
             ((double/state-derivative 'm1 'm2 'l1 'l2 'g)
               (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))]
    (is (= (str "function(t, theta, phi, thetadot, phidot) {\n"
                "  var _0002 = Math.pow(phidot, 2);\n"
                "  var _0003 = Math.sin(phi);\n"
                "  var _0005 = - phi;\n"
                "  var _0007 = Math.sin(theta);\n"
                "  var _0008 = Math.pow(thetadot, 2);\n"
                "  var _000b = _0005 + theta;\n"
                "  var _000e = Math.cos(_000b);\n"
                "  var _000f = Math.sin(_000b);\n"
                "  var _0011 = Math.pow(_000f, 2);\n"
                "  return [1, [thetadot, phidot], [(- l1 * m2 * _0008 * _000f * _000e - l2 * m2 * _0002 * _000f + g * m2 * _000e * _0003 - g * m1 * _0007 - g * m2 * _0007) / (l1 * m2 * _0011 + l1 * m1), (l2 * m2 * _0002 * _000f * _000e + l1 * m1 * _0008 * _000f + l1 * m2 * _0008 * _000f + g * m1 * _0007 * _000e + g * m2 * _0007 * _000e - g * m1 * _0003 - g * m2 * _0003) / (l2 * m2 * _0011 + l2 * m1)]];\n"
                "}")
           (->JavaScript eq :parameter-order '[t theta phi thetadot phidot]))))
  (let [eq (simplify
            ((Hamiltonian->state-derivative
              (Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
             (->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))))]
    (is (= (str "function(theta, psi, p_theta, p_psi) {\n"
                "  var _0004 = Math.pow(m_1, 2);\n"
                "  var _0006 = Math.pow(l_2, 2);\n"
                "  var _0007 = - psi;\n"
                "  var _000a = Math.pow(p_psi, 2);\n"
                "  var _000b = Math.pow(l_1, 2);\n"
                "  var _000c = Math.sin(psi);\n"
                "  var _000d = Math.pow(m_2, 3);\n"
                "  var _000e = Math.pow(l_2, 3);\n"
                "  var _0012 = Math.sin(theta);\n"
                "  var _0013 = Math.pow(p_theta, 2);\n"
                "  var _0014 = Math.pow(m_2, 2);\n"
                "  var _0017 = Math.pow(l_1, 3);\n"
                "  var _0019 = _0007 + theta;\n"
                "  var _001c = _000b * _0006 * _0014;\n"
                "  var _0023 = _000b * _0006 * _0004;\n"
                "  var _002b = Math.cos(_0019);\n"
                "  var _002d = Math.sin(_0019);\n"
                "  var _002e = Math.pow(_002b, 4);\n"
                "  var _0030 = Math.pow(_002b, 2);\n"
                "  var _0033 = Math.pow(_002d, 2);\n"
                "  var _0035 = _000b * _0006 * _0014 * _002e;\n"
                "  var _0036 = -2 * _000b * _0006 * _0014 * _0030;\n"
                "  var _0038 = 2 * _000b * _0006 * m_1 * m_2 * _0033;\n"
                "  var _0039 = _0035 + _0038 + _0036 + _0023 + _001c;\n"
                "  return [1, [(- l_1 * p_psi * _002b + l_2 * p_theta) / (_000b * l_2 * m_2 * _0033 + _000b * l_2 * m_1), (- l_2 * m_2 * p_theta * _002b + l_1 * m_1 * p_psi + l_1 * m_2 * p_psi) / (l_1 * _0006 * _0014 * _0033 + l_1 * _0006 * m_1 * m_2)], [(- g * _0017 * _0006 * m_1 * _0014 * _0012 * _002e - g * _0017 * _0006 * _000d * _0012 * _002e + 2 * g * _0017 * _0006 * _0004 * m_2 * _0012 * _0030 + 4 * g * _0017 * _0006 * m_1 * _0014 * _0012 * _0030 + 2 * g * _0017 * _0006 * _000d * _0012 * _0030 - g * _0017 * _0006 * Math.pow(m_1, 3) * _0012 -3 * g * _0017 * _0006 * _0004 * m_2 * _0012 -3 * g * _0017 * _0006 * m_1 * _0014 * _0012 - g * _0017 * _0006 * _000d * _0012 - l_1 * l_2 * m_2 * p_psi * p_theta * _0030 * _002d + _000b * m_1 * _000a * _002b * _002d + _000b * m_2 * _000a * _002b * _002d + _0006 * m_2 * _0013 * _002b * _002d - l_1 * l_2 * m_1 * p_psi * p_theta * _002d - l_1 * l_2 * m_2 * p_psi * p_theta * _002d) / _0039, (- g * _000b * _000e * _000d * _002e * _000c -2 * g * _000b * _000e * m_1 * _0014 * _0033 * _000c + 2 * g * _000b * _000e * _000d * _0030 * _000c - g * _000b * _000e * _0004 * m_2 * _000c - g * _000b * _000e * _000d * _000c + l_1 * l_2 * m_2 * p_psi * p_theta * _0030 * _002d - _000b * m_1 * _000a * _002b * _002d - _000b * m_2 * _000a * _002b * _002d - _0006 * m_2 * _0013 * _002b * _002d + l_1 * l_2 * m_1 * p_psi * p_theta * _002d + l_1 * l_2 * m_2 * p_psi * p_theta * _002d) / _0039]];\n"
                "}")
           (->JavaScript eq :parameter-order '[theta psi p_theta p_psi])))))
