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
    (is (= '(+ (* -1 (cos θ) g l1 m1)
               (* -1 (cos θ) g l1 m2)
               (* -1 (cos φ) g l2 m2))
           (simplify (V state))))
    (is (= '(+
             (* (cos (+ θ (* -1 φ))) l1 l2 m2 θdot φdot)
             (* 1/2 (expt l1 2) m1 (expt θdot 2))
             (* 1/2 (expt l1 2) m2 (expt θdot 2))
             (* 1/2 (expt l2 2) m2 (expt φdot 2)))
           (simplify (T state))))
    (is (= '(+
             (* (cos (+ θ (* -1 φ))) l1 l2 m2 θdot φdot)
             (* 1/2 (expt l1 2) m1 (expt θdot 2))
             (* 1/2 (expt l1 2) m2 (expt θdot 2))
             (* 1/2 (expt l2 2) m2 (expt φdot 2))
             (* (cos θ) g l1 m1)
             (* (cos θ) g l1 m2)
             (* (cos φ) g l2 m2))
           (simplify (L state))))
    (with-literal-functions [θ φ]
      (is (= '(down (+ (* (expt ((D φ) t) 2) (sin (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) φ) t) (cos (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) θ) t) (expt l1 2) m1)
                       (* (((expt D 2) θ) t) (expt l1 2) m2)
                       (* (sin (θ t)) g l1 m1)
                       (* (sin (θ t)) g l1 m2))
                    (+ (* -1 (sin (+ (θ t) (* -1 (φ t)))) (expt ((D θ) t) 2) l1 l2 m2)
                       (* (((expt D 2) θ) t) (cos (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) φ) t) (expt l2 2) m2)
                       (* (sin (φ t)) g l2 m2)))
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
                "  var _0001 = Math.cos(- phi + theta);\n"
                "  var _0002 = Math.pow(phidot, 2);\n"
                "  var _0003 = Math.sin(phi);\n"
                "  var _0004 = Math.sin(- phi + theta);\n"
                "  var _0005 = - phi;\n"
                "  var _0006 = Math.pow(Math.sin(- phi + theta), 2);\n"
                "  var _0007 = Math.sin(theta);\n"
                "  var _0008 = Math.pow(thetadot, 2);\n"
                "  var _0009 = - phi + theta;\n"
                "  var _000a = Math.cos(_0005 + theta);\n"
                "  var _000b = _0005 + theta;\n"
                "  var _000c = Math.sin(_0005 + theta);\n"
                "  var _000d = Math.pow(Math.sin(_0005 + theta), 2);\n"
                "  var _000e = Math.pow(Math.sin(_000b), 2);\n"
                "  var _000f = Math.sin(_000b);\n"
                "  var _0010 = Math.cos(_000b);\n"
                "  var _0011 = Math.pow(_000f, 2);\n"
                "  return [1, [thetadot, phidot], [(- _000f * _0010 * l1 * m2 * _0008 - _000f * l2 * m2 * _0002 + _0010 * _0003 * g * m2 - _0007 * g * m1 - _0007 * g * m2) / (_0011 * l1 * m2 + l1 * m1), (_000f * _0010 * l2 * m2 * _0002 + _000f * l1 * m1 * _0008 + _000f * l1 * m2 * _0008 + _0007 * _0010 * g * m1 + _0007 * _0010 * g * m2 - _0003 * g * m1 - _0003 * g * m2) / (_0011 * l2 * m2 + l2 * m1)]];\n"
                "}")
           (->JavaScript eq :parameter-order '[t theta phi thetadot phidot]))))
  (let [eq (simplify
            ((Hamiltonian->state-derivative
              (Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
             (->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))))]
    (is (= (str "function(theta, psi, p_theta, p_psi) {\n"
                "  var _0001 = Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_2, 2);\n"
                "  var _0002 = Math.pow(Math.cos(- psi + theta), 4) * Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_2, 2) -2 * Math.pow(Math.cos(- psi + theta), 2) * Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_2, 2) + 2 * Math.pow(Math.sin(- psi + theta), 2) * Math.pow(l_1, 2) * Math.pow(l_2, 2) * m_1 * m_2 + Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_1, 2) + Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_2, 2);\n"
                "  var _0003 = -2 * Math.pow(Math.cos(- psi + theta), 2) * Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_2, 2);\n"
                "  var _0004 = Math.pow(m_1, 2);\n"
                "  var _0005 = Math.pow(l_2, 2);\n"
                "  var _0006 = 2 * Math.pow(Math.sin(- psi + theta), 2) * Math.pow(l_1, 2) * Math.pow(l_2, 2) * m_1 * m_2;\n"
                "  var _0007 = - psi;\n"
                "  var _0008 = Math.cos(- psi + theta);\n"
                "  var _0009 = Math.sin(- psi + theta);\n"
                "  var _000a = Math.pow(p_psi, 2);\n"
                "  var _000b = Math.pow(l_1, 2);\n"
                "  var _000c = Math.sin(psi);\n"
                "  var _000d = Math.pow(m_2, 3);\n"
                "  var _000e = Math.pow(l_2, 3);\n"
                "  var _000f = - psi + theta;\n"
                "  var _0010 = Math.pow(Math.sin(- psi + theta), 2);\n"
                "  var _0011 = Math.sin(theta);\n"
                "  var _0012 = Math.pow(p_theta, 2);\n"
                "  var _0013 = Math.pow(m_2, 2);\n"
                "  var _0014 = Math.pow(Math.cos(- psi + theta), 4) * Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_2, 2);\n"
                "  var _0015 = Math.pow(l_1, 2) * Math.pow(l_2, 2) * Math.pow(m_1, 2);\n"
                "  var _0016 = Math.pow(Math.cos(- psi + theta), 2);\n"
                "  var _0017 = Math.pow(l_1, 3);\n"
                "  var _0018 = Math.pow(Math.cos(- psi + theta), 4);\n"
                "  var _0019 = -2 * Math.pow(Math.cos(_0007 + theta), 2) * _000b * _0005 * _0013;\n"
                "  var _001a = 2 * Math.pow(Math.sin(_0007 + theta), 2) * _000b * _0005 * m_1 * m_2;\n"
                "  var _001b = _000b * _0005 * _0013;\n"
                "  var _001c = Math.sin(_0007 + theta);\n"
                "  var _001d = Math.pow(Math.cos(_0007 + theta), 4);\n"
                "  var _001e = Math.cos(_0007 + theta);\n"
                "  var _001f = _0007 + theta;\n"
                "  var _0020 = Math.pow(Math.cos(_0007 + theta), 2);\n"
                "  var _0021 = Math.pow(Math.sin(_0007 + theta), 2);\n"
                "  var _0022 = _000b * _0005 * _0004;\n"
                "  var _0023 = Math.pow(Math.cos(_0007 + theta), 4) * _000b * _0005 * _0013;\n"
                "  var _0024 = Math.pow(Math.cos(_0007 + theta), 4) * _000b * _0005 * _0013 -2 * Math.pow(Math.cos(_0007 + theta), 2) * _000b * _0005 * _0013 + 2 * Math.pow(Math.sin(_0007 + theta), 2) * _000b * _0005 * m_1 * m_2 + _000b * _0005 * _0004 + _000b * _0005 * _0013;\n"
                "  var _0025 = Math.pow(Math.cos(_001f), 2);\n"
                "  var _0026 = Math.sin(_001f);\n"
                "  var _0027 = Math.pow(Math.cos(_001f), 4);\n"
                "  var _0028 = Math.cos(_001f);\n"
                "  var _0029 = Math.pow(Math.cos(_001f), 4) * _000b * _0005 * _0013;\n"
                "  var _002a = 2 * Math.pow(Math.sin(_001f), 2) * _000b * _0005 * m_1 * m_2;\n"
                "  var _002b = Math.pow(Math.cos(_001f), 4) * _000b * _0005 * _0013 -2 * Math.pow(Math.cos(_001f), 2) * _000b * _0005 * _0013 + 2 * Math.pow(Math.sin(_001f), 2) * _000b * _0005 * m_1 * m_2 + _0022 + _001b;\n"
                "  var _002c = Math.pow(Math.sin(_001f), 2);\n"
                "  var _002d = -2 * Math.pow(Math.cos(_001f), 2) * _000b * _0005 * _0013;\n"
                "  var _002e = Math.pow(_0028, 4);\n"
                "  var _002f = 2 * Math.pow(_0026, 2) * _000b * _0005 * m_1 * m_2;\n"
                "  var _0030 = Math.pow(_0028, 4) * _000b * _0005 * _0013 -2 * Math.pow(_0028, 2) * _000b * _0005 * _0013 + 2 * Math.pow(_0026, 2) * _000b * _0005 * m_1 * m_2 + _0022 + _001b;\n"
                "  var _0031 = Math.pow(_0026, 2);\n"
                "  var _0032 = -2 * Math.pow(_0028, 2) * _000b * _0005 * _0013;\n"
                "  var _0033 = Math.pow(_0028, 2);\n"
                "  var _0034 = Math.pow(_0028, 4) * _000b * _0005 * _0013;\n"
                "  var _0035 = _002e * _000b * _0005 * _0013;\n"
                "  var _0036 = -2 * _0033 * _000b * _0005 * _0013;\n"
                "  var _0037 = 2 * _0031 * _000b * _0005 * m_1 * m_2;\n"
                "  var _0038 = _002e * _000b * _0005 * _0013 -2 * _0033 * _000b * _0005 * _0013 + 2 * _0031 * _000b * _0005 * m_1 * m_2 + _0022 + _001b;\n"
                "  var _0039 = _0035 + _0036 + _0037 + _0022 + _001b;\n"
                "  return [1, [(- _0028 * l_1 * p_psi + l_2 * p_theta) / (_0031 * _000b * l_2 * m_2 + _000b * l_2 * m_1), (- _0028 * l_2 * m_2 * p_theta + l_1 * m_1 * p_psi + l_1 * m_2 * p_psi) / (_0031 * l_1 * _0005 * _0013 + l_1 * _0005 * m_1 * m_2)], [(- _0011 * _002e * g * _0017 * _0005 * m_1 * _0013 - _0011 * _002e * g * _0017 * _0005 * _000d + 2 * _0011 * _0033 * g * _0017 * _0005 * _0004 * m_2 + 4 * _0011 * _0033 * g * _0017 * _0005 * m_1 * _0013 + 2 * _0011 * _0033 * g * _0017 * _0005 * _000d - _0011 * g * _0017 * _0005 * Math.pow(m_1, 3) -3 * _0011 * g * _0017 * _0005 * _0004 * m_2 -3 * _0011 * g * _0017 * _0005 * m_1 * _0013 - _0011 * g * _0017 * _0005 * _000d - _0033 * _0026 * l_1 * l_2 * m_2 * p_psi * p_theta + _0028 * _0026 * _000b * m_1 * _000a + _0028 * _0026 * _000b * m_2 * _000a + _0028 * _0026 * _0005 * m_2 * _0012 - _0026 * l_1 * l_2 * m_1 * p_psi * p_theta - _0026 * l_1 * l_2 * m_2 * p_psi * p_theta) / _0039, (- _002e * _000c * g * _000b * _000e * _000d + 2 * _0033 * _000c * g * _000b * _000e * _000d -2 * _0031 * _000c * g * _000b * _000e * m_1 * _0013 - _000c * g * _000b * _000e * _0004 * m_2 - _000c * g * _000b * _000e * _000d + _0033 * _0026 * l_1 * l_2 * m_2 * p_psi * p_theta - _0028 * _0026 * _000b * m_1 * _000a - _0028 * _0026 * _000b * m_2 * _000a - _0028 * _0026 * _0005 * m_2 * _0012 + _0026 * l_1 * l_2 * m_1 * p_psi * p_theta + _0026 * l_1 * l_2 * m_2 * p_psi * p_theta) / _0039]];\n"
                "}")
           (->JavaScript eq :parameter-order '[theta psi p_theta p_psi])))))
