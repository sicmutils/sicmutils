#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [emmy.env :as e :refer [up down /]]
            [emmy.examples.double-pendulum :as double]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest equations
  (let [state (up 't (up 'θ 'φ) (up 'θdot 'φdot))
        V (double/V 'm1 'm2 'l1 'l2 'g)
        T (double/T 'm1 'm2 'l1 'l2 'g)
        L (double/L 'm1 'm2 'l1 'l2 'g)]
    (is (= '(+ (* -1 g l1 m1 (cos θ))
               (* -1 g l1 m2 (cos θ))
               (* -1 g l2 m2 (cos φ)))
           (v/freeze
            (e/simplify (V state)))))

    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* (/ 1 2) (expt l1 2) m1 (expt θdot 2))
               (* (/ 1 2) (expt l1 2) m2 (expt θdot 2))
               (* (/ 1 2) (expt l2 2) m2 (expt φdot 2)))
           (v/freeze
            (e/simplify (T state)))))
    (is (= '(+ (* l1 l2 m2 θdot φdot (cos (+ θ (* -1 φ))))
               (* (/ 1 2) (expt l1 2) m1 (expt θdot 2))
               (* (/ 1 2) (expt l1 2) m2 (expt θdot 2))
               (* (/ 1 2) (expt l2 2) m2 (expt φdot 2))
               (* g l1 m1 (cos θ))
               (* g l1 m2 (cos θ))
               (* g l2 m2 (cos φ)))
           (v/freeze
            (e/simplify (L state)))))

    (e/with-literal-functions [θ φ]
      (is (= '(down
               (+ (* l1 l2 m2 (sin (+ (θ t) (* -1 (φ t)))) (expt ((D φ) t) 2))
                  (* l1 l2 m2 (cos (+ (θ t) (* -1 (φ t)))) (((expt D 2) φ) t))
                  (* g l1 m1 (sin (θ t)))
                  (* g l1 m2 (sin (θ t)))
                  (* (expt l1 2) m1 (((expt D 2) θ) t))
                  (* (expt l1 2) m2 (((expt D 2) θ) t)))
               (+ (* -1 l1 l2 m2 (sin (+ (θ t) (* -1 (φ t)))) (expt ((D θ) t) 2))
                  (* l1 l2 m2 (cos (+ (θ t) (* -1 (φ t)))) (((expt D 2) θ) t))
                  (* g l2 m2 (sin (φ t)))
                  (* (expt l2 2) m2 (((expt D 2) φ) t))))
             (v/freeze
              (e/simplify (((e/Lagrange-equations
                             (double/L 'm1 'm2 'l1 'l2 'g))
                            (up θ φ))
                           't))))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (double/evolver {:t (/ 3 60) :dt (/ 1 60) :observe observe})
      (is (= 4 (count @o))))))

(deftest infix-forms
  (let [eq (e/simplify
            ((double/state-derivative 'm1 'm2 'l1 'l2 'g)
             (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))]
    (is (= (str "function(t, theta, phi, thetadot, phidot) {\n"
                "  var _0001 = - theta;\n"
                "  var _0005 = Math.pow(phidot, 2);\n"
                "  var _0006 = Math.pow(thetadot, 2);\n"
                "  var _0008 = Math.sin(phi);\n"
                "  var _0009 = Math.sin(theta);\n"
                "  var _000a = phi + _0001;\n"
                "  var _000e = Math.cos(_000a);\n"
                "  var _0010 = Math.sin(_000a);\n"
                "  var _0011 = Math.pow(_0010, 2);\n"
                "  return [1, [thetadot, phidot], [(l1 * m2 * _0006 * _000e * _0010 + l2 * m2 * _0005 * _0010 + g * m2 * _0008 * _000e - g * m1 * _0009 - g * m2 * _0009) / (l1 * m2 * _0011 + l1 * m1), (- l2 * m2 * _0005 * _000e * _0010 - l1 * m1 * _0006 * _0010 - l1 * m2 * _0006 * _0010 + g * m1 * _0009 * _000e + g * m2 * _0009 * _000e - g * m1 * _0008 - g * m2 * _0008) / (l2 * m2 * _0011 + l2 * m1)]];\n"
                "}")
           (e/->JavaScript eq
                           :parameter-order '[t theta phi thetadot phidot]
                           :deterministic? true))))

  (let [eq (e/simplify
            ((double/state-derivative 1 1 1 1 'g)
             (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))]
    (is (= (str "function(g, phi, phidot, theta, thetadot) {\n"
                "  var _0001 = - theta;\n"
                "  var _0006 = Math.pow(phidot, 2);\n"
                "  var _0007 = Math.pow(thetadot, 2);\n"
                "  var _0009 = Math.sin(phi);\n"
                "  var _000a = Math.sin(theta);\n"
                "  var _000c = phi + _0001;\n"
                "  var _0011 = Math.cos(_000c);\n"
                "  var _0013 = Math.sin(_000c);\n"
                "  var _0015 = Math.pow(_0011, 2);\n"
                "  var _0016 = _0015 + -2;\n"
                "  return [1, [thetadot, phidot], [(- _0007 * _0011 * _0013 - g * _0009 * _0011 - _0006 * _0013 + 2 * g * _000a) / _0016, (_0006 * _0011 * _0013 -2 * g * _000a * _0011 + 2 * _0007 * _0013 + 2 * g * _0009) / _0016]];\n"
                "}")
           (e/->JavaScript eq :deterministic? true))))

  (let [eq (e/simplify
            ((e/Hamiltonian->state-derivative
              (e/Lagrangian->Hamiltonian
               (double/L 'm_1 'm_2 'l_1 'l_2 'g)))
             (e/->H-state 't (up 'theta 'psi) (down 'p_theta 'p_psi))))]
    (is (= "up(1, up((- l₁ p_ψ cos(ψ - θ) + l₂ p_θ) / (l₁² l₂ m₂ sin²(ψ - θ) + l₁² l₂ m₁), (- l₂ m₂ p_θ cos(ψ - θ) + l₁ m₁ p_ψ + l₁ m₂ p_ψ) / (l₁ l₂² m₂² sin²(ψ - θ) + l₁ l₂² m₁ m₂)), down((- g l₁³ l₂² m₁ m₂² sin(θ) (cos(ψ - θ))⁴ - g l₁³ l₂² m₂³ sin(θ) (cos(ψ - θ))⁴ + 2 g l₁³ l₂² m₁² m₂ sin(θ) cos²(ψ - θ) + 4 g l₁³ l₂² m₁ m₂² sin(θ) cos²(ψ - θ) + 2 g l₁³ l₂² m₂³ sin(θ) cos²(ψ - θ) - g l₁³ l₂² m₁³ sin(θ) -3 g l₁³ l₂² m₁² m₂ sin(θ) -3 g l₁³ l₂² m₁ m₂² sin(θ) - g l₁³ l₂² m₂³ sin(θ) + l₁ l₂ m₂ p_ψ p_θ cos²(ψ - θ) sin(ψ - θ) - l₁² m₁ p_ψ² cos(ψ - θ) sin(ψ - θ) - l₁² m₂ p_ψ² cos(ψ - θ) sin(ψ - θ) - l₂² m₂ p_θ² cos(ψ - θ) sin(ψ - θ) + l₁ l₂ m₁ p_ψ p_θ sin(ψ - θ) + l₁ l₂ m₂ p_ψ p_θ sin(ψ - θ)) / (l₁² l₂² m₂² (cos(ψ - θ))⁴ + 2 l₁² l₂² m₁ m₂ sin²(ψ - θ) -2 l₁² l₂² m₂² cos²(ψ - θ) + l₁² l₂² m₁² + l₁² l₂² m₂²), (- g l₁² l₂³ m₂³ (cos(ψ - θ))⁴ sin(ψ) -2 g l₁² l₂³ m₁ m₂² sin²(ψ - θ) sin(ψ) + 2 g l₁² l₂³ m₂³ cos²(ψ - θ) sin(ψ) - g l₁² l₂³ m₁² m₂ sin(ψ) - g l₁² l₂³ m₂³ sin(ψ) - l₁ l₂ m₂ p_ψ p_θ cos²(ψ - θ) sin(ψ - θ) + l₁² m₁ p_ψ² cos(ψ - θ) sin(ψ - θ) + l₁² m₂ p_ψ² cos(ψ - θ) sin(ψ - θ) + l₂² m₂ p_θ² cos(ψ - θ) sin(ψ - θ) - l₁ l₂ m₁ p_ψ p_θ sin(ψ - θ) - l₁ l₂ m₂ p_ψ p_θ sin(ψ - θ)) / (l₁² l₂² m₂² (cos(ψ - θ))⁴ + 2 l₁² l₂² m₁ m₂ sin²(ψ - θ) -2 l₁² l₂² m₂² cos²(ψ - θ) + l₁² l₂² m₁² + l₁² l₂² m₂²)))"
           (e/->infix eq)))

    #?(:clj
       ;; even with the deterministic flag, this is not quite reproducing in
       ;; ClojureScript.
       (is (= (str "function(theta, psi, p_theta, p_psi) {\n"
                   "  var _0004 = - theta;\n"
                   "  var _000d = Math.pow(l_1, 2);\n"
                   "  var _000e = Math.pow(l_1, 3);\n"
                   "  var _000f = Math.pow(l_2, 2);\n"
                   "  var _0010 = Math.pow(l_2, 3);\n"
                   "  var _0011 = Math.pow(m_1, 2);\n"
                   "  var _0012 = Math.pow(m_2, 2);\n"
                   "  var _0013 = Math.pow(m_2, 3);\n"
                   "  var _0014 = Math.pow(p_psi, 2);\n"
                   "  var _0015 = Math.pow(p_theta, 2);\n"
                   "  var _0017 = Math.sin(psi);\n"
                   "  var _0018 = Math.sin(theta);\n"
                   "  var _001b = _000d * _000f * _0011;\n"
                   "  var _001d = _000d * _000f * _0012;\n"
                   "  var _001f = psi + _0004;\n"
                   "  var _0029 = Math.cos(_001f);\n"
                   "  var _002d = Math.sin(_001f);\n"
                   "  var _0032 = Math.pow(_0029, 2);\n"
                   "  var _0033 = Math.pow(_0029, 4);\n"
                   "  var _0034 = Math.pow(_002d, 2);\n"
                   "  var _0035 = -2 * _000d * _000f * _0012 * _0032;\n"
                   "  var _0036 = 2 * _000d * _000f * m_1 * m_2 * _0034;\n"
                   "  var _0037 = _000d * _000f * _0012 * _0033;\n"
                   "  var _0039 = _0037 + _0036 + _0035 + _001b + _001d;\n"
                   "  return [1, [(- l_1 * p_psi * _0029 + l_2 * p_theta) / (_000d * l_2 * m_2 * _0034 + _000d * l_2 * m_1), (- l_2 * m_2 * p_theta * _0029 + l_1 * m_1 * p_psi + l_1 * m_2 * p_psi) / (l_1 * _000f * _0012 * _0034 + l_1 * _000f * m_1 * m_2)], [(- g * _000e * _000f * m_1 * _0012 * _0018 * _0033 - g * _000e * _000f * _0013 * _0018 * _0033 + 2 * g * _000e * _000f * _0011 * m_2 * _0018 * _0032 + 4 * g * _000e * _000f * m_1 * _0012 * _0018 * _0032 + 2 * g * _000e * _000f * _0013 * _0018 * _0032 - g * _000e * _000f * Math.pow(m_1, 3) * _0018 -3 * g * _000e * _000f * _0011 * m_2 * _0018 -3 * g * _000e * _000f * m_1 * _0012 * _0018 - g * _000e * _000f * _0013 * _0018 + l_1 * l_2 * m_2 * p_psi * p_theta * _0032 * _002d - _000d * m_1 * _0014 * _0029 * _002d - _000d * m_2 * _0014 * _0029 * _002d - _000f * m_2 * _0015 * _0029 * _002d + l_1 * l_2 * m_1 * p_psi * p_theta * _002d + l_1 * l_2 * m_2 * p_psi * p_theta * _002d) / _0039, (- g * _000d * _0010 * _0013 * _0033 * _0017 -2 * g * _000d * _0010 * m_1 * _0012 * _0034 * _0017 + 2 * g * _000d * _0010 * _0013 * _0032 * _0017 - g * _000d * _0010 * _0011 * m_2 * _0017 - g * _000d * _0010 * _0013 * _0017 - l_1 * l_2 * m_2 * p_psi * p_theta * _0032 * _002d + _000d * m_1 * _0014 * _0029 * _002d + _000d * m_2 * _0014 * _0029 * _002d + _000f * m_2 * _0015 * _0029 * _002d - l_1 * l_2 * m_1 * p_psi * p_theta * _002d - l_1 * l_2 * m_2 * p_psi * p_theta * _002d) / _0039]];\n"
                   "}")
              (e/->JavaScript eq
                              :parameter-order '[theta psi p_theta p_psi]
                              :deterministic? true))))))
