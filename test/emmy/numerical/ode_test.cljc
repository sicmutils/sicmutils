#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.ode-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.generic :as g :refer [- * /]]
            [emmy.numerical.ode :as o]
            [emmy.structure :refer [up]]
            [emmy.value :as v]))

(def ^:private near? (v/within 1e-8))

(deftest simple-odes
  (testing "y' = y"
    ;; we should get exp(x), with suitable initial condition
    (let [result ((o/state-advancer (constantly identity)) (up 1.) 1. {:epsilon 1.e-10})]
      (is ((v/within 1e-8) (Math/exp 1) (nth result 0))))

    (let [f (constantly identity)]
      (doseq [compile? [true false true true]]
        (let [states (atom [])
              result ((o/evolve f)         ;; solve: y' = y
                      (up 1.)                               ;;        y(0) = 1
                      0.1                                   ;; ... with step size 0.1
                      1                                     ;; solve until t = 1
                      {:compile compile?

                       ;; accuracy desired
                       :epsilon 1e-10

                       ;; accumulate results
                       :observe #(swap! states conj [%1 %2])})]
          (is (= 11 (count @states)))
          (is (near? (Math/exp 1) (first result)))))))

  (testing "y'' = -y"
    (let [f (fn [] (fn [[y u]] (up u (- y))))]
      (doseq [compile? [true false true]]
        (let [states (atom [])
              ;; let u = y', then we have the first-order system {y' = u, u' = -y}
              ;; with initial conditions y(0) = 0, y'(0) = 1; we expect y = sin(x).
              result ((o/evolve f)
                      (up 0. 1.)                            ;; y(0) = 0, y'(0) = 1
                      0.1                                   ;; ... with step size 0.1
                      (* 2 Math/PI)                       ;; over [0, 2π]
                      {:compile? compile?
                       :epsilon 1.e-10
                       :observe #(swap! states conj [%1 %2])})]
          (is (= 64 (count @states)))     ;; 0.0 .. 6.2 by .1, plus 2π
          (is (near? 0 (first result)))
          (is (near? 1 (second result)))
          (let [[t [s c]] (nth @states 15)]                  ;; state #15 is t = 1.5
            (is (near? t 1.5))
            (is (near? s (Math/sin 1.5)))
            (is (near? c (Math/cos 1.5))))
          (let [[t [s c]] (nth @states 30)]
            (is (near? t 3.0))
            (is (near? s (Math/sin 3)))
            (is (near? c (Math/cos 3))))))))

  (testing "with parameter"
    (let [f (fn [k] (fn [[y]] (up (* k y))))]
      (doseq [compile? [true false true]]
        (let [result ((o/evolve f 0.2)
                      (up 1)
                      0
                      3
                      {:compile? compile?
                       :epsilon 1e-10})]
          (is (near? (Math/exp 0.6) (first result)))))))

  (testing "with sd-integrator"
    (with-comparator (v/within 1e-5)
      (let [state-derivative (fn [] (fn [[_ y]] [1 y]))
            output (o/integrate-state-derivative
                    state-derivative [] (up 0 1) 1 (/ 1 10))
            expected [[0.0 1.0]
                      [0.1 1.1051709179235594]
                      [0.2 1.2214027531002876]
                      [0.3 1.3498587919571827]
                      [0.4 1.4918246775485524]
                      [0.5 1.648721248240836]
                      [0.6 1.8221187755620267]
                      [0.7 2.0137526801065393]
                      [0.8 2.2255409007561555]
                      [0.9 2.459603091529638]
                      [1.0 2.718281812371165]]]
        (is (ish? expected output)))

      (let [state-derivative (fn [] (fn [[_ y]] [1 (* 2 y)]))
            output (o/integrate-state-derivative
                    state-derivative [] (up 0 1) 1 (/ 1 10))
            expected [[0.0 1.0],
                      [0.1 1.2214027581601699],
                      [0.2 1.4918246976412703],
                      [0.3 1.8221188003905089],
                      [0.4 2.225540928492468],
                      [0.5 2.718281828459045],
                      [0.6 3.3201169227365472],
                      [0.7 4.0551999668446745],
                      [0.8 4.953032424395115],
                      [0.9 6.0496474644129465],
                      [1.0 7.38905609893065]]]
        (is (ish? expected output))))))
