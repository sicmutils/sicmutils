#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sicm.ch2-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [emmy.env :as e :refer [- / zero? ref partial simplify
                                    compose up]]
            [emmy.mechanics.rigid :as r]
            [emmy.mechanics.rotation :refer [Euler->M]]
            [emmy.polynomial.gcd :as pg]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.util :as u]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def ^:private Euler-state
  (up 't
      (up 'θ 'φ 'ψ)
      (up 'θdot 'φdot 'ψdot)))

(deftest section-2-7
  (e/with-literal-functions [θ φ ψ]
    (let [q (up θ φ ψ)
          M-on-path (compose Euler->M q)]
      (is (= '(matrix-by-rows
               (up (+ (* -1 (sin (φ t)) (cos (θ t)) (sin (ψ t)))
                      (* (cos (φ t)) (cos (ψ t))))
                   (+ (* -1 (sin (φ t)) (cos (θ t)) (cos (ψ t)))
                      (* -1 (sin (ψ t)) (cos (φ t))))
                   (* (sin (φ t)) (sin (θ t))))
               (up (+ (* (cos (θ t)) (sin (ψ t)) (cos (φ t)))
                      (* (sin (φ t)) (cos (ψ t))))
                   (+ (* (cos (θ t)) (cos (φ t)) (cos (ψ t)))
                      (* -1 (sin (φ t)) (sin (ψ t))))
                   (* -1 (cos (φ t)) (sin (θ t))))
               (up (* (sin (ψ t)) (sin (θ t)))
                   (* (cos (ψ t)) (sin (θ t)))
                   (cos (θ t))))
             (e/freeze
              (simplify (M-on-path 't)))))

      (is (= '(column-matrix
               (+ (* (sin (ψ t)) (sin (θ t)) ((D φ) t)) (* (cos (ψ t)) ((D θ) t)))
               (+ (* (cos (ψ t)) (sin (θ t)) ((D φ) t)) (* -1 (sin (ψ t)) ((D θ) t)))
               (+ (* (cos (θ t)) ((D φ) t)) ((D ψ) t)))
             (e/freeze
              (simplify
               (((r/M-of-q->omega-body-of-t Euler->M) q) 't)))))

      (is (= '(column-matrix
               (+ (* φdot (sin θ) (sin ψ)) (* θdot (cos ψ)))
               (+ (* φdot (cos ψ) (sin θ)) (* -1 θdot (sin ψ)))
               (+ (* φdot (cos θ)) ψdot))
             (e/freeze
              (simplify
               ((r/M->omega-body Euler->M) Euler-state))))))))

(deftest section-2-9
  (is (v/= '(+ (* A φdot (expt (sin ψ) 2) (expt (sin θ) 2))
               (* B φdot (expt (sin θ) 2) (expt (cos ψ) 2))
               (* A θdot (sin ψ) (sin θ) (cos ψ))
               (* -1 B θdot (sin ψ) (sin θ) (cos ψ))
               (* C φdot (expt (cos θ) 2))
               (* C ψdot (cos θ)))
           (simplify
            (ref (((partial 2) (r/T-rigid-body 'A 'B 'C)) Euler-state) 1))))

  (is (zero?
       (simplify
        (- (ref ((r/Euler-state->L-space 'A 'B 'C) Euler-state) 2)
           (ref (((partial 2) (r/T-rigid-body 'A 'B 'C)) Euler-state) 1)))))

  (is (v/= '(* A B C (expt (sin θ) 2))
           (simplify
            (e/determinant
             (((e/square (partial 2)) (r/T-rigid-body 'A 'B 'C)) Euler-state))))))

(deftest ^:long section-2-9b
  (let [relative-error (fn [value reference-value]
                         (when (zero? reference-value)
                           (u/illegal "zero reference value"))
                         (/ (- value reference-value) reference-value))
        points (atom [])
        monitor-errors (fn [A B C L0 E0]
                         (fn [t state]
                           (let [L ((r/Euler-state->L-space A B C) state)
                                 E ((r/T-rigid-body A B C) state)]
                             (swap! points conj
                                    [t
                                     (relative-error (ref L 0) (ref L0 0))
                                     (relative-error (ref L 1) (ref L0 1))
                                     (relative-error (ref L 2) (ref L0 2))
                                     (relative-error E E0)]))))
        A 1. B (Math/sqrt 2.) C 2. ;; moments of inertia
        state0 (up 0. (up 1. 0. 0.) (up 0.1 0.1 0.1)) ;; initial state
        L0 ((r/Euler-state->L-space A B C) state0)
        E0 ((r/T-rigid-body A B C) state0)]
    (binding [pg/*poly-gcd-time-limit* #?(:clj  [1 :seconds]
                                          :cljs [4 :seconds])]
      ((e/evolve r/rigid-sysder A B C)
       state0
       0.1
       10.0
       {:compile? true
        :epsilon 1.0e-12
        :observe (monitor-errors A B C L0 E0)}))
    ;; check that all observed errors over the whole interval are small
    (is (> 1e-10 (->> @points
                      (mapcat #(drop 1 %))
                      (map e/abs)
                      (reduce max))))))

(deftest section-2-10
  (is (= '(+ (* (/ 1 2) A (expt φdot 2) (expt (sin θ) 2))
             (* (/ 1 2) C (expt φdot 2) (expt (cos θ) 2))
             (* C φdot ψdot (cos θ))
             (* (/ 1 2) A (expt θdot 2))
             (* (/ 1 2) C (expt ψdot 2)))
         (v/freeze
          (simplify ((r/T-rigid-body 'A 'A 'C) Euler-state))))))
