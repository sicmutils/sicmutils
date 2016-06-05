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

(ns sicmutils.sicm-ch2-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :refer :all]
             [structure :refer :all]
             [numsymb]
             [numbers]
             [simplify :refer [hermetic-simplify-fixture]]
             [function :refer :all]
             [operator :refer :all]
             [value :as v]]
            [sicmutils.numerical.ode :refer :all]
            [sicmutils.calculus.derivative :refer :all]
            [sicmutils.mechanics
             [rigid :refer :all]
             [rotation :refer :all]]))

(use-fixtures :once hermetic-simplify-fixture)

(def ^:private Euler-state (up 't
                               (up 'θ 'φ 'ψ)
                               (up 'θdot 'φdot 'ψdot)))

(deftest section-2.7
  (with-literal-functions [θ φ ψ]
    (let [q (up θ φ ψ)
          M-on-path (compose Euler->M q)]
      (is (= '(down (up (+ (* -1 (sin (ψ t)) (cos (θ t)) (sin (φ t))) (* (cos (ψ t)) (cos (φ t))))
                        (+ (* (cos (φ t)) (sin (ψ t)) (cos (θ t))) (* (cos (ψ t)) (sin (φ t))))
                        (* (sin (ψ t)) (sin (θ t))))
                    (up (+ (* -1 (cos (ψ t)) (cos (θ t)) (sin (φ t))) (* -1 (cos (φ t)) (sin (ψ t))))
                        (+ (* (cos (ψ t)) (cos (φ t)) (cos (θ t))) (* -1 (sin (ψ t)) (sin (φ t))))
                        (* (cos (ψ t)) (sin (θ t))))
                    (up (* (sin (φ t)) (sin (θ t)))
                        (* -1 (cos (φ t)) (sin (θ t)))
                        (cos (θ t))))
             (simplify (M-on-path 't))))
      (is (= '(up (+ (* (sin (ψ t)) (sin (θ t)) ((D φ) t)) (* (cos (ψ t)) ((D θ) t)))
                  (+ (* (cos (ψ t)) (sin (θ t)) ((D φ) t)) (* -1 (sin (ψ t)) ((D θ) t)))
                  (+ (* (cos (θ t)) ((D φ) t)) ((D ψ) t)))
             (simplify (((M-of-q->omega-body-of-t Euler->M) q) 't))))
      (is (= '(up (+ (* (sin ψ) (sin θ) φdot)
                     (* (cos ψ) θdot))
                  (+ (* (cos ψ) (sin θ) φdot)
                     (* -1 (sin ψ) θdot))
                  (+ (* (cos θ) φdot) ψdot))
             (simplify ((M->omega-body Euler->M) Euler-state)))))))

(deftest section-2.9
  (is (= '(+ (* (expt (cos ψ) 2) (expt (sin θ) 2) B φdot)
             (* (expt (sin ψ) 2) (expt (sin θ) 2) A φdot)
             (* (cos ψ) (sin ψ) (sin θ) A θdot)
             (* -1 (cos ψ) (sin ψ) (sin θ) B θdot)
             (* (expt (cos θ) 2) C φdot)
             (* (cos θ) C ψdot))
         (simplify (nth (((∂ 2) (T-rigid-body 'A 'B 'C)) Euler-state) 1))))
  (is (zero? (simplify (- (nth ((Euler-state->L-space 'A 'B 'C) Euler-state) 2)
                          (nth (((∂ 2) (T-rigid-body 'A 'B 'C)) Euler-state) 1)))))
  (is (= '(* (expt (sin θ) 2) A B C)
         (simplify (determinant (((square (∂ 2)) (T-rigid-body 'A 'B 'C)) Euler-state))))))

(deftest ^:long section-2.9b
  (let [relative-error (fn [value reference-value]
                         (when (zero? reference-value)
                           (throw (IllegalArgumentException. "zero reference value")))
                         (/ (- value reference-value) reference-value))
        points (atom [])
        monitor-errors (fn [A B C L0 E0]
                         (fn [t state]
                           (let [L ((Euler-state->L-space A B C) state)
                                 E ((T-rigid-body A B C) state)]
                             (swap! points conj
                                    [t
                                     (relative-error (nth L 0) (nth L0 0))
                                     (relative-error (nth L 1) (nth L0 1))
                                     (relative-error (nth L 2) (nth L0 2))
                                     (relative-error E E0)]))))
        A 1. B (Math/sqrt 2.) C 2. ;; moments of inertia
        state0 (up 0. (up 1. 0. 0.) (up 0.1 0.1 0.1)) ;; initial state
        L0 ((Euler-state->L-space A B C) state0)
        E0 ((T-rigid-body A B C) state0)]

    ((evolve rigid-sysder A B C)
     state0
     (monitor-errors A B C L0 E0)
     0.1
     10.0
     1.0e-12
     :compile true)
    ;; check that all observed errors over the whole interval are small
    (is (> 1e-10 (->> @points
                      (mapcat #(drop 1 %))
                      (map abs)
                      (reduce max))))))


(deftest section-2.10
  (is (= '(+
           (* 1/2 (expt (cos θ) 2) C (expt φdot 2))
           (* 1/2 (expt (sin θ) 2) A (expt φdot 2))
           (* (cos θ) C φdot ψdot)
           (* 1/2 A (expt θdot 2))
           (* 1/2 C (expt ψdot 2)))
         (simplify ((T-rigid-body 'A 'A 'C) Euler-state)))))
