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

(ns sicmutils.sicm.ch2-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [sicmutils.env :as e :refer [+ - * / zero? ref D partial simplify
                                         compose up down]]
            [sicmutils.mechanics.rotation :refer [Euler->M]]
            [sicmutils.mechanics.rigid :as r]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.util :as u]))

(use-fixtures :once hermetic-simplify-fixture)

(def ^:private Euler-state
  (up 't
      (up 'θ 'φ 'ψ)
      (up 'θdot 'φdot 'ψdot)))

(deftest section-2-7
  (e/with-literal-functions [θ φ ψ]
    (let [q (up θ φ ψ)
          M-on-path (compose Euler->M q)]
      (is (= '(matrix-by-rows [(+ (* -1 (cos (θ t)) (sin (ψ t)) (sin (φ t))) (* (cos (φ t)) (cos (ψ t))))
                               (+ (* -1 (cos (ψ t)) (cos (θ t)) (sin (φ t))) (* -1 (cos (φ t)) (sin (ψ t))))
                               (* (sin (φ t)) (sin (θ t)))]

                              [(+ (* (cos (φ t)) (cos (θ t)) (sin (ψ t))) (* (cos (ψ t)) (sin (φ t))))
                               (+ (* (cos (φ t)) (cos (ψ t)) (cos (θ t))) (* -1 (sin (ψ t)) (sin (φ t))))
                               (* -1 (cos (φ t)) (sin (θ t)))]

                              [(* (sin (ψ t)) (sin (θ t)))
                               (* (cos (ψ t)) (sin (θ t)))
                               (cos (θ t))])
             (simplify (M-on-path 't))))
      (is (= '(column-matrix (+ (* (sin (ψ t)) (sin (θ t)) ((D φ) t)) (* (cos (ψ t)) ((D θ) t)))
                             (+ (* (cos (ψ t)) (sin (θ t)) ((D φ) t)) (* -1 (sin (ψ t)) ((D θ) t)))
                             (+ (* (cos (θ t)) ((D φ) t)) ((D ψ) t)))
             (simplify (((r/M-of-q->omega-body-of-t Euler->M) q) 't))))
      (is (= '(column-matrix (+ (* φdot (sin ψ) (sin θ)) (* θdot (cos ψ)))
                             (+ (* φdot (cos ψ) (sin θ)) (* -1 θdot (sin ψ)))
                             (+ (* φdot (cos θ)) ψdot))
             (simplify ((r/M->omega-body Euler->M) Euler-state)))))))

(deftest section-2-9
  (is (= '(+ (* A φdot (expt (sin ψ) 2) (expt (sin θ) 2))
             (* B φdot (expt (cos ψ) 2) (expt (sin θ) 2))
             (* A θdot (cos ψ) (sin ψ) (sin θ))
             (* -1 B θdot (cos ψ) (sin ψ) (sin θ))
             (* C φdot (expt (cos θ) 2))
             (* C ψdot (cos θ)))
         (simplify (ref (((partial 2) (r/T-rigid-body 'A 'B 'C)) Euler-state) 1))))

  (is (zero? (simplify (- (ref ((r/Euler-state->L-space 'A 'B 'C) Euler-state) 2)
                          (ref (((partial 2) (r/T-rigid-body 'A 'B 'C)) Euler-state) 1)))))

  (is (= '(* A B C (expt (sin θ) 2))
         (simplify (e/determinant
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
    #?(:clj
       ;; TODO activate when cljs gets an evolver.
       (do ((e/evolve r/rigid-sysder A B C)
            state0
            (monitor-errors A B C L0 E0)
            0.1
            10.0
            1.0e-12
            :compile true)
           ;; check that all observed errors over the whole interval are small
           (is (> 1e-10 (->> @points
                             (mapcat #(drop 1 %))
                             (map e/abs)
                             (reduce max))))))))

(deftest section-2-10
  (is (= '(+ (* #?(:clj 1/2 :cljs 0.5) A (expt φdot 2) (expt (sin θ) 2))
             (* #?(:clj 1/2 :cljs 0.5) C (expt φdot 2) (expt (cos θ) 2))
             (* C φdot ψdot (cos θ))
             (* #?(:clj 1/2 :cljs 0.5) A (expt θdot 2))
             (* #?(:clj 1/2 :cljs 0.5) C (expt ψdot 2)))
         (simplify ((r/T-rigid-body 'A 'A 'C) Euler-state)))))
