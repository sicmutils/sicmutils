;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.sicm-ch2-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.numsymb]
            [math.simplify]
            [math.expression :refer :all]
            [math.function :refer :all]
            [math.operator :refer :all]
            [math.value :as v]
            [math.calculus.derivative :refer :all]
            [math.mechanics.rigid :refer :all]
            [math.mechanics.rotation :refer :all]))

(defn- pe [x] (-> x simplify print-expression))

(deftest section-2.7
  (with-literal-functions [θ φ ψ]
    (let [q (up θ φ ψ)
          M-on-path (compose Euler->M q)]
      (is (= '(down
               (up (+ (* -1 (sin (ψ t)) (cos (θ t)) (sin (φ t))) (* (cos (ψ t)) (cos (φ t))))
                   (+ (* (sin (ψ t)) (cos (θ t)) (cos (φ t))) (* (cos (ψ t)) (sin (φ t))))
                   (* (sin (ψ t)) (sin (θ t))))
               (up (+ (* -1 (cos (ψ t)) (cos (θ t)) (sin (φ t))) (* -1 (sin (ψ t)) (cos (φ t))))
                   (+ (* (cos (ψ t)) (cos (θ t)) (cos (φ t))) (* -1 (sin (ψ t)) (sin (φ t))))
                   (* (cos (ψ t)) (sin (θ t))))
               (up (* (sin (θ t)) (sin (φ t)))
                   (* -1 (sin (θ t)) (cos (φ t)))
                   (cos (θ t))))
             (pe (M-on-path 't))))
      (is (= '(up (+ (* (sin (ψ t)) (sin (θ t)) ((D φ) t))
                     (* (cos (ψ t)) ((D θ) t)))
                  (+ (* (cos (ψ t)) (sin (θ t)) ((D φ) t))
                     (* -1 ((D θ) t) (sin (ψ t))))
                  (+ (* (cos (θ t)) ((D φ) t)) ((D ψ) t)))
             (pe (((M-of-q->omega-body-of-t Euler->M) q) 't))))
      (is (= '(up (+ (* (sin ψ) (sin θ) φdot) (* (cos ψ) θdot))
                  (+ (* (cos ψ) (sin θ) φdot) (* -1 (sin ψ) θdot))
                  (+ (* (cos θ) φdot) ψdot))
             (pe ((M->omega-body Euler->M) (up 't
                                               (up 'θ 'φ 'ψ)
                                               (up 'θdot 'φdot 'ψdot)))))))))

(deftest section-2.9
  (let [Euler-state (up 't
                        (up 'θ 'φ 'ψ)
                        (up 'θdot 'φdot 'ψdot))]
    ;; this is almost what scmutils gives, except the first and third terms
    ;; containing A φdot are reduced to sin^2 psi sin^2 theta, so that is
    ;; a missing piece in our simplification. XXX
    (is (= '(+ (* -1 (expt (sin ψ) 2) (expt (cos θ) 2) A φdot)
               (* (expt (cos ψ) 2) (expt (sin θ) 2) B φdot)
               (* (sin ψ) (cos ψ) (sin θ) A θdot)
               (* -1N (sin ψ) (cos ψ) (sin θ) B θdot)
               (* (expt (sin ψ) 2) A φdot)
               (* (expt (cos θ) 2) C φdot)
               (* (cos θ) C ψdot))
           (pe (nth (((pd 2) (T-rigid-body 'A 'B 'C)) Euler-state) 1))))
    (is (zero? (pe (- (nth ((Euler-state->L-space 'A 'B 'C) Euler-state) 2)
                      (nth (((pd 2) (T-rigid-body 'A 'B 'C)) Euler-state) 1)))))
    (is (= '(* (expt (sin θ) 2) A B C)
           (pe (determinant (((square (pd 2)) (T-rigid-body 'A 'B 'C)) Euler-state)))))))
