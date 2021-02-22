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

(ns sicmutils.sicm.ch5-test
  (:refer-clojure :exclude [+ - * /  ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e
             :refer [+ - * / D ref partial simplify compose
                     up down sin cos square
                     p->r s->m F->CT
                     literal-function]
             #?@(:cljs [:include-macros true])]
            [sicmutils.mechanics.hamilton :as H]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-5-1
  (testing "central field"
    (is (= '(/ (+ (* 2 m (expt r 2) (V r)) (* (expt p_r 2) (expt r 2)) (expt p_phi 2))
               (* 2 m (expt r 2)))
           (simplify ((compose (H/H-central 'm (literal-function 'V))
                               (F->CT p->r))
                       (up 't
                           (up 'r 'phi)
                           (down 'p_r 'p_phi))))))))

(deftest section-5-2
  (let [J-func (fn [[_ dh1 dh2]] (up 0 dh2 (- dh1)))
        T-func (fn [_] (up 1 0 0))]

    (testing "canonical"
      (is (= '(up 0 (up 0 0) (down 0 0))
             (simplify
              ((e/compositional-canonical?
                (F->CT p->r)
                (H/H-central 'm (literal-function 'V)))
               (up 't
                   (up 'r 'phi)
                   (down 'p_r 'p_phi))))))
      (is (= '(up 0 0 0)
             (simplify
              ((e/time-independent-canonical? (e/polar-canonical 'alpha))
               (up 't 'theta 'I)))))

      (let [a-non-canonical-transform (fn [[t theta p]]
                                        (let [x (* p (sin theta))
                                              p_x (* p (cos theta))]
                                          (up t x p_x)))]
        (is (not= '(up 0 0 0)
                  (simplify
                   ((e/time-independent-canonical? a-non-canonical-transform)
                    (up 't 'theta 'p))))))
      (is (= '(matrix-by-rows
               (up 0 0 0 0 0)
               (up 0 0 0 1 0)
               (up 0 0 0 0 1)
               (up 0 -1 0 0 0)
               (up 0 0 -1 0 0))
             (e/freeze
              (simplify (let [s (up 't (up 'x 'y) (down 'px 'py))
                              s* (e/compatible-shape s)]
                          (s->m s* ((D J-func) s*) s*))))))
      (let [symplectic? (fn [C]
                          (fn [s]
                            (let [s* (e/compatible-shape s)
                                  J (s->m s* ((D J-func) s*) s*)
                                  DCs (s->m s* ((D C) s) s)]
                              (- J (* DCs J (e/transpose DCs))))))]
        (is (= '(matrix-by-rows
                 (up 0 0 0 0 0)
                 (up 0 0 0 0 0)
                 (up 0 0 0 0 0)
                 (up 0 0 0 0 0)
                 (up 0 0 0 0 0))
               (e/freeze
                (simplify
                 ((symplectic? (F->CT p->r))
                  (up 't
                      (up 'r 'varphi)
                      (down 'p_r 'p_varphi)))))))))

    (testing "symplectic"
      (is (= '(matrix-by-rows
               (up 0 0 0 0)
               (up 0 0 0 0)
               (up 0 0 0 0)
               (up 0 0 0 0))
             (e/freeze
              (simplify
               ((e/symplectic-transform? (F->CT p->r))
                (up 't
                    (up 'r 'theta)
                    (down 'p_r 'p_theta))))))))

    (testing "rotating coordinates p. 336"
      (let [canonical-K? (fn [C K]
                           (fn [s]
                             (let [s* (e/compatible-shape s)]
                               (- (T-func s*)
                                  (+ (* ((D C) s) (J-func ((D K) s)))
                                     (((partial 0) C) s))))))
            rotating (fn [n]
                       (fn [[t [x y z]]]
                         (up (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
                             (- (* (cos (* n t)) y) (* (sin (* n t)) x))
                             z)))
            C-rotating (fn [Omega]
                         (F->CT (rotating Omega)))
            K (fn [Omega]
                (fn [[_ [x y _] [p_x p_y _]]]
                  (* Omega (- (* x p_y) (* y p_x)))))]
        (is (= '(matrix-by-rows
                 (up 0 0 0 0 0 0)
                 (up 0 0 0 0 0 0)
                 (up 0 0 0 0 0 0)
                 (up 0 0 0 0 0 0)
                 (up 0 0 0 0 0 0)
                 (up 0 0 0 0 0 0))
               (e/freeze
                (simplify
                 ((e/symplectic-transform? (C-rotating 'Omega))
                  (up 't
                      (up 'x 'y 'z)
                      (down 'p_x 'p_y 'p_z)))))))

        ;; Note that the definition of K, above, differs from that given in
        ;; the 1st Ed. of SICM (our definition has the opposite sign). The
        ;; test below does not work if the book's definition is used. In the
        ;; source to SICM in Scheme (time-varying.scm), the sign also differs
        ;; from that given in the book. We therefore consider the book's
        ;; definition an erratum
        (is (= '(up 0 (up 0 0 0) (down 0 0 0))
               (simplify
                ((canonical-K? (C-rotating 'Omega) (K 'Omega))
                 (up 't
                     (up 'x 'y 'z)
                     (down 'p_x 'p_y 'p_z))))))))))

(deftest section-5-3
  (let [omega (fn [zeta1 zeta2]
                (- (* (e/momentum zeta2) (e/coordinate zeta1))
                   (* (e/momentum zeta1) (e/coordinate zeta2))))
        a-polar-state (up 't
                          (up 'r 'phi)
                          (down 'pr 'pphi))
        zeta1 (up 0
                  (up 'dr1 'dphi1)
                  (down 'dpr1 'dpphi1))
        zeta2 (up 0
                  (up 'dr2 'dphi2)
                  (down 'dpr2 'dpphi2))]

    (is (= 0 (let [DCs ((D (F->CT p->r)) a-polar-state)]
               (simplify (- (omega zeta1 zeta2)
                            (omega (* DCs zeta1) (* DCs zeta2)))))))))

(deftest section-5-7
  (let [shift-t (fn [delta-t]
                  (fn [[t q p]]
                    (up (+ t delta-t) q p)))
        C->Cp (fn [C]
                (fn [delta-t]
                  (compose (C delta-t) (shift-t (- delta-t)))))
        H->Hp (fn [delta-t]
                (fn [H]
                  (compose H (shift-t (- delta-t)))))
        ;; going further means solving exercise 5.22.
        ;; XXX at the moment, nothing below is tested, because we have to think
        ;; harder about this exercise
        C (fn [alpha omega omega0]
            (fn [delta-t]
              (fn [[t0 q0 p0]]
                (let [alpha' (/ alpha (- (square omega0) (square omega)))
                      M (e/matrix-by-rows [(* (cos omega0) delta-t) (* (sin omega0) delta-t)]
                                          [(- (* (sin omega0) delta-t)) (* (cos omega0) delta-t)])
                      a (e/column-matrix
                         (- q0 (* alpha' (cos (* omega t0))))
                         (* (/ omega0) (+ p0 (* alpha' omega (sin (* omega t0))))))
                      b (e/column-matrix
                         (* alpha' (cos (* omega (+ t0 delta-t))))
                         (- (* alpha' (/ omega omega0) (sin (* omega (+ t0 delta-t))))))]
                  (+ (* M a) b)))))
        solution (fn [alpha omega omega0]
                   (fn [state0]
                     (fn [t]
                       (((C alpha omega omega0) (- t (first state0))) state0))))
        sol ((solution 'α 'ω 'ω_0) (up 't_0 'q_0 'p_0))
        solution-C (sol 't)
        q (ref solution-C 0)
        p (ref solution-C 1)
        Dsol ((D sol) 't)]))

(deftest section-5-10
  (let [H-harmonic (fn [m k]
                     (fn [state]
                       (+ (/ (square (e/momentum state)) (* 2 m))
                          (* (/ 1 2) k (square (e/coordinate state))))))]
    (is (= '(x0
             (/ (* dt p0) m)
             (/ (* -1 (expt dt 2) k x0) (* 2 m))
             (/ (* -1 (expt dt 3) k p0) (* 6 (expt m 2)))
             (/ (* (expt dt 4) (expt k 2) x0) (* 24 (expt m 2)))
             (/ (* (expt dt 5) (expt k 2) p0) (* 120 (expt m 3))))
           (simplify (take 6 (seq
                              (((e/Lie-transform (H-harmonic 'm 'k) 'dt)
                                e/coordinate)
                               (up 0 'x0 'p0)))))))
    (is (= '(p0
             (* -1 dt k x0)
             (/ (* -1 (expt dt 2) k p0) (* 2 m))
             (/ (* (expt dt 3) (expt k 2) x0) (* 6 m))
             (/ (* (expt dt 4) (expt k 2) p0) (* 24 (expt m 2)))
             (/ (* -1 (expt dt 5) (expt k 3) x0) (* 120 (expt m 2))))
           (simplify (take 6 (seq
                              (((e/Lie-transform (H-harmonic 'm 'k) 'dt)
                                e/momentum)
                               (up 0 'x0 'p0)))))))
    (is (= '((/ (+ (* k m (expt x0 2)) (expt p0 2)) (* 2 m))
             0
             0
             0
             0
             0)
           (simplify (take 6 (seq
                              (((e/Lie-transform (H-harmonic 'm 'k) 'dt)
                                (H-harmonic 'm 'k))
                               (up 0 'x0 'p0)))))))
    (let [state (up 't
                    (up 'r_0 'phi_0)
                    (down 'p_r_0 'p_phi_0))]
      (is (= '(/ (+ (* 2 m (expt r_0 2) (U r_0)) (* (expt p_r_0 2) (expt r_0 2)) (expt p_phi_0 2))
                 (* 2 m (expt r_0 2)))
             (simplify ((H/H-central-polar 'm (literal-function 'U)) state))))
      (is (= '((up r_0 phi_0)
               (up (/ (* dt p_r_0) m)
                   (/ (* dt p_phi_0) (* m (expt r_0 2))))
               (up (/ (+ (* -1 (expt dt 2) m (expt r_0 3) ((D U) r_0)) (* (expt dt 2) (expt p_phi_0 2))) (* 2 (expt m 2) (expt r_0 3)))
                   (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
               (up (/ (+ (* -1 (expt dt 3) m p_r_0 (expt r_0 4) (((expt D 2) U) r_0)) (* -3 (expt dt 3) (expt p_phi_0 2) p_r_0)) (* 6 (expt m 3) (expt r_0 4)))
                   (/ (+ (* (expt dt 3) m p_phi_0 (expt r_0 3) ((D U) r_0)) (* 3 (expt dt 3) p_phi_0 (expt p_r_0 2) (expt r_0 2)) (* -1 (expt dt 3) (expt p_phi_0 3))) (* 3 (expt m 3) (expt r_0 6)))))
             (simplify (take 4 (seq
                                (((e/Lie-transform (H/H-central-polar 'm (literal-function 'U)) 'dt)
                                  e/coordinate)
                                 state)))))))))
