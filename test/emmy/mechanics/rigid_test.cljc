#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.rigid-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.abstract.function :as f]
            [emmy.calculus.derivative :refer [D partial]]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.mechanics.lagrange :as L]
            [emmy.mechanics.rigid :as rig]
            [emmy.mechanics.rotation :as rotation]
            [emmy.numerical.ode :as ode]
            [emmy.quaternion :as q]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest rigid-tests
  (f/with-literal-functions [theta phi psi]
    (is (= '(column-matrix
             (+ (* (sin (theta t)) ((D phi) t) (sin (psi t)))
                (* (cos (psi t)) ((D theta) t)))
             (+ (* (cos (psi t)) (sin (theta t)) ((D phi) t))
                (* -1 ((D theta) t) (sin (psi t))))
             (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t)))
           (simplify
            ((rig/Euler->omega-body
              (up theta phi psi))
             't))))

    (is (= '(column-matrix
             (+ (* (sin (theta t)) ((D phi) t) (sin (psi t)))
                (* (cos (psi t)) ((D theta) t)))
             (+ (* (cos (psi t)) (sin (theta t)) ((D phi) t))
                (* -1 ((D theta) t) (sin (psi t))))
             (+ (* (cos (theta t)) ((D phi) t))
                ((D psi) t)))
           (simplify
            (((rig/M-of-q->omega-body-of-t rotation/Euler->M)
              (up theta phi psi))
             't)))))

  (is (= '(column-matrix
           (+ (* phidot (sin psi) (sin theta)) (* thetadot (cos psi)))
           (+ (* phidot (cos psi) (sin theta)) (* -1 thetadot (sin psi)))
           (+ (* phidot (cos theta)) psidot))
         (simplify
          ((rig/M->omega-body rotation/Euler->M)
           (up 't
               (up 'theta 'phi 'psi)
               (up 'thetadot 'phidot 'psidot)))))))

(deftest more-Euler-tests
  (let [an-Euler-state (up 't
                           (up 'theta 'phi 'psi)
                           (up 'thetadot 'phidot 'psidot))]
    (is (= '(+ (* A phidot (expt (sin psi) 2) (expt (sin theta) 2))
               (* B phidot (expt (sin theta) 2) (expt (cos psi) 2))
               (* A thetadot (sin psi) (sin theta) (cos psi))
               (* -1 B thetadot (sin psi) (sin theta) (cos psi))
               (* C phidot (expt (cos theta) 2))
               (* C psidot (cos theta)))
           (simplify
            (-> (((partial 2) (rig/T-body-Euler 'A 'B 'C))
                 an-Euler-state)
                (get 1)))))

    (is (v/zero?
         (simplify
          ;; this first is the fucked up one
          (- (-> ((rig/L-space-Euler 'A 'B 'C) an-Euler-state)
                 ;; $L_z$
                 (nth 2))
             (-> (((partial 2) (rig/T-body-Euler 'A 'B 'C)) an-Euler-state)
                 ;; $p_\phi$
                 (nth 1))))))

    (is (= '(* A B C (expt (sin theta) 2))
           (simplify
            (g/determinant
             (((comp (partial 2) (partial 2))
               (rig/T-body-Euler 'A 'B 'C))
              an-Euler-state)))))))

(deftest rigid-sysder-tests
  (with-comparator (v/within 1e-8)
    (let [A 1.0
          B (Math/sqrt 2.0)
          C 2.0
          state0 (up 0.0 (up 1.0 0.0 0.0) (up 0.1 0.1 0.1))]
      (is (ish?
           (up 99.99999999999909
               (up 0.6319896958404042
                   1.3610271540831438
                   17.43790048472884)
               (up -0.12343716197081755
                   0.09016109524917856
                   0.07567921658551353))
           ((ode/evolve rig/rigid-sysder A B C)
            state0
            0.1
            100.0
            {:compile? true
             :epsilon 1.0e-12}))
          "ODE solver example from rigid.scm.")))

  (is (= '(+ (* (/ 1 2) A (expt phidot 2) (expt (sin theta) 2))
             (* (/ 1 2) C (expt phidot 2) (expt (cos theta) 2))
             (* C phidot psidot (cos theta))
             (* (/ 1 2) A (expt thetadot 2))
             (* (/ 1 2) C (expt psidot 2)))
         (simplify
          ((rig/T-body-Euler 'A 'A 'C)
           (up 't
               (up 'theta 'phi 'psi)
               (up 'thetadot 'phidot 'psidot))))))

  (testing "Transformation of A(v):"
    (let [Euler (up 'theta 'phi 'psi)
          v (up 'x 'y 'z)
          M (rotation/Euler->M Euler)]
      (is (= '(matrix-by-rows
               (up 0 0 0)
               (up 0 0 0)
               (up 0 0 0))
             (simplify
              (- (* (rig/three-vector-components->antisymmetric (* M v)) M)
                 (* M (rig/three-vector-components->antisymmetric v)))))
          "M^T A(Mv) M = A(v) for arbitrary v orthogonal M")))

  (is (= '(column-matrix
           (+ (* (sin (psi t)) (sin (theta t)) ((D phi) t))
              (* (cos (psi t)) ((D theta) t)))
           (+ (* (cos (psi t)) (sin (theta t)) ((D phi) t))
              (* -1 (sin (psi t)) ((D theta) t)))
           (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t)))
         (f/with-literal-functions [theta phi psi]
           (simplify
            (let [Euler (up theta phi psi)]
              (rig/antisymmetric->column-matrix
               (* (g/transpose ((rotation/Euler->M Euler) 't))
                  ((D (rotation/Euler->M Euler)) 't)))))))
      "Configuration equations for Euler's equations with Euler angles")

  (letfn [(V_eff [p A C gMR]
            (fn [theta]
              (+ (/ (g/square p) (* 2 C))
                 (* (/ (g/square p) (* 2 A))
                    (g/square (g/tan (/ theta 2))))
                 (* gMR (g/cos theta)))))]
    (is (= '(/ (+ (* -1 A gMR) (* (/ 1 4) (expt p_c 2))) A)
           (simplify
            (((g/square D) (V_eff 'p_c 'A 'C 'gMR)) 0)))
        "Critical value of bifurcation when D^2 V_eff (0) = 0"))

  (is (ish? 1119.1203302763215
            (* (/ 60 v/twopi) (/ 7.734804457773965e-3 6.6e-5)))
      "critical angular speed in RPM, for docs"))

(deftest quaternion-state-tests
  (is (= '(up
           (/ (+ (* 2 a vb) (* -2 b va) (* -2 c vd) (* 2 d vc))
              (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
           (/ (+ (* 2 a vc) (* 2 b vd) (* -2 c va) (* -2 d vb))
              (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
           (/ (+ (* 2 a vd) (* -2 b vc) (* 2 c vb) (* -2 d va))
              (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2))))
         (simplify
          (rig/quaternion-state->omega-body
           (up 1 (up 'a 'b 'c 'd)
               (up 'va 'vb 'vc 'vd)))))
      "symbolic expansion to omega-body")

  (is (= '(up (/ (+ (* 2 a vb) (* -2 b va) (* 2 c vd) (* -2 d vc))
                 (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
              (/ (+ (* 2 a vc) (* -2 b vd) (* -2 c va) (* 2 d vb))
                 (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2)))
              (/ (+ (* 2 a vd) (* 2 b vc) (* -2 c vb) (* -2 d va))
                 (+ (expt a 2) (expt b 2) (expt c 2) (expt d 2))))
         (simplify
          (rig/quaternion-state->omega-space
           (up 't
               (up 'a 'b 'c 'd)
               (up 'va 'vb 'vc 'vd)))))
      "Looks similar with some different - signs. Correct?? Matches scmutils,
      anyway.")

  (is (= '(+ (* (/ 8 225) B)
             (* (/ 2 225) C))
         (simplify
          ((rig/T-quaternion-state 'A 'B 'C)
           (up 't (up 1 2 3 4) (up 2 3 4 5)))))
      "A shamefully uninformed example, picked so we have some check against the
  scmutils implementation."))

(deftest quaternion-evolution-tests
  (with-comparator (v/within 1e-8)
    (letfn [(qw-sysder [A B C]
              (let [B-C-over-A (/ (- B C) A)
                    C-A-over-B (/ (- C A) B)
                    A-B-over-C (/ (- A B) C)]
                (fn the-deriv [[_ q [omega**a omega**b omega**c]]]
                  (let [tdot 1
                        qdot ;; driven quaternion
                        (* (/ -1 2)
                           (+ (* omega**a q/I-matrix)
                              (* omega**b q/J-matrix)
                              (* omega**c q/K-matrix))
                           q)
                        omegadot  ;; Euler's equations
                        (up (* B-C-over-A omega**b omega**c)
                            (* C-A-over-B omega**c omega**a)
                            (* A-B-over-C omega**a omega**b))]
                    (up tdot qdot omegadot)))))]

      ;; A, B, C == moments of inertia
      (let [A 1.0
            B (Math/sqrt 2.0)
            C 2.0
            ;; initial state
            Euler-state (up 0.0
                            (up 1. 0.0 0.0)
                            (up 0.1 0.1 0.1))
            M (rotation/Euler->M (L/coordinates Euler-state))
            q (q/->vector
               (q/from-rotation-matrix M))
            qw-state0
            (up (L/time Euler-state)
                q
                (rig/Euler-state->omega-body Euler-state))]
        (is (ish? (up 99.99999999999878
                      (up -0.9501831654548522 -0.05699715799969905
                          -0.3054905540187315 0.024058210063923138)
                      (up -0.07215083472579442 -0.11343682989477462
                          0.1484260290508369))
                  ((ode/evolve qw-sysder A B C)
                   qw-state0
                   0.1                  ;; step between plotted points
                   100.0                ;; final time
                   {:compile? true
                    :epsilon 1.0e-12}))
            "Big example from bottom of rigid.scm.")))))
