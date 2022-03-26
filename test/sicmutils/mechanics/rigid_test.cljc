#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.rigid-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [sicmutils.abstract.function :as f :include-macros true]
            [sicmutils.calculus.derivative :refer [partial]]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.matrix :as m]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.mechanics.rigid :as rig]
            [sicmutils.mechanics.rotation :as rotation]
            [sicmutils.numerical.ode :as ode]
            [sicmutils.quaternion :as q]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up]]
            [sicmutils.value :as v]))

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
           (+ (* phidot (sin theta) (sin psi)) (* thetadot (cos psi)))
           (+ (* phidot (sin theta) (cos psi)) (* -1 thetadot (sin psi)))
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
               (* B phidot (expt (cos psi) 2) (expt (sin theta) 2))
               (* A thetadot (cos psi) (sin psi) (sin theta))
               (* -1 B thetadot (cos psi) (sin psi) (sin theta))
               (* C phidot (expt (cos theta) 2))
               (* C psidot (cos theta)))
           (simplify
            (-> (((partial 2) (rig/T-body-Euler 'A 'B 'C))
                 an-Euler-state)
                (get 1)))))

    (is (v/zero?
         (simplify
          (- (-> ((rig/L-space-Euler 'A 'B 'C) an-Euler-state)
                 ;; $L_z$
                 (get 2))
             (-> (((partial 2) (rig/T-body-Euler 'A 'B 'C)) an-Euler-state)
                 ;; $p_\phi$
                 (get 1))))))

    (is (= '(* A B C (expt (sin theta) 2))
           (simplify
            (m/determinant
             (((comp (partial 2) (partial 2))
               (rig/T-body-Euler 'A 'B 'C))
              an-Euler-state)))))))

(deftest rigid-sysder-tests
  ;; (let ((A 1.) (B (sqrt 2.)) (C 2.)
  ;;       (state0 (up 0.0
  ;;                  (up 1. 0. 0.)
  ;;                  (up 0.1 0.1 0.1))))
  ;;   (let ((L0 ((L-space-Euler A B C) state0))
  ;;        (E0 ((T-body-Euler A B C) state0)))
  ;;     ((evolve rigid-sysder A B C)
  ;;      state0
  ;;      (monitor-errors win A B C L0 E0)
  ;;      0.1
  ;;      100.0
  ;;      1.0e-12)))

  ;; (up 99.99999999999864
  ;;     (up .6319896958334494 1.3610271540875034 17.437900484737938)
  ;;     (up -.12343716197181527 .09016109524808046 .07567921658605782))

  ;; (simplify
  ;;  ((T-body-Euler 'A 'A 'C)
  ;;   (up 't
  ;;       (up 'theta 'phi 'psi)
  ;;       (up 'thetadot 'phidot 'psidot))))
  ;; (+ (* (/ 1 2) A (expt phidot 2) (expt (sin theta) 2))
  ;;    (* (/ 1 2) C (expt phidot 2) (expt (cos theta) 2))
  ;;    (* C phidot psidot (cos theta))
  ;;    (* (/ 1 2) A (expt thetadot 2))
  ;;    (* (/ 1 2) C (expt psidot 2)))

  ;; Transformation of A(v):
  ;; M^T A(Mv) M = A(v) for arbitrary v orthogonal M

  ;; (print-expression
  ;;  (let ((Euler (up 'theta 'phi 'psi))
  ;;       (v (up 'x 'y 'z)))
  ;;    (let ((M (r/Euler->M Euler)))
  ;;      (- (* (three-vector-components->antisymmetric (* M v))
  ;;           M)
  ;;        (* M
  ;;           (three-vector-components->antisymmetric v))))))
  ;; (matrix-by-rows (list 0 0 0) (list 0 0 0) (list 0 0 0))



  ;; Configuration equations for Euler's equations with Euler angles

  ;; (print-expression
  ;;  (let ((Euler (up (literal-function 'theta)
  ;;                  (literal-function 'phi)
  ;;                  (literal-function 'psi))))
  ;;    (antisymmetric->column-matrix
  ;;     (* (transpose ((r/Euler->M Euler) 't))
  ;;       ((D (r/Euler->M Euler)) 't)))))
  ;; (matrix-by-rows
  ;;  (list
  ;;   (+ (* ((D phi) t) (sin (psi t)) (sin (theta t)))
  ;;      (* ((D theta) t) (cos (psi t)))))
  ;;  (list
  ;;   (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
  ;;      (* -1 (sin (psi t)) ((D theta) t))))
  ;;  (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))



  ;; (define ((V_eff p A C gMR) theta)
  ;;   (+ (/ (square p) (* 2 C))
  ;;      (* (/ (square p) (* 2 A))
  ;;        (square (tan (/ theta 2))))
  ;;      (* gMR (cos theta))))


  ;; Critical value of bifurcation when D^2 V_eff (0) = 0

  ;; (print-expression
  ;;  (((square derivative) (V_eff 'p_c 'A 'C 'gMR)) 0))
  ;; (+ (* -1 gMR) (/ (* 1/4 (expt p_c 2)) A))

  ;; critical angular speed in RPM is:
  ;; (* (/ 60 2pi) (/ 7.734804457773965e-3 6.6e-5))
  ;; Value: 1119.1203302763215


  )

(deftest quaternion-evolution-tests
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
      ((ode/evolve qw-sysder A B C)
       qw-state0
       0.1                  ;; step between plotted points
       100.0                ;; final time
       {:compile? true
        :epsilon 1.0e-12})))

  )
