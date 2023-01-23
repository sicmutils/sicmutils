#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.top-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [up literal-function]]
            [emmy.examples.top :as t]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest Top
  (let [state (up 't
                  (up 'theta 'phi 'psi)
                  (up 'thetadot 'phidot 'psidot))]
    (is (= '(+ (* (/ 1 2) A (expt phidot 2) (expt (sin theta) 2))
               (* (/ 1 2) C (expt phidot 2) (expt (cos theta) 2))
               (* C phidot psidot (cos theta))
               (* (/ 1 2) A (expt thetadot 2))
               (* (/ 1 2) C (expt psidot 2))
               (* -1 gMR (cos theta)))
           (e/freeze
            (e/simplify
             ((t/L-axisymmetric 'A 'C 'gMR) state)))))

    (is (= '(up 1
                (up thetadot phidot psidot)
                (up (/ (+ (* A (expt phidot 2) (sin theta) (cos theta))
                          (* -1 C (expt phidot 2) (sin theta) (cos theta))
                          (* -1 C phidot psidot (sin theta))
                          (* gMR (sin theta))) A)
                    (/ (+ (* -2 A phidot thetadot (cos theta))
                          (* C phidot thetadot (cos theta))
                          (* C psidot thetadot))
                       (* A (sin theta)))
                    (/ (+ (* A phidot thetadot (expt (cos theta) 2))
                          (* -1 C phidot thetadot (expt (cos theta) 2))
                          (* -1 C psidot thetadot (cos theta))
                          (* A phidot thetadot))
                       (* A (sin theta)))))
           (e/freeze
            (e/simplify
             ((e/Lagrangian->state-derivative
               (t/L-axisymmetric 'A 'C 'gMR)) state)))))

    (is (= '(up 1
                (up thetadot phidot psidot)
                (up (/ (+ (* -1 A phidot psidot (sin theta)) (* gMR (sin theta))) A)
                    (/ (+ (* -1 phidot thetadot (cos theta)) (* psidot thetadot)) (sin theta))
                    (/ (+ (* -1 psidot thetadot (cos theta)) (* phidot thetadot)) (sin theta))))
           (e/freeze
            (e/simplify
             ((e/Lagrangian->state-derivative
               (t/L 'A 'A 'A 'gMR)) state))))))

  (is (= '(up 0
              (up
               (/ (+ (* A ((D theta) t)) (* -1 (p_theta t))) A)
               (/ (+ (* A (expt (sin (theta t)) 2) ((D phi) t))
                     (* (cos (theta t)) (p_psi t))
                     (* -1 (p_phi t)))
                  (* A (expt (sin (theta t)) 2)))
               (/ (+ (* A C (expt (sin (theta t)) 2) ((D psi) t))
                     (* -1 A (expt (sin (theta t)) 2) (p_psi t))
                     (* -1 C (expt (cos (theta t)) 2) (p_psi t))
                     (* C (cos (theta t)) (p_phi t)))
                  (* A C (expt (sin (theta t)) 2))))
              (down
               (/ (+ (* -1 A gMR (expt (cos (theta t)) 4))
                     (* A (expt (sin (theta t)) 3) ((D p_theta) t))
                     (* 2 A gMR (expt (cos (theta t)) 2))
                     (* (expt (cos (theta t)) 2) (p_psi t) (p_phi t))
                     (* -1 (cos (theta t)) (expt (p_psi t) 2))
                     (* -1 (cos (theta t)) (expt (p_phi t) 2))
                     (* -1 A gMR)
                     (* (p_psi t) (p_phi t)))
                  (* A (expt (sin (theta t)) 3)))
               ((D p_phi) t)
               ((D p_psi) t)))
         (e/freeze
          (e/simplify
           (((e/Hamilton-equations
              (e/Lagrangian->Hamiltonian
               (t/L-axisymmetric 'A 'C 'gMR)))
             (e/coordinate-tuple (literal-function 'theta)
                                 (literal-function 'phi)
                                 (literal-function 'psi))
             (e/momentum-tuple (literal-function 'p_theta)
                               (literal-function 'p_phi)
                               (literal-function 'p_psi)))
            't))))))
