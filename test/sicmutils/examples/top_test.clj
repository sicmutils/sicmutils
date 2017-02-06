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

(ns sicmutils.examples.top-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.examples.top :refer :all]))

(deftest Top
  (let [state (up 't (up 'theta 'phi 'psi) (up 'thetadot 'phidot 'psidot))]
    (is (= '(+ (* 1/2 (expt (sin theta) 2) A (expt phidot 2))
               (* 1/2 (expt (cos theta) 2) C (expt phidot 2))
               (* (cos theta) C phidot psidot)
               (* 1/2 A (expt thetadot 2))
               (* 1/2 C (expt psidot 2))
               (* -1 (cos theta) gMR))
           (simplify ((L-axisymmetric 'A 'C 'gMR) state))))
    (is (= '(up 1
                (up thetadot phidot psidot)
                (up (/ (+ (* (sin theta) (cos theta) A (expt phidot 2))
                          (* -1N (sin theta) (cos theta) C (expt phidot 2))
                          (* -1N (sin theta) C phidot psidot)
                          (* (sin theta) gMR))
                       A)
                    (/ (+ (* -2N (cos theta) A phidot thetadot)
                          (* (cos theta) C phidot thetadot)
                          (* C psidot thetadot))
                       (* (sin theta) A))
                    (/ (+ (* (expt (cos theta) 2) A phidot thetadot)
                          (* -1N (expt (cos theta) 2) C phidot thetadot)
                          (* -1N (cos theta) C psidot thetadot)
                          (* A phidot thetadot))
                       (* (sin theta) A))))
           (simplify ((Lagrangian->state-derivative (L-axisymmetric 'A 'C 'gMR)) state))))
    (is (= '(up 1
                (up thetadot phidot psidot)
                (up (/ (+ (* -1N (sin theta) A phidot psidot) (* (sin theta) gMR)) A)
                    (/ (+ (* -1N (cos theta) phidot thetadot) (* psidot thetadot)) (sin theta))
                    (/ (+ (* -1N (cos theta) psidot thetadot) (* phidot thetadot)) (sin theta))))
           (simplify ((Lagrangian->state-derivative (L 'A 'A 'A 'gMR)) state))))
    ;; to be checked. Where does the dividing through happen?
    (is (= '(/ (+ (* 2N (expt (sin theta) 2) (cos theta) A C gMR)
                  (* (expt (sin theta) 2) A (expt p_psi 2))
                  (* (expt (sin theta) 2) C (expt p_theta 2))
                  (* (expt (cos theta) 2) C (expt p_psi 2))
                  (* -2N (cos theta) C p_phi p_psi)
                  (* C (expt p_phi 2)))
               (* 2N (expt (sin theta) 2) A C))
           (simplify ((Lagrangian->Hamiltonian (L-axisymmetric 'A 'C 'gMR))
                      (->H-state 't
                                 '[theta phi psi]
                                 '[p_theta p_phi p_psi])))))
    ;; to be checked
    (is (= '(up 0
                (up (/ (+ (* ((D theta) t) A) (* -1N (p_theta t))) A)
                    (/ (+ (* (expt (sin (theta t)) 2) ((D phi) t) A) (* (cos (theta t)) (p_psi t)) (* -1 (p_phi t)))
                       (* (expt (sin (theta t)) 2) A))
                    (/ (+ (* (expt (sin (theta t)) 2) ((D psi) t) A C)
                          (* -1 (expt (sin (theta t)) 2) (p_psi t) A)
                          (* -1 (expt (cos (theta t)) 2) (p_psi t) C)
                          (* (cos (theta t)) (p_phi t) C))
                       (* (expt (sin (theta t)) 2) A C)))
                (down (/ (+ (* -1 (expt (cos (theta t)) 4) A gMR)
                            (* (expt (sin (theta t)) 3) ((D p_theta) t) A)
                            (* (expt (cos (theta t)) 2) (p_phi t) (p_psi t))
                            (* 2N (expt (cos (theta t)) 2) A gMR)
                            (* -1 (cos (theta t)) (expt (p_phi t) 2))
                            (* -1 (cos (theta t)) (expt (p_psi t) 2))
                            (* (p_phi t) (p_psi t))
                            (* -1 A gMR))
                         (* (expt (sin (theta t)) 3) A))
                      ((D p_phi) t)
                      ((D p_psi) t)))
           (simplify
            (((Hamilton-equations
               (Lagrangian->Hamiltonian
                (L-axisymmetric 'A 'C 'gMR)))
              (coordinate-tuple (literal-function 'theta)
                                (literal-function 'phi)
                                (literal-function 'psi))
              (momentum-tuple (literal-function 'p_theta)
                              (literal-function 'p_phi)
                              (literal-function 'p_psi)))
             't))))))
