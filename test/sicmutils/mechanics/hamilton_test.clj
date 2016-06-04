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

(ns sicmutils.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.mechanics
             [hamilton :refer :all]
             [lagrange :refer :all]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-3.1.1
  ;; To move further into Hamiltonian mechanics, we will need
  ;; literal functions mapping structures to structures.
  (with-literal-functions [x y v_x v_y p_x p_y [V [1 2] 3]]
    (is (= '(V x y) (simplify (V 'x 'y))))
    (is (= '(up 0
                (up (/ (+ (* ((D x) t) m) (* -1 (p_x t))) m)
                    (/ (+ (* ((D y) t) m) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((∂ 0) V) (x t) (y t)))
                      (+ ((D p_y) t) (((∂ 1) V) (x t) (y t)))))
           (simplify (((Hamilton-equations
                        (H-rectangular
                         'm V))
                       (up x y)
                       (down p_x p_y))
                      't))))
    (is (= '(/ (expt y 2) (* 4 c))
           (simplify ((Legendre-transform (fn [x] (* 'c x x))) 'y))))
    (is (= '(+ (* 1/2 m (expt v_x 2)) (* 1/2 m (expt v_y 2)) (* -1 (V x y)))
           (simplify ((L-rectangular 'm V) (up 't (up 'x 'y) (up 'v_x 'v_y))))))
    (is (= '(/ (+ (* 2 (V x y) m) (expt p_x 2) (expt p_y 2))
               (* 2 m))
           (simplify ((Lagrangian->Hamiltonian
                       (L-rectangular 'm V))
                      (up 't (up 'x 'y) (down 'p_x 'p_y))))))))

(deftest gjs-tests
  (is (= '(up
           0
           (up
            (/ (+ (* ((D x) t) m) (* -1 (p_x t))) m)
            (/ (+ (* ((D y) t) m) (* -1 (p_y t))) m))
           (down
            (+ ((D p_x) t) (((∂ 0) V) (x t) (y t)))
            (+ ((D p_y) t) (((∂ 1) V) (x t) (y t)))))

         (with-literal-functions [x y p_x p_y [V [0 1] 2]]
           (simplify (((Hamilton-equations
                        (H-rectangular
                         'm V))
                       (coordinate-tuple x y)
                       (momentum-tuple p_x p_y))
                      't)))))
  (is (= '(/
           (+
            (* 2N (V r) m (expt r 2))
            (* (expt p_r 2) (expt r 2))
            (expt p_phi 2))
           (* 2N m (expt r 2)))
         (with-literal-functions [[V [0 1] 2]]
           (simplify
            ((Lagrangian->Hamiltonian
              (L-central-polar 'm (literal-function 'V)))
             (->H-state 't
                        (coordinate-tuple 'r 'phi)
                        (momentum-tuple 'p_r 'p_phi)))))))
  (is (= '(up 0
              (up (/ (+ (* ((D r) t) m) (* -1N (p_r t))) m)
                  (/ (+ (* (expt (r t) 2) ((D phi) t) m) (* -1N (p_phi t))) (* (expt (r t) 2) m)))
              (down (/ (+ (* (expt (r t) 3) ((D p_r) t) m)
                          (* (expt (r t) 3) ((D V) (r t)) m)
                          (* -1N (expt (p_phi t) 2)))
                       (* (expt (r t) 3) m))
                    ((D p_phi) t)))
         (with-literal-functions [r phi p_r p_phi V]
           (simplify
            (((Hamilton-equations
               (Lagrangian->Hamiltonian
                (L-central-polar 'm V)))
              (coordinate-tuple r phi)
              (momentum-tuple p_r p_phi))
             't)))))
  (is (= '(up 0
              (up (/ (+ (* ((D r) t) m) (* -1N (p_r t))) m)
                  (/ (+ (* (expt (r t) 2) ((D phi) t) m)
                        (* -1N (p_phi t)))
                     (* (expt (r t) 2) m)))
              (down (/ (+ (* (expt (r t) 3) ((D p_r) t) m)
                          (* (r t) GM (expt m 2))
                          (* -1N (expt (p_phi t) 2)))
                       (* (expt (r t) 3) m))
                    ((D p_phi) t)))
         (with-literal-functions [r phi p_r p_phi]
           (simplify
            (((Hamilton-equations
               (Lagrangian->Hamiltonian
                (L-central-polar 'm
                                 (fn [r] (- (/ (* 'GM 'm) r))))))
              (coordinate-tuple r phi)
              (momentum-tuple p_r p_phi))
             't))))))

(deftest poisson
  (let [a-state (->H-state 't
                           (coordinate-tuple 'x 'y 'z)
                           (momentum-tuple 'p_x 'p_y 'p_z))]
    (is (= '(up (down 1 0 0)
                (down 0 1 0)
                (down 0 0 1))
           (simplify
            ((Poisson-bracket
              (up (comp (component 0) coordinate)
                  (comp (component 1) coordinate)
                  (comp (component 2) coordinate))
              (down (comp (component 0) momentum)
                    (comp (component 1) momentum)
                    (comp (component 2) momentum)))
             a-state))))
    (with-literal-functions [[FF (up 0 (up 1 2) (down 3 4)) 5]
                             [GG (up 0 (up 1 2) (down 3 4)) 5]]
      (is (= '(FF (up t (up x y) (down pa pb)))
             (simplify (FF (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(down
               (((∂ 0) FF) (up t (up x y) (down pa pb)))
               (down
                (((∂ 1 0) FF) (up t (up x y) (down pa pb)))
                (((∂ 1 1) FF) (up t (up x y) (down pa pb))))
               (up
                (((∂ 2 0) FF) (up t (up x y) (down pa pb)))
                (((∂ 2 1) FF) (up t (up x y) (down pa pb)))))
             (simplify ((D FF) (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(+
               (*
                -1
                (((∂ 2 0) FF) (up t (up x y) (down p_x p_y)))
                (((∂ 1 0) GG) (up t (up x y) (down p_x p_y))))
               (*
                -1
                (((∂ 2 1) FF) (up t (up x y) (down p_x p_y)))
                (((∂ 1 1) GG) (up t (up x y) (down p_x p_y))))
               (*
                (((∂ 1 0) FF) (up t (up x y) (down p_x p_y)))
                (((∂ 2 0) GG) (up t (up x y) (down p_x p_y))))
               (*
                (((∂ 1 1) FF) (up t (up x y) (down p_x p_y)))
                (((∂ 2 1) GG) (up t (up x y) (down p_x p_y)))))
             (simplify
              ((* (D FF)
                  (Poisson-bracket identity identity)
                  (D GG))
               (up 't (up 'x 'y) (down 'p_x 'p_y)))))))))
