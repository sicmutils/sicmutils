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

(ns sicmutils.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.mechanics
             [hamilton :as H]
             [lagrange :as L]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-3-1-1
  ;; To move further into Hamiltonian mechanics, we will need
  ;; literal functions mapping structures to structures.

  (with-literal-functions
    [x y v_x v_y p_x p_y [V [1 2] 3]]
    (is (= '(V x y) (simplify (V 'x 'y))))
    (is (= '(up 0 (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m) (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t))) (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
           (simplify (((Hamilton-equations
                        (H/H-rectangular
                         'm V))
                       (up x y)
                       (down p_x p_y))
                      't))))
    (is (= '(/ (expt y 2) (* 4 c))
           (simplify ((Legendre-transform (fn [x] (* 'c x x))) 'y))))
    (is (= '(* 1/4 (expt p 2)) (simplify ((Legendre-transform square) 'p))))
    (is (= '(+ (* 1/2 m (expt v_x 2)) (* 1/2 m (expt v_y 2)) (* -1 (V x y)))
           (simplify ((L/L-rectangular 'm V) (up 't (up 'x 'y) (up 'v_x 'v_y))))))
    (is (= '(/ (+ (* 2 m (V x y)) (expt p_x 2) (expt p_y 2)) (* 2 m))
           (simplify ((Lagrangian->Hamiltonian
                       (L/L-rectangular 'm V))
                      (up 't (up 'x 'y) (down 'p_x 'p_y))))))))

(deftest gjs-tests
  (is (= '(up 0
              (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m) (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
              (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t))) (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))

         (with-literal-functions [x y p_x p_y [V [0 1] 2]]
           (simplify (((Hamilton-equations
                        (H/H-rectangular
                         'm V))
                       (coordinate-tuple x y)
                       (momentum-tuple p_x p_y))
                      't)))))
  (is (= '(/ (+ (* 2 m (expt r 2) (V r)) (* (expt p_r 2) (expt r 2)) (expt p_phi 2)) (* 2 m (expt r 2)))
         (with-literal-functions [[V [0 1] 2]]
           (simplify
            ((Lagrangian->Hamiltonian
              (L/L-central-polar 'm (literal-function 'V)))
             (->H-state 't
                        (coordinate-tuple 'r 'phi)
                        (momentum-tuple 'p_r 'p_phi)))))))
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t))) m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t))) (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t)) (* m (expt (r t) 3) ((D V) (r t))) (* -1 (expt (p_phi t) 2))) (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (with-literal-functions [r phi p_r p_phi V]
           (simplify
            (((Hamilton-equations
               (Lagrangian->Hamiltonian
                (L/L-central-polar 'm V)))
              (coordinate-tuple r phi)
              (momentum-tuple p_r p_phi))
             't)))))
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t))) m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t))) (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t)) (* GM (expt m 2) (r t)) (* -1 (expt (p_phi t) 2))) (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (with-literal-functions [r phi p_r p_phi]
           (simplify
            (((Hamilton-equations
               (Lagrangian->Hamiltonian
                (L/L-central-polar 'm
                                 (fn [r] (- (/ (* 'GM 'm) r))))))
              (coordinate-tuple r phi)
              (momentum-tuple p_r p_phi))
             't)))))
  (let [F (literal-function 'F (Hamiltonian 2))
        G (literal-function 'G (Hamiltonian 2))
        H (literal-function 'G (Hamiltonian 2))
        L_F (Lie-derivative F)
        L_G (Lie-derivative G)]
    (is (= 0 (simplify (((+ (commutator L_F L_G)
                            (Lie-derivative (Poisson-bracket F G)))
                         H)
                        (up 't (up 'x 'y) (down 'px 'py))))))))

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
    (with-literal-functions
      [[FF (up 0 (up 1 2) (down 3 4)) 5]
       [GG (up 0 (up 1 2) (down 3 4)) 5]
       [HH (up 0 (up 1 2) (down 3 4)) 5]]
      (is (= '(FF (up t (up x y) (down pa pb)))
             (simplify (FF (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(down
               (((partial 0) FF) (up t (up x y) (down pa pb)))
               (down
                (((partial 1 0) FF) (up t (up x y) (down pa pb)))
                (((partial 1 1) FF) (up t (up x y) (down pa pb))))
               (up
                (((partial 2 0) FF) (up t (up x y) (down pa pb)))
                (((partial 2 1) FF) (up t (up x y) (down pa pb)))))
             (simplify ((D FF) (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(+
               (*
                -1
                (((partial 2 0) FF) (up t (up x y) (down p_x p_y)))
                (((partial 1 0) GG) (up t (up x y) (down p_x p_y))))
               (*
                -1
                (((partial 2 1) FF) (up t (up x y) (down p_x p_y)))
                (((partial 1 1) GG) (up t (up x y) (down p_x p_y))))
               (*
                (((partial 1 0) FF) (up t (up x y) (down p_x p_y)))
                (((partial 2 0) GG) (up t (up x y) (down p_x p_y))))
               (*
                (((partial 1 1) FF) (up t (up x y) (down p_x p_y)))
                (((partial 2 1) GG) (up t (up x y) (down p_x p_y)))))
             (simplify
              ((* (D FF)
                  (Poisson-bracket identity identity)
                  (D GG))
               (up 't (up 'x 'y) (down 'p_x 'p_y))))))
      (testing "Jacobi identity"
        (is (= 0 (simplify ((+ (Poisson-bracket FF (Poisson-bracket GG HH))
                               (Poisson-bracket GG (Poisson-bracket HH FF))
                               (Poisson-bracket HH (Poisson-bracket FF GG)))
                            (up 't (up 'x 'y) (down 'p_x 'p_y))))))))))

(deftest symplectic
  (testing "unit"
    (is (= (matrix-by-rows [0 0 0 0 0 0 1 0 0 0 0 0]
                           [0 0 0 0 0 0 0 1 0 0 0 0]
                           [0 0 0 0 0 0 0 0 1 0 0 0]
                           [0 0 0 0 0 0 0 0 0 1 0 0]
                           [0 0 0 0 0 0 0 0 0 0 1 0]
                           [0 0 0 0 0 0 0 0 0 0 0 1]
                           [-1 0 0 0 0 0 0 0 0 0 0 0]
                           [0 -1 0 0 0 0 0 0 0 0 0 0]
                           [0 0 -1 0 0 0 0 0 0 0 0 0]
                           [0 0 0 -1 0 0 0 0 0 0 0 0]
                           [0 0 0 0 -1 0 0 0 0 0 0 0]
                           [0 0 0 0 0 -1 0 0 0 0 0 0])
           (symplectic-unit 6)))))

(deftest iterated-map-test
  (let [fail (constantly false)
        M (fn [x y cont fail] (if (> x 10) (fail) (cont (inc x) (dec y))))]
    (is (= '(6 95) ((iterated-map M 5) 1 100 list fail)))
    (is (= '(10 91) ((iterated-map M 9) 1 100 list fail)))
    (is (= false ((iterated-map M 20) 1 100 list fail )))))
