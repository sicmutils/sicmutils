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

(ns net.littleredcomputer.math.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math
             [generic :refer :all]
             [function :refer :all]
             [numbers]
             [simplify :refer [pe]]
             [expression :refer :all]
             [structure :refer :all]]
            [net.littleredcomputer.math.mechanics
             [hamilton :refer :all]
             [lagrange :refer [L-rectangular]]]))

(deftest section-3.1.1
  ;; To move further into Hamiltonian mechanics, we will need
  ;; literal functions mapping structures to structures.
  (with-literal-functions [x y v_x v_y p_x p_y [V [1 2] 3]]
    (is (= '(V x y) (simplify (V 'x 'y))))
    (is (= '(up 0
                (up (+ (* -2 (/ 1 (* 2 m)) (p_x t)) ((D x) t))
                    (+ (* -2 (/ 1 (* 2 m)) (p_y t)) ((D y) t)))
                (down (+ ((D p_x) t) (((partial-derivative 0) V) (x t) (y t)))
                      (+ ((D p_y) t) (((partial-derivative 1) V) (x t) (y t)))))
           (simplify (((Hamilton-equations
                        (H-rectangular
                         'm V))
                       (up x y)
                       (down p_x p_y))
                      't))))
    ;; this works out to y^2 / 4c, which we expect. But at this point
    ;; our inability to simplify things with fractions is causing
    ;; some trouble. Going further into the Hamilton material is
    ;; going to need simplification that can handle rational functions.
    (is (= '(+ (* -1 (expt (/ y (* 2 c)) 2) c)
               (* (/ y (* 2 c)) y))
           (simplify ((Legendre-transform (fn [x] (* 'c x x))) 'y))))
    (is (= '(+ (* 1/2 m (expt v_x 2))
               (* 1/2 m (expt v_y 2))
               (* -1 (V x y)))
           (simplify ((L-rectangular 'm V) (up 't (up 'x 'y) (up 'v_x 'v_y))))))
    ;; correct, modulo the lame simplification that happens because we don't
    ;; simplify fractions yet.
    (is (= '(+ (* -1/2 (expt (/ m (expt m 2)) 2) m (expt p_x 2))
               (* -1/2 (expt (/ m (expt m 2)) 2) m (expt p_y 2))
               (* (/ m (expt m 2)) (expt p_x 2))
               (* (/ m (expt m 2)) (expt p_y 2))
               (V x y))
           (simplify ((Lagrangian->Hamiltonian
                       (L-rectangular 'm V))
                      (up 't (up 'x 'y) (down 'p_x 'p_y))))))))
