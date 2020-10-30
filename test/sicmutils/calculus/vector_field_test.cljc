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

(ns sicmutils.calculus.vector-field-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.coordinate :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.calculus.manifold :as m :refer [R2-rect R3-cyl R3-rect]]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [cos sin + - * /]]
            [sicmutils.operator :as o]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest vectorfield
  (testing "literal"
    (let [f (m/literal-manifold-function 'f-rect R2-rect)
          v (vf/literal-vector-field 'b R2-rect)
          R2-rect-chi-inverse (m/point R2-rect)
          p (R2-rect-chi-inverse (up 'x0 'y0))]
      (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
                 (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
             (g/simplify ((v f) p))))
      (is (= '(up (b↑0 (up x0 y0)) (b↑1 (up x0 y0)))
             (g/simplify ((v (m/chart R2-rect)) p))))
      (is (= ::vf/vector-field (v/kind v)))))

  (testing "exponential"
    (let-coordinates [[x y] R2-rect]
      (let [circular (- (* x d:dy) (* y d:dx))]
        (is (= '(up (+ (* (/ -1 720) (expt a 6))
                       (* (/ 1 24) (expt a 4))
                       (* (/ -1 2) (expt a 2))
                       1)
                    (+ (* (/ 1 120) (expt a 5))
                       (* (/ -1 6) (expt a 3))
                       a))
               (v/freeze
                (g/simplify
                 ((((vf/evolution 6) 'a circular) (m/chart R2-rect))
                  ((m/point R2-rect) (up 1 0))))))))))

  (testing "naming"
    (let-coordinates [[x y] R2-rect]
      (is (= 0 ))))

  (testing "gjs-examples"
    (let-coordinates [[x y z] R3-rect]
      (is (= '(+ (* -1 a b (cos a) (cos b)) (* -2 a (cos a) (sin b)))
             (g/simplify (((* (g/expt d:dy 2) x y d:dx) (* (sin x) (cos y)))
                          ((m/point R3-rect) (up 'a 'b 'c))))))
      (let [counter-clockwise (- (* x d:dy) (* y d:dx))
            outward (+ (* x d:dx) (* y d:dy))
            mr ((m/point R3-rect) (up 'x0 'y0 'z0))]
        (is (= 0 (g/simplify ((counter-clockwise
                               (g/sqrt (+ (g/square x)
                                          (g/square y)))) mr))))
        (is (= '(+ (expt x0 2) (* -1 (expt y0 2)))
               (g/simplify ((counter-clockwise (* x y)) mr))))
        (is (= '(* 2 x0 y0) (g/simplify ((outward (* x y)) mr))))))

    (let-coordinates [[r theta zeta] R3-cyl
                      [x y z] R3-rect]
      (testing "McQuistan"
        (let [A (+ (* 'A_r d:dr) (* 'A_theta d:dtheta) (* 'A_z d:dzeta))
              p ((m/point R3-rect) (up 'x 'y 'z))]
          ;; TODO: simplification isn't all it could be here (?)
          (is (= '(up (/ (+ (* -1N A_theta y (sqrt (+ (expt x 2) (expt y 2)))) (* A_r x)) (sqrt (+ (expt x 2) (expt y 2))))
                      (/ (+ (* A_theta x (sqrt (+ (expt x 2) (expt y 2)))) (* A_r y)) (sqrt (+ (expt x 2) (expt y 2))))
                      A_z)
                 (g/simplify ((vf/vector-field->components A R3-rect) (up 'x 'y 'z)))))
          (is (= '(up (* -1 y) x 0) (g/simplify ((d:dtheta (up x y z)) p))))
          (is (= '(up (/ x (sqrt (+ (expt x 2) (expt y 2))))
                      (/ y (sqrt (+ (expt x 2) (expt y 2))))
                      0)
                 (g/simplify ((d:dr (up x y z)) p))))
          (is (= '(up 0 0 1) (g/simplify ((d:dzeta (up x y z)) p))))
          (is (= '(up 0 0 1) (g/simplify ((d:dz (up x y z)) p)))) ;; suspicious. GJS has d:dz but I think d:dzeta was meant here (as above)
          ;; "so introduce..."
          (let [e-theta (* (/ 1 r) d:dtheta)
                e-r d:dr
                e-z d:dzeta
                A (+ (* 'A_r e-r) (* 'A_theta e-theta) (* 'A_z e-z))]
            (is (= '(up (/ (+ (* A_r x) (* -1 A_theta y)) (sqrt (+ (expt x 2) (expt y 2))))
                        (/ (+ (* A_r y) (* A_theta x)) (sqrt (+ (expt x 2) (expt y 2))))
                        A_z)
                   (g/simplify ((vf/vector-field->components A R3-rect) (up 'x 'y 'z))))))))
      (is (= (up 0 1 0) ((vf/vector-field->components d:dy R3-rect) (up 'x0 'y0 'z0))))
      (is (= (up 0 1 0) ((vf/vector-field->components d:dy R3-rect) (up 'r0 'theta0 'z0))))
      (is (= (up 1. 0 0) ((vf/vector-field->components d:dy R3-cyl) (up 1 (/ Math/PI 2) 0))))
      (is (= (up 0 1 0) ((vf/vector-field->components d:dy R3-cyl) (up 1 0 0))))
      (is (= '(up (sin theta0) (/ (cos theta0) r0) 0)
             (g/simplify ((vf/vector-field->components d:dy R3-cyl) (up 'r0 'theta0 'z)))))

      (testing "coordinatize"
        (let [coordinatize (fn [sfv coordsys]
                             (let [v (fn [f]
                                       (fn [x]
                                         (let [b (f/compose (sfv (m/chart coordsys))
                                                            (m/point coordsys))]
                                           (* ((D f) x) (b x)))))]
                               (o/make-operator v 'coordinatize)))]
          (is (= '(+ (* (((partial 0) f) (up x0 y0 z0)) (v↑0 (up x0 y0 z0)))
                     (* (((partial 1) f) (up x0 y0 z0)) (v↑1 (up x0 y0 z0)))
                     (* (((partial 2) f) (up x0 y0 z0)) (v↑2 (up x0 y0 z0))))
                 (g/simplify (((coordinatize (vf/literal-vector-field 'v R3-rect) R3-rect)
                               (f/literal-function 'f (up 1 2 3) 1))
                              (up 'x0 'y0 'z0))))))))))
