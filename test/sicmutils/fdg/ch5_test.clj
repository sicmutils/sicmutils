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

(ns sicmutils.fdg.ch5-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-5-1
  (let-coordinates [[x y z] R3-rect
                    [r theta z] R3-cyl]
    (let [p ((point R3-rect) (up 'X 'Y 'Z))
          q ((point R3-cyl) (up 'R 'THETA 'Z))
          u (+ (* 'u↑0 d:dx) (* 'u↑1 d:dy))
          v (+ (* 'v↑0 d:dx) (* 'v↑1 d:dy))
          a (+ (* 'a↑0 d:dr) (* 'a↑1 d:dtheta))
          b (+ (* 'b↑0 d:dr) (* 'b↑1 d:dtheta))]
      (is (= '(+ (* u↑0 v↑1) (* -1 u↑1 v↑0))
             (simplify-and-freeze
              (((wedge dx dy) u v) p))))
      (is (= '(+ (* a↑0 b↑1) (* -1 a↑1 b↑0))
             (simplify-and-freeze
              (((wedge dr dtheta) a b) q)))))
    (let [p ((point R3-rect) (up 'X 'Y 'Z))
          u (+ (* 'u↑0 d:dx) (* 'u↑1 d:dy) (* 'u↑2 d:dz))
          v (+ (* 'v↑0 d:dx) (* 'v↑1 d:dy) (* 'v↑2 d:dz))
          w (+ (* 'w↑0 d:dx) (* 'w↑1 d:dy) (* 'w↑2 d:dz))
          ]
      (is (= '(+
               (* u↑0 v↑1 w↑2)
               (* -1 u↑0 v↑2 w↑1)
               (* -1 u↑1 v↑0 w↑2)
               (* u↑1 v↑2 w↑0)
               (* u↑2 v↑0 w↑1)
               (* -1 u↑2 v↑1 w↑0))
             (simplify-and-freeze
              (((wedge dx dy dz) u v w) p))))
      (is (= 0 (simplify
                (- (((wedge dx dy dz) u v w) p)
                   (determinant
                    (matrix-by-rows '[u↑0 u↑1 u↑2]
                                    '[v↑0 v↑1 v↑2]
                                    '[w↑0 w↑1 w↑2]))))))
      (is (= 1 (((wedge dx dy dz) d:dx d:dy d:dz) p))))))

(deftest section-5-2
  (let-coordinates [[x y z] R3-rect]
    (let [a (literal-manifold-function 'α R3-rect)
          b (literal-manifold-function 'β R3-rect)
          c (literal-manifold-function 'γ R3-rect)
          θ (+ (* a dx) (* b dy) (* c dz))
          X (literal-vector-field 'X-rect R3-rect)
          Y (literal-vector-field 'Y-rect R3-rect)
          p ((point R3-rect) (up 'x0 'y0 'z0))
          ω (+ (* a (wedge dy dz))
               (* b (wedge dz dx))
               (* c (wedge dx dy)))
          Z (literal-vector-field 'Z-rect R3-rect)]
      (is (= 0 (simplify
                (((- (d θ)
                     (+ (wedge (d a) dx)
                        (wedge (d b) dy)
                        (wedge (d c) dz)))
                  X Y)
                 p))))
      (is (= 0 (simplify
                (((- (d ω)
                     (+ (wedge (d a) dy dz)
                        (wedge (d b) dz dx)
                        (wedge (d c) dx dy)))
                  X Y Z)
                 p)) ))
      (is (= 0 (simplify (((d (d θ)) X Y Z) p)))))))

(deftest section-5-4
  (let [v (literal-vector-field 'v-rect R2-rect)
        w (literal-vector-field 'w-rect R2-rect)
        α (literal-function 'α (-> (UP Real Real) Real))
        β (literal-function 'β (-> (UP Real Real) Real))
        R2-rect-basis (coordinate-system->basis R2-rect)
        [dx dy] (basis->oneform-basis R2-rect-basis)
        p ((point R2-rect) (up 'x0 'y0))]
    (is (= 0 (simplify (((- (d (+ (* (compose α (chart R2-rect)) dx)
                                  (* (compose β (chart R2-rect)) dy)))
                            (* (compose (- ((∂ 0) β)
                                           ((∂ 1) α))
                                        (chart R2-rect))
                               (wedge dx dy)))
                         v w)
                        p)))))
  (let-coordinates [[x y z] R3-rect]
    (let [a (literal-manifold-function 'a-rect R3-rect)
          b (literal-manifold-function 'b-rect R3-rect)
          c (literal-manifold-function 'c-rect R3-rect)
          flux-through-boundary-element (+ (* a (wedge dy dz))
                                           (* b (wedge dz dx))
                                           (* c (wedge dx dy)))
          production-in-volume-element (* (+ (d:dx a) (d:dy b) (d:dz c))
                                          (wedge dx dy dz))
          X (literal-vector-field 'X-rect R3-rect)
          Y (literal-vector-field 'Y-rect R3-rect)
          Z (literal-vector-field 'Z-rect R3-rect)
          p ((point R3-rect) (up 'x0 'y0 'z0))]
      (is (= 0 (simplify (((- production-in-volume-element
                              (d flux-through-boundary-element))
                           X Y Z)
                          p)))))))
