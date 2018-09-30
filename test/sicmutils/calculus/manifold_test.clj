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

(ns sicmutils.calculus.manifold-test
  (:refer-clojure :exclude [* - / + ref zero? partial])
  (:require
    [clojure.test :refer :all]
    [sicmutils
     [env :refer :all]
     [value :as v]
     [generic :as g]
     [simplify :refer [hermetic-simplify-fixture]]]
    [sicmutils.calculus.manifold :as m]))

(use-fixtures :once hermetic-simplify-fixture)

(defn ^:private near
  [p q]
  ((v/within 1e-12) 0 (sqrt (square (- p q)))))

(deftest coordinate-systems
  (testing "R2"
    (testing "Rect"
      (testing "check-coordinates"
        (is (m/check-coordinates R1-rect 1))
        (is (m/check-coordinates R1-rect (up 1)))
        (is (not (m/check-coordinates R2-rect (up 1))))
        (is (m/check-coordinates R2-rect (up 1 2)))
        (is (not (m/check-coordinates R2-rect (up 1 2 3))))
        (is (not (m/check-coordinates R2-rect 99))))
      (testing "coords->point"
        (let [p (m/coords->point R2-rect (up 3 4))
              Tp (m/coords->point R1-rect 3)]
          ;; the throw continuation is meant to assert that the thunk is
          ;; not called when retrieving the coordinates from the system
          ;; with which the manifold-point was created.
          (is (= (up 3 4) (m/get-coordinates p R2-rect #(throw (Exception.)))))
          (is (m/check-point R2-rect p))
          (is (m/check-point R1-rect Tp))
          (is (= 99 (m/point->coords R1-rect (m/coords->point R1-rect 99))))
          (is (= ::m/manifold-point (:type p)))
          (is (not (g/numerical? p))))))
    (testing "Polar"
      (testing "polar m/check-coordinates"
        (is (not (m/check-coordinates R2-polar (up 1))))
        (is (m/check-coordinates R2-polar (up 1 2)))
        (is (not (m/check-coordinates R2-polar (up 1 2 3))))
        (is (not (m/check-coordinates R2-polar 99))))
      (testing "m/coords->point"
        (let [p (m/coords->point R2-polar (up 1 2))]
          (is (= (up 1 2) (m/get-coordinates p R2-polar #(throw (Exception.))))))))
    (testing "Rect <-> Polar"
      (let [Pr (m/coords->point R2-rect (up (Math/sqrt 2) (Math/sqrt 2)))
            xy (m/coords->point R2-rect (up 'x 'y))
            rt (m/coords->point R2-polar (up 'ρ 'θ))]
        (is (near (up 2 (/ Math/PI 4)) (m/point->coords R2-polar Pr)))
        (is (= (up 'ρ 'θ) (m/point->coords R2-polar rt)))
        (is (= (up (sqrt (+ (square 'x) (square 'y)))
                   (atan 'y 'x)) (m/point->coords R2-polar xy)))
        (is (= (up (* 'ρ (cos 'θ)) (* 'ρ (sin 'θ))) (m/point->coords R2-rect rt)))
        (is (= (up 'x 'y) (m/point->coords R2-rect xy)))))
    (testing "SO(3)"
      (is (= '(up (asin (* (sin theta) (cos psi)))
                  (atan (+ (* (cos psi) (cos theta) (sin phi))
                           (* (cos phi) (sin psi)))
                        (+ (* (cos psi) (cos phi) (cos theta))
                           (* -1 (sin psi) (sin phi))))
                  (atan (* -1 (sin theta) (sin psi)) (cos theta)))
             (simplify ((compose (chart alternate-angles)
                                 (point Euler-angles))
                        (up 'theta 'phi 'psi)))))
      (is (= '(up theta phi psi)
             (simplify ((compose (chart Euler-angles)
                                 (point alternate-angles)
                                 (chart alternate-angles)
                                 (point Euler-angles))
                        (up 'theta 'phi 'psi))))))))
