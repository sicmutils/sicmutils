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
  (:refer-clojure :exclude [* - / +])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [cos sin * - / +]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest coordinate-systems
  (testing "R2"
    (testing "Rect"
      (testing "check-coordinates"
        (is (m/check-coordinates m/R1-rect 1))
        (is (m/check-coordinates m/R1-rect (up 1)))
        (is (not (m/check-coordinates m/R2-rect (up 1))))
        (is (m/check-coordinates m/R2-rect (up 1 2)))
        (is (not (m/check-coordinates m/R2-rect (up 1 2 3))))
        (is (not (m/check-coordinates m/R2-rect 99))))

      (testing "coords->point"
        (let [p (m/coords->point m/R2-rect (up 3 4))
              Tp (m/coords->point m/R1-rect 3)]
          ;; the throw continuation is meant to assert that the thunk is
          ;; not called when retrieving the coordinates from the system
          ;; with which the manifold-point was created.
          (is (= (up 3 4) (m/get-coordinates p m/R2-rect #(u/exception ""))))
          (is (m/check-point m/R2-rect p))
          (is (m/check-point m/R1-rect Tp))
          (is (= 99 (m/point->coords m/R1-rect (m/coords->point m/R1-rect 99))))
          (is (= ::m/manifold-point (v/kind p)))
          (is (not (v/numerical? p))))))

    (testing "Polar"
      (testing "polar m/check-coordinates"
        (is (not (m/check-coordinates m/R2-polar (up 1))))
        (is (m/check-coordinates m/R2-polar (up 1 2)))
        (is (not (m/check-coordinates m/R2-polar (up 1 2 3))))
        (is (not (m/check-coordinates m/R2-polar 99))))

      (testing "m/coords->point"
        (let [p (m/coords->point m/R2-polar (up 1 2))]
          (is (= (up 1 2) (m/get-coordinates p m/R2-polar #(u/exception "")))))))

    (testing "Rect <-> Polar"
      (let [Pr (m/coords->point m/R2-rect (up (Math/sqrt 2) (Math/sqrt 2)))
            xy (m/coords->point m/R2-rect (up 'x 'y))
            rt (m/coords->point m/R2-polar (up 'ρ 'θ))]
        (is (= (up 'x 'y) (m/point->coords m/R2-rect xy)))
        (is (ish? (up 2 (/ Math/PI 4)) (m/point->coords m/R2-polar Pr)))
        (is (= (up 'ρ 'θ) (m/point->coords m/R2-polar rt)))
        (is (= (up (g/sqrt (+ (g/square 'x) (g/square 'y)))
                   (g/atan 'y 'x)) (m/point->coords m/R2-polar xy)))
        (is (= (up (* 'ρ (cos 'θ)) (* 'ρ (sin 'θ))) (m/point->coords m/R2-rect rt)))))

    (testing "SO(3)"
      (is (= '(up theta phi psi)
             (v/freeze
              (g/simplify
               ((f/compose (m/chart m/Euler-angles)
                           (m/point m/alternate-angles)
                           (m/chart m/alternate-angles)
                           (m/point m/Euler-angles))
                (up 'theta 'phi 'psi))))))

      (is (= '(up (asin (* (sin theta) (cos psi)))
                  (atan (+ (* (cos psi) (sin phi) (cos theta))
                           (* (cos phi) (sin psi)))
                        (+ (* (cos psi) (cos phi) (cos theta))
                           (* -1 (sin psi) (sin phi))))
                  (atan (* -1 (sin theta) (sin psi)) (cos theta)))
             (v/freeze
              (g/simplify
               ((f/compose (m/chart m/alternate-angles)
                           (m/point m/Euler-angles))
                (up 'theta 'phi 'psi)))))))))
