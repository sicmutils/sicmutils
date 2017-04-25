;
; Copyright (C) 2017 Colin Smith.
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

(ns sicmutils.calculus.manifold-test
  (:refer-clojure :exclude [* - / + ref zero? partial])
  (:require
    [clojure.test :refer :all]
    [sicmutils
     [env :refer :all]
     [value :as v]]
    ;; objects in manifold ns will need to graduate to env
    [sicmutils.calculus.manifold :refer :all]))

(defn ^:private near
  [p q]
  ((v/within 1e-12) 0 (sqrt (square (- p q)))))

(deftest coordinate-systems
  (testing "R2"
    (testing "Rect"
      (testing "check-coordinates"
        (is (not (check-coordinates R2-rect (up 1))))
        (is (check-coordinates R2-rect (up 1 2)))
        (is (not (check-coordinates R2-rect (up 1 2 3))))
        (is (thrown? UnsupportedOperationException (check-coordinates R2-rect 99))))
      (testing "coords->point"
        (let [p (coords->point R2-rect (up 3 4))]
          ;; the throw continuation is meant to assert that the thunk is
          ;; not called when retrieving the coordinates from the system
          ;; with which the manifold-point was created.
          (is (= (up 3 4) (get-coordinates p R2-rect #(throw (Exception.)))))
          (is (check-point R2-rect p)))))
    (testing "Polar"
      (testing "polar check-coordinates"
        (is (not (check-coordinates R2-polar (up 1))))
        (is (check-coordinates R2-polar (up 1 2)))
        (is (not (check-coordinates R2-polar (up 1 2 3))))
        (is (not (check-coordinates R2-polar 99))))
      (testing "coords->point"
        (let [p (coords->point R2-polar (up 1 2))]
          (is (= (up 1 2) (get-coordinates p R2-polar #(throw (Exception.))))))))
    (testing "Rect <-> Polar"
      (let [Pr (coords->point R2-rect (up (Math/sqrt 2) (Math/sqrt 2)))
            xy (coords->point R2-rect (up 'x 'y))
            rt (coords->point R2-polar (up 'ρ 'θ))]
        (is (near (up 2 (/ Math/PI 4)) (point->coords R2-polar Pr)))
        (is (= (up 'ρ 'θ) (point->coords R2-polar rt)))
        (is (= (up (sqrt (+ (square 'x) (square 'y)))
                   (atan 'y 'x)) (point->coords R2-polar xy)))
        (is (= (up (* 'ρ (cos 'θ)) (* 'ρ (sin 'θ))) (point->coords R2-rect rt)))
        (is (= (up 'x 'y) (point->coords R2-rect xy)))))))

