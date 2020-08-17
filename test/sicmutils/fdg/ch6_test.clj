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

(ns sicmutils.fdg.ch6-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-6-3
  (testing "walking on a sphere"
    (let-coordinates [[theta phi] S2-spherical
                      t R1-rect]
      (let [S2-basis (coordinate-system->basis S2-spherical)
            mu (compose (point S2-spherical)
                        (up (literal-function 'theta)
                            (literal-function 'phi))
                        (chart R1-rect))
            S2-basis-over-mu (basis->basis-over-map mu S2-basis)
            h (literal-manifold-function 'h-spherical S2-spherical)]
        (is (= '(down
                 (((partial 0) h-spherical) (up (theta t0) (phi t0)))
                 (((partial 1) h-spherical) (up (theta t0) (phi t0))))
               (simplify (((basis->vector-basis S2-basis-over-mu) h)
                          ((point R1-rect) 't0)))))
        (is (= '(up (down 1 0)
                    (down 0 1))
               (simplify (((basis->oneform-basis S2-basis-over-mu)
                           (basis->vector-basis S2-basis-over-mu))
                          ((point R1-rect) 't0)))))
        (is (= '(up ((D theta) t0) ((D phi) t0))
               (simplify (((basis->oneform-basis S2-basis-over-mu)
                           ((differential mu) d:dt))
                          ((point R1-rect) 't0)))))))))

(deftest section-6-4
  (let [mu (literal-manifold-map 'MU R2-rect R3-rect)
        f (literal-manifold-function 'f-rect R3-rect)
        X (literal-vector-field 'X-rect R2-rect)]
    (is (zero?
         (simplify (((- ((pullback mu) (d f)) (d ((pullback mu) f))) X)
                    ((point R2-rect) (up 'x0 'y0))))))
    (let [theta (literal-oneform-field 'THETA R3-rect)
          Y (literal-vector-field 'Y-rect R2-rect)]
      (is (zero?
           (simplify (((- ((pullback mu) (d theta))
                          (d ((pullback mu) theta)))
                       X Y)
                      ((point R2-rect) (up 'x0 'y0)))))))))
