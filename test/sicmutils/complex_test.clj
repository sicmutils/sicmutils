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

(ns sicmutils.complex-test
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :as g]
             [complex :refer :all]]))

(defn ^:private near [w z]
  (< (g/abs (g/- w z)) 1e-12))

(deftest complex-numbers
  (testing "arithmetic"
    (let [i (complex 0 1)
          pi Math/PI]
      (is (g/numerical-quantity? i))
      (is (= (complex 4 6) (g/+ (complex 1 2) (complex 3 4))))
      (is (= (complex -2 -2) (g/- (complex 1 2) (complex 3 4))))
      (is (= 5.0 (g/abs (complex 3 4))))
      (is (g/zero? (g/+ i (g/invert i))))
      (is (near (complex -1) (g/exp (g/* i pi)))) ;; Euler identity
      (is (near (complex 0 -8) (g/cube (g/* 2 i))))
      (is (= (complex 0 -1) (g/divide 1 i)))
      (is (= (complex 2 2) (g/divide (complex 4 4) 2)))
      (is (= (complex 1 3) (g/+ (complex 0 3) 1)))
      (is (near (complex 1.57079632679489 -0.443568254385115)
                (g/asin (complex 1.1))))
      (is (near -1 (g/expt (complex 0 1) 2))))))

(deftest promotions-from-real
  (is (= (complex 0 1) (g/sqrt -1)))
  (is (near (complex 1.57079632679489 -0.443568254385115) (g/asin 1.1)))
  (is (near (complex 0 0.4435682543851153) (g/acos 1.1)))
  (is (near (complex 0 Math/PI) (g/log -1))))


(deftest extra-functions
  (testing "functions needed for docs"
    (is (near (real-part (complex 3 4)) 3))
    (is (near (imag-part (complex 3 4)) 4))
    (is (near (imag-part (conjugate (complex 3 4))) -4))
    (is (near (g/magnitude (complex 0 1)) 1))
    (is (near (g/magnitude (complex 1 0)) 1))
    (is (near (g/magnitude (complex 1 1)) (g/sqrt 2)))
    (is (near (angle (complex 3 4)) (g/atan 4/3)))))

