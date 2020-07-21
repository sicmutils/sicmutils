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

(ns sicmutils.complex-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.complex :as c]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(defn ^:private near [w z]
  (< (g/abs (g/- w z)) 1e-12))

(deftest complex-numbers
  (testing "arithmetic"
    (let [i (c/complex 0 1)
          pi Math/PI]
      (is (g/numerical-quantity? i))
      (is (= (c/complex 4 6) (g/+ (c/complex 1 2) (c/complex 3 4))))
      (is (= (c/complex -2 -2) (g/- (c/complex 1 2) (c/complex 3 4))))
      (is (= 5.0 (g/abs (c/complex 3 4))))
      (is (v/nullity? (g/+ i (g/invert i))))
      (is (near (c/complex -1) (g/exp (g/* i pi)))) ;; Euler identity
      (is (near (c/complex 0 -8) (g/cube (g/* 2 i))))
      (is (= (c/complex 0 -1) (g/divide 1 i)))
      (is (= (c/complex 2 2) (g/divide (c/complex 4 4) 2)))
      (is (= (c/complex 1 3) (g/+ (c/complex 0 3) 1)))
      (is (near (c/complex 1.57079632679489 -0.443568254385115)
                (g/asin (c/complex 1.1))))
      (is (near -1 (g/expt (c/complex 0 1) 2))))))

(deftest promotions-from-real
  (is (= (c/complex 0 1) (g/sqrt -1)))
  (is (near (c/complex 1.57079632679489 -0.443568254385115) (g/asin 1.1)))
  (is (near (c/complex 0 0.4435682543851153) (g/acos 1.1)))
  (is (near (c/complex 0 Math/PI) (g/log -1))))


(deftest extra-functions
  (testing "functions needed for docs"
    (is (near (c/real-part (c/complex 3 4)) 3))
    (is (near (c/imag-part (c/complex 3 4)) 4))
    (is (near (c/imag-part (c/conjugate (c/complex 3 4))) -4))
    (is (near (g/magnitude (c/complex 0 1)) 1))
    (is (near (g/magnitude (c/complex 1 0)) 1))
    (is (near (g/magnitude (c/complex 1 1)) (g/sqrt 2)))

    ;; This looks awkward in cljs due to the ratio literal.
    (is (near (c/angle (c/complex 3 4))
              (g/atan #?(:clj 4/3 :cljs (/ 4 3)))))))
