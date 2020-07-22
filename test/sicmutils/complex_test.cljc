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
            [sicmutils.numbers]
            [sicmutils.complex :as c]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(defn ^:private near [w z]
  (< (g/abs (g/- w z)) 1e-12))

(deftest value-protocol
  (testing "v/Value protocol implementation"
    (is (v/nullity? c/ZERO))
    (is (v/nullity? (c/complex 0.0)))
    (is (not (v/nullity? c/ONE)))
    (is (not (v/nullity? (c/complex 1.0))))
    (is (v/nullity? (v/zero-like (c/complex 100))))
    (is (= c/ZERO (v/zero-like (c/complex 2))))
    (is (= c/ZERO (v/zero-like (c/complex 0 3.14))))

    (is (v/unity? c/ONE))
    (is (v/unity? (c/complex 1.0)))
    (is (v/unity? (v/one-like c/ZERO)))
    (is (not (v/unity? (c/complex 2))))
    (is (not (v/unity? (c/complex 0.0))))

    (is (= '(complex 10.0 0.0) (v/freeze (c/complex 10))))
    (is (v/numerical? (c/complex 10)))

    (testing "exact?"
      (is (not (v/exact? (c/complex 0 10.1))))

      ;; cljs is able to maintain exact numbers here.
      #?@(:clj
          [(is (not (v/exact? (c/complex 10))))
           (is (not (v/exact? (c/complex 10 12))))]

          :cljs
          [(is (v/exact? (c/complex 10)))
           (is (v/exact? (c/complex 10 12)))]))))

(let [i (c/complex 0 1)
      pi Math/PI]
  (deftest complex-numbers
    (testing "complex constructor and predicate"
      (is (c/complex? c/ONE))
      (is (c/complex? (c/complex 0 1)))
      (is (c/complex? (c/complex 2)))
      (is (not (c/complex? 4))))

    (testing "add"
      (is (= (c/complex 4 6) (g/add (c/complex 1 2) (c/complex 3 4))))
      (is (= (c/complex 1 3) (g/add (c/complex 0 3) 1)))
      (is (= (c/complex 1 3)
             (g/add 1 (c/complex 0 3))
             (g/add (c/complex 0 3) 1))))

    (testing "sub"
      (is (= (c/complex -2 -2) (g/sub (c/complex 1 2)
                                      (c/complex 3 4))))
      (is (= (c/complex 10 2) (g/sub (c/complex 20 2) 10)))
      (is (= (g/negate (c/complex 10 2))
             (g/sub 10 (c/complex 20 2)))))

    (testing "mul between numbers and complex numbers in both orders"
      ;; rotate 7 by pi/2
      (is (near (g/mul i 7) (g/mul 7 (g/exp (g/mul i (/ pi 2))))))
      (is (near (c/complex 0 7) (g/mul (c/complex 7) (g/exp (g/mul i (/ pi 2)))))))

    (testing "div in either order"
      (is (= (c/complex 0 -1) (g/div 1 i)))
      (is (= (c/complex 2 2) (g/div (c/complex 4 4) 2)))
      )

    (testing "expt"
      (is (near -1 (g/expt (c/complex 0 1) 2)))
      (is (near (c/complex 16) (g/expt 2 (c/complex 4))))
      (is (near (c/complex 16) (g/expt (c/complex 2) (c/complex 4)))))

    (testing "negate"
      (is (= (c/complex -10 2)
             (g/negate (c/complex 10 -2)))))

    (testing "invert"
      (is (v/nullity? (g/add i (g/invert i)))))

    (testing "abs"
      (is (= 5.0 (g/abs (c/complex 3 4)))))

    (testing "exp"
      ;; Euler identity
      (is (near (c/complex -1) (g/exp (g/mul i pi)))))

    (testing "log"
      (is (= (g/mul i pi) (g/log (g/exp (g/mul i pi))))))

    (testing "square"
      (is (near (g/mul i 200) (g/square (c/complex 10 10)))))

    (testing "cube"
      (is (near (c/complex 0 -8) (g/cube (g/* 2 i)))))

    (testing "sqrt"
      (is (near (c/complex 10 10) (g/sqrt (g/mul i 200)))))

    (testing "sin"
      (is (near (g/sin (c/complex 10))
                (Math/sin 10))))

    (testing "cos"
      (is (near (g/cos (c/complex 10))
                (Math/cos 10))))

    (testing "tan"
      (is (near (g/tan (c/complex 10))
                (Math/tan 10))))

    (testing "asin"
      (is (near (g/asin (c/complex 1.1))
                (c/complex 1.57079632679489 -0.443568254385115))))

    (testing "acos"
      (is (near (g/acos (c/complex 1.1))
                (c/complex 0 0.4435682543851153))))

    (testing "atan"
      (is (near (g/atan (c/complex 1.1))
                (c/complex 0.8329812666744317 0.0))))

    (testing "arithmetic"
      (is (g/numerical-quantity? i)))))

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
