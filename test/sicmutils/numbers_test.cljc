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

(ns sicmutils.numbers-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.complex :as c]
            [sicmutils.util :as u]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numbers :as n]))

(def near (v/within 1e-12))

(deftest arithmetic
  #?(:clj
     (testing "with-ratios"
       (is (= 13/40 (g/add 1/5 1/8)))
       (is (= 1/8 (g/sub 3/8 1/4)))
       (is (= 5/4 (g/div 5 4)))
       (is (thrown? IllegalArgumentException (g/exact-divide 10/2 2/10)))
       (is (= 1 (g/exact-divide 2/10 2/10)))
       (is (= 1/2 (g/div 1 2)))
       (is (= 1/4 (reduce g/div [1 2 2])))
       (is (= 1/8 (reduce g/div [1 2 2 2])))
       (is (= 1/8 (g/invert 8)))))

  (testing "add"
    (is (= 4 (g/add 2 2)))
    (is (= 2 (g/add 2 0)))
    (is (= 3.5 (g/add 1.5 2)))
    (is (= 10 (reduce g/add [1 2 3 4]))))

  (testing "mul"
    (is (= 20 (g/mul 5 4)))
    (is (= 4 (g/mul 2 2)))
    (is (= 8 (reduce g/mul [2 2 2]))))

  (testing "sub"
    (is (= -4 (g/sub 0 4)))
    (is (= 2.14 (g/sub 3.14 1))))

  (testing "negate"
    (is (= -4 (g/negate 4)))
    (is (= 4 (g/negate (g/negate 4))))
    (is (= -4 (g/negate 4)))
    (is (= -4.2 (g/negate 4.2))))

  (testing "div"
    (is (= 5 (g/div 20 4)))
    (is (= -14 (reduce g/sub [10 9 8 7]))))

  (testing "invert"
    (is (= 1 (g/invert 1)))
    (is (= (g/div 1 21) (g/invert 21))))

  (testing "expt"
    (is (= 32 (g/expt 2 5))))

  (testing "abs"
    (is (= 1 (g/abs -1)))
    (is (= 1 (g/abs 1))))

  (testing "magnitude"
    (is (= 123 (g/magnitude -123)))
    (is (= 123 (g/magnitude 123))))

  (testing "square/cube"
    (is (= 4 (g/square 2)))
    (is (= 4 (g/square -2)))
    (is (= 27 (g/cube 3)))
    (is (= -27 (g/cube -3))))

  (testing "trig"
    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0))))

  (testing ">1 gets promoted to complex for asin, acos"
    (is (c/complex? (g/asin 2)))
    (is (c/complex? (g/acos 2))))

  (testing "sqrt handles negative numbers, 0"
    (is (= 0 (g/sqrt 0)))
    (is (= 9 (g/sqrt 81)))
    (is (c/complex? (g/sqrt -81)))
    (is (= (c/complex 0 9) (g/sqrt -81))))

  (testing "sqrt of one preserves type"
    (is (v/one-like (g/sqrt c/ONE)))
    (is (c/complex? (g/sqrt c/ONE))))

  (testing "log"
    (is (c/complex? (g/log -10)))
    (is (= 0 (g/log 1)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "exp"
    (is (= 1 (g/exp 0))))

  (testing "exp/log preserve exactness together"
    (is (= 0 (g/log (g/exp 0)))
        (= 10 (g/log (g/exp 10)))))

  (testing "negative?"
    (is (g/negative? (g/negate 4)))
    (is (not (g/negative? (g/negate (g/negate 4))))))

  (testing "quotient"
    (is (= 2 (g/quotient 5 2)))
    (is (= 2 (g/quotient 5N 2)))
    (is (= 2 (g/quotient 5 2N)))
    (is (= 2 (g/quotient 5N 2N))))

  #?(:clj
     (testing "bigint quotient"
       (is (= 2 (g/quotient (BigInteger/valueOf 5) 2)))
       (is (= 2 (g/quotient 5 (BigInteger/valueOf 2))))))

  (testing "remainder"
    (is (= 1 (g/remainder 5 2)))
    (is (= 1 (g/remainder 5N 2)))
    (is (= 1 (g/remainder 5 2N)))
    (is (= 1 (g/remainder 5N 2N)))))

;; Test of generic wrapper operations.

(deftest generic-plus
  "sicmutils.numbers provides implementations of the methods needed by g/+. Test
  that these functions now come work with numbers."
  (testing "simple"
    (is (= 7 (g/+ 3 4)))
    (is (= 4 (g/+ 2 2)))
    (is (= 3.5 (g/+ 1.5 2))))

  (testing "many"
    (is (= 10 (g/+ 1 2 3 4)))
    (is (= 33 (g/+ 3 4 5 6 7 8)))))

(deftest generic-minus
  "numbers provides implementations, so test behaviors."
  (is (= -3.14 (g/- 3.14)))
  (is (= 2.14 (g/- 3.14 1)))

  (testing "many"
    (is (= -14 (g/- 10 9 8 7)))))

(deftest generic-times
  "numbers provides implementations, so test behaviors."
  (is (= 20 (g/* 5 4)))
  (is (= 4 (g/* 2 2)))
  (is (= 8 (g/* 2 2 2))))

(deftest generic-divide
  "numbers provides implementations, so test behaviors."
  (is (= 5 (g/divide 20 4)))
  (is (= 2 (g/divide 8 2 2))))

#?(:clj
   (deftest with-ratios
     (is (= 13/40 (g/+ 1/5 1/8)))
     (is (= 1/8 (g/- 3/8 1/4)))
     (is (= 5/4 (g/divide 5 4)))
     (is (= 1/2 (g/divide 1 2)))
     (is (= 1/4 (g/divide 1 2 2)))
     (is (= 1/8 (g/divide 1 2 2 2)))))
