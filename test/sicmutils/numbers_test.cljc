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
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numbers :as n]))

(def near (v/within 1e-12))

(deftest arithmetic
  #?(:clj
     (testing "with-ratios"
       (is (= 13/40 (g/+ 1/5 1/8)))
       (is (= 1/8 (g/- 3/8 1/4)))
       (is (= 5/4 (g/divide 5 4)))
       (is (= 1/8 (g/divide 8)))
       (is (= 1/2 (g/divide 1 2)))
       (is (= 1/4 (g/divide 1 2 2)))
       (is (= 1/8 (g/divide 1 2 2 2)))))

  #?(:clj
     (testing "bigint quotient"
       (is (= 2 (g/quotient (BigInteger/valueOf 5) 2)))
       (is (= 2 (g/quotient 5 (BigInteger/valueOf 2))))))

  (testing "with-numbers"
    (is (= 4 (g/+ 2 2)))
    (is (= 3.5 (g/+ 1.5 2)))
    (is (= 20 (g/* 5 4)))
    (is (= 5 (g/divide 20 4))))

  (testing "more-numbers"
    (is (= 10 (g/+ 1 2 3 4)))
    (is (= -14 (g/- 10 9 8 7)))
    (is (= 0 (g/+)))
    (is (= 3.14 (g/+ 3.14)))
    (is (= 1 (g/*)))
    (is (= 2 (g/* 2)))
    (is (= 4 (g/* 2 2)))
    (is (= 8 (g/* 2 2 2)))
    (is (= 1 (g/divide 1)))
    (is (= 2.14 (g/- 3.14 1))))

  (testing "trig"
    (is (near (/ (Math/PI) 4) (g/asin (/ (Math/sqrt 2) 2))))
    (is (near (/ (Math/PI) 4) (g/acos (/ (Math/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ (Math/PI) 2) (g/acos 0))))

  (testing "square/cube"
    (is (= 4 (g/square 2)))
    (is (= 4 (g/square -2)))
    (is (= 27 (g/cube 3)))
    (is (= -27 (g/cube -3))))

  (testing "neg"
    (is (= -4 (g/- 0 4)))
    (is (= -4 (g/negate 4)))
    (is (= 4 (g/negate (g/- 4))))
    (is (= -4 (g/- 4)))
    (is (= -4.2 (g/- 4.2))))

  (testing "zero? one?"
    (is (v/nullity? 0))
    (is (not (v/nullity? 1)))
    (is (v/nullity? 0.0))
    (is (not (v/nullity? 1.0)))
    (is (v/unity? 1))
    (is (not (v/unity? 2)))
    (is (v/unity? 1.0))
    (is (not (v/unity? 0.0))))

  (testing "zero-like"
    (is (= 0 (v/zero-like 2)))
    (is (= 0 (v/zero-like 3.14))))

  (testing "abs"
    (is (= 1 (g/abs -1)))
    (is (= 1 (g/abs 1))))

  (testing "sqrt"
    (is (= 9 (g/sqrt 81))))

  (testing "expt"
    (is (= 32 (g/expt 2 5))))

  (testing "log optimizations"
    (is (= 0 (g/log 1))))

  (testing "quotient"
    (is (= 2 (g/quotient 5 2)))
    (is (= 2 (g/quotient 5N 2)))
    (is (= 2 (g/quotient 5 2N)))
    (is (= 2 (g/quotient 5N 2N)))))
