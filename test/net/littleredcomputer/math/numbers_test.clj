;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns net.littleredcomputer.math.numbers-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math
             [generic :as g]
             [value :as v]
             [simplify]
             [numbers]]))

(def near (v/within 1e-12))

(deftest arithmetic
  (testing "with-numbers"
    (is (= 4 (g/+ 2 2)))
    (is (= 3.5 (g/+ 1.5 2)))
    (is (= 13/40 (g/+ 1/5 1/8)))
    (is (= 1/8 (g/- 3/8 1/4)))
    (is (= 20 (g/* 5 4)))
    (is (= 5 (g/divide 20 4)))
    (is (= 5/4 (g/divide 5 4)))
    (is (= 1/8 (g/divide 8))))
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
    (is (= 1/2 (g/divide 1 2)))
    (is (= 1/4 (g/divide 1 2 2)))
    (is (= 1/8 (g/divide 1 2 2 2)))
    (is (= 2.14 (g/- 3.14 1))))
  (testing "trig"
    (is (= 1.0 (g/cos 0)))
    (is (= 0.0 (g/sin 0)))
    (is (near (/ (Math/PI) 4) (g/asin (/ (Math/sqrt 2) 2))))
    (is (near (/ (Math/PI) 4) (g/acos (/ (Math/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ (Math/PI) 2) (g/acos 0)))
    (is (= '(tan x) (g/simplify (g/tan 'x)))))
  (testing "square/cube"
    (is (= 4 (g/square 2)))
    (is (= 4 (g/square -2)))
    (is (= 27 (g/cube 3)))
    (is (= -27 (g/cube -3)))
    )
  (testing "with-symbols"
    (is (= '(+ x 4) (g/simplify (g/+ 4 'x))))
    (is (= '(+ y 5) (g/simplify (g/+ 'y 5))))
    (is (= '(/ 5 y) (g/simplify (g/divide 5 'y))))
    (is (= '(* 5 y) (g/simplify (g/* 5 'y))))
    (is (= '(/ x y) (g/simplify (g/divide 'x 'y))))
    (is (= '(* x y) (g/simplify (g/* 'x 'y))))
    )
  (testing "zero/one elimination"
    (is (= 'x (g/+ 0 'x)))
    (is (= 'x (g/* 1 'x)))
    (is (= (g/negate 'x) (g/- 0 'x)))
    (is (= 'x (g/+ 'x 0)))
    (is (= 'x (g/* 'x 1)))
    (is (= 'x (g/- 'x 0)))
    (is (= 'x (g/+ 0.0 'x)))
    (is (= 'x (g/* 1.0 'x)))
    (is (= 'x (g/+ 'x 0.0)))
    (is (= 'x (g/* 'x 1.0)))
    (is (= 'x (g/divide 'x 1.0)))
    (is (= 'x (g/divide 'x 1)))
    (is (= 0 (g/divide 0 'x)))
    (is (= 0 (g/* 0 'x)))
    (is (= 0 (g/* 'x 0)))
    (is (thrown? ArithmeticException (g/divide 'x 0)))
    )
  (testing "neg"
    (is (= '(* -1 x) (g/simplify (g/negate 'x))))
    (is (= -4 (g/- 0 4)))
    (is (= -4 (g/negate 4)))
    (is (= 4 (g/negate (g/- 4))))
    (is (= (g/negate 'x) (g/- 0 'x)))
    (is (= -4 (g/- 4)))
    (is (= -4.2 (g/- 4.2))))
  (testing "zero? one?"
    (is (g/zero? 0))
    (is (not (g/zero? 1)))
    (is (g/zero? 0.0))
    (is (not (g/zero? 1.0)))
    (is (g/one? 1))
    (is (not (g/one? 2)))
    (is (g/one? 1.0))
    (is (not (g/one? 0.0)))
    )
  (testing "zero-like"
    (is (= 0 (v/zero-like 2)))
    (is (= 0 (v/zero-like 3.14))))
  (testing "abs"
    (is (= 1 (g/abs -1)))
    (is (= 1 (g/abs 1)))
    (is (= '(abs x) (g/simplify (g/abs 'x))))
    )
  (testing "sqrt"
    (is (= 9 (g/sqrt 81)))
    (is (= '(sqrt x) (g/simplify (g/sqrt 'x))))
    )
  (testing "expt"
    (is (= 32 (g/expt 2 5)))
    (is (= '(expt x 2) (g/simplify (g/expt 'x 2))))
    (is (= '(expt x y) (g/simplify (g/expt 'x 'y))))
    (is (= '(expt 2 y) (g/simplify (g/expt 2 'y))))
    (is (= 1 (g/expt 1 'x)))
    (is (= 1 (g/expt 'x 0)))
    (is (= 'x (g/simplify (g/expt 'x 1))))
    (is (= 'x (g/simplify (g/expt (g/sqrt 'x) 2))))
    (is (= '(expt x 3) (g/simplify (g/expt (g/sqrt 'x) 6))))
    (is (= '(expt x 12) (g/simplify (g/expt (g/expt 'x 4) 3))))
    (is (= '(/ 1 (expt x 3)) (g/simplify (g/expt 'x -3))))
    )
  (testing "exp/log"
    (is (= 1.0 (g/exp 0)))
    (is (= '(exp x) (g/simplify (g/exp 'x))))
    (is (= 0.0 (g/log 1)))
    (is (= '(log x) (g/simplify (g/log 'x))))
    (is (= 0.0 (g/log (g/exp 0))))
    )
)
