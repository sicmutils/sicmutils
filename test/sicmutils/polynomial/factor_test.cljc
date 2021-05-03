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

(ns sicmutils.polynomial.factor-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.expression.analyze :as a]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.polynomial :as p]
            [sicmutils.polynomial.factor :as pf]
            [sicmutils.value :as v]
            [sicmutils.simplify
             :refer [hermetic-simplify-fixture
                     simplify-expression]]))

(use-fixtures :each hermetic-simplify-fixture)

(def poly-analyzer
  (p/->PolynomialAnalyzer))

(defn ->poly [x]
  (a/expression-> poly-analyzer x (fn [p _] p)))

(deftest factoring
  (testing "simple test cases"
    (let [fpe #(pf/factor-polynomial-expression g/simplify poly-analyzer %)
          unity2 (p/make 2 [[[0 0] 1]])
          x-y (->poly '(- x y))
          x+y (->poly '(+ x y))
          U0 (g/* (g/square (g/- 'x 'y)) (g/cube (g/+ 'x 'y)))
          U1 (g/square (g/- 'x 'y))
          U2 (g/* 3 (g/cube 'z) (g/+ (g/square 'x) 'y))
          U3 (g/* 3 (g/square 'z) (g/+ (g/square 'x) 'y))
          U (->poly '(* (square (- x y)) (cube (+ x y))))]
      (is (= [unity2 unity2 x-y x+y] (pf/split-polynomial U)))
      (is (= [1 1 '(+ x (* -1 y)) '(+ x y)] (fpe U0)))
      (is (= [1 1 '(+ x (* -1 y)) 1] (fpe U1)))
      (is (= [3 '(+ (expt x 2) y) 1 'z] (fpe U2)))
      (is (= [3 '(+ (expt x 2) y) 'z 1] (fpe U3))))))

(deftest factoring-2
  (testing "test poly - first example from split-poly.scm"
    (let [z (g/square (g/+ 'x (g/* 'x (g/expt 'y 2))))
          test-poly (g/simplify
                     (g/* (g/expt (g/+ (g/cos z) 'y) 2)
                          (g/expt (g/- (g/cos z) 'y) 3)))]
      (is (= '(* -1
                 (expt (+ y (cos (expt (+ (* x (expt y 2)) x) 2))) 2)
                 (expt (+ y (* -1 (cos (expt (+ (* x (expt y 2)) x) 2)))) 3))
             (pf/factor
              (v/freeze test-poly)))))))

(deftest root-out-squares-test
  (testing "one step"
    (is (= '(+ x (* -1 y))
           (pf/root-out-squares
            '(sqrt (square (- x y))))))

    (is (= '(sqrt (expt (+ x (* -1 y)) 3))
           (pf/root-out-squares
            '(sqrt (cube (- x y))))))

    (is (= '(expt (+ x (* -1 y)) 2)
           (pf/root-out-squares
            '(sqrt (expt (- x y) 4)))))

    (is (= '(* (sqrt (expt (+ x (* -1 y)) 3)) (+ x y))
           (pf/root-out-squares
            '(sqrt (* (square (+ x y)) (cube (- x y)))))))

    (is (= '(* (sqrt (expt (+ x (* -1 y)) 3)) (+ x y))
           (pf/root-out-squares
            (pf/root-out-squares
             '(sqrt (* (square (+ x y)) (cube (- x y))))))))

    (is (= '(+ a b c)
           (pf/root-out-squares
            '(sqrt (+ (expt a 2) (* 2 a b) (* b b) (* 2 a c) (* 2 b c) (expt c 2))))))

    (is (= '(+ a b c d)
           (pf/root-out-squares
            '(sqrt (+ (expt a 2) (* 2 a b) (* b b) (* 2 a c) (* 2 b c) (expt c 2)
                      (* 2 a d) (* b d) (* b d) (* 2 c d) (* d d))))))

    (is (= '(+ (expt c 2) a b d)
           (pf/root-out-squares
            '(sqrt (+ (expt a 2) (* 2 a b) (* b b) (* 2 a c c) (* 2 b c c)
                      (expt c 4) (* 2 a d) (* b d) (* b d) (* 2 c c d) (* d d)))))))

  (testing "Second example from split-poly.scm"
    (is (= '(+ (* -1 R (cos phi)
                  (expt (cos theta) 3)
                  (((partial 0) f)
                   (up (* R (cos phi) (sin theta))
                       (* R (sin theta) (sin phi))
                       (* R (cos theta)))))
               (* -1 R (sin phi)
                  (expt (cos theta) 3)
                  (((partial 1) f)
                   (up (* R (cos phi) (sin theta))
                       (* R (sin theta) (sin phi))
                       (* R (cos theta)))))
               (* -1 R (sin theta)
                  (expt (cos theta) 2)
                  (((partial 2) f)
                   (up (* R (cos phi) (sin theta))
                       (* R (sin theta) (sin phi))
                       (* R (cos theta))))))
           (pf/root-out-squares
            (simplify-expression
             '(/ (+ (* -1
                       (expt R 2)
                       (((partial 0) f)
                        (up (* R (cos phi) (sin theta))
                            (* R (sin phi) (sin theta))
                            (* R (cos theta))))
                       (cos phi)
                       (expt (cos theta) 3)
                       (sin theta))
                    (* -1
                       (expt R 2)
                       (((partial 1) f)
                        (up (* R (cos phi) (sin theta))
                            (* R (sin phi) (sin theta))
                            (* R (cos theta))))
                       (expt (cos theta) 3)
                       (sin phi)
                       (sin theta))
                    (* (((partial 2) f)
                        (up (* R (cos phi) (sin theta))
                            (* R (sin phi) (sin theta))
                            (* R (cos theta))))
                       (sqrt
                        (+ (* (expt R 4) (expt (cos theta) 4))
                           (* -2 (expt R 4) (expt (cos theta) 2))
                           (expt R 4)))
                       (expt (cos theta) 2)))
                 (* R (sin theta)))))))))
