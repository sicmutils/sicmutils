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

(ns sicmutils.polynomial-factor-test
  (:require [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [sicmutils
             [generic :as g]
             [value :as v]
             [numbers]
             [polynomial :refer :all]
             [simplify :refer [hermetic-simplify-fixture]]
             [polynomial-factor :refer :all]]))

(use-fixtures :once hermetic-simplify-fixture)

(defn ^:private ->poly [x] (expression-> x (fn [p _] p)))

(deftest factoring
  (testing "simple test cases"
    (let [unity2 (make 2 [[[0 0] 1]])
          x-y (->poly '(- x y))
          x+y (->poly '(+ x y))
          U0 (g/* (g/square (g/- 'x 'y)) (g/cube (g/+ 'x 'y)))
          U1 (g/square (g/- 'x 'y))
          U2 (g/* 3 (g/cube 'z) (g/+ (g/square 'x) 'y))
          U3 (g/* 3 (g/square 'z) (g/+ (g/square 'x) 'y))
          U (->poly '(* (square (- x y)) (cube (+ x y))))]
      (is (= [unity2 unity2 x-y x+y] (split U)))
      (is (= [1 1 '(+ x (* -1 y)) '(+ x y)] (factor-polynomial-expression U0)))
      (is (= [1 1 '(+ x (* -1 y)) 1] (factor-polynomial-expression U1)))
      (is (= [3 '(+ (expt x 2) y) 1 'z] (factor-polynomial-expression U2)))
      (is (= [3 '(+ (expt x 2) y) 'z 1] (factor-polynomial-expression U3))))))

(deftest factoring-2
  (testing "test poly"
    (let [x 'x
          y 'y
          z (g/square (g/+ x (g/* x (g/expt y 2))))
          test-poly (g/simplify (g/* (g/expt (g/+ (g/cos z) y) 2)
                                     (g/expt (g/- (g/cos z) y) 3)))]
      (is (= '(* (expt (+ (cos (expt (+ (* x (expt y 2)) x) 2)) y) 2)
                 (expt (+ (cos (expt (+ (* x (expt y 2)) x) 2)) (* -1 y)) 3))
             (-> test-poly v/freeze factor))))))

(deftest root-out-squares-test
  (testing "one step"
    (is (= '(+ x (* -1N y))
           (root-out-squares
            '(sqrt (square (- x y))))))
    (is (= '(sqrt (expt (+ x (* -1 y)) 3))
           (root-out-squares
            '(sqrt (cube (- x y))))))
    (is (= '(expt (+ x (* -1N y)) 2)
           (root-out-squares
            '(sqrt (expt (- x y) 4)))))
    (is (= '(* (sqrt (expt (+ x (* -1 y)) 3)) (+ x y))
           (root-out-squares
            '(sqrt (* (square (+ x y)) (cube (- x y)))))))
    (is (= '(* (sqrt (expt (+ x (* -1 y)) 3)) (+ x y))
           (root-out-squares
            (root-out-squares
             '(sqrt (* (square (+ x y)) (cube (- x y))))))))
    (is (= '(+ a b c)
           (root-out-squares
            '(sqrt (+ (expt a 2) (* 2 a b) (* b b) (* 2 a c) (* 2 b c) (expt c 2))))))
    (is (= '(+ a b c d)
           (root-out-squares
            '(sqrt (+ (expt a 2) (* 2 a b) (* b b) (* 2 a c) (* 2 b c) (expt c 2)
                      (* 2 a d) (* b d) (* b d) (* 2 c d) (* d d))))))
    (is (= '(+ (expt c 2) a b d)
           (root-out-squares
            '(sqrt (+ (expt a 2) (* 2 a b) (* b b) (* 2 a c c) (* 2 b c c) (expt c 4)
                      (* 2 a d) (* b d) (* b d) (* 2 c c d) (* d d)))))))
  #_(testing "ex.2"
    (is (= '(+ (* -1
                  R
                  (((partial 0) f)
                   (up (* R (cos phi) (sin theta))
                       (* R (sin phi) (sin theta))
                       (* R (cos theta))))
                  (cos phi)
                  (expt (cos theta) 3))
               (* -1
                  R
                  (((partial 1) f)
                   (up (* R (cos phi) (sin theta))
                       (* R (sin phi) (sin theta))
                       (* R (cos theta))))
                  (sin phi)
                  (expt (cos theta) 3))
               (* -1
                  R
                  (((partial 2) f)
                   (up (* R (cos phi) (sin theta))
                       (* R (sin phi) (sin theta))
                       (* R (cos theta))))
                  (expt (cos theta) 2)
                  (sin theta)))
           (root-out-squares
            (g/simplify
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
