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
                 (expt (+ (cos (expt (+ (* x (expt y 2)) x) 2)) (* -1N y)) 3))
             (-> test-poly v/freeze factor-analyzer))))))
