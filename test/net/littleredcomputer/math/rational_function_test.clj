;
; Copyright (C) 2015 Colin Smith.
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

(ns net.littleredcomputer.math.rational-function-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math
             [rational-function :refer :all]
             [expression :as x]
             [generic :as g]
             [value :as v]
             [polynomial :as p]
             [numbers]
             [simplify]]))

(deftest make-test
  (let [zap #(p/make 0 [[[] %]])      ;; "zero-arity polynomial"
        zarf #(make (zap %) (zap 1))  ;; "zero-arity rational function"
        R (make (p/make [2]) (p/make [3]))
        S (make (p/make [4]) (p/make [2]))
        x+1 (p/make [1 1])
        x-1 (p/make [-1 1])
        x+1:x-1 (make x+1 x-1)
        x-1:x+1 (make x-1 x+1)
        one (make (p/make [1]) (p/make [1]))]
    (is (= one (make x+1 x+1)))
    (is (= one (mul x+1:x-1 x-1:x+1)))
    (is (= one (mul x-1:x+1 x+1:x-1)))
    (is (= (make (p/make [1 -1]) (p/make [1 1])) (negate x-1:x+1)))
    (is (= x+1:x-1 (invert x-1:x+1)))
    (is (= (make (p/make [3]) (p/make [1])) (make-constant 1 3)))
    (is (= one (mul x-1:x+1 (invert x-1:x+1))))
    (is (= (make (p/make [2 0 2]) (p/make [-1 0 1])) (add x-1:x+1 x+1:x-1)))
    (is (= (make (p/make [2 0 2]) (p/make [-1 0 1])) (add x+1:x-1 x-1:x+1)))
    (is (= (make (p/make [1 2 1]) (p/make [1 -2 1])) (expt x+1:x-1 S)))
    (is (= (make (p/make [1 -2 1]) (p/make [1 2 1])) (expt x+1:x-1 (negate S))))
    (is (= (zarf 5) (add (zarf 2) (zarf 3))))
    (is (= (make (zap 5) (zap 3)) (div (zarf 5) (zarf 3))))
    (is (= (zarf 4) (div (zarf 8) (zarf 2))))))

(deftest rf-as-simplifier
  (testing "make-vars"
    (is (= [(make (p/make [0 1]) (p/make [1]))] (new-variables 1)))
    (is (= [(make (p/make 2 [[[1 0] 1]]) (p/make 2 [[[0 0] 1]]))
            (make (p/make 2 [[[0 1] 1]]) (p/make 2 [[[0 0] 1]]))] (new-variables 2))))
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= [(make (p/make [-3 -2 1]) (p/make [1])) '(x)] (expression-> exp1 receive)))
      (is (= [(make (p/make [-3 -2 1]) (p/make [1])) '(x)] (expression-> exp1 receive)))
      (is (= [(make (p/make [1 5 10 10 5 1]) (p/make [1])) '(y)] (expression-> exp2 receive)))
      (is (= [(make (p/make [0 -11 5 -30 10 -7 1]) (p/make [1])) '(y)] (expression-> exp3 receive)))))
  (testing "expr-simplify"
    (let [rf-simp #(expression-> % ->expression)
          exp1 (:expression (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
          exp2 (:expression (g/+ (g/* 'y 'y) (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x) (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2))) (rf-simp exp1)))
      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2))) (rf-simp exp2)))
      (is (= 'y (rf-simp exp3)))
      (is (= '(+ g1 g2) (rf-simp (:expression (g/+ 'g1 'g2)))))
      (is (= 12 (rf-simp '(+ 3 9))))
      (is (= '(* 2 g1) (rf-simp (:expression (g/+ 'g1 'g1)))))
      (is (= '(+ b (* -1 f)) (rf-simp '(- (+ a b c) (+ a c f)))))
      (is (= '(+ (* -1 b) f) (rf-simp '(- (+ a c f) (+ c b a)))))
      (is (= '(* a c e) (rf-simp '(/ (* a b c d e f) (* b d f))))))))
