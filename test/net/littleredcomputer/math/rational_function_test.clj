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
             [structure :as s]
             [polynomial :as p]
             [numbers]
             [simplify]]))

(deftest properties
  (is (v/nullity? (make-constant 1 0)))
  (is (v/unity? (make-constant 1 1)))
  (is (not (v/nullity? (make-constant 1 1))))
  (is (not (v/unity? (make-constant 1 0))))
  (is (make-constant 0 0)))

(deftest make-test
  (let [zap #(p/make 0 [[[] %]])      ;; "zero-arity polynomial"
        zarf #(make (zap %) (zap 1))  ;; "zero-arity rational function"
        R (make (p/make [2]) (p/make [3]))
        S (make (p/make [2]) (p/make [1]))
        x+1 (p/make [1 1])
        x-1 (p/make [-1 1])
        x+1:x-1 (make x+1 x-1)
        x-1:x+1 (make x-1 x+1)
        one (make (p/make [1]) (p/make [1]))]
    (is (= (make (p/make [-1 -2 -3]) (p/make [-4 -5 6]))
           (make (p/make [1 2 3]) (p/make [4 5 -6]))))
    (is (= (make (p/make [1 2 3]) (p/make [-4 5 6]))
           (make (p/make [1 2 3]) (p/make [-4 5 6]))))
    (is (= one (mul x+1:x-1 x-1:x+1)))
    (is (= one (mul x-1:x+1 x+1:x-1)))
    (is (= (make (p/make [0 15 10]) (p/make [0 0 15 18]))
           (make (p/make [0 1/2 1/3]) (p/make [0 0 1/2 3/5]))))
    (is (= (make (p/make [1 -1]) (p/make [1 1])) (negate x-1:x+1)))
    (is (= x+1:x-1 (invert x-1:x+1)))
    (is (= (make (p/make [3]) (p/make [1])) (make-constant 1 3)))
    (is (= one (mul x-1:x+1 (invert x-1:x+1))))
    (is (= (make (p/make [2 0 2]) (p/make [-1 0 1])) (add x-1:x+1 x+1:x-1)))
    (is (= (make (p/make [2 0 2]) (p/make [-1 0 1])) (add x+1:x-1 x-1:x+1)))
    (is (= (make (p/make [1 2 1]) (p/make [1 -2 1])) (expt x+1:x-1 2)))
    (is (= (make (p/make [1 -2 1]) (p/make [1 2 1])) (expt x+1:x-1 -2)))
    (is (= (zarf 5) (add (zarf 2) (zarf 3))))
    (is (= (make (zap 5) (zap 3)) (div (zarf 5) (zarf 3))))
    (is (= (zarf 4) (div (zarf 8) (zarf 2))))
    (is (= (zarf 1) (div (zarf 1) (zarf 1))))
    (is (= (zarf 0) (div (zarf 0) (zarf 1))))))

(deftest rf-arithmetic
  (testing "invert-hilbert-matrix"
    (let [zap #(p/make 0 [[[] %]])        ;; "zero-arity polynomial"
          zarf #(make (zap %1) (zap %2))  ;; "zero-arity rational function"
          N 3
          H (apply s/up (for [i (range 1 (inc N))]
                        (apply s/up (for [j (range 1 (inc N))] (zarf 1 (+ i j -1))))))]
      (is (= (s/mapr #(zarf % 1)
                     (s/down (s/down 9 -36 30)
                             (s/down -36 192 -180)
                             (s/down 30 -180 180)))
             (g/invert H))))))

(deftest rf-operations
  (let [x+1 (p/make [1 1])
        x-1 (p/make [-1 1])]
    (= 'foo (g/mul x-1 (make x+1 x-1)))))

(deftest rf-as-simplifier
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= [(p/make [-3 -2 1]) '(x)] (expression-> exp1 receive)))
      (is (= [(p/make [-3 -2 1]) '(x)] (expression-> exp1 receive)))
      (is (= [(p/make [1 5 10 10 5 1]) '(y)] (expression-> exp2 receive)))
      (is (= [(p/make [0 -11 5 -30 10 -7 1]) '(y)] (expression-> exp3 receive)))))
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
      (is (= '(* a c e) (rf-simp '(/ (* a b c d e f) (* b d f)))))
      (is (= '(+ (* B a) (* B b) (* B c) (* B d)) (rf-simp '(+ (* B a) (* B b) (* B c) (* B d)))))
      (is (= '(+ a b c d) (rf-simp '(/ (+ (* B a) (* B b) (* B c) (* B d)) B))))
      (is (= '(/ 1 (+ a b c d)) (rf-simp '(/ B (+ (* B a) (* B b) (* B c) (* B d))))))
      (is (= '(+ a b) (rf-simp '(/ (+ (* B a) (* B b)) B))))
      (is (= '(/ 1 (+ a b)) (rf-simp '(/ B (+ (* B a) (* B b))))))
      (is (= '(/ (+ (* (expt dX 2) m2)
                    (* (expt dY 2) m2)
                    (* (expt dx 2) m1)
                    (* (expt dy 2) m1)
                    (* 2 m1 m2))
                 2)
             (rf-simp '(/ (+ (* K (expt dX 2) m2)
                             (* K (expt dY 2) m2)
                             (* K (expt dx 2) m1)
                             (* K (expt dy 2) m1)
                             (* 2 K m1 m2))
                          (* 2 K))))))))
