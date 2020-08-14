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

(ns sicmutils.rational-function-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.analyze :as a]
            [sicmutils.generic :as g]
            [sicmutils.matrix]
            [sicmutils.numbers]
            [sicmutils.polynomial :as p]
            [sicmutils.rational-function :as rf]
            [sicmutils.structure :as s]))

(deftest make-test
  (let [p #(p/make 1 [[[0] %]])      ;; constant arity 1 polynomial
        rf #(rf/make (p %1) (p %2))       ;; ratio of constant arity 1 polynomials
        x+1 (p/make [1 1])
        x-1 (p/make [-1 1])
        x+1:x-1 (rf/make x+1 x-1)
        x-1:x+1 (rf/make x-1 x+1)
        one (p/make [1])]
    (is (= (rf/make (p/make [-1 -2 -3]) (p/make [-4 -5 6]))
           (rf/make (p/make [1 2 3]) (p/make [4 5 -6]))))
    (is (= (rf/make (p/make [1 2 3]) (p/make [-4 5 6]))
           (rf/make (p/make [1 2 3]) (p/make [-4 5 6]))))
    (is (= one (rf/mul x+1:x-1 x-1:x+1)))
    (is (= one (rf/mul x-1:x+1 x+1:x-1)))
    (is (= (rf/make (p/make [1 -1]) (p/make [1 1])) (rf/negate x-1:x+1)))
    (is (= x+1:x-1 (rf/invert x-1:x+1)))
    (is (= one (rf/mul x-1:x+1 (rf/invert x-1:x+1))))
    (is (= (rf/make (p/make [2 0 2]) (p/make [-1 0 1])) (rf/add x-1:x+1 x+1:x-1)))
    (is (= (rf/make (p/make [2 0 2]) (p/make [-1 0 1])) (rf/add x+1:x-1 x-1:x+1)))
    (is (= (rf/make (p/make [1 2 1]) (p/make [1 -2 1])) (rf/expt x+1:x-1 2)))
    (is (= (rf/make (p/make [1 -2 1]) (p/make [1 2 1])) (rf/expt x+1:x-1 -2)))
    (is (= (p 3) (rf/add (rf 3 2) (rf 3 2))))
    (is (= (rf 5 3) (rf/div (rf 5 2) (rf 3 2))))
    (is (= (rf 14 3) (rf/div (rf 8 3) (rf 4 7))))

    #?(:clj
       (is (= (rf/make (p/make [0 15 10]) (p/make [0 0 15 18]))
              (rf/make (p/make [0 1/2 1/3]) (p/make [0 0 1/2 3/5])))))))

(deftest rf-arithmetic
  (testing "invert-hilbert-matrix"
    (let [p #(p/make 1 [[[0] %]])         ;; constant arity 1 polynomial
          rf #(rf/make (p %1) (p %2))        ;; arity 1 rational function out of two constants
          N 3
          H (apply s/up (for [i (range 1 (inc N))]
                          (apply s/up (for [j (range 1 (inc N))] (rf 1 (+ i j -1))))))]
      (is (= (s/mapr #(rf % 1)
                     (s/down (s/down 9 -36 30)
                             (s/down -36 192 -180)
                             (s/down 30 -180 180)))
             (g/invert H))))))

(deftest rf-operations
  (let [x+1 (p/make [1 1])
        x-1 (p/make [-1 1])]
    (= 'foo (g/mul x-1 (rf/make x+1 x-1)))))

(def ^:private rf-analyzer (rf/->RationalFunctionAnalyzer (p/->PolynomialAnalyzer)))
(def ^:private rf-simp #(a/expression-> rf-analyzer % (fn [a b] (a/->expression rf-analyzer a b))))

(deftest rf-as-simplifier
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))]
      (is (= [(p/make [-3 -2 1]) '(x)] (a/expression-> rf-analyzer exp1 vector)))
      (is (= [(p/make [-3 -2 1]) '(x)] (a/expression-> rf-analyzer exp1 vector)))
      (is (= [(p/make [1 5 10 10 5 1]) '(y)] (a/expression-> rf-analyzer exp2 vector)))
      (is (= [(p/make [0 -11 5 -30 10 -7 1]) '(y)] (a/expression-> rf-analyzer exp3 vector)))))

  (testing "expr-simplify"
    (let [exp1 (:expression (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
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
                          (* 2 K)))))))

  (testing "gcd"
    (is (= '(* x y) (rf-simp '(gcd (* w x y) (* x y z)))))
    (is (= '(/ s c) (rf-simp '(gcd (/ (* r s) c) (/ (* s t) (* c))))))
    (is (= 2 (rf-simp '(gcd (* 2 x y) 2))))
    (is (= 3 (rf-simp '(gcd 9 (* x 6 y)))))
    (is (= '(* 7 y) (rf-simp '(gcd (* 14 x y) (* 21 y z)))))
    #?(:clj
       (is (= 1 (rf-simp '(gcd (* 5/2 x y) (* 7/3 y z)))))))

  (testing "quotients"
    (is (= '(/ 1 (* 2 x)) (rf-simp (:expression (g/divide 1 (g/* 2 'x))))))
    (is (= 4 (rf-simp (:expression (g/divide (g/* 28 'x) (g/* 7 'x))))))
    (is (= '(/ 1 (expt x 21)) (rf-simp (:expression (g/divide (g/expt 'x 7) (g/expt 'x 28))))))))
