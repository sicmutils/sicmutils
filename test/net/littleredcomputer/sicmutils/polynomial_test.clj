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

(ns net.littleredcomputer.sicmutils.polynomial-test
  (:import (com.google.common.base Stopwatch)
           (java.util.concurrent TimeUnit))
  (:require [clojure.test :refer :all]
            [clojure.math.numeric-tower :as nt]
            [net.littleredcomputer.sicmutils
             [value :as v]
             [polynomial :refer :all]
             [generic :as g]
             [numbers]
             [expression :refer [variables-in]]
             [simplify]
             [modint :as modular]]))

(set! *warn-on-reflection* true)

(deftest poly-core
  (testing "kind"
    (is (= :net.littleredcomputer.sicmutils.polynomial/polynomial (v/kind (make [])))))
  (testing "zero"
    (is (g/zero? (make [])))
    (is (g/zero? (make [0])))
    (is (v/nullity? (make [])))
    (is (g/zero? (make 2 [])))
    (is (v/nullity? (make 2 [])))
    (is (not (g/zero? (make [1])))))
  (testing "unity"
    (is (v/unity? (make [1])))
    (is (v/unity? (make 2 [[[0 0] 1]])))
    (is (v/unity? (make 3 [[[0 0 0] 1]])))
    (is (not (v/unity? (make 3 [[[0 0 0] 1] [[0 0 1] 2]]))))
    (is (not (v/unity? (make [1.1]))))
    (is (v/unity? (make [1.0])))
    (is (v/unity? (make [(make [1])])))
    (is (not (v/unity? (make [(make [2])])))))
  (testing "make-constant"
    (is (= (make [99]) (make-constant 1 99)))
    (is (= (make 2 [[[0 0] 88]]) (make-constant 2 88)))
    (is (= (make 3 [[[0 0 0] 77]]) (make-constant 3 77))))
  (testing "degree"
    (is (= (degree (make [])) -1))
    (is (= (degree (make [-1 1])) 1))
    (is (= (degree (make [0 1])) 1))
    (is (= (degree (make [-1 0 2])) 2))
    (is (= (degree (make [-1 2 0])) 1))
    (is (= (degree (make [0 0])) -1)))
  (testing "zero-like"
    (is (= (make []) (v/zero-like (make [1 2 3]))))
    (is (= (make 2 []) (v/zero-like (make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (make 3 []) (v/zero-like (make 3 [[[1 2 1] 4] [[0 1 0] 5]])))))
  (testing "one-like"
    (is (= (make [1]) (v/one-like (make [1 2 3]))))
    (is (= (make 2 [[[0 0] 1]]) (v/one-like (make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (make 3 [[[0 0 0] 1]]) (v/one-like (make 3 [[[1 2 1] 4] [[0 1 0] 5]]))))
    ;; we can't deduce the unit element from the zero polynomial over an
    ;; "unknown" ring
    (is (thrown? IllegalArgumentException (v/one-like (make 2 [])))))
  (testing "add constant"
    (is (= (make [3 0 2]) (add (make [0 0 2]) (make [3]))))
    (is (= (make [0 0 2]) (add (make [2 0 2]) (make [-2])))))
  (testing "add/sub"
    (is (g/zero? (add (make [0 0 2]) (make [0 0 -2]))))
    (is (= (make []) (add (make [0 0 2]) (make [0 0 -2]))))
    (is (= (make [3]) (add (make [3 0 2]) (make [0 0 -2]))))
    (is (= (make [-1 1]) (add (make [0 1]) (make [-1]))))
    (is (g/zero? (sub (make [0 0 2]) (make [0 0 2]))))
    (is (= (make [-3]) (sub (make [0 0 2]) (make [3 0 2]))))
    (is (= (make [0 1 2]) (sub (make [3 1 2]) (make [3]))))
    (is (= (make [-2 -2 -1]) (sub (make [1]) (make [3 2 1]))))
    (is (= (make [0 0 1 0 1 -1]) (sub (make [1 0 1 0 1]) (make [1 0 0 0 0 1]))))
    (is (= (make [0 0 -1 0 -1 1]) (sub (make [1 0 0 0 0 1]) (make [1 0 1 0 1]))))
    (is (= (make [-1 -2 -3]) (negate (make [1 2 3])))))
  (testing "with symbols"
    (is (= (make [(g/+ 'a 'c) (g/+ 'b 'd) 'c]) (add (make '[a b c]) (make '[c d])))))
  (testing "mul"
    (is (= (make []) (mul (make [1 2 3]) (make [0]))))
    (is (= (make []) (mul (make [0]) (make [1 2 3]))))
    (is (= (make []) (mul (make []) (make [1 2 3]))))
    (is (= (make [1 2 3]) (mul (make [1 2 3]) (make [1]))))
    (is (= (make [1 2 3]) (mul (make [1]) (make [1 2 3]))))
    (is (= (make [3 6 9]) (mul (make [1 2 3]) (make [3]))))
    (is (= (make [0 1 2 3]) (mul (make [0 1]) (make [1 2 3]))))
    (is (= (make [0 -1 -2 -3]) (mul (make [0 -1]) (make [1 2 3]))))
    (is (= (make [-1 0 1]) (mul (make [1 1]) (make [-1 1]))))
    (is (= (make [1 3 3 1]) (mul (make [1 1]) (mul (make [1 1]) (make [1 1])))))
    (is (= (make [1 -4 6 -4 1]) (mul (mul (make [-1 1]) (make [-1 1]))
                                     (mul (make [-1 1]) (make [-1 1]))))))
  (testing "div"
    (is (= [(make [1 1]) (make [])]
           (divide (make [-1 0 1]) (make [-1 1]))))
    (is (= [(make [-10 1]) (make [-32 -21])]
           (divide (make [-42 0 -12 1]) (make [1 -2 1]))))
    (is (= [(make [3 1 1]) (make [5])]
           (divide (make [-4 0 -2 1]) (make [-3 1]))))
    (is (= [(make [-5 0 3]) (make [60 -27 -11])]
           (divide (make [-45 18 72 -27 -27 0 9]) (make [21 -9 -4 0 3]))))
    (let [U (make [-5 2 8 -3 -3 0 1 0 1])
          V (make [21 -9 -4 0 5 0 3])
          [pr d] (pseudo-remainder U V)]
      (is (= [(make [-2/9 0 1/3]) (make [-1/3 0 1/9 0 -5/9])] (divide U V)))
      (is (= [(make [-3 0 1 0 -5]) 2] [pr d]))
      (is (= (make []) (sub (mul (make [(nt/expt 3 d)]) U) (add (mul (make [-2 0 3]) V) pr)))))
    ;; examples from http://www.mathworks.com/help/symbolic/mupad_ref/pdivide.html
    (let [p (make [1 1 0 1])
          q (make [1 1 3])]
      (is (= [(make [10 7]) 2] (pseudo-remainder p q))))
    (let [p (make [3 0 4])
          q (make [2 2])]
      (is (= [(make [28]) 2] (pseudo-remainder p q))))
    (is (= [(make 2 []) (make 2 [[[2 1] 1] [[1 2] 1]])]
           (divide (make 2 [[[2 1] 1] [[1 2] 1]]) (make 2 [[[1 2] 1]]))))
    (is (= [(make [1]) (make [])] (divide (make [3]) (make [3]))))
    (is (= [(make [0]) 1] (pseudo-remainder (make [7]) (make [2])))))
  (testing "expt"
    (let [x+1 (make [1 1])]
      (is (= (make [1]) (expt x+1 0)))
      (is (= x+1 (expt x+1 1)))
      (is (= (make [1 2 1]) (expt x+1 2)))
      (is (= (make [1 3 3 1]) (expt x+1 3)))
      (is (= (make [1 4 6 4 1]) (expt x+1 4)))
      (is (= (make [1 5 10 10 5 1]) (expt x+1 5)))))
  (testing "other coefficient rings: GF(2)"
    (let [mod2 #(modular/make % 2)
          x0 (mod2 0)
          x1 (mod2 1)
          P (make [x1 x0 x1])]
      (is (= (make [x1 x0 x0 x0 x1]) (expt P 2)))
      (is (= (make [x1 x0 x1 x0 x1 x0 x1]) (expt P 3)))
      (is (= (make [x1 x0 x0 x0 x0 x0 x0 x0 x1]) (mul (expt P 3) P)))
      (is (= (make []) (sub P P)))
      (is (= (make []) (add P P)))
      (is (= (make [x0 x0 x1]) (add P (make [1]))))))
  (testing "CRC polynomials"
    ;; https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
    ;; http://www.lammertbies.nl/comm/info/crc-calculation.html
    (let [mod2 #(modular/make % 2)
          o (mod2 0)
          i (mod2 1)
          x8 (make [o o o o o o o o i])
          CRC-8-ATM (make [i i i o o o o o i])
          M (make [i i i 0 i o i])
          Mx8 (mul x8 M)
          [q1 r1] (divide Mx8 CRC-8-ATM)
          CRC-16-CCITT (make [i o o o o i o o o o o o i o o o i])
          x16 (mul x8 x8)
          T (make [o o i o i o i])
          Tx16 (mul x16 T)
          [q2 r2] (divide Tx16 CRC-16-CCITT)
          ]
      (is (= (make [o i o o o i o i]) r1))
      (is (= (make [i o o o i i i o o i o i i]) r2))))
  (testing "monomial order"
    (let [x3 [3 0 0]
          x2z2 [2 0 2]
          xy2z [1 2 1]
          z2 [0 0 2]
          monomials [x3 x2z2 xy2z z2]
          monomial-sort #(sort-by identity % monomials)]
      (is (= [z2 xy2z x2z2 x3] (monomial-sort lex-order)))
      (is (= [z2 x3 x2z2 xy2z] (monomial-sort graded-reverse-lex-order)))
      (is (= [z2 x3 xy2z x2z2] (monomial-sort graded-lex-order))))))

(defn ^:private ->poly [x] (expression-> x (fn [p v] p)))
(deftest poly-apply
  (testing "arity 1"
    (let [p (->poly '(+ 2 (* x 3)))]
      (is (= 14 (p 4)))
      (is (thrown? IllegalArgumentException (p 3 2))))
    (is (= 256 ((->poly '(expt x 8)) 2)))
    (is (= 272 ((->poly '(+ (expt x 4) (expt x 8))) 2))))
  (testing "arity 2"
    (let [p (->poly '(expt (+ x y) 2))]
      (is (= p (p)))
      (is (= 25 (p 2 3)))
      (let [q (p 3)]
        (is (= 49 (q 4))))))
  (testing "arity 3"
    (let [p (->poly '(+ (expt x 3) (expt y 2) z 1))]
      (is (= 19 (p 2 3 1)))))
  (testing "arity 4"
    (let [p (->poly '(expt (- w x y z) 2))]
      (is (= 36 (p 10 1 2 1)))))
  (testing "arity 5"
    (let [p (->poly '(expt (- v w x y z) 2))]
      (is (= 16 (p 10 1 2 1 2)))))
  (testing "arity 10 (via apply)"
    (let [p (->poly '(expt (- x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) 3))]
      (is (= 216 (apply p [10 1 2 1 2 -3 1 -2 -1 3])))))
  (testing "constant polys"
    (let [p1 (make [3])
          p2 (make 2 [[[0 0] 5]])
          p3 (make 3 [[[1 0 0] 1]])
          p4 (make 3 [[[0 1 0] 1]])
          p5 (make 3 [[[0 0 1] 1]])]
      (is (= 3 (p1 99)))
      (is (= 5 (p2 99 98)))
      (is (= 7 (p3 7 8 9)))
      (is (= 8 (p4 7 8 9)))
      (is (= 9 (p5 7 8 9))))))

(deftest poly-as-simplifier
  (testing "arity"
    (is (= 1 (:arity (make [0 1])))))
  (testing "make-vars"
    (is (= (list (make [0 1])) (new-variables 1)))
    (is (= [(make 3 [[[1 0 0] 1]])
            (make 3 [[[0 1 0] 1]])
            (make 3 [[[0 0 1] 1]])] (new-variables 3))))
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{* + x} (variables-in exp1)))
      (is (= [(make [-3 -2 1]) '(x)] (expression-> exp1 receive)))
      (is (= [(make [-3 -2 1]) '(x)] (expression-> exp1 receive)))
      (is (= [(make [1 5 10 10 5 1]) '(y)] (expression-> exp2 receive)))
      (is (= [(make [0 -11 5 -30 10 -7 1]) '(y)] (expression-> exp3 receive)))))
  (testing "monomial order"
    (let [poly-simp #(expression-> (:expression %) ->expression)]
      (is (= '(+ (expt x 2) x 1) (poly-simp (g/+ 'x (g/expt 'x 2) 1))))
      (is (= '(+ (expt x 4) (* 4 (expt x 3)) (* 6 (expt x 2)) (* 4 x) 1) (poly-simp (g/expt (g/+ 1 'x) 4))))
      (is (= '(+
               (expt x 4)
               (* 4 (expt x 3) y)
               (* 6 (expt x 2) (expt y 2))
               (* 4 x (expt y 3))
               (expt y 4))
             (poly-simp (g/expt (g/+ 'x 'y) 4))))
      (is (= '(+
               (expt x 4)
               (* 4 (expt x 3) y)
               (* 6 (expt x 2) (expt y 2))
               (* 4 x (expt y 3))
               (expt y 4))
             (poly-simp (g/expt (g/+ 'y 'x) 4))))))
  (testing "expr-simplify"
    (let [poly-simp #(expression-> % ->expression)
          exp1 (:expression (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
          exp2 (:expression (g/+ (g/* 'y 'y) (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x) (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2))) (poly-simp exp1)))
      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2))) (poly-simp exp2)))
      (is (= 'y (poly-simp exp3)))
      (is (= '(+ g1 g2) (poly-simp (:expression (g/+ 'g1 'g2)))))
      (is (= '(* 2 g1) (poly-simp (:expression (g/+ 'g1 'g1)))))
      (is (= 3 (poly-simp '(+ 2 1))))
      (is (= '(+ b (* -1 f)) (poly-simp '(- (+ a b c) (+ a c f)))))
      (is (= '(+ (* -1 b) f) (poly-simp '(- (+ a c f) (+ c b a))))))))
