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

(ns net.littleredcomputer.math.polynomial-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math
             [value :as v]
             [polynomial :refer :all]
             [generic :as g]
             [numbers]
             [expression :refer [variables-in]]
             [simplify]
             [modint :as modular]]))

(deftest poly-core
  (testing "kind"
    (is (= :net.littleredcomputer.math.polynomial/polynomial (v/kind (make [])))))
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
          [q r] (divide U V)
          [pq pr d] (divide U V {:pseudo true})]
      (is (= [(make [-2/9 0 1/3]) (make [-1/3 0 1/9 0 -5/9])] [q r]))
      (is (= [(make [-2 0 3]) (make [-3 0 1 0 -5]) 9] [pq pr d]))
      (is (= (make []) (sub (mul (make [d]) U) (add (mul pq V) pr))))
      (is (= (make [1]) (gcd U V)))
      (is (= (make [1]) (gcd V U))))
    ;; examples from http://www.mathworks.com/help/symbolic/mupad_ref/pdivide.html
    (let [p (make [1 1 0 1])
          q (make [1 1 3])]
      (is (= [(make [-1 3]) (make [10 7]) 9] (divide p q {:pseudo true}))))
    (let [p (make [3 0 4])
          q (make [2 2])]
      (is (= [(make [-8 8]) (make [28]) 4] (divide p q {:pseudo true}))))
    (is (= [(make 2 [[[0 0] 1]]) (make 2 [[[2 1] 1]])]
           (divide (make 2 [[[2 1] 1] [[1 2] 1]]) (make 2 [[[1 2] 1]]))))
    (let [a 2
          p (make 2 [[[3 0] 1] [[1 0] 1] [[0 1] 1]])
          q (make 2 [[[2 0] a] [[1 0] 1] [[0 0] 1]])]
      (is (= [(make 2 [[[1 0] a] [[0 0] -1]])
              (make 2 [[[0 1] (* a a)] [[1 0] (+ (* a a) (- a) 1)] [[0 0] 1]])
              (* a a)]
             (divide p q {:pseudo true}))))
    (is (= [(make [1]) (make [])] (divide (make [3]) (make [3]))))
    (is (= [(make [7]) (make [0]) 2] (divide (make [7]) (make [2]) {:pseudo true}))))
  (testing "expt"
    (let [x+1 (make [1 1])]
      (is (= (make [1]) (expt x+1 (make []))))
      (is (= x+1 (expt x+1 (make [1]))))
      (is (= (make [1 2 1]) (expt x+1 (make [2]))))
      (is (= (make [1 3 3 1]) (expt x+1 (make [3]))))
      (is (= (make [1 4 6 4 1]) (expt x+1 (make [4]))))
      (is (= (make [1 5 10 10 5 1]) (expt x+1 (make [5]))))))
  (testing "other coefficient rings: GF(2)"
    (let [mod2 #(modular/make % 2)
          x0 (mod2 0)
          x1 (mod2 1)
          P (make [x1 x0 x1])]
      (is (= (make [x1 x0 x0 x0 x1]) (expt P (make [2]))))
      (is (= (make [x1 x0 x1 x0 x1 x0 x1]) (expt P (make [3]))))
      (is (= (make [x1 x0 x0 x0 x0 x0 x0 x0 x1]) (mul (expt P (make [3])) P)))
      (is (= (make []) (sub P P)))
      (is (= (make []) (add P P)))
      (is (= (make [x0 x0 x1]) (add P (make [1]))))))
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

(deftest poly-gcd
  (let [zap #(make 0 [[[] %]])  ;; zero-arity polynomial
        u (make [6 7 1])  ;; some polynomials of arity 1
        v (make [-6 -5 1])
        w (make [-3 -6 9])
        x (make [0 1])
        xx (mul x x)
        xxx (mul x xx)
        xxx+xx+x (add xxx (add xx x))
        X (make 2 [[[1 0] 1]]) ;; some polynomials of arity 2
        Y (make 2 [[[0 1] 1]])
        XY (mul X Y)
        XXY (make 2 [[[2 1] 1]])
        XYY (make 2 [[[1 2] 1]])
        XXY+XYY (add XXY XYY)
        Q (make 2 [[[1 1] 4] [[3 0] 6] [[1 2] 6] [[3 1] 9]])]
    (testing "constant-term"
      (is (= 6 (constant-term u)))
      (is (= -6 (constant-term v)))
      (is (= 0 (constant-term x)))
      (is (= 5 (constant-term (zap 5))))
      (is (= 0 (constant-term (make 4 [])))))
    (testing "GCD: arity 0 case"
      (is (= (zap 3) (gcd (zap 12) (zap 15))))
      (is (= (zap 1) (gcd (zap 7) (zap 11))))
      (is (= (zap 5) (gcd (zap -15) (zap 20)))))
    (testing "GCD: arity 1 case"
      (let [x+1 (make [1 1])
            x+2 (make [2 1])
            x+3 (make [3 1])
            x+4 (make [4 1])
            U (mul x+1 (mul x+1 (mul x+2 x+4)))
            V (mul x+1 (mul x+2 x+3))]
        (is (= (make [2 3 1]) (gcd U V)))
        (is (= (make [4]) (gcd (make [8]) (make [4]))))
        (is (= (make [1]) (gcd (make [7]) (make [11]))))
        (is (= (make [11]) (gcd (make []) (make [11])))))
      (let [x+4 (make [4 1])
            x+3 (make [3 1])
            x-2 (make [-2 1])
            x+1 (make [1 1])
            U (reduce mul [x+4 x+4 x+3 x+3 x-2 x-2 x-2 x-2 x-2 x+1])
            V (reduce mul [x+4 x+3 x+3 x+3 x-2 x-2 x+1 x+1])
            W (reduce mul [x+4 x+3 x+3 x-2 x-2 x+1])
            Z (make [])]
        (is (= W (gcd U V)))
        (is (= W (gcd V U)))
        (is (= U (gcd U U)))
        (is (= V (gcd V V)))
        (is (= W (gcd W W)))
        (is (= U (gcd U Z)))
        (is (= U (gcd Z U)))
        (is (= V (gcd V Z)))
        (is (= V (gcd Z V)))))
    (testing "divide constant arity 2"
      (is (= [(make 2 []) X] (divide X Y)))
      (is (= [(make 2 []) X 1] (divide X Y {:pseudo true})))
      (is (= [(make 2 []) Y] (divide Y X)))
      (is (= [(make 2 []) Y 1] (divide Y X {:pseudo true}))))
    (testing "GCD: arity 2 case"
      (let [I (make 2 [[[0 0] 1]])
            II (make 2 [[[0 0] 2]])
            III (make 2 [[[0 0] 3]])
            IV (make 2 [[[0 0] 4]])
            V (make 2 [[[0 0] 5]])
            X (make 2 [[[1 0] 1]])
            Y (make 2 [[[0 1] 1]])
            X+Y (add X Y)
            X+1 (add X I)
            Y+1 (add Y I)
            X+Y_2 (mul X+Y X+Y)
            X+Y_3 (mul X+Y_2 X+Y)
            U (reduce mul [(expt X+1 III) (expt X+Y II) (expt Y+1 IV)])
            V (reduce mul [(expt X+1 II) (expt X+Y V) (expt Y+1 III)])
            G (reduce mul [(expt X+1 II) (expt X+Y II) (expt Y+1 III)])]
        (is (= X+Y_2 (gcd X+Y_2 X+Y_3)))
        (is (= X+Y_3 (gcd X+Y_3 X+Y_3)))
        (is (= G (gcd U V)))))

    (testing "GCD: arity 3 case"
      (let [I (make 3 [[[0 0 0] 1]])
            II (make 3 [[[0 0 0] 2]])
            III (make 3 [[[0 0 0] 3]])
            IV (make 3 [[[0 0 0] 4]])
            V (make 3 [[[0 0 0] 5]])
            X (make 3 [[[1 0 0] 1]])
            Y (make 3 [[[0 1 0] 1]])
            Z (make 3 [[[0 0 1] 1]])
            X+Y (add X Y)
            X+Z (add X Z)
            Y+Z (add Y Z)
            X+Y+Z (add X+Y Z)
            X+1 (add X I)
            Y+1 (add Y I)
            Z+1 (add Z I)
            U (reduce mul [(expt X+1 III) (expt X+Y II) (expt Y+Z V)  (expt X+Y+Z IV) (expt Y+1 IV) (expt Z+1 III)])
            V (reduce mul [(expt X+1 II)  (expt X+Y V)  (expt Y+Z III) (expt X+Y+Z V) (expt Y+1 II) (expt Z+1 I) X+1])
            G (reduce mul [(expt X+1 III) (expt X+Y II) (expt Y+Z III) (expt X+Y+Z IV) (expt Y+1 II) Z+1])]
        (is (= [(reduce mul [(expt Y+Z II) (expt Y+1 II) (expt Z+1 II)]) (make 3 [])] (divide U G)))
        (is (= [(reduce mul [(expt X+Y III) X+Y+Z]) (make 3 [])] (divide V G)))
        (is (= X+Z (gcd (mul X+Y X+Z) (mul Y+Z X+Z))))
        (is (= (mul X+Z X+Y+Z) (gcd (reduce mul [X+Y X+Z X+Y+Z]) (reduce mul [X+Z X+Y+Z Y+Z]))))
        (is (= (mul X+Z (mul X+Z X+Y)) (gcd (reduce mul [X+Z X+Z X+Y X+Y+Z Y+1]) (reduce mul [X+Z X+Z X+Y X+1 Z+1 X+Z]))))
        (is (= G (gcd U V)))
        ))
    (testing "division of zero arity polynomials"
      (let [o (zap 0)
            iii (zap 3)
            vii (zap 7)
            xiv (zap 14)
            xxi (zap 21)]
        (is (g/zero? o))
        (is (v/nullity? o))
        (is (= [iii o] (divide xxi vii)))
        (is (= [o o] (divide o iii)))
        (is (= [o o] (divide o iii {:pseudo true})))
        (is (thrown? IllegalArgumentException (divide o o)))))))

(deftest simple-gcd-3
  (testing "GCD: arity 3 case"
      (let [X (make 3 [[[1 0 0] 1]])
            Y (make 3 [[[0 1 0] 1]])
            Z (make 3 [[[0 0 1] 1]])
            X+Y (add X Y)
            X+Z (add X Z)
            Y+Z (add Y Z)]
        (is (= X+Z (gcd (mul X+Y X+Z) (mul Y+Z X+Z)))))))

(deftest gjs
  (testing "GJS cases (see sparse-gcd.scm:666)"
   (let [gcd-test (fn [d f g]
                    (is (= d (gcd (mul d f) (mul d g)))))
         d1 (make [3 1 2])
         f1 (make [1 2 2])
         g1 (make [2 2 1])

         d2 (make 2 [[[2 2] 2] [[1 1] 1] [[1 0] 2]])
         f2 (make 2 [[[0 2] 1] [[2 1] 2] [[2 0] 1] [[0 0] 1]])
         g2 (make 2 [[[2 2] 1] [[2 1] 1] [[1 1] 1] [[2 0] 1] [[1 0] 1]])]
     (gcd-test d1 f1 g1)
     (let [df (mul d2 f2)
           dg (mul d2 g2)]
       (is (= (make [0
                     (make [2 1])
                     (make [0 0 2])])
              (lower-arity d2)))
       (is (= d2 (raise-arity (lower-arity d2))))
       (is (= (make [0
                     (make [2 1 2 1])
                     (make [0 0 2 0 2])
                     (make [2 5 2])
                     (make [0 0 2 4])])
              (lower-arity df)))
       (is (= df (raise-arity (lower-arity df))))
       (is (= (make [0
                     0
                     (make [4 4 5 4 1])
                     (make [0 0 8 4 8 4])
                     (make [4 12 9 2 4 0 4])
                     (make [0 0 8 20 8])
                     (make [0 0 0 0 4 8])])
              (mul (lower-arity d2) (lower-arity df))))
       (is (= (mul d2 df) (raise-arity (mul (lower-arity d2) (lower-arity df)))))
       (gcd-test d2 f2 g2)
       ))))

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
