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
  (testing "zero"
    (is (g/zero? (make []))))
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
      ;;(is (= (make [1]) (gcd U V)))
      ;;(is (= (make [1]) (gcd V U)))

      )
    (is (= [(make 2 [[[0 0] 1]]) (make 2 [[[2 1] 1]])]
           (divide (make 2 [[[2 1] 1] [[1 2] 1]]) (make 2 [[[1 2] 1]])))))

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
    (is (= [[2 0 2] [1 2 1] [3 0 0] [0 0 2]]
           (sort-by identity graded-lex-order [[1 2 1] [2 0 2] [0 0 2] [3 0 0]])))))

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
    (testing "coefficients"
      (is (thrown? IllegalArgumentException (coefficients (zap 9))))
      (is (= [(zap 6) (zap 7) (zap 1)] (coefficients u)))
      (is (= [(zap -6) (zap -5) (zap 1)] (coefficients v)))
      (is (= [(zap 1)] (coefficients x) (coefficients xx) (coefficients xxx)))
      (is (= (repeat 3 (zap 1)) (coefficients xxx+xx+x)))
      (is (= [xx x] (coefficients XXY+XYY)))
      (is (= [(make [0 4 6]) (make [6 9])] (coefficients Q))))
    (testing "coefficients2"
      (let [X (fn [c e] (make 2 [[[e 0] c]]))
            Y (fn [c e] (make 2 [[[0 e] c]]))
            Q (reduce add
                      [(mul (X 4 1) (Y 1 1))
                       (X 6 3)
                       (mul (X 1 1) (Y 6 2))
                       (mul (X 9 3) (Y 1 1))])]
        (is (= [(make [0 4 6]) (make [6 9])] (coefficients Q)))))
    (testing "gcd1"
      (let [x+1 (make [1 1])
            x+2 (make [2 1])
            x+3 (make [3 1])
            x+4 (make [4 1])
            U (mul x+1 (mul x+1 (mul x+2 x+4)))
            V (mul x+1 (mul x+2 x+3))]
        (is (= (make [2 3 1]) (gcd1 U V)))
        (is (= (make [4]) (gcd1 (make [8]) (make [4]))))
        (is (= (make [1]) (gcd1 (make [7]) (make [11]))))
        (is (= (make [11]) (gcd1 (make []) (make [11])))))
      (let [x+4 (make [4 1])
            x+3 (make [3 1])
            x-2 (make [-2 1])
            U (reduce mul [x+4 x+4 x+3 x+3 x-2 x-2 x-2])
            V (reduce mul [x+4 x+3 x+3 x+3 x-2 x-2])
            W (reduce mul [x+4 x+3 x+3 x-2 x-2])
            Z (make [])]
        ;; XXX fix sign
        (is (= W (gcd1 U V)))
        (is (= W (gcd1 V U)))
        (is (= U (gcd1 U U)))
        (is (= V (gcd1 V V)))
        (is (= W (gcd1 W W)))
        (is (= U (gcd1 U Z)))
        (is (= U (gcd1 Z U)))
        (is (= V (gcd1 V Z)))
        (is (= V (gcd1 Z V)))))
    #_(testing "content"
      (is (= (make []) (content (make []))))
      (is (= (make [3]) (content (make [3]))))
      (is (= (make [1]) (content u)))
      (is (= (make [1]) (content v)))
      (is (= (make [3]) (content w)))
      (is (= (make [0 1]) (content x)))
      (is (= (make [0 1]) (content (add x xx))))
      (is (= (make [0 0 1]) (content (add xx xxx))))
      (is (= XY (content XXY+XYY)))
      (is (= (zap 5) (content (zap 4)))))
    (testing "gcd"
      (let []
        ;;(is (= x (gcd x↑3+x↑2+x x)))
        ;;(is (= x (gcd x x↑3+x↑2+x)))
        )

      (let [o (zap 0)
            iii (zap 3)
            vii (zap 7)
            xiv (zap 14)
            xxi (zap 21)]
        ;;(is (= (make 0 [[[] 7]]) (gcd xiv xxi)))
        (is (= [iii o] (divide xxi vii))))
      (is (= [(make 2 []) X] (divide X Y)))
      (is (= [(make 2 []) X 1] (divide X Y {:pseudo true})))
      (is (= [(make 2 []) Y] (divide Y X)))
      (is (= [(make 2 []) Y 1] (divide Y X {:pseudo true})))
      ;;(is (= 'foo (gcd x↑2y xy↑2)))
      ;;(is (= 'foo (gcd x y)))

      )))

(deftest poly-as-simplifier
  (testing "arity"
    (is (= 1 (:arity (make [0 1])))))
  (testing "make-vars"
    (is (= (list (make [0 1])) (new-variables 1))))
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
