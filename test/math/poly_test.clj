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

(ns math.poly-test
  (:require [clojure.test :refer :all]
            [math.poly :refer :all]
            [math.generic :as g]
            [math.numbers]
            [math.expression :as x]
            [math.modint :as modular]
            ))

(deftest poly-core
  (testing "zero"
    (is (= 0 (make))))
  (testing "degree"
    (is (= (degree (make)) -1))
    (is (= (degree (make -1 1)) 1))
    (is (= (degree (make 0 1)) 1))
    (is (= (degree (make -1 0 2)) 2))
    (is (= (degree (make -1 2 0)) 1))
    (is (= (degree (make 0 0)) -1)))
  (testing "add constant"
    (is (= (make 3 0 2) (add (make 0 0 2) 3)))
    (is (= (make 0 0 2) (add (make 2 0 2) -2))))
  (testing "add/sub"
    (is (= (make) (add (make 0 0 2) (make 0 0 -2))))
    (is (= 0 (add (make 0 0 2) (make 0 0 -2))))
    (is (= 3 (add (make 3 0 2) (make 0 0 -2))))
    (is (= (make -1 1) (add (make 0 1) (make -1))))
    (is (= (make) (sub (make 0 0 2) (make 0 0 2))))
    (is (= 0 (sub (make 0 0 2) (make 0 0 2))))
    (is (= -3 (sub (make 0 0 2) (make 3 0 2))))
    (is (= (make 0 1 2) (sub (make 3 1 2) 3)))
    (is (= (make -2 -2 -1) (sub 1 (make 3 2 1))))
    (is (= (make 0 0 1 0 1 -1) (sub (make 1 0 1 0 1) (make 1 0 0 0 0 1))))
    (is (= (make 0 0 -1 0 -1 1) (sub (make 1 0 0 0 0 1) (make 1 0 1 0 1))))
    (is (= (make -1 -2 -3) (negate (make 1 2 3))))
    )

  (testing "mul"
    (is (= 0 (mul (make 1 2 3) 0)))
    (is (= 0 (mul 0 (make 1 2 3))))
    (is (= 0 (mul (make) (make 1 2 3))))
    (is (= (make 1 2 3) (mul (make 1 2 3) 1)))
    (is (= (make 1 2 3) (mul 1 (make 1 2 3))))
    (is (= (make 3 6 9) (mul (make 1 2 3) 3)))
    (is (= (make 0 1 2 3) (mul (make 0 1) (make 1 2 3))))
    (is (= (make 0 -1 -2 -3) (mul (make 0 -1) (make 1 2 3))))
    (is (= (make -1 0 1) (mul (make 1 1) (make -1 1))))
    (is (= (make 1 3 3 1) (mul (make 1 1) (mul (make 1 1) (make 1 1)))))
    (is (= (make 1 -4 6 -4 1) (mul (mul (make -1 1) (make -1 1))
                                   (mul (make -1 1) (make -1 1))))))
  (testing "expt"
    (let [x+1 (make 1 1)]
      (is (= 1 (expt x+1 0)))
      (is (= x+1 (expt x+1 1)))
      (is (= (make 1 2 1) (expt x+1 2)))
      (is (= (make 1 3 3 1) (expt x+1 3)))
      (is (= (make 1 4 6 4 1) (expt x+1 4)))
      (is (= (make 1 5 10 10 5 1) (expt x+1 5)))
      ))
  (testing "equals"
    (is (not= 22 (make 2 2)))
    (is (= 22 (make 22))))
  (testing "other coefficient rings: GF(2)"
    (let [mod2 #(modular/make % 2)
          x0 (mod2 0)
          x1 (mod2 1)
          P (make x1 x0 x1)]
      (is (= (make x1 x0 x0 x0 x1) (expt P 2)))
      (is (= (make x1 x0 x1 x0 x1 x0 x1) (expt P 3)))
      (is (= (make x1 x0 x0 x0 x0 x0 x0 x0 x1) (mul (expt P 3) P)))
      (is (= (make) (sub P P)))
      (is (= (make) (add P P)))
      (is (= (make x0 x0 x1) (add P 1)))))
  )

(deftest poly-as-simplifier
  (testing "arity"
    (is (= 1 (:arity (make 0 1)))))
  (testing "make-vars"
    (is (= (list (make 0 1)) (new-variables 1)))
    ;(is (= 'bar (make-vars 2)))
    )
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{math.generic/* math.generic/+ x} (x/variables-in exp1)))
      (is (= [(make -3 -2 1) '(x)] (expression-> exp1 receive)))
      (is (= [(make -3 -2 1) '(x)] (expression-> exp1 receive)))
      (is (= [(make 1 5 10 10 5 1) '(y)] (expression-> exp2 receive)))
      (is (= [(make 0 -11 5 -30 10 -7 1) '(y)] (expression-> exp3 receive)))
      ))
  (testing "expr-simplify"
    (let [pe x/print-expression
          poly-simp #(-> % (expression-> ->expression) pe)
          exp1 (:expression (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
          exp2 (:expression (g/+ (g/* 'y 'y) (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x) (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (* 2 (expt x 2)) (expt x 3)) (poly-simp exp1)))
      (is (= '(+ (expt x 3) (* 2 (expt y 2)) (* 2 (expt x 2))) (poly-simp exp2)))
      (is (= 'y (poly-simp exp3)))
      (is (= '(+ g1 g2) (poly-simp (:expression (g/+ 'g1 'g2)))))
      (is (= '(* 2 g1) (poly-simp (:expression (g/+ 'g1 'g1)))))
      (is (= '(+ b (* -1 f)) (poly-simp `(g/- (g/+ a b c) (g/+ a c f)))))
      (is (= '(+ f (* -1 b)) (poly-simp `(g/- (g/+ a c f) (g/+ c b a)))))
      ))
  )
