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
      (let [X (fn [c e] (make 2 [[[e 0] c]]))
            Y (fn [c e] (make 2 [[[0 e] c]]))
            Q (reduce add
                      [(mul (X 4 1) (Y 1 1))
                       (X 6 3)
                       (mul (X 1 1) (Y 6 2))
                       (mul (X 9 3) (Y 1 1))])]
        (is (= [(make [0 4 6]) (make [6 9])] (coefficients Q)))
        (is (= (make [2 3]) (reduce gcd (coefficients Q))))
        (is (= [(make 2 [[[1 1] 2] [[3 0] 3]]) (make 2 [])]
               (divide Q (make 2 [[[0 0] 2] [[0 1] 3]]))))))
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
    (testing "GJS cases (see sparse-gcd.scm:666)"
      (let [gcd-test (fn [d f g]
                       (is (= d (gcd (mul d f) (mul d g)))))
            d1 (make 1 [[[0] 3] [[1] 1] [[2] 1]])
            f1 (make 1 [[[0] 1] [[1] 2] [[2] 2]])
            g1 (make 1 [[[0] 2] [[1] 2] [[2] 1]])

            d2 (make 2 [[[2 2] 2]])
            f2 (make 2 [[[0 2] 1] [[2 1] 2] [[2 0] 1] [[0 0] 1]])
            g2 (make 2 [[[2 2] 1] [[2 1] 1] [[1 1] 1] [[2 0] 1] [[1 0] 1]])]
        (gcd-test d1 f1 g1)
        ;; this one seems to suffer from the euclid remainder problem.
        ;; more to learn!
        #_(gcd-test d2 f2 g2)
        ))
    #_(testing "GCD: arity 3 case"
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
            G (reduce mul [(expt X+1 II) (expt X+Y II) (expt Y+Z III) (expt X+Y+Z IV) (expt Y+1 II) Z+1])]
        (println "U/G:" (divide U G))
        (println "V/G:" (divide V G))
        (is (= [(reduce mul [X+1 (expt Y+Z II) (expt Y+1 II) (expt Z+1 II)]) (make 3 [])] (divide U G)))
        (is (= [(reduce mul [(expt X+Y III) X+Y+Z X+1]) (make 3 [])] (divide V G)))
        (is (= 'foo (divide U V {:pseudo true})))
        (is (= 'bar (divide V U {:pseudo true})))

        #_(is (= G (gcd U V)))
        ))
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
    (testing "division of zero arity polynomials (do we care?)"
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
