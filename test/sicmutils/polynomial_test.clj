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

(ns sicmutils.polynomial-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.math.numeric-tower :as nt]
            [sicmutils
             [polynomial :refer :all]
             [generic :as g]
             [expression :refer [variables-in]]
             [simplify]
             [analyze :as a]]))

(set! *warn-on-reflection* true)

(defn ^:private fully-evaluate [p xs]
  (let [v (partial-evaluate p xs)]
    (is (zero? (.arity v)))
    (first (coefficients v))))

(deftest poly-core
  (testing "zero"
    (is (g/zero? (make [])))
    (is (g/zero? (make [0])))
    (is (g/zero? (make [])))
    (is (g/zero? (make 2 [])))
    (is (g/zero? (make 2 [])))
    (is (not (g/zero? (make [1])))))
  (testing "one"
    (is (g/one? (make [1])))
    (is (g/one? (make 2 [[[0 0] 1]])))
    (is (g/one? (make 3 [[[0 0 0] 1]])))
    (is (not (g/one? (make 3 [[[0 0 0] 1] [[0 0 1] 2]]))))
    (is (not (g/one? (make [1.1]))))
    (is (g/one? (make [1.0])))
    (is (g/one? (make [(make [1])])))
    (is (not (g/one? (make [(make [2])])))))
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
    (is (= (make []) (g/zero-like (make [1 2 3]))))
    (is (= (make 2 []) (g/zero-like (make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (make 3 []) (g/zero-like (make 3 [[[1 2 1] 4] [[0 1 0] 5]])))))
  (testing "one-like"
    (is (= (make [1]) (g/one-like (make [1 2 3]))))
    (is (= (make 2 [[[0 0] 1]]) (g/one-like (make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (make 3 [[[0 0 0] 1]]) (g/one-like (make 3 [[[1 2 1] 4] [[0 1 0] 5]]))))
    (is (= (make 2 [[[0 0] 1]]) (g/one-like (make 2 [])))))
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
  (testing "CRC polynomials"
    ;; https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
    ;; http://www.lammertbies.nl/comm/info/crc-calculation.html
    (let [x8 (make [0 0 0 0 0 0 0 0 1])
          CRC-8-ATM (make [1 1 1 0 0 0 0 0 1])
          M (make [1 1 1 0 1 0 1])
          Mx8 (mul x8 M)
          r1 (univariate-modular-remainder 2 Mx8 CRC-8-ATM)
          CRC-16-CCITT (make [1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1])
          x16 (mul x8 x8)
          T (make [0 0 1 0 1 0 1])
          Tx16 (mul x16 T)
          r2 (univariate-modular-remainder 2 Tx16 CRC-16-CCITT)]
      (is (= (make [0 1 0 0 0 1 0 1]) r1))
      (is (= (make [1 0 0 0 1 1 1 0 0 1 0 1 1]) r2))))
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

(def ^:private poly-analyzer (->PolynomialAnalyzer))
(defn ->poly [x] (a/expression-> poly-analyzer x (fn [p _] p)))

(deftest poly-evaluate
  (testing "arity 1"
    (let [p (->poly '(+ 2 (* x 3)))]
      (is (= 14 (fully-evaluate p [4])))
      (is (thrown? IllegalArgumentException (fully-evaluate p [3 2]))))
    (is (= 256 (fully-evaluate (->poly '(expt x 8)) [2])))
    (is (= 272 (fully-evaluate (->poly '(+ (expt x 4) (expt x 8))) [2]))))
  (testing "arity 2"
    (let [p (->poly '(expt (+ x y) 2))]
      (is (= 25 (fully-evaluate p [2 3])))))
  (testing "arity 3"
    (let [p (->poly '(+ (expt x 3) (expt y 2) z 1))]
      (is (= 19 (fully-evaluate p [2 3 1])))))
  (testing "arity 4"
    (let [p (->poly '(expt (- w x y z) 2))]
      (is (= 36 (fully-evaluate p [10 1 2 1])))))
  (testing "arity 5"
    (let [p (->poly '(expt (- v w x y z) 2))]
      (is (= 16 (fully-evaluate p [10 1 2 1 2])))))
  (testing "arity 10"
    (let [p (->poly '(expt (- x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) 3))]
      (is (= 216 (fully-evaluate p [10 1 2 1 2 -3 1 -2 -1 3])))))
  (testing "constant polys"
    (let [p1 (make [3])
          p2 (make 2 [[[0 0] 5]])
          p3 (make 3 [[[1 0 0] 1]])
          p4 (make 3 [[[0 1 0] 1]])
          p5 (make 3 [[[0 0 1] 1]])]
      (is (= 3 (fully-evaluate p1 [99])))
      (is (= 5 (fully-evaluate p2 [99 98])))
      (is (= 7 (fully-evaluate p3 [7 8 9])))
      (is (= 8 (fully-evaluate p4 [7 8 9])))
      (is (= 9 (fully-evaluate p5 [7 8 9])))))
  (testing "partial application"
    (let [P (->poly '(+ 1 (* 2 x) (* 3 x y) (* 4 x y z)))]
      (is (= (->poly '(+ 3 (* 3 y) (* 4 y z))) (partial-evaluate P [1])))
      (is (= (->poly '(+ 1 (* 2 x) (* 7 x y))) (partial-evaluate P [1] :direction :right)))
      (is (= (->poly '(+ 9 (* 8 z))) (partial-evaluate P [1 2])))
      (is (= (->poly '(+ 1 (* 13 x) )) (partial-evaluate P [1 2] :direction :right)))
      (is (= (make 0 [[[] 33]]) (partial-evaluate P [1 2 3])))
      (is (= (make 0 [[[] 33]]) (partial-evaluate P [1 2 3] :direction :right)))
      (is (thrown? IllegalArgumentException (partial-evaluate P [1 2 3 4])))
      (is (thrown? IllegalArgumentException (partial-evaluate P [1 2 3 4] :direction :right))))))

(deftest poly-partial-derivatives
  (let [V (make [1 2 3 4])
        U (make 2 [[[1 1] 3] [[2 2] 4] [[0 0] 5] [[0 3] 7] [[4 0] -2]])]
    (is (= (make [2 6 12]) (partial-derivative V 0)))
    (is (= [(make [2 6 12])] (partial-derivatives V)))
    (is (= (make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]]) (partial-derivative U 0)))
    (is (= (make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]]) (partial-derivative U 1)))
    (is (= [(make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]])
            (make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]])]
           (partial-derivatives U)))))

(deftest poly-as-simplifier
  (testing "arity"
    (let [^sicmutils.polynomial.Polynomial p (make [0 1])]
      (is (= 1 (.arity p)))))
  (testing "make-vars"
    (is (= (list (make [0 1])) (a/new-variables poly-analyzer 1)))
    (is (= [(make 3 [[[1 0 0] 1]])
            (make 3 [[[0 1 0] 1]])
            (make 3 [[[0 0 1] 1]])] (a/new-variables poly-analyzer 3))))
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{* + x} (variables-in exp1)))
      (is (= [(make [-3 -2 1]) '(x)] (a/expression-> poly-analyzer exp1 receive)))
      (is (= [(make [-3 -2 1]) '(x)] (a/expression-> poly-analyzer exp1 receive)))
      (is (= [(make [1 5 10 10 5 1]) '(y)] (a/expression-> poly-analyzer exp2 receive)))
      (is (= [(make [0 -11 5 -30 10 -7 1]) '(y)] (a/expression-> poly-analyzer exp3 receive)))))
  (testing "monomial order"
    (let [poly-simp #(a/expression-> poly-analyzer (:expression %) (fn [p vars] (a/->expression poly-analyzer p vars)))]
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
    (let [poly-simp #(a/expression-> poly-analyzer % (fn [p vars] (a/->expression poly-analyzer p vars)))
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

(defn generate-poly
  [arity]
  (gen/fmap #(make arity %)
            (gen/vector
              (gen/tuple
                (gen/vector gen/pos-int arity)
                gen/int))))

(defn generate-nonzero-poly
  [arity]
  (gen/such-that (complement g/zero?) (generate-poly arity)))

(def ^:private num-tests 50)

(defspec ^:long p+p=2p num-tests
  (prop/for-all [^sicmutils.polynomial.Polynomial p (gen/bind gen/nat generate-poly)]
                (= (add p p) (mul p (make-constant (.arity p) 2)))))

(defspec ^:long p-p=0 num-tests
  (prop/for-all [p (gen/bind gen/nat generate-poly)]
                (g/zero? (sub p p))))

(defspec ^:long pq-div-p=q num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-nonzero-poly arity)]
                  (let [[Q R] (divide (mul p q) q)]
                    (and (g/zero? R)
                         (= Q p))))))

(defspec ^:long p+q=q+p num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)]
                  (= (add p q) (add q p)))))

(defspec ^:long pq=qp num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)]
                  (= (mul p q) (mul q p)))))

(defspec ^:long p*_q+r_=p*q+p*r num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)
                   r (generate-poly arity)]
                  (= (mul p (add q r)) (add (mul p q) (mul p r))))))

(defspec ^:long lower-and-raise-arity-are-inverse num-tests
  (prop/for-all [p (gen/bind (gen/choose 2 10) generate-nonzero-poly)]
                (= p (raise-arity (lower-arity p)))))

(defspec ^:long evaluation-homomorphism num-tests
  (gen/let [arity (gen/choose 1 6)]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)
                   xs (gen/vector gen/int arity)]
                  (= (*' (fully-evaluate p xs) (fully-evaluate q xs))
                     (fully-evaluate (mul p q) xs)))))
