;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.polynomial-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.abstract.number]
            [sicmutils.expression :refer [variables-in expression-of]]
            [sicmutils.expression.analyze :as a]
            [sicmutils.function :as f]
            [sicmutils.generators :as sg]
            [sicmutils.laws :as sl]
            [sicmutils.generic :as g]
            [sicmutils.modint :as modular]
            [sicmutils.polynomial :as p]
            [sicmutils.polynomial.exponent :as xpt]
            [sicmutils.polynomial.impl :as pi]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(deftest polynomial-type-tests
  (checking "polynomials are both explicit polys and polynomial? == true" 100
            [p (sg/polynomial)]
            (is (p/polynomial? p))
            (is (= ::p/polynomial (v/kind p))))

  (checking "IArity" 100 [p (sg/polynomial)]
            (is (= (f/arity p)
                   [:between 0 (p/arity p)])))

  (checking "make-term round trip" 100
            [expts (gen/vector gen/nat)
             coef sg/number]
            (let [expts (xpt/dense->exponents expts)
                  term  (pi/make-term expts coef)]
              (is (= expts (pi/exponents term)))
              (is (= coef (pi/coefficient term)))))

  (testing "term getter defaults"
    (is (= 0 (pi/coefficient [])))
    (is (= xpt/empty (pi/exponents []))))

  (testing "dense make returns 0 for no entries or a zero first entry"
    (is (v/zero? (p/make [])))
    (is (v/zero? (p/make [0])))
    (is (not (v/zero? (p/make [1])))))

  (checking "zero only if first entry is zero" 100
            [arity gen/nat
             x     sg/number]
            (if (v/zero? x)
              (is (v/zero? (p/make [x])))
              (is (not (v/zero? (p/make [x])))))

            (if (v/zero? x)
              (is (v/zero? (p/constant arity x)))
              (is (not (v/zero? (p/constant arity x))))))

  (checking "zero-like" 100 [p (sg/polynomial)]
            (is (v/zero?
                 (v/zero-like p))))

  (testing "one"
    (is (not (v/one? (p/make []))))
    (is (v/one? (p/make [1])))
    (is (v/one? (p/make 2 [[[0 0] 1]])))
    (is (v/one? (p/make 3 [[[0 0 0] 1]])))
    (is (not (v/one? (p/make 3 [[[0 0 0] 1] [[0 0 1] 2]]))))
    (is (not (v/one? (p/make [1.1]))))
    (is (v/one? (p/make [1.0])))
    (is (v/one? (p/make [(p/make [1])])))
    (is (not (v/one? (p/make [(p/make [2])])))))

  (checking "one-like" 100 [p (sg/polynomial)]
            (is (v/one?
                 (v/one-like p))))

  (testing "one-like unit tests"
    (is (= (p/constant 1 1)
           (v/one-like (p/make [1 2 3]))))

    (is (= (p/constant 2 1)
           (v/one-like (p/make 2 [[[1 0] 1] [[2 1] 3]]))))

    (is (= (p/constant 3 1)
           (v/one-like (p/make 3 [[[1 2 1] 4] [[0 1 0] 5]]))))

    (is (= (p/make 2 [[[0 0] 1]])
           (v/one-like (p/make 2 [])))
        "If we can't deduce the unit element from the zero polynomial over an
        unknown ring, assume it's 1"))

  (testing "identity unit tests"
    (is (v/identity? (p/make [0 1])))
    (is (not (v/identity? (p/make []))))
    (is (not (v/identity? (p/make [0]))))

    (testing "identity? only returns true for monomials."
      (is (v/identity? (p/identity 1)))
      (is (not (v/identity? (p/identity 2 1))))))

  (checking "identity-like (only on monomials)" 100
            [p (sg/polynomial :arity 1)]
            (is (v/identity?
                 (v/identity-like p))))

  (testing "identity-like unit tests"
    (is (= (p/make [0 1])
           (v/identity-like (p/make [0 0 0 1]))))

    (is (= (p/make [0 1])
           (v/identity-like (p/make [1 2 3]))))

    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (v/identity-like (p/constant 10 1)))
        "identity-like is only supported on monomials."))

  (testing "constant"
    (let [c (p/constant 1 99)]
      (is (p/polynomial? c))
      (is (= c 99))
      (is (v/= 99 c)))

    (let [c (p/constant 2 88)]
      (is (p/polynomial? c))
      (is (= c 88))
      (is (v/= 88 c)))

    (let [c (p/constant 3 77)]
      (is (p/polynomial? c))
      (is (= c 77))
      (is (v/= 77 c))))

  (checking "terms, lead term" 100 [x sg/any-integral]
            (is (= (p/->terms x)
                   (p/->terms (p/constant 0 x))))

            (is (= (p/leading-term x)
                   (p/leading-term (p/constant 0 x))))

            (is (= (p/leading-coefficient x)
                   (p/leading-coefficient (p/constant x)))))

  (testing "degree"
    (is (= -1 (p/degree (p/constant 1 0))))
    (is (= -1 (p/degree (p/make []))))
    (is (= -1 (p/degree (p/make [0 0]))))
    (is (= 1 (p/degree (p/make [-1 1]))))
    (is (= 1 (p/degree (p/make [0 1]))))
    (is (= 1 (p/degree (p/make [-1 2 0]))))
    (is (= 2 (p/degree (p/make [-1 0 2]))))))

(deftest arithmetic-tests
  (let [coefs (gen/fmap #(g/modulo % 1000) sg/small-integral)]
    (testing "algebraic laws"
      (sl/ring 50 (sg/polynomial :arity 3 :coefs coefs)
               "polynomial is a ring"
               :commutative? true
               :with-one? true)

      (sl/ring 50 (sg/polynomial :arity 1 :coefs coefs)
               "polynomial arity 1 is a ring"
               :commutative? true
               :with-one? true)))

  (testing "add constant"
    (is (= (p/make [3 0 2])
           (g/add (p/make [0 0 2])
                  (p/constant 3))))

    (is (= (p/make [0 0 2])
           (g/add (p/make [2 0 2])
                  (p/constant -2)))))

  (checking "dense add, sub, negate" 100
            [[l r] (gen/sized
                    (fn [size]
                      (gen/tuple
                       (gen/vector sg/small-integral size)
                       (gen/vector sg/small-integral size))))]
            (is (= (p/make (map g/+ l r))
                   (g/+ (p/make l)
                        (p/make r))))

            (is (= (p/make (map g/- l r))
                   (g/- (p/make l)
                        (p/make r))))

            (is (= (p/make (map g/negate l))
                   (g/negate (p/make l)))))

  (testing "add/sub unit tests"
    (is (v/zero?
         (g/add (p/make [0 0 2])
                (p/make [0 0 -2]))))

    (is (= (p/make [])
           (g/add (p/make [0 0 2])
                  (p/make [0 0 -2]))))

    (is (= (p/make [3])
           (g/add (p/make [3 0 2])
                  (p/make [0 0 -2]))))

    (is (= (p/make [-1 1])
           (g/add (p/make [0 1])
                  (p/make [-1]))))

    (is (v/zero?
         (g/sub (p/make [0 0 2])
                (p/make [0 0 2]))))

    (is (= (p/make [-3])
           (g/sub (p/make [0 0 2])
                  (p/make [3 0 2]))))

    (is (= (p/make [0 1 2])
           (g/sub (p/make [3 1 2])
                  (p/make [3]))))

    (is (= (p/make [-2 -2 -1])
           (g/sub (p/make [1])
                  (p/make [3 2 1]))))

    (is (= (p/make [0 0 1 0 1 -1])
           (g/sub (p/make [1 0 1 0 1])
                  (p/make [1 0 0 0 0 1]))))

    (is (= (p/make [0 0 -1 0 -1 1])
           (g/sub (p/make [1 0 0 0 0 1])
                  (p/make [1 0 1 0 1]))))

    (is (= (p/make [-1 -2 -3])
           (p/negate (p/make [1 2 3])))))

  (testing "addition with symbols"
    (is (= (p/make [(g/+ 'a 'c) (g/+ 'b 'd) 'c])
           (g/add (p/make '[a b c])
                  (p/make '[c d])))))

  (checking "p+p=2p" 30 [p (sg/polynomial)]
            (is (= (g/add p p)
                   (g/mul p (p/constant (p/bare-arity p) 2)))))

  (checking "pq-div-p=q" 30
            [[p q] (gen/let [arity gen/nat]
                     (gen/tuple (sg/polynomial :arity arity)
                                (sg/polynomial :arity arity
                                               :nonzero? true)))]
            (let [p*q (g/mul p q)
                  [Q R] (p/divide p*q q)]
              (is (p/divisible? p*q q))
              (is (v/zero? R))
              (is (= p Q))))

  (testing "mul"
    (is (= (p/make [])
           (g/mul (p/make [1 2 3])
                  (p/make [0]))))

    (is (= (p/make [])
           (g/mul (p/make [0])
                  (p/make [1 2 3]))))
    (is (= (p/make [])
           (g/mul (p/make [])
                  (p/make [1 2 3]))))

    (is (= (p/make [1 2 3])
           (g/mul (p/make [1 2 3])
                  (p/make [1]))))

    (is (= (p/make [1 2 3])
           (g/mul (p/make [1])
                  (p/make [1 2 3]))))

    (is (= (p/make [3 6 9])
           (g/mul (p/make [1 2 3])
                  (p/make [3]))))

    (is (= (p/make [0 1 2 3])
           (g/mul (p/make [0 1])
                  (p/make [1 2 3]))))

    (is (= (p/make [0 -1 -2 -3])
           (g/mul (p/make [0 -1])
                  (p/make [1 2 3]))))

    (is (= (p/make [-1 0 1])
           (g/mul (p/make [1 1])
                  (p/make [-1 1]))))

    (is (= (p/make [1 3 3 1])
           (g/mul (p/make [1 1])
                  (g/mul (p/make [1 1])
                         (p/make [1 1])))))

    (is (= (p/make [1 -4 6 -4 1])
           (g/mul (g/mul (p/make [-1 1])
                         (p/make [-1 1]))
                  (g/mul (p/make [-1 1])
                         (p/make [-1 1]))))))

  (testing "expt"
    (let [x+1 (p/make [1 1])]
      (is (= (p/make [1])
             (g/expt x+1 0)))

      (is (= x+1 (g/expt x+1 1)))

      (is (= (p/make [1 2 1])
             (g/expt x+1 2)))

      (is (= (p/make [1 3 3 1])
             (g/expt x+1 3)))

      (is (= (p/make [1 4 6 4 1])
             (g/expt x+1 4)))

      (is (= (p/make [1 5 10 10 5 1])
             (g/expt x+1 5)))))

  (testing "div, psuedo-remainder"
    (is (= [(p/make [1 1])
            (p/make [])]
           (p/divide (p/make [-1 0 1])
                     (p/make [-1 1]))))

    (is (= [(p/make [-10 1])
            (p/make [-32 -21])]
           (p/divide (p/make [-42 0 -12 1])
                     (p/make [1 -2 1]))))

    (is (= [(p/make [3 1 1])
            (p/make [5])]
           (p/divide (p/make [-4 0 -2 1])
                     (p/make [-3 1]))))

    (is (= [(p/make [-5 0 3])
            (p/make [60 -27 -11])]
           (p/divide (p/make [-45 18 72 -27 -27 0 9])
                     (p/make [21 -9 -4 0 3]))))

    (let [U (p/make [-5 2 8 -3 -3 0 1 0 1])
          V (p/make [21 -9 -4 0 5 0 3])
          [pr d] (p/pseudo-remainder U V)]
      #?(:clj (is (= [(p/make [#sicm/ratio -2/9
                               0
                               #sicm/ratio 1/3])
                      (p/make [#sicm/ratio -1/3
                               0
                               #sicm/ratio 1/9
                               0
                               #sicm/ratio -5/9])]
                     (p/divide U V))))

      (is (= [(p/make [-3 0 1 0 -5]) 2]
             [pr d]))

      (is (zero?
           (g/- (g/* (p/make [(g/expt 3 d)])
                     U)
                (g/+ (g/* (p/make [-2 0 3]) V)
                     pr)))))

    (testing "examples from http://www.mathworks.com/help/symbolic/mupad_ref/pdivide.html"
      (let [p (p/make [1 1 0 1])
            q (p/make [1 1 3])]
        (is (= [(p/make [10 7]) 2]
               (p/pseudo-remainder p q))))

      (let [p (p/make [3 0 4])
            q (p/make [2 2])]
        (is (= [(p/make [28]) 2]
               (p/pseudo-remainder p q))))

      (is (= [(p/make 2 []) (p/make 2 [[[2 1] 1] [[1 2] 1]])]
             (p/divide (p/make 2 [[[2 1] 1] [[1 2] 1]])
                       (p/make 2 [[[1 2] 1]]))))

      (is (= [1 0] (p/divide (p/make [3])
                             (p/make [3]))))

      (is (= [0 1] (p/pseudo-remainder
                    (p/constant 7)
                    (p/constant 2)))))))

(deftest poly-core
  (testing "other coefficient rings: GF(11)"
    (sl/ring 50 (sg/polynomial
                 :arity 1
                 :coefs (gen/fmap #(modular/make % 11)
                                  gen/small-integer))
             "polynomial is a ring"
             :commutative? true
             :with-one? true))

  (testing "other coefficient rings, unit: GF(2), unit"
    (let [mod2 #(modular/make % 2)
          x0 (mod2 0)
          x1 (mod2 1)
          P (p/make [x1 x0 x1])]
      (is (= (p/make [x1 x0 x0 x0 x1])
             (g/expt P 2)))

      (is (= (p/make [x1 x0 x1 x0 x1 x0 x1])
             (g/expt P 3)))

      (is (= (p/make [x1 x0 x0 x0 x0 x0 x0 x0 x1])
             (g/mul (g/expt P 3) P)))

      (is (= (p/make [])
             (g/sub P P)))
      (is (= (p/make [])
             (g/add P P)))

      (is (= (p/make [x0 x0 x1])
             (g/add P (p/make [1]))))))

  (testing "CRC polynomials"
    ;; https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
    ;; http://www.lammertbies.nl/comm/info/crc-calculation.html
    (let [mod2 #(modular/make % 2)
          o (mod2 0)
          i (mod2 1)
          x8 (p/make [o o o o o o o o i])
          CRC-8-ATM (p/make [i i i o o o o o i])
          M (p/make [i i i 0 i o i])
          Mx8 (g/mul x8 M)
          [_ r1] (p/divide Mx8 CRC-8-ATM)
          CRC-16-CCITT (p/make [i o o o o i o o o o o o i o o o i])
          x16 (g/mul x8 x8)
          T (p/make [o o i o i o i])
          Tx16 (g/mul x16 T)
          [_ r2] (p/divide Tx16 CRC-16-CCITT)]
      (is (= (p/make [o i o o o i o i])
             r1))

      (is (= (p/make [i o o o i i i o o i o i i])
             r2)))))

(defn ->poly [x]
  (a/expression-> p/analyzer x (fn [p _] p)))

(deftest poly-evaluate
  (testing "arity 1"
    (let [p (->poly '(+ 2 (* x 3)))]
      (is (= 14 (p/evaluate p [4])))
      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (p/evaluate p [3 2]))
          "Too many arguments supplied."))

    (is (= 256 (-> (->poly '(expt x 8))
                   (p/evaluate [2]))))

    (is (= 272 (-> (->poly '(+ (expt x 4) (expt x 8)))
                   (p/evaluate [2])))))

  (testing "arity 2"
    (let [p (->poly '(expt (+ x y) 2))]
      (is (= 25 (p/evaluate p [2 3])))))

  (testing "arity 3"
    (let [p (->poly '(+ (expt x 3) (expt y 2) z 1))]
      (is (= 19 (p/evaluate p [2 3 1])))))

  (testing "arity 4"
    (let [p (->poly '(expt (- w x y z) 2))]
      (is (= 36 (p/evaluate p [10 1 2 1])))))

  (testing "arity 5"
    (let [p (->poly '(expt (- v w x y z) 2))]
      (is (= 16 (p/evaluate p [10 1 2 1 2])))))

  (testing "arity 10"
    (let [p (->poly '(expt (- x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) 3))]
      (is (= 216 (p/evaluate p [10 1 2 1 2 -3 1 -2 -1 3])))))

  (testing "constant polynomial evaluation"
    (let [p1 (p/make [3])
          p2 (p/make 2 [[[0 0] 5]])
          p3 (p/make 3 [[[1 0 0] 1]])
          p4 (p/make 3 [[[0 1 0] 1]])
          p5 (p/make 3 [[[0 0 1] 1]])]
      (is (= 3 (p/evaluate p1 [99])))
      (is (= 5 (p/evaluate p2 [99 98])))
      (is (= 7 (p/evaluate p3 [7 8 9])))
      (is (= 8 (p/evaluate p4 [7 8 9])))
      (is (= 9 (p/evaluate p5 [7 8 9])))))

  (testing "partial application"
    (let [P (->poly '(+ 1 (* 2 x) (* 3 x y) (* 4 x y z)))]
      (is (= (->poly '(+ 3 (* 3 y) (* 4 y z))) (p/evaluate P [1])))
      (is (= (->poly '(+ 9 (* 8 z))) (p/evaluate P [1 2])))
      (is (= 33 (p/evaluate P [1 2 3])))
      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (p/evaluate P [1 2 3 4]))
          "Too many arguments supplied."))))

(deftest poly-partial-derivatives
  (let [V (p/make [1 2 3 4])
        U (p/make 2 [[[1 1] 3] [[2 2] 4] [[0 0] 5] [[0 3] 7] [[4 0] -2]])]
    (is (= (p/make [2 6 12]) (p/partial-derivative V 0)))
    (is (= [(p/make [2 6 12])] (p/partial-derivatives V)))
    (is (= (p/make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]]) (p/partial-derivative U 0)))
    (is (= (p/make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]]) (p/partial-derivative U 1)))
    (is (= [(p/make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]])
            (p/make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]])]
           (p/partial-derivatives U)))))

(deftest poly-as-simplifier
  ;; TODO move up and test!
  (testing "arity"
    (let [p (p/make [0 1])]
      (is (= 1 (p/arity p)))))

  ;; TODO move up, test without analyzer too.
  (testing "make-vars"
    (is (= [(p/make [0 1])]
           (p/new-variables 1)))

    (is (= [(p/make 3 [[[1 0 0] 1]])
            (p/make 3 [[[0 1 0] 1]])
            (p/make 3 [[[0 0 1] 1]])]
           (p/new-variables 3))))

  (testing "expr"
    (let [exp1 (expression-of (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (expression-of (g/expt (g/+ 1 'y) 5))
          exp3 (expression-of (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{* + x} (variables-in exp1)))
      (is (= [(p/make [-3 -2 1]) '(x)]
             (a/expression-> p/analyzer exp1 receive)))

      (is (= [(p/make [-3 -2 1]) '(x)]
             (a/expression-> p/analyzer exp1 receive)))

      (is (= [(p/make [1 5 10 10 5 1]) '(y)]
             (a/expression-> p/analyzer exp2 receive)))

      (is (= [(p/make [0 -11 5 -30 10 -7 1]) '(y)]
             (a/expression-> p/analyzer exp3 receive)))))

  (testing "monomial order"
    (let [poly-simp #(a/expression->
                      p/analyzer
                      (expression-of %)
                      (fn [p vars]
                        (a/->expression p/analyzer p vars)))]
      (is (= '(+ (expt x 2) x 1)
             (poly-simp (g/+ 'x (g/expt 'x 2) 1))))

      (is (= '(+ (expt x 4) (* 4 (expt x 3)) (* 6 (expt x 2)) (* 4 x) 1)
             (poly-simp (g/expt (g/+ 1 'x) 4))))

      (is (= '(+ (expt x 4)
                 (* 4 (expt x 3) y)
                 (* 6 (expt x 2) (expt y 2))
                 (* 4 x (expt y 3))
                 (expt y 4))
             (poly-simp (g/expt (g/+ 'x 'y) 4))))

      (is (= '(+ (expt x 4)
                 (* 4 (expt x 3) y)
                 (* 6 (expt x 2) (expt y 2))
                 (* 4 x (expt y 3))
                 (expt y 4))
             (poly-simp (g/expt (g/+ 'y 'x) 4))))))

  (testing "expr-simplify"
    (let [poly-simp #(a/expression->
                      p/analyzer
                      %
                      (fn [p vars]
                        (a/->expression p/analyzer p vars)))
          exp1 (expression-of (g/+ (g/* 'x 'x 'x)
                                   (g/* 'x 'x)
                                   (g/* 'x 'x)))
          exp2 (expression-of (g/+ (g/* 'y 'y)
                                   (g/* 'x 'x 'x)
                                   (g/* 'x 'x)
                                   (g/* 'x 'x)
                                   (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2)))
             (poly-simp exp1)))

      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2)))
             (poly-simp exp2)))

      (is (= 'y (poly-simp exp3)))

      (is (= '(+ g1 g2)
             (poly-simp (expression-of (g/+ 'g1 'g2)))))
      (is (= '(* 2 g1)
             (poly-simp (expression-of (g/+ 'g1 'g1)))))

      (is (= 3 (poly-simp '(+ 2 1))))

      (is (= '(+ b (* -1 f))
             (poly-simp '(- (+ a b c) (+ a c f)))))

      (is (= '(+ (* -1 b) f)
             (poly-simp '(- (+ a c f) (+ c b a))))))))

(deftest lower-raise-tests
  (is (= (p/make 1 {[1] (p/constant 2 2)
                    [2] (p/constant 2 2)})
         (p/lower-arity
          (p/make 3 {[1 0 0] 2 [2 0 0] 2}))))

  (testing "lower, raise are inverse"

    (let [->poly (fn [x] (p/expression-> x (fn [p _] p)))
          f2 (->poly
              '(+ (expt x2 2)
                  (* 2 (expt x1 2) x2)
                  (expt x1 2)
                  1))
          d2 (->poly
              '(+ (* 2 (expt x1 2) (expt x2 2))
                  (* x1 x2)
                  (* 2 x1)))
          d3 (->poly
              '(+ (* x2 x2 x3 x3)
                  (* x2 x2 x3)
                  (* 2 x1 x1 x2 x3)
                  (* x1 x3)))
          d4 (->poly
              '(+ (* x1 x1 x4 x4)
                  (* x2 x2 x3 x4)
                  (* x1 x1 x2 x4)
                  (* x2 x4)
                  (* x1 x1 x2 x3)))]
      (is (= (p/make [0
                      (p/make [2 1])
                      (p/make [0 0 2])])
             (p/lower-arity d2)))

      (is (= (p/make [0
                      (p/make [2 1 2 1])
                      (p/make [0 0 2 0 2])
                      (p/make [2 5 2])
                      (p/make [0 0 2 4])])
             (p/lower-arity
              (g/* d2 f2))))

      (is (= (p/make [0
                      0
                      (p/make [4 4 5 4 1])
                      (p/make [0 0 8 4 8 4])
                      (p/make [4 12 9 2 4 0 4])
                      (p/make [0 0 8 20 8])
                      (p/make [0 0 0 0 4 8])])
             (g/* (p/lower-arity d2)
                  (p/lower-arity (g/* d2 f2)))))))

  (checking "lower-and-raise-arity-are-inverse" 30
            [p (gen/let [arity (gen/choose 2 10)]
                 (sg/polynomial :arity arity
                                :nonzero? true))]
            (is (= p (-> (p/lower-arity p)
                         (p/raise-arity (p/arity p))))))

  (checking "TODO name this..." 100
            [x sg/any-integral
             arity (gen/choose 2 10)]
            (is (= (p/constant arity x)
                   (-> (p/constant x)
                       (p/raise-arity arity))))))

(deftest evaluation-homomorphism-tests
  (checking "evaluation-homomorphism" 30
            [[p q xs] (gen/let [arity (gen/choose 1 6)]
                        (gen/tuple
                         (sg/polynomial :arity arity)
                         (sg/polynomial :arity arity)
                         (gen/vector sg/bigint arity)))]
            (is (= (u/bigint
                    (g/mul (p/evaluate p xs)
                           (p/evaluate q xs)))
                   (u/bigint
                    (p/evaluate (g/mul p q) xs)))))


  (testing "specific test cases from generative tests"
    (let [p (p/make 4 [[[0 0 0 0] -2] [[1 6 3 3] 3]])
          q (p/make 4 [[[0 0 0 3] 3] [[4 0 6 2] 1]])
          xs [-2 3 -3 -3]]
      (is (= (g/mul (p/evaluate p xs)
                    (p/evaluate q xs))
             (p/evaluate (g/mul p q) xs))))

    (let [p (p/make 5 [])
          q (p/make 5 [[[0 5 4 0 1] -2]
                       [[2 0 4 7 3] 4]
                       [[1 6 0 1 9] -9]
                       [[4 4 3 2 4] -5]
                       [[6 1 8 1 5] -3]
                       [[7 3 8 2 2] 9]
                       [[3 5 3 6 9] 2]
                       [[3 8 4 6 9] -8]
                       [[1 8 7 9 8] -2]])
          xs (map u/bigint [-9 7 0 9 -9])]
      (is (= (g/mul (p/evaluate p xs)
                    (p/evaluate q xs))
             (p/evaluate (g/mul p q) xs))))))

(deftest analyzer-test
  (let [new-analyzer (fn [] (a/make-analyzer
                            p/analyzer
                            (a/monotonic-symbol-generator "k%08d")))
        A #((a/default-simplifier
             (new-analyzer)) %)]
    (is (= '(+ x 1) (A '(+ 1 x))))
    (is (= '(+ x 1) (A '[+ 1 x])))
    (is (= '(* y (sin y) (cos (+ (expt (sin y) 4) (* 2 (sin y)) 1)))
           (A '(* y (sin y) (cos (+ 1 (sin y) (sin y) (expt (sin y) 4)))))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt cos 2) (expt sin 2)))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt sin 2) (expt cos 2)))))

    (is (= '(+ (up (+ (/ d a) (/ (* -1 b) a))) (* -1 (up (/ (* -1 y) (+ a b)))))
           (A '(- (up (+ (/ d a)
                         (/ (- b) a)))
                  (up (/ (- y) (+ a b))))))
        "This test exploded without the doall that forces add-symbol! calls in
        analyze.clj.")

    (is (= 'x (A '(* (/ 1 2) (+ x x)))))
    (is (= '(+ (* -1 m (expt ((D phi) t) 2) (r t)) (* m (((expt D 2) r) t)) ((D U) (r t)))
           (A '(- (* (/ 1 2) m (+ (((expt D 2) r) t) (((expt D 2) r) t)))
                  (+ (* (/ 1 2) m (+ (* ((D phi) t) ((D phi) t) (r t))
                                     (* ((D phi) t) ((D phi) t) (r t))))
                     (* -1 ((D U) (r t))))))))))

(deftest new-tests
  (testing "contract, expand tests"
    (is (= (p/make 1 {[1] 2 [2] 3})
           (-> (p/make 2 {[0 1] 2 [0 2] 3})
               (p/contract 0))))

    (is (= (-> (p/make 1 {[1] 2 [2] 3})
               (p/extend 0))
           (p/make 2 {[0 1] 2 [0 2] 3})))

    (is (= (-> (p/make 1 {[1] 2 [2] 3})
               (p/extend 12))
           (p/make 13 {[1] 2 [2] 3})))))


;; TODO test that you can normalize by the lead coefficient to get a monic.
;; Generate a dense then do that.


(comment
  ;; TODO test:
  ;;
  ;; TODO make this work JUST on terms and then turn the result into a poly!
  (let [u (make [10])
        v (make [10 20])
        [q r] (divide u v)]
    (g/+ (g/* q v) r))

  (= (make 3 {[3 0 0] 5 [2 0 1] 2 [0 2 1] 3})
     (reciprocal
      (make 3 {[0 0 0] 5 [1 0 1] 2 [3 2 1] 3})))

  (let [p (make 3 {[3 0 0] 5 [2 0 1] 2 [0 2 1] 3})]
    ;; because there is a constant term...
    (= p (reciprocal (reciprocal p)))))
