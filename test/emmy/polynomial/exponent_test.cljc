#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.polynomial.exponent-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.generators :as sg]
            [emmy.polynomial.exponent :as xpt]))

(deftest exponents-tests
  (checking "(l*r)/r == l" 100
            [l (sg/poly:exponents 10)
             r (sg/poly:exponents 10)]
            (is (= l (-> (xpt/mul l r)
                         (xpt/div r)))))

  (checking "(l*r)/gcd(l,r) == lcm(l,r)" 100
            [l (sg/poly:exponents 10)
             r (sg/poly:exponents 10)]
            (is (= (xpt/lcm l r)
                   (-> (xpt/mul l r)
                       (xpt/div (xpt/gcd l r))))))

  (checking "->sort+unsort" 100
            [m (sg/poly:exponents 100)]
            (let [[sort-m unsort-m] (xpt/->sort+unsort m)]
              (and (= (vals (sort-m m))
                      (sort (vals m)))
                   (= m (unsort-m (sort-m m))))))

  (checking "assoc" 100 [m (sg/poly:exponents 10)
                         x gen/nat
                         n gen/nat]
            (is (= n (-> (xpt/assoc m x n)
                         (xpt/monomial-degree x)))))

  (checking "raise/lower" 100
            [m (sg/poly:exponents 10)
             x gen/nat]
            (is (= m (xpt/lower
                      (xpt/raise m)))
                "raise, lower with defalt indices")

            (is (= m (-> (xpt/raise m x)
                         (xpt/lower x)))
                "raise, lower with explicit index"))

  (checking "lowering all the way == empty" 100
            [m (sg/poly:exponents 10)]
            (is (= xpt/empty
                   (-> (iterate xpt/lower m)
                       (nth 10))))))

(deftest monomial-ordering-tests
  (testing "monomial orderings"
    (let [x3 (xpt/dense->exponents [3 0 0])
          x2z2 (xpt/dense->exponents [2 0 2])
          xy2z (xpt/dense->exponents [1 2 1])
          z2   (xpt/dense->exponents [0 0 2])
          monomials [x3 x2z2 xy2z z2]
          sort-with #(sort % monomials)]
      (is (= [z2 xy2z x2z2 x3]
             (sort-with xpt/lex-order)))

      (is (= [z2 x3 xy2z x2z2]
             (sort-with xpt/graded-lex-order)))

      (is (= [z2 x3 x2z2 xy2z]
             (sort-with xpt/graded-reverse-lex-order))))

    (testing "monomial ordering example from wikipedia"
      (let [x2 (xpt/make 0 2)
            xy (xpt/make 0 1 1 1)
            xz (xpt/make 0 1 2 1)
            y2 (xpt/make 1 2)
            yz (xpt/make 1 1 2 1)
            z2 (xpt/make 2 2)
            monomials [x2 xy xz y2 yz z2]
            sort-with #(sort % monomials)]
        (is (= [z2 yz y2 xz xy x2]
               (sort-with xpt/lex-order)
               (sort-with xpt/graded-lex-order))
            "grlex and lex match when all orders are the same")

        (is (= [z2 yz xz y2 xy x2]
               (sort-with xpt/graded-reverse-lex-order)))))))
