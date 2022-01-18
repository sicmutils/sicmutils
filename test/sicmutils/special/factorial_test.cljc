;;
;; Copyright © 2022 Sam Ritchie.
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

(ns sicmutils.special.factorial-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.special.factorial :as sf]))

;; TODO see https://proofwiki.org/wiki/Properties_of_Falling_Factorial tests for
;; falling

;; TEST: coefficients of expansions are stirling numbers of the first kind, see
;; https://en.wikipedia.org/wiki/Falling_and_rising_factorials#cite_note-10

;; Test this implementation for falling factorial with negative expts:
#_(comment
    ;; here is another impl for negative that we can test against.
    (g/invert
     (transduce (comp
                 (map #(g/add x (inc %)))
                 #?(:cljs (map u/bigint)))
                g/*
                (range (- n)))))

;; TODO make tests checking these!! Check that these three cases work.

;; NOTE from wiki: The rising and falling factorials are well defined in any
;; unital ring, and therefore x can be taken to be, for example, a complex
;; number, including negative integers, or a polynomial with complex
;; coefficients, or any complex-valued function.

;; TODO close the stirling ticket after we get this in.



(deftest factorial-tests
  (testing "factorial"
    (is (= (apply g/* (range 1 8))
           (sf/factorial 7)))

    (is (= #sicm/bigint "15511210043330985984000000"
           (sf/factorial 25))
        "factorial can handle `n` that triggers overflow in cljs and clj."))

  (testing "falling-factorial"
    (is (g/infinite?
         (sf/falling-factorial -10 -10)))
    (is (= -1320 (sf/falling-factorial -10 3)))
    (is (= #sicm/ratio -1/504 (sf/falling-factorial -10 -3)))
    (is (= #sicm/ratio 1/1716 (sf/falling-factorial 10 -3))))

  (testing "double-factorial"
    ;; confirmed via wolfram
    (is (= 1 (sf/double-factorial 0)))
    (is (= 1 (sf/double-factorial -1)))
    (is (= -1 (sf/double-factorial -3)))
    (is (= #sicm/ratio 1/3 (sf/double-factorial -5)))
    (is (= #sicm/ratio -1/15 (sf/double-factorial -7)))
    (is (= #sicm/ratio 1/105 (sf/double-factorial -9))))

  (testing "subfactorial"
    (is (= 1 (sf/subfactorial 0)))
    (is (= 0 (sf/subfactorial 1)))
    (is (= 1 (sf/subfactorial 2)))
    (is (= 2 (sf/subfactorial 3)))
    (is (= 9 (sf/subfactorial 4)))
    (is (= 44 (sf/subfactorial 5)))
    (is (= 265 (sf/subfactorial 6)))))

(deftest binomial-tests
  (is (= -9 (sf/binomial-coefficient -9 -10)))
  (is (= 0 (sf/binomial-coefficient -11 -10)))

  (letfn [(n-choose-k [n k]
            ;; simple but inefficient implementation for comparison with the
            ;; more efficient method in the library.
            (if (or (< k 0) (> k n))
              0
              (g/quotient
               (sf/factorial n)
               (g/* (sf/factorial (- n k))
                    (sf/factorial k)))))
          (check [n k expected explanation]
            (is (= expected
                   (n-choose-k n k)
                   (sf/binomial-coefficient n k))
                explanation))]
    (is (= (n-choose-k 1000 290)
           (sf/binomial-coefficient 1000 290))
        "n choose k with large values")

    (check 100 -2  0 "k < 0")
    (check 100 0   1 "k == 0")
    (check 100 200 0 "k > n")))

(deftest stirling-tests
  (checking "stirling first kind identities" 100
            [n (gen/choose 1 40)
             k (gen/choose 1 40)]

            (when (> k n)
              (is (zero? (sf/stirling-first-kind n k))
                  "top right half of triangle in
                  https://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind"))

            (is (zero? (sf/stirling-first-kind 0 k)))
            (is (zero? (sf/stirling-first-kind n 0)))

            (is (= (sf/factorial (dec n))
                   (sf/stirling-first-kind n 1 :unsigned? true)))

            (is (= 1 (sf/stirling-first-kind n n)))

            (when (> n 2)
              (is (= (* (/ (dec (* 3 n)) 4)
                        (sf/binomial-coefficient n 3))
                     (sf/stirling-first-kind n (- n 2))))))

  (testing "Stirling numbers of the first kind, from numeric.scm"
    (testing "initial conditions from wikipedia:"
      (is (= 1 (sf/stirling-first-kind 0 0)))
      (is (zero? (sf/stirling-first-kind 10 0)))
      (is (zero? (sf/stirling-first-kind 0 10))))

    (is (= 1 (sf/stirling-first-kind 1 1)))
    (is (= -1 (sf/stirling-first-kind 2 1)))
    (is (= 1 (sf/stirling-first-kind 2 2)))
    (is (= 2 (sf/stirling-first-kind 3 1)))
    (is (= -3 (sf/stirling-first-kind 3 2)))
    (is (= -50 (sf/stirling-first-kind 5 2)))
    (is (= 1624 (sf/stirling-first-kind 7 3)))
    (is (= #sicm/bigint "-62262192842035613491057459200000"
           (sf/stirling-first-kind 30 3))))

  (checking "stirling second kind identities" 100
            [n (gen/choose 1 40)
             k (gen/choose 1 40)]

            (when (> k n)
              (is (zero? (sf/stirling-second-kind n k))
                  "top right half of triangle in
                  https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind"))

            (is (zero? (sf/stirling-second-kind 0 k)))
            (is (zero? (sf/stirling-second-kind n 0)))

            (is (= 1 (sf/stirling-second-kind n n)))

            (when (> n 1)
              (is (= (sf/binomial-coefficient n 2)
                     (sf/stirling-second-kind n (dec n)))))

            (when (> n 1)
              (is (= (sf/binomial-coefficient n 2)
                     (sf/stirling-second-kind n (dec n)))
                  "dividing n elements into n − 1 sets necessarily means
                  dividing it into one set of size 2 and n − 2 sets of size
                  1"))

            (is (= (dec (g/expt 2 (dec n)))
                   (sf/stirling-second-kind n 2))))

  (testing "Stirling, second kind"
    (is (= 1 (sf/stirling-second-kind 0 0)))
    (is (= 25 (sf/stirling-second-kind 5 3)))))
