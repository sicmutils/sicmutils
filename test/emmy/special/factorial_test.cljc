#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.special.factorial-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.generic :as g]
            [emmy.polynomial :as poly]
            [emmy.series :as series]
            [emmy.special.factorial :as sf]
            [emmy.value :as v]))

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
    (is (= #sicm/ratio 1/1716 (sf/falling-factorial 10 -3)))

    (testing "falling factorial works on unital rings, functions are game!"
      (is (= '(/ 1 (+ (expt x 3) (* 6 (expt x 2)) (* 11 x) 6))
             (v/freeze
              (g/simplify
               ((sf/falling-factorial g/+ -3) 'x))))
          "negative second arg")

      (is (= '(+ (expt x 3) (* -3 (expt x 2)) (* 2 x))
             (v/freeze
              (g/simplify
               ((sf/falling-factorial g/+ 3) 'x))))
          "positive second arg"))

    (letfn [(check-against-stirling [n]
              (let [coeffs (poly/coefficients
                            (sf/falling-factorial (poly/identity) n))]
                (is (v/= coeffs
                         (map #(sf/stirling-first-kind n %)
                              (range 1 (inc n))))
                    "algebraic definition of stirling numbers; they are the
                    coefficients of the polynomial expansion of the falling
                    factorial.")))]
      (check-against-stirling 4)
      (check-against-stirling 8))

    (checking "small input falling-factorial laws" 10
              [x (gen/choose 1 40)
               m (gen/choose 1 40)
               n (gen/choose 1 40)]
              (is (= (sf/falling-factorial x n)
                     (sf/factorial-power x n))
                  "checking alias")

              (is (= (sf/factorial x)
                     (sf/falling-factorial x x)
                     (sf/falling-factorial x (dec x)))
                  "integer to power of itself (falling), or itself-1 falling, ==
                  factorial")

              (is (= (sf/rising-factorial x n)
                     (sf/falling-factorial (+ x n -1) n))
                  "rising in terms of falling")

              (is (= (sf/falling-factorial x (+ m n))
                     (g/* (sf/falling-factorial x m)
                          (sf/falling-factorial (- x m) n)))
                  "x^(m+n) falling == x^m falling (x-m)^n falling"))

    (checking "falling-factorial laws" 100 [x gen/small-integer]
              (is (= x (sf/falling-factorial x 1))
                  "x^1 falling == x")

              (is (= 1 (sf/falling-factorial x 0))
                  "x^0 falling == 1")))

  (testing "rising-factorial"
    (is (g/infinite?
         (sf/rising-factorial 3 -5)))
    (is (= -720 (sf/rising-factorial -10 3)))
    (is (= #sicm/ratio -1/1716 (sf/rising-factorial -10 -3)))
    (is (= #sicm/ratio 1/504 (sf/rising-factorial 10 -3)))

    (testing "rising factorial works on unital rings, functions are game!"
      (is (= '(/ 1 (+ (expt x 3) (* -6 (expt x 2)) (* 11 x) -6))
             (v/freeze
              (g/simplify
               ((sf/rising-factorial g/+ -3) 'x))))
          "negative second arg")

      (is (= '(+ (expt x 3) (* 3 (expt x 2)) (* 2 x))
             (v/freeze
              (g/simplify
               ((sf/rising-factorial g/+ 3) 'x))))
          "positive second arg"))

    (checking "small input rising-factorial laws" 10
              [x (gen/choose 1 40)
               m (gen/choose 1 40)
               n (gen/choose 1 40)]
              (is (= (sf/rising-factorial x n)
                     (sf/pochhammer x n))
                  "checking alias")

              (is (= (sf/factorial x)
                     (sf/rising-factorial 1 x))
                  "1^x rising == factorial")

              (is (= (sf/rising-factorial x (+ m n))
                     (g/* (sf/rising-factorial x m)
                          (sf/rising-factorial (+ x m) n)))
                  "x^(m+n) rising == x^m rising (x+m)^n rising"))

    (checking "rising-factorial laws" 100 [x gen/small-integer]
              (is (= x (sf/rising-factorial x 1))
                  "x^1 falling == x")

              (is (= 1 (sf/falling-factorial x 0))
                  "x^0 falling == 1")))

  (testing "multi-factorial"
    (is (= 162 (sf/multi-factorial 9 3)))

    (checking "multi-factorial laws" 100 [x (gen/choose 1 40)]
              (is (= (sf/factorial x)
                     (sf/multi-factorial x 1)))

              (is (= 1 (sf/multi-factorial 0 x)))
              (is (= 1 (sf/multi-factorial 1 x)))

              (when (> x 1)
                (is (= (sf/factorial x)
                       (g/* (sf/multi-factorial x 3)
                            (sf/multi-factorial (- x 1) 3)
                            (sf/multi-factorial (- x 2) 3)))))))

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
  (is (= 1001 (sf/binomial-coefficient -11 4)))

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
  (testing "harmonic numbers defined via stirling"
    (letfn [(harmonic [n]
              (g// (sf/stirling-first-kind
                    (inc n) 2 :unsigned? true)
                   (sf/factorial n)))]
      (is (every? true?
                  (map =
                       (map harmonic (range 1 50))
                       series/harmonic-series))
          "Check the first 50 harmonic numbers against the harmonic series.")))

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
              (is (= (g/* (/ (dec (* 3 n)) 4)
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
    (is (= 25 (sf/stirling-second-kind 5 3))))

  (testing "Bell numbers"
    (is (= [1 1 2 5 15 52 203 877 4140 21147 115975]
           (map sf/bell (range 11))))))
