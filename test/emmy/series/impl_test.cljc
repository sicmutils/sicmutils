#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.series.impl-test
  (:require [clojure.test :refer [is deftest testing ]]
            [emmy.series.impl :as i]
            [emmy.value :as v]))

(deftest sequence-series-tests
  (testing "make a sequence"
    (is (= [1 2 3 4 0 0 0 0 0 0]
           (take 10 (i/->series [1 2 3 4])))))

  (testing "negate"
    (let [xs [1 2 3 4]]
      (is (= [-1 -2 -3 -4 0 0 0]
             (take 7 (i/negate (i/->series xs)))))))

  (testing "series addition"
    (is (= [0 2 4 6 8]
           (take 5 (i/seq:+ (range) (range)))))

    (let [series (i/->series [1 2 3 4])]
      (is (= [11 2 3 4 0 0]
             (take 6 (i/seq+c series 10))
             (take 6 (i/c+seq 10 series))))))

  (testing "series subtraction"
    (is (= [0 0 0 0 0]
           (take 5 (i/seq:- (range) (range)))))

    (is (= [-10 1 2 3 4]
           (take 5 (i/seq-c (range) 10))))

    (is (= [10 -1 -2 -3 -4]
           (take 5 (i/c-seq 10 (range))))))

  (testing "series multiplication"
    (is (= [0 4 11 20 30 40 50 60 70 80]
           (take 10 (i/seq:* (range) (i/->series [4 3 2 1]))))))

  (testing "series division"
    (let [series (i/->series [0 0 0 4 3 2 1])]
      (is (= [1 0 0 0 0]
             (take 5 (i/div series series))))))

  (testing "sequence invert"
    (let [series (iterate inc 3)]
      (is (= (take 5 (i/invert series))
             (take 5 (i/div (i/->series [1]) series)))
          "invert(xs) matches div(1, xs)"))

    (let [series (iterate inc 3)]
      (is (= [1 0 0 0 0]
             (take 5 (i/seq:* series (i/invert series)))
             (take 5 (i/div series series))))))

  (testing "constant division"
    (let [nats (iterate inc 1)]
      (is (= [4 -8 4 0 0 0]
             (take 6 (i/c-div-seq 4 nats)))))

    (let [nats       (iterate inc 1)
          divided    (i/c-div-seq 4 nats)
          seq-over-4 (i/invert divided)
          original   (i/seq*c seq-over-4 4)]
      (is (= (take 5 nats)
             (take 5 original))
          "We can recover the nats as expected."))

    (let [nats (iterate inc 1)]
      (is (= [1 2 3 4 5]
             (take 5 (i/seq-div-c (i/seq*c nats 2) 2))))))

  (testing "series composition"
    (is (= [1 0 1 0 1 0 1 0 1 0]
           (take 10 (i/compose
                     (repeat 1)
                     (i/->series [0 0 1]))))
        "square all entries"))

  (testing "series reversion"
    (let [f (cons 0 (iterate inc 1))]
      (is (= [0 1 0 0 0]
             (take 5 (i/compose f (i/revert f))))
          "compose with reversion gives identity"))

    (is (= [0 1 -2 5 -14]
           (take 5 (i/revert
                    (cons 0 (iterate inc 1)))))))

  (testing "series derivative"
    (is (= [1 2 3 4 5 6] ;; 1 + 2x + 3x^2 + ...
           (take 6 (i/deriv (repeat 1))))))

  (testing "series integral"
    (is (= [5 1 1 1 1 1]
           (take 6 (i/integral
                    (iterate inc 1) 5))))

    (is (= [0 1 1 1 1 1]
           (take 6 (i/integral
                    (iterate inc 1))))
        "By default, constant is 0."))

  (testing "expt"
    (is (= (take 11 (i/binomial 10))
           (take 11 (i/expt (i/->series [1 1]) 10)))
        "(1 + x)^10 = binomial expansion")

    (let [s (i/->series [1 0 3 1])]
      (is (= (take 13 (i/seq:* (i/seq:* s s)
                               (i/seq:* s s)))
             (take 13 (i/expt s 4)))
          "(1 + 3x^2 + x^3)^4, expt matches mul")))

  (testing "series sqrt"
    (let [xs (iterate inc 1)]
      (is (= [1 2 3 4 5 6]
             (take 6 (i/seq:*
                      (i/sqrt xs)
                      (i/sqrt xs))))))

    (let [xs (iterate inc 9)]
      (is (= [9 10 11 12 13 14]
             (take 6 (i/seq:*
                      (i/sqrt xs)
                      (i/sqrt xs))))))

    (let [xs (concat [0 0] (iterate inc 9))]
      (is (= [0 0 9 10 11 12]
             (take 6 (i/seq:*
                      (i/sqrt xs)
                      (i/sqrt xs)))))))

  (testing "more examples"
    (is (= [1 0 -6 0 12 0 -8 0 0 0]
           (take 10 (i/expt
                     (i/->series [1 0 -2]) 3)))
        "correct expansion of (1-2x^2)^3")

    (is (= (take 10 (repeat 1))
           (take 10 (i/div (i/->series [1])
                           (i/->series [1 -1]))))
        "power series of 1/(1-x)")

    (is (= (take 10 (iterate inc 1))
           (take 10 (i/div (i/->series [1])
                           (-> (i/->series [1 -1])
                               (i/expt 2)))))
        "deriv gives series of 1/(1-x)^2")))

(deftest power-series-examples
  (testing "expansion of e^x"
    (is (= '(1
             1
             (/ 1 2)
             (/ 1 6)
             (/ 1 24)
             (/ 1 120)
             (/ 1 720)
             (/ 1 5040)
             (/ 1 40320)
             (/ 1 362880))
           (v/freeze (take 10 i/expx)))))

  (testing "sine expansion"
    (is (= '(0
             1
             0
             (/ -1 6)
             0
             (/ 1 120)
             0
             (/ -1 5040)
             0
             (/ 1 362880))
           (v/freeze (take 10 i/sinx)))))

  (testing "cosine expansion"
    (is (= '(1
             0
             (/ -1 2)
             0
             (/ 1 24)
             0
             (/ -1 720)
             0
             (/ 1 40320)
             0)
           (v/freeze (take 10 i/cosx)))))

  (testing "catalan numbers"
    (is (= [1 1 2 5 14 42 132 429 1430 4862]
           (take 10 i/catalan)))))

(deftest series-identity-tests
  (is (->> (i/seq:- i/sinx
                    (i/sqrt (i/c-seq 1 (i/expt i/cosx 2))))
           (take 30)
           (every? v/zero?))
      "sin(x) = sqrt(1-cos(x)^2) to 30 terms")

  (is (->> (i/seq:- i/tanx (i/revert i/atanx))
           (take 30)
           (every? v/zero?))
      "tan(x) = revert(arctan(x))")

  (is (->> (i/seq:- i/atanx
                    (i/integral
                     (i/invert (i/->series [1 0 1]))))
           (take 30)
           (every? v/zero?))
      "atan(x) = integral(1/(1+x^2))"))
