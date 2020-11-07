;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.series.impl-test
  (:require [clojure.test :refer [is deftest testing ]]
            [sicmutils.series.impl :as i]
            [sicmutils.value :as v]))

(deftest bare-sequence-tests
  (let [->series i/->series]
    (testing "make a sequence"
      (is (= [1 2 3 4 0 0 0 0 0 0]
             (take 10 (->series [1 2 3 4])))))

    (testing "negate"
      (let [xs [1 2 3 4]]
        (is (= [-1 -2 -3 -4 0 0 0]
               (take 7 (i/negate (->series xs)))))))

    (testing "series addition"
      (is (= [0 2 4 6 8]
             (take 5 (i/seq:+ (range) (range)))))

      (let [series (->series [1 2 3 4])]
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
             (take 10 (i/seq:* (range) (->series [4 3 2 1]))))))

    (testing "series division"
      (let [series (->series [0 0 0 4 3 2 1])]
        (= [1 0 0 0 0]
           (take 5 (i/div series series)))))

    (testing "sequence invert"
      (let [series (iterate inc 3)]
        (is (= (take 5 (i/invert series))
               (take 5 (i/div (->series [1]) series)))
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
                       (->series [0 0 1]))))))

    (testing "series reversion"
      (let [f (cons 0 (iterate inc 1))]
        (is (= [0 1 0 0 0]
               (take 5 (i/compose f (i/revert f))))))

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
                       (->series [1 0 -2]) 3)))
          "polynomial example"))))

(deftest more-mcilroy-tests
  (is (->> (i/seq:- i/sinx
                    (i/sqrt (i/c-seq 1 (i/expt i/cosx 2))))
           (take 30)
           (every? v/nullity?)))

  (is (->> (i/seq:- (i/div i/sinx i/cosx)
                    (i/revert
                     (i/integral
                      (i/invert (i/->series [1 0 1])))))
           (take 30)
           (every? v/nullity?))))

(comment
  "TODO
- p-value tests for the case I worked out.
- test that sqrt is fine with a rational number, and ish? with floating
  point.

- test that nth works on series.
- test that power-series, series keeps its type with fmap.
- add compose to the general API
- add revert to the general API JUST for power series.
- add integrate to the general API JUST for power series!
- add sqrt
")
