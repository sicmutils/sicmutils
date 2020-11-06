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

(ns sicmutils.series-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.generic :as g]
            [sicmutils.series :as s]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)


(deftest bare-sequence-tests
  (let [->series @#'s/->series]
    (testing "make a sequence"
      (is (= [1 2 3 4 0 0 0 0 0 0]
             (take 10 (->series [1 2 3 4])))))

    (testing "seq:negate"
      (let [xs [1 2 3 4]]
        (is (= [-1 -2 -3 -4 0 0 0]
               (take 7 (@#'s/seq:negate (->series xs)))))))

    (testing "series addition"
      (is (= [0 2 4 6 8]
             (take 5 (@#'s/seq:+ (range) (range)))))

      (let [series (->series [1 2 3 4])]
        (is (= [11 2 3 4 0 0]
               (take 6 (@#'s/seq+c series 10))
               (take 6 (@#'s/c+seq 10 series))))))

    (testing "series subtraction"
      (is (= [0 0 0 0 0]
             (take 5 (@#'s/seq:- (range) (range)))))

      (is (= [-10 1 2 3 4]
             (take 5 (@#'s/seq-c (range) 10))))

      (is (= [10 -1 -2 -3 -4]
             (take 5 (@#'s/c-seq 10 (range))))))

    (testing "series multiplication"
      (is (= [0 4 11 20 30 40 50 60 70 80]
             (take 10 (@#'s/seq:* (range) (->series [4 3 2 1]))))))

    (testing "series division"
      (let [series (->series [0 0 0 4 3 2 1])]
        (= [1 0 0 0 0]
           (take 5 (@#'s/seq:div series series)))))

    (testing "sequence invert"
      (let [series (iterate inc 3)]
        (is (= (take 5 (@#'s/seq:invert series))
               (take 5 (@#'s/seq:div (->series [1]) series)))
            "invert(xs) matches div(1, xs)"))

      (let [series (iterate inc 3)]
        (is (= [1 0 0 0 0]
               (take 5 (@#'s/seq:* series (@#'s/seq:invert series)))
               (take 5 (@#'s/seq:div series series))))))

    (testing "constant division"
      (let [nats (iterate inc 1)]
        (is (= [4 -8 4 0 0 0]
               (take 6 (@#'s/c-div-seq 4 nats)))))

      (let [nats       (iterate inc 1)
            divided    (@#'s/c-div-seq 4 nats)
            seq-over-4 (@#'s/seq:invert divided)
            original   (@#'s/seq*c seq-over-4 4)]
        (is (= (take 5 nats)
               (take 5 original))
            "We can recover the nats as expected."))

      (let [nats (iterate inc 1)]
        (is (= [1 2 3 4 5]
               (take 5 (@#'s/seq-div-c (@#'s/seq*c nats 2) 2))))))

    (testing "series composition"
      (is (= [1 0 1 0 1 0 1 0 1 0]
             (take 10 (@#'s/seq:compose
                       (repeat 1)
                       (->series [0 0 1]))))))

    (testing "series reversion"
      (let [f (cons 0 (iterate inc 1))]
        (is (= [0 1 0 0 0]
               (take 5 (@#'s/seq:compose f (@#'s/seq:revert f))))))

      (is (= [0 1 -2 5 -14]
             (take 5 (@#'s/seq:revert
                      (cons 0 (iterate inc 1)))))))

    (testing "series derivative"
      (is (= [1 2 3 4 5 6] ;; 1 + 2x + 3x^2 + ...
             (take 6 (@#'s/seq:deriv (repeat 1))))))

    (testing "series integral"
      (is (= [5 1 1 1 1 1]
             (take 6 (@#'s/seq:integral
                      (iterate inc 1) 5))))

      (is (= [0 1 1 1 1 1]
             (take 6 (@#'s/seq:integral
                      (iterate inc 1))))
          "By default, constant is 0."))

    (testing "series sqrt"
      (let [xs (iterate inc 1)]
        (is (= [1 2 3 4 5 6]
               (take 6 (@#'s/seq:*
                        (@#'s/seq:sqrt xs)
                        (@#'s/seq:sqrt xs))))))

      (let [xs (iterate inc 9)]
        (is (= [9 10 11 12 13 14]
               (take 6 (@#'s/seq:*
                        (@#'s/seq:sqrt xs)
                        (@#'s/seq:sqrt xs))))))

      (let [xs (concat [0 0] (iterate inc 9))]
        (is (= [0 0 9 10 11 12]
               (take 6 (@#'s/seq:*
                        (@#'s/seq:sqrt xs)
                        (@#'s/seq:sqrt xs)))))))

    (testing "more examples"
      (is (= [1 0 -6 0 12 0 -8 0 0 0]
             (take 10 (@#'s/seq:expt
                       (->series [1 0 -2]) 3)))
          "polynomial example"))))

(deftest more-mcilroy-tests
  (is (->> (@#'s/seq:- @#'s/sinx
            (@#'s/seq:sqrt (@#'s/c-seq 1 (@#'s/seq:expt @#'s/cosx 2))))
           (take 30)
           (every? zero?)))

  (is (->> (@#'s/seq:- (@#'s/seq:div @#'s/sinx @#'s/cosx)
            (@#'s/seq:revert
             (@#'s/seq:integral
              (@#'s/seq:invert (@#'s/->series [1 0 1])))))
           (take 30)
           (every? zero?))))

(comment
  "TODO
- seq:p-value tests for the case I worked out.
- test that seq:sqrt is fine with a rational number, and ish? with floating
  point.

- test that nth works on series.
- test that power-series, series keeps its type with fmap.
- add compose to the general API
- add revert to the general API JUST for power series.
- add integrate to the general API JUST for power series!
- add sqrt
")

(deftest series-test
  (testing "basics"
    (let [Q (s/series 4)
          R (s/series 4 3)
          S (s/series 4 3 2)
          ones (s/generate (constantly 1))
          nats0 (s/generate identity)
          nats (s/generate inc)]

      (testing "series act as seqs"
        (is (= '(4 0 0 0 0 0 0 0) (take 8 Q)))
        (is (= '(4 3 0 0 0 0 0 0) (take 8 R)))
        (is (= '(4 3 2 0 0 0 0 0) (take 8 S)))
        (is (= '(0 1 2 3) (take 4 nats0))))

      (testing "generating series"
        (is (= '(0 1 4 9)
               (take 4 (s/generate g/square))))

        (is (= '(0 2 6 12)
               (->> (s/generate g/square)
                    (g/+ nats0)
                    (take 4)))))

      (testing "series addition"
        (is (= '(8 6 2 0 0 0 0 0) (take 8 (g/+ R S)))))

      (testing "series subtraction"
        (is (= '(-3 -6 -9 -12) (take 4 (g/negate (g/* nats 3)))))
        (is (= '(-3 -6 -9 -12) (take 4 (g/negate (g/* 3 nats))))))

      (testing "series multiplication"
        (is (= '(3 6 9 12) (take 4 (g/* 3 nats))))
        (is (= '(3 6 9 12) (take 4 (g/* nats 3))))
        (is (= '(ε (* 2 ε) (* 3 ε) (* 4 ε))
               (g/simplify
                (take 4 (g/* nats 'ε)))))

        (is (= '(ε (* 2 ε) (* 3 ε) (* 4 ε))
               (g/simplify
                (take 4 (g/* 'ε nats))))))

      (testing "division"
        )

      (testing "square"
        (is (= '(1 2 3 4 5 6)
               (take 6 (g/square ones)))))

      (testing "fmap"
        (is (= '(1 4 9 16) (take 4 (s/fmap g/square nats)))))

      (testing "summing N elements of a series"
        (is (= 4 (s/sum S 0)))
        (is (= 7 (s/sum S 1)))
        (is (= 9 (s/sum S 2)))
        (is (= 7 (s/sum R 8)))
        (is (= 4 (s/sum Q 20)))
        (is (= 9 (s/sum S 3)))
        (is (= 9 (s/sum S 4))))

      (is (= '(0 -2 -6 -12)
             (take 4 (g/negate
                      (g/+ nats0 (s/generate g/square))))))

      (is (= '(0 m (* 2 m) (* 3 m))
             (g/simplify
              (take 4 (g/* 'm nats0)))))
      (is (= '(0 r (* 2 r) (* 3 r))
             (g/simplify (take 4 (g/* 'r nats0)))))
      (is (= '(3 5 7 0 0 0 0 0)
             (take 8 (g/+ (s/series 1 2 3)
                          (s/series 2 3 4)))))
      (is (= '(1 4 10 12 9 0 0)
             (g/simplify
              (take 7 (g/*
                       (s/series 1 2 3)
                       (s/series 1 2 3))))))

      ;; the tetrahedral numbers
      (is (= '(1 4 10 20 35 56 84)
             (take 7 (g/square nats))))
      (is (= '(m (* 4 m) (* 10 m) (* 20 m))
             (->> (s/generate inc)
                  g/square
                  (g/* 'm)
                  (take 4)
                  g/simplify)))

      ;; the triangular numbers, via convolution
      (is (= '(1 3 6 10 15 21)
             (take 6 (g/* ones nats))))

      ;; again, via partial sums
      (is (= '(1 3 6 10 15 21)
             (take 6 (s/partial-sums nats))))

      (is (= '(1 2 3 4 5 6)
             (take 6 (s/partial-sums ones))))

      (is (= [#sicm/ratio 17/4
              #sicm/ratio 7/2
              #sicm/ratio 11/4
              1]
             (take 4 (g/+ (g/* #sicm/ratio 1/4 nats) S)))))))
