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
            [sicmutils.series :as series]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest series-test
  (testing "basics"
    (let [Q (series/starting-with 4)
          R (series/starting-with 4 3)
          S (series/starting-with 4 3 2)
          ones (series/generate (constantly 1))
          nats0 (series/generate identity)
          nats (series/generate inc)]

      (testing "series act as seqs"
        (is (= '(4 0 0 0 0 0 0 0) (take 8 Q)))
        (is (= '(4 3 0 0 0 0 0 0) (take 8 R)))
        (is (= '(4 3 2 0 0 0 0 0) (take 8 S)))
        (is (= '(0 1 2 3) (take 4 nats0))))

      (testing "generating series"
        (is (= '(0 1 4 9)
               (take 4 (series/generate g/square))))

        (is (= '(0 2 6 12)
               (->> (series/generate g/square)
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
        (is (= '(1 4 9 16) (take 4 (series/fmap g/square nats)))))

      (testing "summing N elements of a series"
        (is (= 4 (series/sum S 0)))
        (is (= 7 (series/sum S 1)))
        (is (= 9 (series/sum S 2)))
        (is (= 7 (series/sum R 8)))
        (is (= 4 (series/sum Q 20)))
        (is (= 9 (series/sum S 3)))
        (is (= 9 (series/sum S 4))))

      (is (= '(0 -2 -6 -12)
             (take 4 (g/negate
                      (g/+ nats0 (series/generate g/square))))))

      (is (= '(0 m (* 2 m) (* 3 m))
             (g/simplify
              (take 4 (g/* 'm nats0)))))
      (is (= '(0 r (* 2 r) (* 3 r))
             (g/simplify (take 4 (g/* 'r nats0)))))
      (is (= '(3 5 7 0 0 0 0 0)
             (take 8 (g/+ (series/starting-with 1 2 3)
                          (series/starting-with 2 3 4)))))
      (is (= '(1 4 10 12 9 0 0)
             (g/simplify
              (take 7 (g/*
                       (series/starting-with 1 2 3)
                       (series/starting-with 1 2 3))))))

      ;; the tetrahedral numbers
      (is (= '(1 4 10 20 35 56 84)
             (take 7 (g/square nats))))
      (is (= '(m (* 4 m) (* 10 m) (* 20 m))
             (->> (series/generate inc)
                  g/square
                  (g/* 'm)
                  (take 4)
                  g/simplify)))

      ;; the triangular numbers, via convolution
      (is (= '(1 3 6 10 15 21)
             (take 6 (g/* ones nats))))

      ;; again, via partial sums
      (is (= '(1 3 6 10 15 21)
             (take 6 (series/partial-sums nats))))

      (is (= '(1 2 3 4 5 6)
             (take 6 (series/partial-sums ones))))

      (is (= [#sicm/ratio 17/4
              #sicm/ratio 7/2
              #sicm/ratio 11/4
              1]
             (take 4 (g/+ (g/* #sicm/ratio 1/4 nats) S)))))))
