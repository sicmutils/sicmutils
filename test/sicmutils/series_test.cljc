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
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.series :as s]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :once hermetic-simplify-fixture)

(comment
  "TODO
- seq:p-value tests for the case I worked out.
- test that seq:sqrt is fine with a rational number, and ish? with floating
  point.

- test that nth works on series.
- test that power-series, series keeps its type with fmap.
- add sqrt
")

(deftest generic-series-tests
  (let [Q (s/power-series 4)
        R (s/power-series 4 3)
        S (s/power-series 4 3 2)
        ones (s/generate (constantly 1))
        nats0 (s/generate identity)
        nats (s/generate inc)]

    (testing "series act as seqs"
      (is (= [4 0 0 0 0 0 0 0] (take 8 Q)))
      (is (= [4 3 0 0 0 0 0 0] (take 8 R)))
      (is (= [4 3 2 0 0 0 0 0] (take 8 S)))
      (is (= [0 1 2 3] (take 4 nats0)))
      (is (= 4 (nth nats0 4))))

    (testing "generating series"
      (is (= [0 1 4 9]
             (take 4 (s/generate g/square))))

      (is (= [0 2 6 12]
             (->> (g/+ nats0 (s/generate g/square))
                  (take 4)))))

    (testing "series addition"
      (is (= [8 6 2 0 0 0 0 0]
             (take 8 (g/+ R S))))

      (is (= '(3 5 7 0 0 0 0 0)
             (take 8 (g/+ (s/series 1 2 3)
                          (s/series 2 3 4))))))

    (testing "series subtraction"
      (is (= [-3 -6 -9 -12]
             (take 4 (g/negate (g/* nats 3)))))

      (is (= '(-3 -6 -9 -12)
             (take 4 (g/negate (g/* 3 nats))))))

    (testing "series multiplication"
      (is (= '(3 6 9 12) (take 4 (g/* 3 nats))))
      (is (= '(3 6 9 12) (take 4 (g/* nats 3))))
      (is (= '(1 4 10 12 9 0 0)
             (g/simplify
              (take 7 (g/*
                       (s/series 1 2 3)
                       (s/series 1 2 3))))))

      (testing "rational"
        (is (= [#sicm/ratio 17/4
                #sicm/ratio 7/2
                #sicm/ratio 11/4
                1]
               (take 4 (g/+ (g/* #sicm/ratio 1/4 nats) S)))))

      (is (= '(ε (* 2 ε) (* 3 ε) (* 4 ε))
             (g/simplify
              (take 4 (g/* nats 'ε)))))

      (is (= '(0 r (* 2 r) (* 3 r))
             (g/simplify
              (take 4 (g/* 'r nats0)))))

      (is (= '(ε (* 2 ε) (* 3 ε) (* 4 ε))
             (g/simplify
              (take 4 (g/* 'ε nats)))))

      (is (= '(0 m (* 2 m) (* 3 m))
             (g/simplify
              (take 4 (g/* 'm nats0))))))

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

    (testing "partial-sums"
      (is (= '(1 2 3 4 5 6)
             (take 6 (s/partial-sums ones)))))

    (testing "miscellaneous"
      (is (= '(0 -2 -6 -12)
             (take 4 (g/negate
                      (g/+ nats0 (s/generate g/square)))))))

    (let [triangle (g/* ones nats)]
      (testing "triangular numbers https://en.wikipedia.org/wiki/Triangular_number"
        (is (= '(1 3 6 10 15 21)
               (take 6 triangle))
            "via convolution")

        (is (= '(1 3 6 10 15 21)
               (take 6 (s/partial-sums nats)))
            "via partial sums"))

      (testing "tetrahedral numbers https://en.wikipedia.org/wiki/Tetrahedral_number"
        (is (= '(1 4 10 20 35 56 84)
               (take 7 (g/square nats))))
        (is (= '(m (* 4 m) (* 10 m) (* 20 m))
               (->> (s/generate inc)
                    g/square
                    (g/* 'm)
                    (take 4)
                    g/simplify)))

        (is (= [1 4 10 20 35 56 84]
               (take 7 (s/partial-sums triangle)))
            "The tetrahedral numbers are the partial sums of the triangular
              numbers")))))


(deftest new-series-fns-test
  (testing "inflate"
    (is (= [0 0 0 1 0]
           (take 5 (s/inflate s/identity 3))))

    (is (= [1 0 2 0 3 0]
           (take 6 (s/inflate (s/generate inc) 2)))))

  (testing "woah, more generics"
    (with-comparator (v/within 1e-6)
      (ish? (Math/exp (Math/sin 2.2))
            (s/sum ((g/exp s/sin-series) 2.2) 50)))))
