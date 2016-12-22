;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
  (:require [clojure.test :refer :all]
            [sicmutils
             [numsymb]
             [simplify]
             [generic :as g]
             [series :as series]
             [value :as v]
             ]))


(deftest series-test
  (let [Q (series/starting-with 4)
        R (series/starting-with 4 3)
        S (series/starting-with 4 3 2)]
    (is (= '(4 0 0 0 0 0 0 0) (take 8 (series/->seq Q))))
    (is (= '(4 3 0 0 0 0 0 0) (take 8 (series/->seq R))))
    (is (= '(4 3 2 0 0 0 0 0) (take 8 (series/->seq S))))
    (is (= 4 (series/sum S 1)))
    (is (= 7 (series/sum S 2)))
    (is (= 7 (series/sum R 8)))
    (is (= 4 (series/sum Q 20)))
    (is (= 9 (series/sum S 3)))
    (is (= 9 (series/sum S 4)))
    (is (= '(0 1 2 3) (take 4 (series/->seq (series/generate identity)))))
    (is (= '(0 1 4 9) (take 4 (series/->seq (series/generate g/square)))))
    (is (= '(0 2 6 12) (take 4 (series/->seq
                                (g/+
                                 (series/generate identity)
                                 (series/generate g/square)
                                 )))))
    (is (= '(0 m (* 2 m) (* 3 m))
           (->> (series/generate identity)
                (g/* 'm)
                series/->seq
                (take 4)
                g/simplify)))
    (is (= '(0 r (* 2 r) (* 3 r))
           (g/simplify (take 4 (series/->seq (g/* 'r (series/generate identity)))))))
    (is (= '(3 5 7 0 0 0 0 0)
           (take 8
                 (series/->seq
                  (g/+ (series/starting-with 1 2 3)
                       (series/starting-with 2 3 4))))))
    (is (= '(1 4 10 12 9 0 0)
           (g/simplify
            (take 7
                  (series/->seq
                   (g/*
                    (series/starting-with 1 2 3)
                    (series/starting-with 1 2 3)))))))
    (is (= '(1 4 10 20 35 56 84)
           (take 7
                 (series/->seq
                  (g/square
                   (series/generate inc))))))
    (is (= '(m (* 4 m) (* 10 m) (* 20 m))
           (->> (series/generate inc)
                g/square
                (g/* 'm)
                series/->seq
                (take 4)
                g/simplify
                )))
    (is (= '(1 2 3 4 5 6)
           (->> (series/generate (constantly 1))
                g/square
                series/->seq
                (take 6))))
    ;; the triangular numbers
    (is (= '(1 3 6 10 15 21)
           (->> (g/* (series/generate (constantly 1))
                     (series/generate inc))
                series/->seq
                (take 6))))))
