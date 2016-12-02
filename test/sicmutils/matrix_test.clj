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

(ns sicmutils.matrix-test
  (:refer-clojure :exclude [+ - * / zero? partial ref map])
  (:require [clojure.test :refer :all]
            [sicmutils
             [matrix :as matrix]
             [structure :as s]
             [generic :as g]
             [value :as v]]
            [sicmutils.calculus.derivative :as d]))

(deftest matrix-basics
  (let [M (matrix/by-rows (list 1 2 3)
                   (list 4 5 6))
        v (matrix/column 7 8 9)]
    (is (= ::sicmutils.matrix/matrix (v/kind M)))
    (is (= '(matrix-by-rows [1 2 3] [4 5 6]) (v/freeze M)))
    (is (= (matrix/by-rows [1 4] [2 5] [3 6]) (matrix/transpose M)))
    (is (= (matrix/by-rows [0 0 0] [0 0 0]) (v/zero-like M)))
    (is (thrown? IllegalArgumentException (matrix/by-rows [1 2 3] [4 5])))
    (is (thrown? AssertionError (matrix/by-rows)))
    (is (= 5 (matrix/get-in M [1 1])))
    (is (= 3 (matrix/get-in M [0 2])))
    (is (= [4 5 6] (matrix/get-in M [1])))
    (is (= 8 (matrix/get-in v [1])))
    (is (= (matrix/by-rows [2 3 4]
                           [5 6 7]) (matrix/map inc M)))
    ))

(deftest structure
  (let [A (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))]
    (is (= 8 (s/dimension A)))
    (let [vs (s/up (s/up 'vx1 'vy1) (s/up 'vx2 'vy2))
          L1 (fn [[v1 v2]]
               (g/+ (g/* 1/2 'm1 (g/square v1))
                    (g/* 1/2 'm2 (g/square v2))))]
      (is (= '(matrix-by-rows [m1 0 0 0]
                              [0 m1 0 0]
                              [0 0 m2 0]
                              [0 0 0 m2])
             (g/simplify (matrix/s->m vs (((g/expt d/D 2) L1) vs) vs))))))
  (let [M (matrix/by-rows [1 2 3] [4 5 6])]
    (is (= (s/down (s/up 1 4)
                   (s/up 2 5)
                   (s/up 3 6))
           (matrix/->structure M)))))

(deftest matrix-mul
  (let [M (matrix/by-rows '[a b] '[c d])
        S (matrix/by-rows '[e f] '[g h])]

    (is (= '(matrix-by-rows [(+ (* a e) (* b g)) (+ (* a f) (* b h))]
                            [(+ (* c e) (* d g)) (+ (* c f) (* d h))])
           (g/simplify (g/* M S)))))
  (let [M (matrix/by-rows [1 2 3]
                   [2 3 4])
        S (matrix/by-rows [3 4]
                   [4 5]
                   [5 6])]
    (is (= (matrix/by-rows [26 32] [38 47]) (g/* M S))))
  (let [M (matrix/by-rows '[a b] '[c d])
        d (s/down 'x 'y)
        u (s/up 'x 'y)]
    (is (= (s/up (g/+ (g/* 'a 'x) (g/* 'b 'y)) (g/+ (g/* 'c 'x) (g/* 'd 'y)))
           (g/* M u)))
    (is (= (s/down (g/+ (g/* 'x 'a) (g/* 'y 'b)) (g/+ (g/* 'x 'c) (g/* 'y 'd)))
           (g/* d M)))
    (is (= '(+ (* a (expt x 2)) (* b x y) (* c x y) (* d (expt y 2)))
           (g/simplify (g/* d M u))))
    (is (thrown? IllegalArgumentException 'foo (g/* u M)))
    (is (thrown? IllegalArgumentException (g/* M d)))))
