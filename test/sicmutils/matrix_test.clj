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
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all :exclude [function?]]
            [sicmutils
             [env :refer :all]
             [matrix :as m]
             [structure :as s]
             [value :as v]]))

(deftest matrix-basics
  (let [M (matrix-by-rows (list 1 2 3)
                          (list 4 5 6))]
    (is (= ::m/matrix (v/kind M)))
    (is (= '(matrix-by-rows [1 2 3] [4 5 6]) (v/freeze M)))
    (is (= (matrix-by-rows [1 4] [2 5] [3 6]) (m/transpose M)))
    (is (= (matrix-by-rows [0 0 0] [0 0 0]) (v/zero-like M)))
    (is (thrown? IllegalArgumentException (matrix-by-rows [1 2 3] [4 5])))
    (is (thrown? AssertionError (matrix-by-rows)))
    ))

(deftest structure
  (let [A (up 1 2 'a (down 3 4) (up (down 'c 'd) 'e))]
    (is (= 8 (s/dimension A)))
    (let [vs (up (up 'vx1 'vy1) (up 'vx2 'vy2))
          L1 (fn [[v1 v2]]
               (+ (* 1/2 'm1 (square v1))
                  (* 1/2 'm2 (square v2))))]
      (is (= '(matrix-by-rows [m1 0 0 0]
                              [0 m1 0 0]
                              [0 0 m2 0]
                              [0 0 0 m2])
             (simplify (m/s->m vs (((expt D 2) L1) vs) vs)))))))

(deftest matrix-mul
  (let [M (matrix-by-rows '[a b] '[c d])
        S (matrix-by-rows '[e f] '[g h])]

    (is (= '(matrix-by-rows [(+ (* a e) (* b g)) (+ (* a f) (* b h))]
                            [(+ (* c e) (* d g)) (+ (* c f) (* d h))])
           (simplify (* M S)))))
  (let [M (matrix-by-rows [1 2 3]
                          [2 3 4])
        S (matrix-by-rows [3 4]
                          [4 5]
                          [5 6])]
    (is (= (matrix-by-rows [26 32] [38 47]) (* M S)))))
