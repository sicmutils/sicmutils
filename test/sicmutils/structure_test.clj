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

(ns sicmutils.structure-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.structure :as s]
            [sicmutils.mechanics.lagrange :refer [velocity-tuple]]
            [sicmutils.value :as v]))

(deftest structures
  (testing "type"
    (is (= ::s/up (v/kind (up 1 2))))
    (is (= ::s/down (v/kind (down (up 1 2) (up 2 3))))))
  (testing "s+t"
    (is (= (+ (up 1 2) (up 2 3)) (up 3 5)))
    (is (= (+ (down 3 4) (down 1 2)) (down 4 6)))
    (is (= (down (+ 'u 4) (+ 2 'v)) (+ (down 'u 2) (down 4 'v)))))
  (testing "s-t"
      (is (= (- (up 1 2) (up 2 3)) (up -1 -1)))
      (is (= (- (down 8 5) (down 4 -1)) (down 4 6)))
      (is (= (- (down 8 5)) (down -8 -5)))
      (is (= (- (up 10 10) (up 2 3) (up 3 4)) (up 5 3))))
  (testing "s +/- t mixed"
    (is (= (+ (up (down 1 2) (down 3 4))
                (up (down 2 3) (down -7 2)))
           (up (down 3 5) (down -4 6))))
    (is (= (- (up (down 1 2) (down 3 4))
                (up (down 2 3) (down -7 2)))
           (up (down -1 -1) (down 10 2))))
    (is (= (+ (down (up 1 2) (up 3 4))
                (down (up 2 3) (up -7 2)))
           (down (up 3 5) (up -4 6))))
    (is (= (- (down (up 1 2) (up 3 4))
                (down (up 2 3) (up -7 2)))
           (down (up -1 -1) (up 10 2)))))
  (testing "a*s"
    (is (= (up 2 4 6) (* 2 [1 2 3])))
    (is (= (down 3 6 9) (* 3 (down 1 2 3))))
    (is (= (down 12 24 36) (* 3 4 (down 1 2 3)))))
  (testing "s/a"
    (is (= (up 1 2 -3) (/ (up 2 4 -6) 2))))
  (testing "neg"
    (is (= (up -1 2 -3) (- (up 1 -2 3))))
    (is (= (up -1 2 -3) (negate (up 1 -2 3)))))
  (testing "a*s with literals"
    (is (= (up 2 (* 2 't) 6) (* 2 (up 1 't 3))))
    (is (= (down (* 3 'x_0) (* 3 'x_1)) (* 3 (down 'x_0 'x_1)))))
  (testing "s*t outer simple"
    (is (= (up (up 3 6) (up 4 8))
           (* (up 1 2) (up 3 4))))
    (is (= (down (down 3 6) (down 4 8))
           (* (down 1 2) (down 3 4))))
    (is (= (down (up 3 6) (up 4 8) (up 5 10))
           (* (up 1 2) (down 3 4 5)))))
  (testing "s*t inner simple"
    (is (= 11 (* (up 1 2) (down 3 4))))
    (is (= 22 (* (down 2 3) (up 5 4)))))
  (testing "s*t inner with vars"
    (is (= (+ 'y (* 'x 4)) (* (up 1 'x) (down 'y 4)))))
  (testing "examples from refman"
    (is (= 652 (* (up (up 2 3) (down 5 7 11))
                    (down (down 13 17) (up 19 23 29)))))
    (is (= (up (up 10 15) (up 14 21) (up 22 33)) (* (up 2 3) (up 5 7 11))))
    (is (= (up (up 10 14 22) (up 15 21 33)) (* (up 5 7 11) (up 2 3)))))
  (testing "up?"
    (is (up? [1 2]))
    (is (up? (up 3 4)))
    (is (not (up? (down 3 4)))))
  (testing "zero?"
    (is (zero? (up)))
    (is (zero? (down)))
    (is (zero? (down 0)))
    (is (zero? (up 0 0)))
    (is (zero? (up 0)))
    (is (zero? (down 0 0)))
    (is (zero? (up 0 (down (up 0 0) (up 0 0))))))
  (testing "zero-like"
    (is (= (up 0 0 0) (v/zero-like (up 1 2 3))))
    (is (= (up) (v/zero-like (up))))
    (is (= (down 0 0 0) (v/zero-like (down 1 2 3))))
    (is (= (down) (v/zero-like (down))))
    (is (= (up 0 (down (up 0 0) (up 0 0))) (v/zero-like (up 1 (down (up 2 3) (up 4 5)))))))
  (testing "exact?"
    (is (v/exact? (up 0 1 3/2)))
    (is (not (v/exact? (up 0 0 0.00001)))))
  (testing "to vector"
    (is (= [1 2 3] (structure->vector (up 1 2 3))))
    (is (= [1 2 3] (structure->vector (down 1 2 3))))
    (is (= (type []) (type (structure->vector (up 1 2 3)))))
    (is (= (type []) (type (structure->vector (down 1 2 3)))))
    (is (= [(up 1 2) (up 3 4)] (structure->vector (down (up 1 2) (up 3 4))))))
  (testing "from vector"
    (is (= (up 1 2 3) (vector->up [1 2 3])))
    (is (= (down 4 5 6) (vector->down [4 5 6])))
    (is (thrown? AssertionError (vector->up '(1 2 3)))))
  (testing "function - rotate about x axis"
    (is (= (up 0 0 1) ((Rx 'pi-over-2) (up 0 1 0))))
    (is (= (up 'x (sicmutils.generic/- 'z) 'y) ((Rx 'pi-over-2) (up 'x 'y 'z)))))
  (testing "square/cube"
    (is (= 14 (square (up 1 2 3))))
    (is (= (up (up (up 1 2 3) (up 2 4 6) (up 3 6 9))
               (up (up 2 4 6) (up 4 8 12) (up 6 12 18))
               (up (up 3 6 9) (up 6 12 18) (up 9 18 27))) (cube (up 1 2 3)))))
  (testing "matrix-like"
    (let [M (down (up 'a 'c) (up 'b 'd))
          S (up (down 'a 'b) (down 'c 'd))
          x (up 'x 'y)
          xt (down 'x 'y)]
      (is (= (up (+ (* 'x 'a) (* 'y 'b))
                 (+ (* 'x 'c) (* 'y 'd))) (* M x)))
      (is (= (up (+ (* 'x 'a) (* 'y 'b))
                 (+ (* 'x 'c) (* 'y 'd))) (* x S)))
      (is (= (down (+ (* 'x 'a) (* 'y 'c))
                 (+ (* 'x 'b) (* 'y 'd))) (* xt M)))
      (is (= (+ (* (+ (* 'x 'a) (* 'y 'c)) 'x)
                (* (+ (* 'x 'b) (* 'y 'd)) 'y))
             (* xt M x)))
      (is (= (+ (* (+ (* 'x 'a) (* 'y 'c)) 'x)
                (* (+ (* 'x 'b) (* 'y 'd)) 'y))
             (* (* xt M) x)))
      (is (= (+ (* 'x (+ (* 'x 'a) (* 'y 'b)))
                (* 'y (+ (* 'x 'c) (* 'y 'd))))
             (* xt (* M x)))))
    (let [M (up (down 'a 'b) (down 'c 'd))
          x (down 'x 'y)]
      (is (= (down (+ (* 'x 'a) (* 'y 'c))
                   (+ (* 'x 'b) (* 'y 'd))) (* M x)))
      (is (= (down (+ (* 'x 'a) (* 'y 'c))
                   (+ (* 'x 'b) (* 'y 'd))) (* x M))))
    (let [M (up (down 'a 'c) (down 'b 'd))]
      (is (= (up (down (+ (* 'a 'a) (* 'c 'b))
                       (+ (* 'a 'c) (* 'c 'd)))
                 (down (+ (* 'b 'a) (* 'd 'b))
                       (+ (* 'b 'c) (* 'd 'd)))) (* M M)))))
  (testing "fibonacci-matrix"
    (let [n 20
          fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))
          fib (fn [i] (nth fibs i))
          M (down (up 1 1) (up 1 0))]
      (is (= (fib n) (-> (expt M n) first second)))))
  (testing "expt"
    (is (= (up
            (up
             (up (up 1 2) (up 2 4))
             (up (up 2 4) (up 4 8)))
            (up
             (up (up 2 4) (up 4 8))
             (up (up 4 8) (up 8 16))))
           (expt (up 1 2) 4)))
    (is (= (* (up 1 2) (up 1 2) (up 1 2) (up 1 2))
           (expt (up 1 2) 4)))))

(deftest structural-operations
  (testing "mapr"
    (let [S0 (up 2)
          S1 (up 2 3)
          S2 (down (up 1 2) (up 3 4))
          S3 (up (down 1 2) (down 3 4))]
      (is (= (up 4) (mapr square S0)))
      (is (= (up 4 9) (mapr square S1)))
      (is (= (down (up 1 4) (up 9 16)) (mapr square S2)))
      (is (= (up (down 1 4) (down 9 16)) (mapr square S3)))
      (is (= (up 4 9) (mapr square [2 3])))))
  (testing "mapr - two arg fn"
    (let [S (down 'a 'b (up 'c 'd) (down 'e (down 'f 'g)) 'h)]
      (is (= (down '(a 0) '(b 1) (up '(c 2 0) '(d 2 1)) (down '(e 3 0) (down '(f 3 1 0) '(g 3 1 1))) '(h 4))
             (mapr cons S (s/structure->access-chains S))))))
  (testing "mapr - etc"
    (is (= (up 1 4 9) (mapr square [1 2 3])))
    (is (= (up 11 22) (mapr + (up 1 2) (up 10 20))))
    (is (= (up 11 22) (mapr + [1 2] (up 10 20))))
    (is (= (up 11 22) (mapr + (up 10 20) [1 2]))))
  (testing "a structure has a nth element"
    (is (= 5 (nth (up 4 5 6) 1)))
    (is (thrown? IndexOutOfBoundsException (nth (up 4 5 6) 4))))
  (testing "can be counted"
    (is (= 3 (count (up 4 5 6))))
    (is (= 2 (count (down (up 1 2) (up 3 4)))))
    (is (= [2 3] (map count (down (up 1 2) (up 3 4 5))))))
  (testing "support take"
    (is (= (up 1 2) (take 2 (up 1 2 3))))
    (is (= (down (up 1 2)
                 (up 3 4))
           (take 2 (down (up 1 2)
                         (up 3 4)
                         (up 5 6))))))
  (testing "support drop"
    (is (= (up 3) (drop 2 (up 1 2 3))))
    (is (= (down (up 3 4)
                 (up 5 6))
           (drop 1 (down (up 1 2)
                         (up 3 4)
                         (up 5 6))))))
  (testing "can be mapped"
    (is (= (up 1 4 9) (map square (up 1 2 3)))))
  (testing "a structure can produce a seq"
    (is (= [1 2 3] (seq (up 1 2 3))))
    (is (= [4 5 6] (seq (down 4 5 6))))
    (is (= [(up 1 2) (up 3 4)] (seq (down (up 1 2) (up 3 4)))))
    (is (= [1 2 3 4] (flatten (down (up 1 2) (up 3 4))))))
  (testing "unflatten"
    (is (= (up (down 0 1) (down 2 3))
           (s/unflatten (range) (up (down 'x 'y) (down 'z 't)))))
    (is (= (down 3 (up 4 5) (down (up (down 6 7) (up 8 9) 10)) 11)
           (s/unflatten (range 3 12)
                      (down 'a (up 'b 'c) (down (up (down 'd 'e) (up 'f 'g) 'h)) 'i))))
    (is (= 9 (s/unflatten [9] 3)))
    (is (= (up 2) (s/unflatten [2] (up 0.0)))))
  (testing "get-in"
    (is (= 5 (get-in (up 4 5 6) [1])))
    (is (= 4 (get-in (up 4 5 6) [0])))
    (is (= 4 (get-in (down (up 1 2) (up 3 4)) [1 1])))
    (is (= 2 (get-in (down (up 1 2) (up 3 4)) [0 1]))))
  (testing "assoc-in"
    (is (= (up 4 55 6) (s/structure-assoc-in (up 4 5 6) [1] 55)))
    (is (= (down (up 1 22) (up 3 4)) (s/structure-assoc-in (down (up 1 2) (up 3 4)) [0 1] 22))))
  (testing "access-chains"
    (is (= (up [0] [1] [2]) (s/structure->access-chains (up 1 2 3))))
    (is (= (up [0] (up [1 0] [1 1]) (down [2 0] [2 1]))
           (s/structure->access-chains (up 't (up 'x 'y) (down 'p_x 'p_y)))))
    (is (= (up (down (up [0 0 0] [0 0 1]) (up [0 1 0] [0 1 1]))
               (down (up [1 0 0] [1 0 1]) (up [1 1 0] [1 1 1])))
           (s/structure->access-chains (up (down (up 1 2) (up 2 3)) (down (up 3 4) (up 4 5)))))))
  ;; this is wrong and needs to be fixed.
  (testing "compatible-shape"
    (let [o (s/compatible-shape (up 1 2))]
      (is (= ::s/down (v/kind o)))
      (is (every? symbol? o)))
    (let [o (s/compatible-shape (down 3 (up 1 2) (up 3 4)))]
      (is (= ::s/up (v/kind o)))
      (is (symbol? (ref o 0)))
      (is (= ::s/down (v/kind (ref o 1))))
      (is (= ::s/down (v/kind (ref o 2)))))))

(deftest other-operations
  (let [A (up 1 2 'a (down 3 4) (up (down 'c 'd) 'e))]
    (is (= 8 (s/dimension A)))
    (let [vs (velocity-tuple
              (velocity-tuple 'vx1 'vy1)
              (velocity-tuple 'vx2 'vy2))
          L1 (fn [[v1 v2]]
               (+ (* 1/2 'm1 (square v1))
                  (* 1/2 'm2 (square v2))))]
      (is (= '(down
               (down (down (down m1 0) (down 0 0)) (down (down 0 m1) (down 0 0)))
               (down (down (down 0 0) (down m2 0)) (down (down 0 0) (down 0 m2))))
             (simplify (((expt D 2) L1) vs)))))))

(deftest some-tensors
  (let [ε_ijk (down (down (down  0  0  0)
                          (down  0  0  1)
                          (down  0 -1  0))
                    (down (down  0  0 -1)
                          (down  0  0  0)
                          (down  1  0  0))
                    (down (down  0  1  0)
                          (down -1  0  0)
                          (down  0  0  0)))
        δ-il (up (up 1 0 0)
                 (up 0 1 0)
                 (up 0 0 1))]

    (is (= (down 0 0 0) (* δ-il ε_ijk)))))

(deftest matrices
  (let [A (up (up 1 2) (up 3 4))
        B (down (up 1 2 3) (up 3 4 5))
        C (down (up 1 2 3) (up 0 4 5) (up 1 0 6))
        D (up (down 3))
        E (up 1)
        F (down (up 1 2) (up 3 4))
        G (down (up 4 0 0 0) (up 0 0 2 0) (up 0 1 2 0) (up 1 0 0 1))]
    (testing "inverse"
      (is (= (down (down -2 1) (down 3/2 -1/2)) (/ A)))
      (is (= 5/2 (* A (/ A))))
      (is (= 5/2 (* (/ A) A)))
      (is (= (* 1/22 (down (up 24 -12 -2) (up 5 3 -5) (up -4 2 4)) (/ C))))
      (is (= (up (down 1/3)) (/ D)))
      (is (= (up (down 1)) (* D (/ D))))
      (is (= (down (up 1 0) (up 0 1)) (* F (/ F))))
      (is (= (down (up 1 0) (up 0 1)) (/ F F)))
      (is (= (down (up 1 0) (up 0 1)) (* (/ F) F)))
      (is (= (down (up 1/4 0 0 0) (up 0 -1 1 0) (up 0 1/2 0 0) (up -1/4 0 0 1)) (/ G)))
      (is (= (down (up 1/4 0 0 0) (up 0 -1 1 0) (up 0 1/2 0 0) (up -1/4 0 0 1)) (/ G)))
      (is (= (down (up 1 0 0 0) (up 0 1 0 0) (up 0 0 1 0) (up 0 0 0 1)) (/ G G)))
      (is (= (down (up 1/8)) (/ (down (up 8))))))
    (testing "invert-hilbert-matrix"
      (let [N 3
            H (apply up (for [i (range 1 (inc N))]
                          (apply up (for [j (range 1 (inc N))] (/ 1 (+ i j -1))))))]
        (is (= (down (down 9 -36 30) (down -36 192 -180) (down 30 -180 180)) (/ H)))))
    (testing "transpose"
      (is (= (down (up 1 2) (up 3 4)) (transpose A)))
      (is (= (up (up 1 2 3) (up 3 4 5)) (transpose B)))
      (is (= (up (up 1 2 3) (up 0 4 5) (up 1 0 6)) (transpose C)))
      (is (= (down (down 3)) (transpose D)))
      (is (= (down 1) (transpose E)))
      (is (= (up (up 1 2) (up 3 4)) (transpose F))))))

(defn ^:private near [w z]
  (< (abs (- w z)) 1e-12))

(deftest struct-magnitude
  (testing "magnitude of structures as per GJS - 'plain' vectors"
    (is (= (magnitude [3 4]) 5))
    (is (near (magnitude [3 4 5]) (sqrt 50))))
  (testing "magnitude of structures as per GJS - structures"
    (is (= (magnitude (up 3 4)) 5))
    (is (= (magnitude (down 3 4)) 5))
    (is (near (magnitude (up 3 4 5)) (sqrt 50)))
    (is (near (magnitude (down 3 4 5)) (sqrt 50)))))
