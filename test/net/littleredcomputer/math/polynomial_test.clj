;
; Copyright (C) 2015 Colin Smith.
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

(ns net.littleredcomputer.math.polynomial-test
  (:import (com.google.common.base Stopwatch)
           (java.util.concurrent TimeUnit))
  (:require [clojure.test :refer :all]
            [clojure.math.numeric-tower :as nt]
            [net.littleredcomputer.math
             [value :as v]
             [polynomial :refer :all]
             [generic :as g]
             [numbers]
             [expression :refer [variables-in]]
             [simplify]
             [modint :as modular]]))

(set! *warn-on-reflection* true)

(deftest poly-core
  (testing "kind"
    (is (= :net.littleredcomputer.math.polynomial/polynomial (v/kind (make [])))))
  (testing "zero"
    (is (g/zero? (make [])))
    (is (g/zero? (make [0])))
    (is (v/nullity? (make [])))
    (is (g/zero? (make 2 [])))
    (is (v/nullity? (make 2 [])))
    (is (not (g/zero? (make [1])))))
  (testing "unity"
    (is (v/unity? (make [1])))
    (is (v/unity? (make 2 [[[0 0] 1]])))
    (is (v/unity? (make 3 [[[0 0 0] 1]])))
    (is (not (v/unity? (make 3 [[[0 0 0] 1] [[0 0 1] 2]]))))
    (binding [*poly-require-euclidean-coefficients* false]
      (is (not (v/unity? (make [1.1]))))
      (is (v/unity? (make [1.0]))))
    (is (v/unity? (make [(make [1])])))
    (is (not (v/unity? (make [(make [2])])))))
  (testing "make-constant"
    (is (= (make [99]) (make-constant 1 99)))
    (is (= (make 2 [[[0 0] 88]]) (make-constant 2 88)))
    (is (= (make 3 [[[0 0 0] 77]]) (make-constant 3 77))))
  (testing "degree"
    (is (= (degree (make [])) -1))
    (is (= (degree (make [-1 1])) 1))
    (is (= (degree (make [0 1])) 1))
    (is (= (degree (make [-1 0 2])) 2))
    (is (= (degree (make [-1 2 0])) 1))
    (is (= (degree (make [0 0])) -1)))
  (testing "zero-like"
    (is (= (make []) (v/zero-like (make [1 2 3]))))
    (is (= (make 2 []) (v/zero-like (make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (make 3 []) (v/zero-like (make 3 [[[1 2 1] 4] [[0 1 0] 5]])))))
  (testing "one-like"
    (is (= (make [1]) (v/one-like (make [1 2 3]))))
    (is (= (make 2 [[[0 0] 1]]) (v/one-like (make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (make 3 [[[0 0 0] 1]]) (v/one-like (make 3 [[[1 2 1] 4] [[0 1 0] 5]]))))
    ;; we can't deduce the unit element from the zero polynomial over an
    ;; "unknown" ring
    (is (thrown? IllegalArgumentException (v/one-like (make 2 [])))))
  (testing "add constant"
    (is (= (make [3 0 2]) (add (make [0 0 2]) (make [3]))))
    (is (= (make [0 0 2]) (add (make [2 0 2]) (make [-2])))))
  (testing "add/sub"
    (is (g/zero? (add (make [0 0 2]) (make [0 0 -2]))))
    (is (= (make []) (add (make [0 0 2]) (make [0 0 -2]))))
    (is (= (make [3]) (add (make [3 0 2]) (make [0 0 -2]))))
    (is (= (make [-1 1]) (add (make [0 1]) (make [-1]))))
    (is (g/zero? (sub (make [0 0 2]) (make [0 0 2]))))
    (is (= (make [-3]) (sub (make [0 0 2]) (make [3 0 2]))))
    (is (= (make [0 1 2]) (sub (make [3 1 2]) (make [3]))))
    (is (= (make [-2 -2 -1]) (sub (make [1]) (make [3 2 1]))))
    (is (= (make [0 0 1 0 1 -1]) (sub (make [1 0 1 0 1]) (make [1 0 0 0 0 1]))))
    (is (= (make [0 0 -1 0 -1 1]) (sub (make [1 0 0 0 0 1]) (make [1 0 1 0 1]))))
    (is (= (make [-1 -2 -3]) (negate (make [1 2 3])))))
  (testing "with symbols"
    (binding [*poly-require-euclidean-coefficients* false]
      (is (= (make [(g/+ 'a 'c) (g/+ 'b 'd) 'c]) (add (make '[a b c]) (make '[c d]))))))
  (testing "mul"
    (is (= (make []) (mul (make [1 2 3]) (make [0]))))
    (is (= (make []) (mul (make [0]) (make [1 2 3]))))
    (is (= (make []) (mul (make []) (make [1 2 3]))))
    (is (= (make [1 2 3]) (mul (make [1 2 3]) (make [1]))))
    (is (= (make [1 2 3]) (mul (make [1]) (make [1 2 3]))))
    (is (= (make [3 6 9]) (mul (make [1 2 3]) (make [3]))))
    (is (= (make [0 1 2 3]) (mul (make [0 1]) (make [1 2 3]))))
    (is (= (make [0 -1 -2 -3]) (mul (make [0 -1]) (make [1 2 3]))))
    (is (= (make [-1 0 1]) (mul (make [1 1]) (make [-1 1]))))
    (is (= (make [1 3 3 1]) (mul (make [1 1]) (mul (make [1 1]) (make [1 1])))))
    (is (= (make [1 -4 6 -4 1]) (mul (mul (make [-1 1]) (make [-1 1]))
                                     (mul (make [-1 1]) (make [-1 1]))))))
  (testing "div"
    (is (= [(make [1 1]) (make [])]
           (divide (make [-1 0 1]) (make [-1 1]))))
    (is (= [(make [-10 1]) (make [-32 -21])]
           (divide (make [-42 0 -12 1]) (make [1 -2 1]))))
    (is (= [(make [3 1 1]) (make [5])]
           (divide (make [-4 0 -2 1]) (make [-3 1]))))
    (is (= [(make [-5 0 3]) (make [60 -27 -11])]
           (divide (make [-45 18 72 -27 -27 0 9]) (make [21 -9 -4 0 3]))))
    (let [U (make [-5 2 8 -3 -3 0 1 0 1])
          V (make [21 -9 -4 0 5 0 3])
          [pr d] (pseudo-remainder U V)]
      (binding [*poly-require-euclidean-coefficients* false]
        (is (= [(make [-2/9 0 1/3]) (make [-1/3 0 1/9 0 -5/9])] (divide U V))))
      (is (= [(make [-3 0 1 0 -5]) 2] [pr d]))
      (is (= (make []) (sub (mul (make [(nt/expt 3 d)]) U) (add (mul (make [-2 0 3]) V) pr))))
      (is (= (make [1]) (gcd U V)))
      (is (= (make [1]) (gcd V U))))
    ;; examples from http://www.mathworks.com/help/symbolic/mupad_ref/pdivide.html
    (let [p (make [1 1 0 1])
          q (make [1 1 3])]
      (is (= [(make [10 7]) 2] (pseudo-remainder p q))))
    (let [p (make [3 0 4])
          q (make [2 2])]
      (is (= [(make [28]) 2] (pseudo-remainder p q))))
    (is (= [(make 2 []) (make 2 [[[2 1] 1] [[1 2] 1]])]
           (divide (make 2 [[[2 1] 1] [[1 2] 1]]) (make 2 [[[1 2] 1]]))))
    (is (= [(make [1]) (make [])] (divide (make [3]) (make [3]))))
    (is (= [(make [0]) 1] (pseudo-remainder (make [7]) (make [2])))))
  (testing "expt"
    (let [x+1 (make [1 1])]
      (is (= (make [1]) (expt x+1 0)))
      (is (= x+1 (expt x+1 1)))
      (is (= (make [1 2 1]) (expt x+1 2)))
      (is (= (make [1 3 3 1]) (expt x+1 3)))
      (is (= (make [1 4 6 4 1]) (expt x+1 4)))
      (is (= (make [1 5 10 10 5 1]) (expt x+1 5)))))
  (testing "other coefficient rings: GF(2)"
    (binding [*poly-require-euclidean-coefficients* false]
      ;; XXX: the modular integers are certainly Euclidean.
      ;; TODO: make euclidean? a member of the Value protocol.
      (let [mod2 #(modular/make % 2)
           x0 (mod2 0)
           x1 (mod2 1)
           P (make [x1 x0 x1])]
       (is (= (make [x1 x0 x0 x0 x1]) (expt P 2)))
       (is (= (make [x1 x0 x1 x0 x1 x0 x1]) (expt P 3)))
       (is (= (make [x1 x0 x0 x0 x0 x0 x0 x0 x1]) (mul (expt P 3) P)))
       (is (= (make []) (sub P P)))
       (is (= (make []) (add P P)))
       (is (= (make [x0 x0 x1]) (add P (make [1])))))))
  (testing "CRC polynomials"
    ;; https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
    ;; http://www.lammertbies.nl/comm/info/crc-calculation.html
    (binding [*poly-require-euclidean-coefficients* false]
      ;; XXX TODO: see above
      (let [mod2 #(modular/make % 2)
           o (mod2 0)
           i (mod2 1)
           x8 (make [o o o o o o o o i])
           CRC-8-ATM (make [i i i o o o o o i])
           M (make [i i i 0 i o i])
           Mx8 (mul x8 M)
           [q1 r1] (divide Mx8 CRC-8-ATM)
           CRC-16-CCITT (make [i o o o o i o o o o o o i o o o i])
           x16 (mul x8 x8)
           T (make [o o i o i o i])
           Tx16 (mul x16 T)
           [q2 r2] (divide Tx16 CRC-16-CCITT)
           ]
       (is (= (make [o i o o o i o i]) r1))
       (is (= (make [i o o o i i i o o i o i i]) r2)))))
  (testing "modular polynomial reduction"
    (binding [*poly-require-euclidean-coefficients* false]
      (let [A (make [-360 -171 145 25 1])
            B (make [-15 -14 -1 15 14 1])
            Z5 #(modular/make % 5)
            A:Z5 (map-coefficients Z5 A)
            B:Z5 (map-coefficients Z5 B)
            G5 (gcd A:Z5 B:Z5)]
        (is (= (make [(Z5 0) (Z5 -1) (Z5 0) (Z5 0) (Z5 1)]) A:Z5))
        (is (= (make [(Z5 0) (Z5 1) (Z5 -1) (Z5 0) (Z5 -1) (Z5 1)]) B:Z5))
        (is (= (make [(Z5 0) (Z5 -1) (Z5 0) (Z5 0) (Z5 1)]) G5)))))
  (testing "monomial order"
    (let [x3 [3 0 0]
          x2z2 [2 0 2]
          xy2z [1 2 1]
          z2 [0 0 2]
          monomials [x3 x2z2 xy2z z2]
          monomial-sort #(sort-by identity % monomials)]
      (is (= [z2 xy2z x2z2 x3] (monomial-sort lex-order)))
      (is (= [z2 x3 x2z2 xy2z] (monomial-sort graded-reverse-lex-order)))
      (is (= [z2 x3 xy2z x2z2] (monomial-sort graded-lex-order))))))

(deftest poly-gcd
  (let [u (make [6 7 1])  ;; some polynomials of arity 1
        v (make [-6 -5 1])
        w (make [-3 -6 9])
        x (make [0 1])
        xx (mul x x)
        xxx (mul x xx)
        xxx+xx+x (add xxx (add xx x))
        X (make 2 [[[1 0] 1]]) ;; some polynomials of arity 2
        Y (make 2 [[[0 1] 1]])
        XY (mul X Y)
        XXY (make 2 [[[2 1] 1]])
        XYY (make 2 [[[1 2] 1]])
        XXY+XYY (add XXY XYY)
        Q (make 2 [[[1 1] 4] [[3 0] 6] [[1 2] 6] [[3 1] 9]])]
    (testing "constant-term"
      (is (= 6 (constant-term u)))
      (is (= -6 (constant-term v)))
      (is (= 0 (constant-term x)))
      (is (= 0 (constant-term (make 4 [])))))
    (testing "GCD: arity 1 case"
      (let [x+1 (make [1 1])
            x+2 (make [2 1])
            x+3 (make [3 1])
            x+4 (make [4 1])
            U (mul x+1 (mul x+1 (mul x+2 x+4)))
            V (mul x+1 (mul x+2 x+3))]
        (is (= (make [2 3 1]) (gcd U V)))
        (is (= (make [4]) (gcd (make [8]) (make [4]))))
        (is (= (make [1]) (gcd (make [7]) (make [11]))))
        (is (= (make [11]) (gcd (make []) (make [11])))))
      (let [x+4 (make [4 1])
            x+3 (make [3 1])
            x-2 (make [-2 1])
            x+1 (make [1 1])
            U (reduce mul [x+4 x+4 x+3 x+3 x-2 x-2 x-2 x-2 x-2 x+1])
            V (reduce mul [x+4 x+3 x+3 x+3 x-2 x-2 x+1 x+1])
            W (reduce mul [x+4 x+3 x+3 x-2 x-2 x+1])
            Z (make [])]
        (is (= W (gcd U V)))
        (is (= W (gcd V U)))
        (is (= U (gcd U U)))
        (is (= V (gcd V V)))
        (is (= W (gcd W W)))
        (is (= U (gcd U Z)))
        (is (= U (gcd Z U)))
        (is (= V (gcd V Z)))
        (is (= V (gcd Z V)))))
    (testing "divide constant arity 2"
      (is (= [(make 2 []) X] (divide X Y)))
      (is (= [(make 2 []) Y] (divide Y X))))
    (testing "GCD: arity 2 case"
      (let [I (make 2 [[[0 0] 1]])
            X (make 2 [[[1 0] 1]])
            Y (make 2 [[[0 1] 1]])
            X+Y (add X Y)
            X+1 (add X I)
            Y+1 (add Y I)
            X+Y_2 (mul X+Y X+Y)
            X+Y_3 (mul X+Y_2 X+Y)
            U (reduce mul [(expt X+1 3) (expt X+Y 2) (expt Y+1 4)])
            V (reduce mul [(expt X+1 2) (expt X+Y 5) (expt Y+1 3)])
            G (reduce mul [(expt X+1 2) (expt X+Y 2) (expt Y+1 3)])]
        (is (= X+Y_2 (gcd X+Y_2 X+Y_3)))
        (is (= X+Y_3 (gcd X+Y_3 X+Y_3)))
        (is (= G (gcd U V)))))

    (testing "GCD: arity 3 case"
      (let [I (make 3 [[[0 0 0] 1]])
            X (make 3 [[[1 0 0] 1]])
            Y (make 3 [[[0 1 0] 1]])
            Z (make 3 [[[0 0 1] 1]])
            X+Y (add X Y)
            X+Z (add X Z)
            Y+Z (add Y Z)
            X+Y+Z (add X+Y Z)
            X+1 (add X I)
            Y+1 (add Y I)
            Z+1 (add Z I)
            U (reduce mul [(expt X+1 3) (expt X+Y 2) (expt Y+Z 5) (expt X+Y+Z 4) (expt Y+1 4) (expt Z+1 3)])
            V (reduce mul [(expt X+1 2)  (expt X+Y 5) (expt Y+Z 3) (expt X+Y+Z 5) (expt Y+1 2) (expt Z+1 1) X+1])
            G (reduce mul [(expt X+1 3) (expt X+Y 2) (expt Y+Z 3) (expt X+Y+Z 4) (expt Y+1 2) Z+1])]
        (is (= [(reduce mul [(expt Y+Z 2) (expt Y+1 2) (expt Z+1 2)]) (make 3 [])] (divide U G)))
        (is (= [(reduce mul [(expt X+Y 3) X+Y+Z]) (make 3 [])] (divide V G)))
        (is (= X+Z (gcd (mul X+Y X+Z) (mul Y+Z X+Z))))
        (is (= (mul X+Z X+Y+Z) (gcd (reduce mul [X+Y X+Z X+Y+Z]) (reduce mul [X+Z X+Y+Z Y+Z]))))
        (is (= (mul X+Z (mul X+Z X+Y)) (gcd (reduce mul [X+Z X+Z X+Y X+Y+Z Y+1]) (reduce mul [X+Z X+Z X+Y X+1 Z+1 X+Z]))))
        (is (= G (gcd U V)))))))

(deftest simple-gcd-3
  (testing "GCD: arity 3 case"
    (let [I (make 3 [[[0 0 0] 1]])
          II (add I I)
          X (make 3 [[[1 0 0] 1]])
          Y (make 3 [[[0 1 0] 1]])
          Z (make 3 [[[0 0 1] 1]])
          X+Y (add X Y)
          X+Z (add X Z)
          Y+Z (add Y Z)]
      (is (= X+Z (gcd (mul X+Y X+Z) (mul Y+Z X+Z))))
      (is (= II (gcd II (add Z Z))))
      (is (= II (gcd (add Z Z) II)))
      (is (= II (gcd II (add (add X X) (add Z Z))))))))

(defn ^:private ->poly [x] (expression-> x (fn [p v] p)))
(defn ^:private gcd-test [dx fx gx]
  (let [d (->poly dx)
        f (->poly fx)
        g (->poly gx)]
    (is (= d (gcd (mul d f) (mul d g))))))

(deftest poly-apply
  (testing "arity 1"
    (let [p (->poly '(+ 2 (* x 3)))]
      (is (= 14 (p 4)))
      (is (thrown? IllegalArgumentException (p 3 2))))
    (is (= 256 ((->poly '(expt x 8)) 2)))
    (is (= 272 ((->poly '(+ (expt x 4) (expt x 8))) 2))))
  (testing "arity 2"
    (let [p (->poly '(expt (+ x y) 2))]
      (is (= p (p)))
      (is (= 25 (p 2 3)))
      (let [q (p 3)]
        (is (= 49 (q 4))))))
  (testing "arity 3"
    (let [p (->poly '(+ (expt x 3) (expt y 2) z 1))]
      (is (= 19 (p 2 3 1)))))
  (testing "arity 4"
    (let [p (->poly '(expt (- w x y z) 2))]
      (is (= 36 (p 10 1 2 1)))))
  (testing "arity 5"
    (let [p (->poly '(expt (- v w x y z) 2))]
      (is (= 16 (p 10 1 2 1 2)))))
  (testing "arity 10 (via apply)"
    (let [p (->poly '(expt (- x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) 3))]
      (is (= 216 (apply p [10 1 2 1 2 -3 1 -2 -1 3])))))
  (testing "constant polys"
    (let [p1 (make [3])
          p2 (make 2 [[[0 0] 5]])
          p3 (make 3 [[[1 0 0] 1]])
          p4 (make 3 [[[0 1 0] 1]])
          p5 (make 3 [[[0 0 1] 1]])]
      (is (= 3 (p1 99)))
      (is (= 5 (p2 99 98)))
      (is (= 7 (p3 7 8 9)))
      (is (= 8 (p4 7 8 9)))
      (is (= 9 (p5 7 8 9))))))

(deftest gjs
  (testing "GJS cases (see sparse-gcd.scm:666)"
    (let [d1 '(+ (expt x1 2) x1 3)
          f1 '(+ (* 2 (expt x1 2)) (* 2 x1) 1)
          g1 '(+ (expt x1 2) (* 2 x1) 2)

          d2 '(+ (* 2 (expt x1 2) (expt x2 2))
                 (* x1 x2)
                 (* 2 x1))
          f2 '(+ (expt x2 2)
                 (* 2 (expt x1 2) x2)
                 (expt x1 2)
                 1)
          g2 '(+ (* (expt x1 2) (expt x2 2))
                 (* (expt x1 2) x2)
                 (* x1 x2)
                 (expt x1 2)
                 x1)

          d3 '(+ (* x2 x2 x3 x3)
                 (* x2 x2 x3)
                 (* 2 x1 x1 x2 x3)
                 (* x1 x3))
          f3 '(+ (* x3 x3)
                 (* x2 x2 x3)
                 (* x1 x1 x2 x3)
                 (* x1 x3)
                 (* x1 x1 x2 x2))
          g3 '(+ (* x2 x3)
                 (* 2 x1 x3)
                 x3
                 x1)

          d4 '(+ (* x1 x1 x4 x4)
                 (* x2 x2 x3 x4)
                 (* x1 x1 x2 x4)
                 (* x2 x4)
                 (* x1 x1 x2 x3))
          f4 '(+ (* x1 x2 x3 x3 x4 x4)
                 (* x1 x3 x3 x4 x4)
                 (* x1 x4 x4)
                 (* x4 x4)
                 (* x1 x3 x4))
          g4 '(+ (* x1 x3 x3 x4 x4)
                 (* x3 x3 x4 x4)
                 (* x4 x4)
                 (* x1 x2 x2 x3 x4)
                 (* x1 x2 x2))

          d5 '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 x5 x5)
                 (* x1 x2 x2 x5 x5)
                 (* x1 x1 x1 x3 x4 x4 x5)
                 (* x1 x1 x1 x2 x3 x3 x4 x5)
                 (* x1 x1 x2 x3 x3 x4 x4))
          f5 '(+ (* x1 x2 x2 x5 x5)
                 (* x1 x2 x3 x3 x4 x5)
                 (* x1 x2 x3 x3 x4 x4)
                 (* x1 x2 x2 x4 x4)
                 1)
          g5 '(+ (* x1 x3 x3 x4 x5 x5)
                 (* x2 x5 x5)
                 (* x1 x2 x4 x5)
                 (* x2 x5)
                 (* x1 x2 x3 x4 x4))]

      (is (= (->poly d2) (-> d2 ->poly lower-arity raise-arity)))
      (is (= (->poly d3) (-> d3 ->poly lower-arity raise-arity)))
      (is (= (->poly d4) (-> d4 ->poly lower-arity raise-arity)))

      (is (= (make [0
                    (make [2 1])
                    (make [0 0 2])])
             (-> d2 ->poly lower-arity)))

      (is (= (make [0
                    (make [2 1 2 1])
                    (make [0 0 2 0 2])
                    (make [2 5 2])
                    (make [0 0 2 4])])
             (lower-arity (mul (->poly d2) (->poly f2)))))

      (is (= (make [0
                    0
                    (make [4 4 5 4 1])
                    (make [0 0 8 4 8 4])
                    (make [4 12 9 2 4 0 4])
                    (make [0 0 8 20 8])
                    (make [0 0 0 0 4 8])])
             (mul (lower-arity (->poly d2))
                  (lower-arity (mul (->poly d2) (->poly f2))))))

      (gcd-test d1 f1 g1)
      (gcd-test d2 f2 g2)
      (gcd-test d3 f3 g3)
      (gcd-test d4 f4 g4)
      (gcd-test d5 f5 g5)
      (gcd-stats))))

(deftest more-gjs
  (testing "GJS cases (see sparse-gcd.scm)"
    (let [d6 '(+ (* x1 x2 x4 x4 x5 x5 x6 x6)
                 (* x1 x2 x2 x3 x3 x4 x5 x5 x6 x6)
                 (* x1 x1 x3 x6 x6)
                 (* x1 x1 x2 x3 x3 x4 x5 x5 x6)
                 (* x1 x1 x3 x5 x6))
          f6 '(+ (* x1 x1 x2 x4 x5 x5 x6 x6)
                 (* x1 x3 x5 x5 x6 x6)
                 (* x1 x2 x2 x6 x6)
                 (* x1 x1 x2 x2 x3 x3 x5 x6)
                 (* x1 x3 x3 x4 x5))
          g6 '(+ (* x2 x2 x3 x3 x4 x5 x5 x6)
                 (* x1 x4 x4 x5 x6)
                 (* x2 x2 x3 x3 x4 x5 x6)
                 (* x1 x2 x2 x3 x4 x4 x6)
                 (* x1 x1 x3 x5 x5))
          d7 '(+ (* x1 x2 x2 x4 x4 x6 x6 x7 x7)
                 (* x1 x1 x3 x4 x6 x6 x7 x7)
                 (* x3 x3 x4 x4 x7 x7)
                 (* x1 x1 x2 x4 x4 x6)
                 (* x3 x4 x5 x5))
          f7 '(+ (* x1 x1 x2 x4 x4 x5 x6 x6 x7 x7)
                 (* x1 x2 x3 x6 x7)
                 (* x3 x4 x4 x5 x5 x7)
                 (* x1 x1 x2 x3 x4 x4 x5 x6))
          g7 '(+ (* x1 x3 x5 x6 x6 x7 x7)
                 (* x2 x2 x3 x3 x4 x4 x5 x6 x7 x7)
                 (* x4 x6 x7 x7)
                 (* x1 x1 x2 x3 x5 x6 x7)
                 (* x1 x1 x3 x3 x4 x5 x5))]
      ;; aren't quite good enough for these yet.
      #_(gcd-test d6 f6 g6)
      #_(gcd-test d7 f7 g7))))

(deftest ^:long big-gcd
  (let [u (make 10 [[[0 0 1 0 0 0 1 1 0 1] 1]
                    [[0 1 1 0 0 0 1 1 1 0] 2]
                    [[0 0 1 2 0 1 0 1 0 1] 1]
                    [[0 0 1 2 1 0 0 1 0 1] -1]
                    [[1 1 0 1 0 1 0 2 0 0] 1]
                    [[1 1 0 1 1 0 0 2 0 0] -1]
                    [[2 0 1 0 0 1 0 1 0 1] -1]
                    [[2 0 1 0 1 0 0 1 0 1] 1]
                    [[0 1 1 2 0 1 0 1 1 0] -2]
                    [[1 0 2 1 0 1 0 0 1 1] 2]
                    [[1 0 2 1 1 0 0 0 1 1] -2]])
        v (make 10 [[[0 0 1 4 1 1 0 0 0 0] 1]
                    [[2 0 1 2 1 1 0 0 0 0] 2]
                    [[4 0 1 0 1 1 0 0 0 0] 1]])]
    (let [t (fn []
              (let [sw (Stopwatch/createStarted)
                    g (gcd u v)]
                (println sw)
                (gcd-stats)
                g))]
      (binding [*poly-gcd-time-limit* [3 TimeUnit/SECONDS]]
        (is (= (make 10 [[[0 0 0 0 0 0 0 0 0 0] 1]]) (t)))
        ;; for profiling
        (binding [*poly-gcd-cache-enable* false]
          (dotimes [_ 1] (t)))))))

#_(deftest ^:long troublesome-gcd
  (let [u (make 10 [[[0 1 1 2 1 0 1 1 0 1] -1]
                    [[2 1 1 0 0 1 1 1 0 1] -1]
                    [[0 2 1 2 1 0 1 1 1 0] -2]
                    [[1 1 2 1 0 1 1 0 1 1] 1]
                    [[1 1 2 1 1 0 1 0 1 1] -1]
                    [[2 2 1 0 0 1 1 1 1 0] -2]
                    [[0 1 1 4 1 1 0 1 0 1] -1]
                    [[0 1 1 4 2 0 0 1 0 1] 1]
                    [[1 2 0 3 1 1 0 2 0 0] -1]
                    [[1 2 0 3 2 0 0 2 0 0] 1]
                    [[1 2 2 1 0 1 1 0 2 0] 1]
                    [[1 2 2 1 1 0 1 0 2 0] -1]
                    [[2 1 1 2 0 2 0 1 0 1] 1]
                    [[2 1 1 2 1 1 0 1 0 1] -2]
                    [[2 1 1 2 2 0 0 1 0 1] 1]
                    [[3 2 0 1 0 2 0 2 0 0] -1]
                    [[3 2 0 1 1 1 0 2 0 0] 1]
                    [[4 1 1 0 0 2 0 1 0 1] 1]
                    [[4 1 1 0 1 1 0 1 0 1] -1]
                    [[0 2 1 4 1 1 0 1 1 0] 2]
                    [[1 1 2 3 0 2 0 0 1 1] -1]
                    [[1 1 2 3 2 0 0 0 1 1] 1]
                    [[2 2 1 2 0 2 0 1 1 0] 2]
                    [[2 2 1 2 2 0 0 1 1 0] 2]
                    [[3 1 2 1 0 2 0 0 1 1] -1]
                    [[3 1 2 1 2 0 0 0 1 1] 1]
                    [[4 2 1 0 1 1 0 1 1 0] 2]
                    [[1 2 2 3 0 2 0 0 2 0] -1]
                    [[1 2 2 3 1 1 0 0 2 0] 1]
                    [[3 2 2 1 1 1 0 0 2 0] -1]
                    [[3 2 2 1 2 0 0 0 2 0] 1]])
        v (make 10 [[[0 0 1 4 1 1 0 0 0 0] 1]
                    [[2 0 1 2 1 1 0 0 0 0] 2]
                    [[4 0 1 0 1 1 0 0 0 0] 1]])
        g (binding [*poly-gcd-time-limit* [5 TimeUnit/SECONDS]
                    *poly-gcd-debug* true]
            (is (= 'foo (gcd u v))))]))

(deftest kuniaki-tsuji-examples
  ;; (only have 1 of these, will add more)
  ;; http://www.sciencedirect.com/science/article/pii/S0747717108001016
  (testing "ex1"
    (let [d (->poly '(+ (* x x) (* 2 (expt y 23) (expt z 24))))
          p (mul d (->poly '(+ (* x x) (* (expt y 23) (expt z 22)))))
          q (mul d (->poly '(+ (* y z (expt x 3))
                               (* 2 (expt z 35) (expt y 41) (expt x 2))
                               (* (expt z 3) (expt y 5) x)
                               525)))]
      (is (= d (gcd p q))))))

(deftest poly-as-simplifier
  (testing "arity"
    (is (= 1 (:arity (make [0 1])))))
  (testing "make-vars"
    (is (= (list (make [0 1])) (new-variables 1)))
    (is (= [(make 3 [[[1 0 0] 1]])
            (make 3 [[[0 1 0] 1]])
            (make 3 [[[0 0 1] 1]])] (new-variables 3))))
  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{* + x} (variables-in exp1)))
      (is (= [(make [-3 -2 1]) '(x)] (expression-> exp1 receive)))
      (is (= [(make [-3 -2 1]) '(x)] (expression-> exp1 receive)))
      (is (= [(make [1 5 10 10 5 1]) '(y)] (expression-> exp2 receive)))
      (is (= [(make [0 -11 5 -30 10 -7 1]) '(y)] (expression-> exp3 receive)))))
  (testing "monomial order"
    (let [poly-simp #(expression-> (:expression %) ->expression)]
      (is (= '(+ (expt x 2) x 1) (poly-simp (g/+ 'x (g/expt 'x 2) 1))))
      (is (= '(+ (expt x 4) (* 4 (expt x 3)) (* 6 (expt x 2)) (* 4 x) 1) (poly-simp (g/expt (g/+ 1 'x) 4))))
      (is (= '(+
               (expt x 4)
               (* 4 (expt x 3) y)
               (* 6 (expt x 2) (expt y 2))
               (* 4 x (expt y 3))
               (expt y 4))
             (poly-simp (g/expt (g/+ 'x 'y) 4))))
      (is (= '(+
               (expt x 4)
               (* 4 (expt x 3) y)
               (* 6 (expt x 2) (expt y 2))
               (* 4 x (expt y 3))
               (expt y 4))
             (poly-simp (g/expt (g/+ 'y 'x) 4))))))
  (testing "expr-simplify"
    (let [poly-simp #(expression-> % ->expression)
          exp1 (:expression (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
          exp2 (:expression (g/+ (g/* 'y 'y) (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x) (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2))) (poly-simp exp1)))
      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2))) (poly-simp exp2)))
      (is (= 'y (poly-simp exp3)))
      (is (= '(+ g1 g2) (poly-simp (:expression (g/+ 'g1 'g2)))))
      (is (= '(* 2 g1) (poly-simp (:expression (g/+ 'g1 'g1)))))
      (is (= 3 (poly-simp '(+ 2 1))))
      (is (= '(+ b (* -1 f)) (poly-simp '(- (+ a b c) (+ a c f)))))
      (is (= '(+ (* -1 b) f) (poly-simp '(- (+ a c f) (+ c b a))))))))
