;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.function-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.numbers]
            [math.value :as v]
            [math.operator :as o]
            [math.structure :refer :all]
            [math.simplify]
            [math.function :refer :all]))

(def ^:private near (v/within 1.0e-6))

(deftest function-basic
  (let [f (literal-function 'F)]
    (testing "a"
      (is (= '(F x) (g/simplify (f 'x))))
      (is (= '(F 7) (g/simplify (f (g/+ 3 4))))))
    (testing "kind"
      (is (= :math.function/function (v/kind f))))
    (testing "arity > 1"
      (let [g (literal-function 'g [0 0] 0)]
        (is (= '(g a b) (g/simplify (g 'a 'b))))))
    ))

(deftest function-algebra
  (let [add2 (fn [x] (g/+ x 2))
        explog (g/exp g/log)
        mul3 #(* 3 %)]
    (testing "unary"
      (is (= 4 (add2 2)))
      (is (= -4 ((g/- add2) 2)))
      (is (= 9 ((g/sqrt add2) 79)))
      (is (= 1/9 ((g/invert add2) 7)))
      (is (= 1.0 (explog 1.0)))
      (is (near 99.0 (explog 99.0)))
      (is (near 20.08553692 ((g/exp add2) 1.0)))
      (is (near 4.718281828 ((add2 g/exp) 1.0))))
    (testing "binary"
      (is (= 12 ((g/+ add2 4) 6)))
      (is (= 14 ((g/+ add2 mul3) 3)))
      (is (= 10 ((g/+ mul3 4) 2)))
      (is (= 32 ((g/expt 2 add2) 3)))
      (is (= 25 ((g/expt add2 2) 3)))
      (is (= :math.value/function (v/kind (g/expt add2 2)))))
    (testing "arity 2"
      (let [f (fn [x y] (+ x y))
            g (fn [x y] (* x y))
            h (g/+ f g)
            k (g/+ 4 (g/- f 2))
            m (g/+ g (g/- f 2))]
        (is (= 11 (h 2 3)))
        (is (= 7 (k 2 3)))
        (is (= 9 (m 2 3)))))))

(deftest operators
  (let [f (fn [x] (+ x 5))
        double (fn [f] (fn [x] (* 2 (f x))))
        double-op (o/make-operator double "double")]
    ;(is (= 8 ((double f) 1)))
    ;(is (= 16 ((double (double f)) 1)))
    ;(is (= 8 ((double-op f) 1)))
    ;(is (= 16 ((double-op (double-op f)) 1)))
    (is (= 2 ((double-op identity) 1)))
    (is (= 6 (((g/expt double-op 0) f) 1)))
    (is (= 12 (((g/expt double-op 1) f) 1)))
    (is (= 24 (((g/expt double-op 2) f) 1)))
    (is (= 18 (((g/+ double-op double-op double-op) identity) 3)))
    (is (= 24 (((g/+ double-op 4 double-op) identity) 3)))))

(deftest function-differential
  (testing "structural utilities"
    (is (symbolic-derivative? '(D f)))
    (is (not (symbolic-derivative? '(e f))))
    (is (not (iterated-symbolic-derivative? '(expt D 2))))
    (is (iterated-symbolic-derivative? '((expt D 2) f)))
    (is (= '((expt D 2) f) (symbolic-increase-derivative '(D f))))
    (is (= '((expt D 3) f) (symbolic-increase-derivative '((expt D 2) f))))))
