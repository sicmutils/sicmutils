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

(ns net.littleredcomputer.math.function-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math
             [generic :as g]
             [numbers]
             [value :as v]
             [operator :as o]
             [structure :refer :all]
             [simplify]
             [function :refer :all]]))

(def ^:private near (v/within 1.0e-6))

(deftest function-basic
  (let [f (literal-function 'F)]
    (testing "a"
      (is (= '(F x) (g/simplify (f 'x))))
      (is (= '(F 7) (g/simplify (f (g/+ 3 4))))))
    (testing "kind"
      (is (= :net.littleredcomputer.math.function/function (v/kind f))))
    (testing "arity > 1"
      (let [g (literal-function 'g [0 0] 0)]
        (is (= '(g a b) (g/simplify (g 'a 'b))))))
    (testing "structured range"
      (let [h (literal-function 'h [0] (up 0 0 0))
            k (literal-function 'k [0] (up 0 (up 0 0) (down 0 0)))]
        (is (= '(up (h↑0 t) (h↑1 t) (h↑2 t)) (g/simplify (h 't))))
        (is (= '(up (k↑0 t)
                    (up (k↑1↑0 t) (k↑1↑1 t))
                    (down (k↑2_0 t) (k↑2_1 t)))
               (g/simplify (k 't))))))))

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
      (is (= :net.littleredcomputer.math.value/function (v/kind (g/expt add2 2)))))
    (testing "arity 2"
      (let [f (fn [x y] (+ x y))
            g (fn [x y] (* x y))
            h (g/+ f g)
            k (g/+ 4 (g/- f 2))
            m (g/+ g (g/- f 2))]
        (is (= 11 (h 2 3)))
        (is (= 7 (k 2 3)))
        (is (= 9 (m 2 3)))))
    (testing "arity 0"
      (let [f (fn [] 3)
            g (fn [] 4)
            h (g/+ f g)
            k (g/- f g)
            j (g/* f g)
            q (g/divide f g)]
        (is (= 7 (h)))
        (is (= -1 (k)))
        (is (= 12 (j)))
        (is (= 3/4 (q)))))
    (testing "at least 0 arity"
      (let [add (fn [& xs] (reduce + 0 xs))
            mul (fn [& xs] (reduce * 1 xs))
            add+mul (g/+ add mul)
            add-mul (g/- add mul)
            mul-add (g/- mul add)]
        (is (= [:at-least 0] (v/arity add)))
        (is (= [:at-least 0] (v/arity mul)))
        (is (= [:at-least 0] (v/arity add+mul)))
        (is (= 33 (add+mul 2 3 4)))
        (is (= -15 (add-mul 2 3 4)))
        (is (= 15 (mul-add 2 3 4)))))))

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
