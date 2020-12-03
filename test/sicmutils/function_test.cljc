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

(ns sicmutils.function-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.operator :as o]
            [sicmutils.series :as series]
            [sicmutils.structure :as s]
            [sicmutils.simplify :as ss :refer [hermetic-simplify-fixture]]
            [sicmutils.function :as f]))

(use-fixtures :once hermetic-simplify-fixture)

(def ^:private near (v/within 1.0e-6))

(deftest trig-tests
  (testing "tan, sin, cos"
    (let [f (g/- g/tan (g/div g/sin g/cos))]
      (is (zero? (g/simplify (f 'x))))))

  (testing "sin/asin"
    (let [f (f/compose g/sin g/asin)]
      (is (near 0.5 (f 0.5)))
      (testing "outside real range"
        (is (near 10 (g/magnitude (f 10)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "cos/acos"
    (let [f (f/compose g/cos g/acos)]
      (is (near 0.5 (f 0.5)))

      (testing "outside real range"
        (is (near 5 (g/magnitude (f -5)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "tan/atan"
    (let [f (f/compose g/tan g/atan)]
      (is (near (/ 0.5 0.2) (f 0.5 0.2))
          "two-arity version!")
      (is (near 0.5 (f 0.5))
          "one-arity version")))

  (testing "cot"
    (let [f (g/- g/cot (g/invert g/tan))]
      (is (zero? (g/simplify (f 'x))))))

  (testing "tanh"
    (let [f (g/- (g/div g/sinh g/cosh) g/tanh)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "sec"
    (let [f (g/- (g/invert g/cos) g/sec)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "csc"
    (let [f (g/- (g/invert g/sin) g/csc)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "sech"
    (let [f (g/- (g/invert g/cosh) g/sech)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "cosh"
    (is (near ((g/cosh g/square) 2)
              (g/cosh 4))))

  (testing "sinh"
    (is (near ((g/sinh g/square) 2)
              (g/sinh 4))))

  (testing "acosh"
    (let [f (f/compose g/cosh g/acosh)]
      (is (near 10 (f 10))
          "TODO this can't handle the full generic simplification yet. Sub in
          when we get more rules.")

      (testing "outside real range"
        (is (near 5 (g/magnitude (f -5)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "asinh"
    (let [f (f/compose g/sinh g/asinh)]
      (is (near 10 (f 10)))

      (testing "outside real range"
        (is (near 5 (g/magnitude (f -5)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "atanh"
    (let [f (f/compose g/tanh g/atanh)]
      (is (near 0.5 (f 0.5)))

      (testing "outside real range"
        (is (near 10 (g/magnitude (f 10)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals.")))))

(deftest complex-tests
  (testing "gcd/lcm unit"
    (is (= (g/gcd 10 5)
           (let [deferred (g/gcd (g/+ 1 g/square) 5)]
             (deferred 3))))
    (is (= (g/lcm 10 6)
           (let [deferred (g/lcm (g/+ 1 g/square) 6)]
             (deferred 3))))))

(defn transpose-defining-relation [T g a]
  "T is a linear transformation T:V -> W
  the transpose of T, T^t:W* -> V*
  Forall a in V, g in W*,  g:W -> R
  (T^t(g))(a) = g(T(a))."
  (g/- (((g/transpose T) g) a)
       (g (T a))))

(deftest transpose-test
  (testing "transpose"
    (let [f #(str "f" %)
          g #(str "g" %)]
      (is (= "fg" (f (g ""))))
      (is (= "gf" (((g/transpose f) g) ""))
          "g/transpose for functions returns a fn that takes ANOTHER fn, and
    returns a fn that applies them in reverse order. Like a curried andThen (the
    reverse of compose)"))))

(deftest string-form-test
  (is (= "1" (ss/expression->string
              ((g/+ (g/square g/sin) (g/square g/cos)) 'x))))

  (is (= "(/ (+ (* -1 (expt (cos x) 4)) 1) (expt (cos x) 2))"
         (ss/expression->string
          ((g/+ (g/square g/sin) (g/square g/tan)) 'x)))))

(deftest function-algebra
  (let [add2 (fn [x] (g/+ x 2))
        explog (g/exp g/log)
        mul3 #(* 3 %)]
    (testing "unary"
      (is (= 4 (add2 2)))
      (is (= -4 ((g/- add2) 2)))
      (is (= 9 ((g/sqrt add2) 79)))
      (is (= #sicm/ratio 1/9 ((g/invert add2) 7)))
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
      (is (= ::v/function (v/kind (g/expt add2 2))))

      (testing "determinant"
        (is (= 20 ((g/determinant *) 4 5))))

      (testing "cross-product"
        (let [deferred (g/cross-product #(g/* 2 %)
                                        #(g/+ (s/up 4 3 1) %))
              v (s/up 1 2 3)]
          (is (= (g/cross-product (g/* 2 v)
                                  (g/+ (s/up 4 3 1) v))
                 (deferred v))
              "Slightly tougher since this works with structures"))))

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
        (is (= #sicm/ratio 3/4 (q)))))

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
    (is (= 12 ((double f) 1)))
    (is (= 24 ((double (double f)) 1)))
    (is (= 12 ((double-op f) 1)))
    (is (= 24 ((double-op (double-op f)) 1)))
    (is (= 24 (((g/* double-op double-op) f) 1))) ;; * for operators is composition
    (is (= 144 (((g/* double double) f) 1)))      ;; * for functions is pointwise multiply
    (is (= 2 ((double-op identity) 1)))
    (is (= 6 (((g/expt double-op 0) f) 1)))
    (is (= 12 (((g/expt double-op 1) f) 1)))
    (is (= 24 (((g/expt double-op 2) f) 1)))
    (is (= 18 (((g/+ double-op double-op double-op) identity) 3)))
    (is (= 24 (((g/+ double-op 4 double-op) identity) 3)))))

(deftest moved-from-series
  (let [simp4 (fn [x] (g/simplify (take 4 x)))
        V (series/series g/sin g/cos g/tan)]

    (testing "derivatives"
      (is (= '[(sin t) (cos t) (tan t) 0]
             (simp4 (V 't))))
      (is (= '[(cos t) (* -1 (sin t)) (/ 1 (expt (cos t) 2)) 0]
             (simp4 ((D V) 't)))))

    (testing "f -> Series"
      (let [F (fn [k] (series/series
                      (fn [t] (g/* k t))
                      (fn [t] (g/* k k t))))]
        (is (= '((* q z) (* (expt q 2) z) 0 0) (simp4 ((F 'q) 'z))))
        (is (= '(z (* 2 q z) 0 0) (simp4 (((D F) 'q) 'z))))))))
