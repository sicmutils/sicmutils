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

(ns sicmutils.numbers-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.complex :as c]
            [sicmutils.util :as u]
            [sicmutils.generic :as g]
            [sicmutils.generic-test :as gt]
            [sicmutils.generators :as sg]
            [sicmutils.laws :as l]
            [sicmutils.numbers :as n]
            [sicmutils.value :as v]))

(def near (v/within 1e-12))

(deftest numeric-laws
  ;; All native types available on clj and cljs form fields.
  (l/field 100 sg/bigint #?(:clj "clojure.lang.BigInt" :cljs "js/BigInt"))
  (l/field 100 sg/long #?(:clj "java.lang.Long" :cljs "goog.math.Long"))
  (l/field 100 sg/integer #?(:clj "java.lang.Integer" :cljs "goog.math.Integer"))

  #?(:clj
     ;; There's no biginteger / bigint distinction in cljs.
     (l/field 100 sg/biginteger "java.math.BigInteger"))

  #?(:cljs
     ;; this is covered by sg/long in clj.
     (l/field 100 sg/native-integral "integral js/Number")))

(deftest floating-point-laws
  ;; Doubles form a field too.
  (l/field 100 (sg/reasonable-double) #?(:clj  "java.lang.Double"
                                         :cljs "floating point js/Number")))

(deftest number-generics
  (gt/integral-tests identity :exclusions #{:exact-divide})
  (gt/floating-point-tests identity :eq near)

  (testing "log converts to complex"
    (is (c/complex? (g/log -10)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "exp/log preserve exactness together"
    (is (= 0 (g/log (g/exp 0)))))

  (testing "exp/log goes approx if forced"
    (is (= 10.0 (g/log (g/exp 10)))))

  (testing "div"
    (is (= 5 (g/div 20 4))))

  (testing "invert"
    (is (= 1 (g/invert 1)))
    (is (= (g/div 1 21) (g/invert 21))))

  (testing "trig"
    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0))))

  (testing ">1 gets promoted to complex for asin, acos"
    (is (c/complex? (g/asin 2)))
    (is (c/complex? (g/acos 2))))

  (testing "sqrt handles negative numbers, 0"
    (is (= 0 (g/sqrt 0)))
    (is (= 9 (g/sqrt 81)))
    (is (c/complex? (g/sqrt -81)))
    (is (= (c/complex 0 9) (g/sqrt -81))))

  (testing "sqrt of one preserves type"
    (is (v/one-like (g/sqrt c/ONE)))
    (is (c/complex? (g/sqrt c/ONE)))))

(deftest integer-generics
  (gt/integral-tests u/int)
  (gt/integral-a->b-tests u/int identity :exclusions #{:exact-divide}))

(deftest long-generics
  (gt/integral-tests u/long)
  (gt/integral-a->b-tests u/long identity :exclusions #{:exact-divide}))

(deftest bigint-generics
  (gt/integral-tests u/bigint)
  (gt/integral-a->b-tests u/bigint identity)
  (gt/integral-a->b-tests identity u/bigint))

#?(:clj
   (deftest biginteger-generics
     (gt/integral-tests u/biginteger)))

(deftest double-generics
  (gt/integral-tests double
                     :eq near
                     :exclusions #{:exact-divide :gcd :remainder :modulo :quotient})
  (gt/floating-point-tests double :eq near))

(deftest arithmetic
  (testing "trig"
    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0))))

  (testing ">1 gets promoted to complex for asin, acos"
    (is (c/complex? (g/asin 2)))
    (is (c/complex? (g/acos 2))))

  (testing "sqrt handles negative numbers, 0"
    (is (= 0 (g/sqrt 0)))
    (is (= 9 (g/sqrt 81)))
    (is (c/complex? (g/sqrt -81)))
    (is (= (c/complex 0 9) (g/sqrt -81))))

  (testing "sqrt of one preserves type"
    (is (v/one-like (g/sqrt c/ONE)))
    (is (c/complex? (g/sqrt c/ONE))))

  (testing "log"
    (is (c/complex? (g/log -10)))
    (is (= 0 (g/log 1)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "exp"
    (is (= 1 (g/exp 0))))

  (testing "exp/log preserve exactness together"
    (is (= 0 (g/log (g/exp 0)))
        (= 10 (g/log (g/exp 10))))))

;; Test of generic wrapper operations.

(deftest generic-plus
  "sicmutils.numbers provides implementations of the methods needed by g/+. Test
  that these functions now come work with numbers."
  (testing "simple"
    (is (= 7 (g/+ 3 4)))
    (is (= 4 (g/+ 2 2)))
    (is (= 3.5 (g/+ 1.5 2))))

  (testing "many"
    (is (= 10 (g/+ 1 2 3 4)))
    (is (= 33 (g/+ 3 4 5 6 7 8)))))

(deftest generic-minus
  "numbers provides implementations, so test behaviors."
  (is (= -3.14 (g/- 3.14)))
  (is (= 2.14 (g/- 3.14 1)))

  (testing "many"
    (is (= -14 (g/- 10 9 8 7)))))

(deftest generic-times
  "numbers provides implementations, so test behaviors."
  (is (= 20 (g/* 5 4)))
  (is (= 4 (g/* 2 2)))
  (is (= 8 (g/* 2 2 2))))

(deftest generic-divide
  "numbers provides implementations, so test behaviors."
  (is (= 5 (g/divide 20 4)))
  (is (= 2 (g/divide 8 2 2))))
