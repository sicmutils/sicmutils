;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.numbers-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
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
  (with-comparator (v/within 1e-3)
    (l/field 100 (sg/reasonable-double) #?(:clj  "java.lang.Double"
                                           :cljs "floating point js/Number"))))

(deftest number-generics
  (gt/integral-tests identity :exclusions #{:exact-divide})
  (gt/floating-point-tests identity :eq near)

  (testing "log converts to complex"
    (is (c/complex? (g/log -10)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "expt goes rational with negative expt"
    (is (= #sicm/ratio 1/4 (g/expt 2 -2))))

  (testing "exp/log round-trip, but coerce to double on the JVM"
    (is (= 0.0 (g/log (g/exp 0))))
    (is (= 10.0 (g/log (g/exp 10)))))

  (testing "div"
    (is (= 5 (g/div 20 4))))

  (testing "invert"
    (is (= 1 (g/invert 1)))
    (is (= (g/div 1 21) (g/invert 21))))

  (testing "sqrt handles negative numbers, 0"
    (is (= 0 (g/sqrt 0)))
    (is (= 9 (g/sqrt 81)))
    (is (c/complex? (g/sqrt -81)))
    (is (= (c/complex 0 9) (g/sqrt -81))))

  (testing "sqrt of one preserves type"
    (is (v/one-like (g/sqrt c/ONE)))
    (is (c/complex? (g/sqrt c/ONE))))

  (checking "transpose, determinant, trace act as id" 100 [x sg/real]
            (is (= x (g/transpose x)))
            (is (= x (g/determinant x)))
            (is (= x (g/trace x))))

  (checking "dimension always returns 1" 100 [x sg/real]
            (is (= 1 (g/dimension x)))))

(deftest integer-generics
  (gt/integral-tests u/int)
  (gt/integral-a->b-tests u/int identity :exclusions #{:exact-divide})

  (testing "g/expt"
    (is (= (g/expt (u/int 2) (u/int -2))
           (g/invert (u/int 4))))))

(deftest long-generics
  (gt/integral-tests u/long)
  (gt/integral-a->b-tests u/long identity :exclusions #{:exact-divide})

  (testing "g/expt"
    (is (= (g/expt (u/long 2) (u/long -2))
           (g/invert (u/long 4))))))

(deftest bigint-generics
  (gt/integral-tests u/bigint)
  (gt/integral-a->b-tests u/bigint identity)
  (gt/integral-a->b-tests identity u/bigint)

  (testing "bigint/float compatibility"
    (testing "g/expt"
      (is (= (g/expt (u/bigint 2) (u/bigint -2))
             (g/invert (u/bigint 4)))))

    (testing "g/add with floating point"
      (is (= 12.5 (g/add (u/bigint 10) 2.5)))
      (is (= 12.5 (g/add 2.5 (u/bigint 10)))))

    (testing "g/mul with floating point"
      (is (ish? 25 (g/mul (u/bigint 10) 2.5)))
      (is (ish? 25 (g/mul 2.5 (u/bigint 10)))))

    (testing "g/sub with floating point"
      (is (= 7.5 (g/sub (u/bigint 10) 2.5)))
      (is (= -7.5 (g/sub 2.5 (u/bigint 10)))))

    (testing "g/expt with floating point"
      (is (ish? 316.2277660168379 (g/expt (u/bigint 10) 2.5)))
      (is (ish? 9536.7431640625 (g/expt 2.5 (u/bigint 10)))))))

#?(:clj
   (deftest biginteger-generics
     (gt/integral-tests u/biginteger)))

(deftest double-generics
  (gt/integral-tests double
                     :eq near
                     :exclusions #{:exact-divide :gcd :remainder :modulo :quotient})
  (gt/floating-point-tests double :eq near))

(deftest arithmetic
  (testing "misc trig"
    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0)))
    (is (ish? (/ Math/PI 2) (g/asin 1)))
    (is (ish? (/ Math/PI 2) (g/acos 0)))
    (is (ish? (/ Math/PI 6) (g/asin 0.5)))
    (is (ish? (/ Math/PI 4) (g/atan 1)))
    (is (ish? (/ Math/PI 4) (g/atan 1 1)))
    (is (ish? (- (/ Math/PI 4)) (g/atan -1)))
    (is (ish? (* -3 (/ Math/PI 4)) (g/atan -1 -1)))
    (is (ish? (* 3 (/ Math/PI 4)) (g/atan 1 -1)))
    (is (ish? (/ Math/PI -4) (g/atan -1 1)))
    (is (ish? (/ Math/PI 3) (g/acos #?(:clj 1/2 :cljs (/ 1 2))))))

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
    (is (= 0.0 (g/log 1)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "exp"
    (is (= 1.0 (g/exp 0)))))

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

(deftest floor-identity
  ;; via https://www.johndcook.com/blog/2020/08/14/multiply-divide-and-floor/
  (checking "floor identity" 100
            [n (->> gen/pos-int
                    (gen/fmap u/int)
                    (gen/such-that (complement zero?)))
             x sg/real]
            (is (= (g/floor x)
                   (g/floor (g/div (g/floor (g/mul n x))
                                   n)))))

  (checking "ceiling identity" 100
            [n (->> gen/pos-int
                    (gen/fmap u/int)
                    (gen/such-that (complement zero?)))
             x sg/real]
            (is (= (g/ceiling x)
                   (g/ceiling (g/div (g/ceiling (g/mul n x))
                                     n))))))

(deftest numeric-trig-tests
  (testing "trig"
    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0))))

  (testing ">1 gets promoted to complex for asin, acos"
    (is (c/complex? (g/asin 2)))
    (is (c/complex? (g/acos 2))))

  (checking "sin" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/sin n) (g/sin n))))

  (checking "cos" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/cos n) (g/cos n))))

  (checking "tan native" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/tan n) (g/tan n))))

  (checking "tan" 100 [n sg/real]
            (when-not (v/zero? (g/cos n))
              (is (ish? (g/div (g/sin n)
                               (g/cos n))
                        (g/tan n)))))

  (checking "asin native" 100 [n (sg/reasonable-double
                                  {:min (+ -1 1e-10)
                                   :max (- 1 1e-10)})]
            (is (ish? (Math/asin n) (g/asin n))))

  (with-comparator (v/within 1e-8)
    (checking "cos/acos" 100 [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/cos (g/acos n)))))

    (checking "sin/asin" 100 [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/sin (g/asin n)))))

    (checking "tan/atan" 100 [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/tan (g/atan n))))))

  (testing "tan/atan, 2 arity version"
    (is (ish? (/ 0.5 0.2)
              (g/tan (g/atan 0.5 0.2)))))

  (checking "cot" 100 [n sg/real]
            (when-not (or (v/zero? (g/sin n))
                          (v/zero? (g/cos n)))
              (is (ish? (g/cot n)
                        (g/invert (g/tan n))))))

  (checking "cosh" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/cosh n) (g/cosh n))))

  (checking "sinh" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/sinh n) (g/sinh n))))

  (checking "tanh" 100 [n (sg/reasonable-double {:min -100 :max 100})]
            (when-not (v/zero? (g/cosh n))
              (is (ish? (g/tanh n)
                        (g/div (g/sinh n) (g/cosh n))))))

  (checking "sec" 100 [n sg/real]
            (when-not (v/zero? (g/cosh n))
              (is (ish? (g/sec n)
                        (g/invert (g/cos n))))))

  (checking "csc" 100 [n sg/real]
            (when-not (v/zero? (g/sin n))
              (is (ish? (g/csc n)
                        (g/invert (g/sin n))))))

  (checking "sech" 100 [n sg/real]
            (let [cosh-n (g/cosh n)]
              (when-not (v/zero? cosh-n)
                (is (ish? (g/sech n)
                          (g/invert cosh-n))))))

  (testing "acosh, numbers that need to convert to complex"
    (is (ish? (g/acosh 0) (g/acosh (u/int 0))))
    (is (ish? (g/acosh 0) (g/acosh (u/long 0))))
    (is (ish? (g/acosh 0) (g/acosh (u/bigint 0)))))

  (with-comparator (v/within 1e-8)
    (checking "acosh" 100
              [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/cosh (g/acosh n)))))

    (checking "asinh" 100
              [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/sinh (g/asinh n)))))

    (checking "atanh" 100
              [n (sg/reasonable-double {:min -10 :max 10})]
              (when-not (v/one? (g/abs n))
                (is (ish? n (g/tanh (g/atanh n))))))))

(deftest complex-constructor-tests
  (checking "make-rectangular with zero acts as id" 100 [n sg/real]
            (let [z (g/make-rectangular n 0)]
              (is (= n z))
              (is (not (c/complex? z))))

            #?(:clj
               (is (c/complex? (g/make-rectangular n 0.0))
                   "On the JVM we can make a non-exact zero and show that this
                   forces a conversion to complex.")))

  (checking "make-polar with zero angle or radius acts as id" 100 [n sg/real]
            (let [z (g/make-polar n 0)]
              (is (= n z))
              (is (not (c/complex? z))))

            #?(:clj
               (if (v/exact-zero? n)
                 (is (not (c/complex? (g/make-polar n 0.0)))
                     "exactly-zero radius stays itself.")
                 (is (c/complex? (g/make-polar n 0.0))
                     "On the JVM we can make a non-exact zero and show that this
                   forces a conversion to complex."))))

  (checking "make-rectangular" 100
            [real-part      (sg/reasonable-real 1e4)
             imaginary-part (sg/reasonable-real 1e4)]
            (let [z (g/make-rectangular real-part imaginary-part)]
              (is (== real-part (g/real-part z)))
              (is (== imaginary-part (g/imag-part z)))))

  (with-comparator (v/within 1e-8)
    (checking "make-rect, make-polar together" 100
              [real-part (sg/reasonable-real 1e3)
               imaginary-part (sg/reasonable-real 1e3)]
              (let [z (g/make-rectangular real-part imaginary-part)]
                (is (ish? z (g/make-polar
                             (g/magnitude z)
                             (g/angle z)))))))

  (checking "make-polar" 100
            [radius (sg/reasonable-real 1e4)
             angle (sg/reasonable-real 1e4)]
            (let [z (g/make-polar radius angle)]
              (is (ish? (g/magnitude z)
                        (g/sqrt (g/* z (g/conjugate z)))))
              (is (ish? (g/* radius (g/exp (g/* c/I angle)))
                        z))
              (is (ish? (g/abs radius)
                        (g/magnitude z))))))

(deftest complex-accessor-tests
  (checking "real/imag-part" 100 [x sg/real]
            (is (= x (g/real-part x)))
            (is (zero? (g/imag-part x))))

  (checking "conjugate" 100 [x sg/real]
            (is (= x (g/conjugate x))))

  (checking "angle" 100 [x sg/real]
            (if (neg? x)
              (is (ish? Math/PI (g/angle x))
                  "the angle of a negative number is pi in the complex plane.")
              (is (v/zero? (g/angle x))))))
