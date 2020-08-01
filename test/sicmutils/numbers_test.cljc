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
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.complex :as c]
            [sicmutils.util :as u]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numbers :as n]))

(def near (v/within 1e-12))

(defn integral-unary-tests [int->a & {:keys [exclusions]}]
  (when-not (:negate exclusions)
    (testing "negate"
      (is (= (int->a -4) (g/negate (int->a 4))))
      (is (= (int->a 4) (g/negate (g/negate (int->a 4)))))))

  (when-not (:abs exclusions)
    (testing "abs"
      (is (= (int->a 1) (g/abs (int->a -1))))
      (is (= (int->a 1) (g/abs (int->a 1))))))

  (when-not (:magnitude exclusions)
    (testing "magnitude"
      (is (= (int->a 123) (g/magnitude (int->a -123))))
      (is (= (int->a 123) (g/magnitude (int->a 123))))))

  (when-not (:square exclusions)
    (testing "square"
      (is (= (int->a 4) (g/square (int->a 2))))
      (is (= (int->a 4) (g/square (int->a -2))))))

  (when-not (:cube exclusions)
    (testing "cube"
      (is (= (int->a 27) (g/cube (int->a 3))))
      (is (= (int->a -27) (g/cube (int->a -3))))))

  (when-not (:negative? exclusions)
    (testing "negative?"
      (is (g/negative? (g/negate (int->a 4))))
      (is (not (g/negative? (g/negate (g/negate (int->a 4)))))))))

(defn integral-binary-tests [int->a int->b & {:keys [exclusions]}]
  (when-not (:add exclusions)
    (testing "add"
      (is (= (int->a 4)
             (g/add (int->a 2) (int->b 2))
             (g/add (int->b 2) (int->a 2))))
      (is (= (int->a 2)
             (g/add (int->a 2) (int->b 0))
             (g/add (int->b 2) (int->a 0))))
      (is (= (int->a 10)
             (reduce g/add (map int->a [1 2 3 4]))))))

  (when-not (:mul exclusions)
    (testing "mul"
      (is (= (int->a 20)
             (g/mul (int->a 5) (int->b 4))
             (g/mul (int->b 5) (int->a 4))))
      (is (= (int->a 4)
             (g/mul (int->a 2) (int->b 2))
             (g/mul (int->b 2) (int->a 2))))
      (is (= (int->a 8)
             (reduce g/mul (map int->a [2 2 2]))))))

  (when-not (:sub exclusions)
    (testing "sub"
      (is (= (int->a -4)
             (g/sub (int->a 0) (int->b 4))
             (g/sub (int->b 0) (int->a 4))))))

  (when-not (:expt exclusions)
    (testing "expt"
      (is (= (int->a 32)
             (g/expt (int->a 2) (int->b 5))
             (g/expt (int->b 2) (int->a 5))))))

  (when-not (:quotient exclusions)
    (testing "quotient"
      (is (= (int->a 2)
             (g/quotient (int->a 5) (int->b 2))
             (g/quotient (int->b 5) (int->a 2))))))

  (when-not (:remainder exclusions)
    (testing "remainder"
      (is (= (int->a 1)
             (g/remainder (int->a 5) (int->b 2))
             (g/remainder (int->b 5) (int->a 2)))))))

(defn integral-tests
  "Battery of tests that check the behavior of standard generic operations on
  types that handle integral numbers."
  [int->a & {:keys [exclusions]}]
  (integral-unary-tests int->a :exclusions exclusions)
  (integral-binary-tests int->a int->a :exclusions exclusions))

#_(defn floating-point-tests
    "Battery of tests that check the behavior of standard generic operations on
  types that handle floating point numbers."
    [from-float]
    (testing "add"
      (is (= 4 (g/add 2 2)))
      (is (= 2 (g/add 2 0)))
      (is (= 3.5 (g/add 1.5 2)))
      (is (= 10 (reduce g/add [1 2 3 4]))))

    (testing "mul"
      (is (= 20 (g/mul 5 4)))
      (is (= 4 (g/mul 2 2)))
      (is (= 8 (reduce g/mul [2 2 2]))))

    (testing "sub"
      (is (= -4 (g/sub 0 4)))
      (is (= 2.14 (g/sub 3.14 1)))
      (is (= -14 (reduce g/sub [10 9 8 7]))))

    (testing "negate"
      (is (= -4 (g/negate 4)))
      (is (= 4 (g/negate (g/negate 4))))
      (is (= -4 (g/negate 4)))
      (is (= -4.2 (g/negate 4.2))))

    (testing "div"
      (is (= 5 (g/div 20 4))))

    (testing "invert"
      (is (= 1 (g/invert 1)))
      (is (= (g/div 1 21) (g/invert 21))))

    (testing "expt"
      (is (= 32 (g/expt 2 5))))

    (testing "abs"
      (is (= 1 (g/abs -1)))
      (is (= 1 (g/abs 1))))

    (testing "magnitude"
      (is (= 123 (g/magnitude -123)))
      (is (= 123 (g/magnitude 123))))

    (testing "square/cube"
      (is (= 4 (g/square 2)))
      (is (= 4 (g/square -2)))
      (is (= 27 (g/cube 3)))
      (is (= -27 (g/cube -3))))

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
      (is (= 0 (g/log (g/exp 0))))

      ;; this might actually fail in CLJ.
      (is (= 10 (g/log (g/exp 10)))))

    (testing "negative?"
      (is (g/negative? (g/negate 4)))
      (is (not (g/negative? (g/negate (g/negate 4))))))

    (testing "quotient"
      (is (= 2 (g/quotient 5 2)))
      (is (= 2 (g/quotient 5N 2)))
      (is (= 2 (g/quotient 5 2N)))
      (is (= 2 (g/quotient 5N 2N))))

    #?(:clj
       (testing "bigint quotient"
         (is (= 2 (g/quotient (BigInteger/valueOf 5) 2)))
         (is (= 2 (g/quotient 5 (BigInteger/valueOf 2))))))

    (testing "remainder"
      (is (= 1 (g/remainder 5 2)))
      (is (= 1 (g/remainder 5N 2)))
      (is (= 1 (g/remainder 5 2N)))
      (is (= 1 (g/remainder 5N 2N)))))

(deftest number-generics
  (integral-tests identity))

(deftest integer-generics
  (integral-tests u/int))

(deftest long-generics
  (integral-tests u/long))

#?(:cljs
   (deftest bigint-generics
     (integral-tests js/BigInt)))

(deftest arithmetic
  #?(:clj
     (testing "with-ratios"
       (is (= 13/40 (g/add 1/5 1/8)))
       (is (= 1/8 (g/sub 3/8 1/4)))
       (is (= 5/4 (g/div 5 4)))
       (is (thrown? IllegalArgumentException (g/exact-divide 10/2 2/10)))
       (is (= 1 (g/exact-divide 2/10 2/10)))
       (is (= 1/2 (g/div 1 2)))
       (is (= 1/4 (reduce g/div [1 2 2])))
       (is (= 1/8 (reduce g/div [1 2 2 2])))
       (is (= 1/8 (g/invert 8)))))

  (testing "add"
    (is (= 4 (g/add 2 2)))
    (is (= 2 (g/add 2 0)))
    (is (= 3.5 (g/add 1.5 2)))
    (is (= 10 (reduce g/add [1 2 3 4]))))

  (testing "mul"
    (is (= 20 (g/mul 5 4)))
    (is (= 4 (g/mul 2 2)))
    (is (= 8 (reduce g/mul [2 2 2]))))

  (testing "sub"
    (is (= -4 (g/sub 0 4)))
    (is (= 2.14 (g/sub 3.14 1))))

  (testing "negate"
    (is (= -4 (g/negate 4)))
    (is (= 4 (g/negate (g/negate 4))))
    (is (= -4 (g/negate 4)))
    (is (= -4.2 (g/negate 4.2))))

  (testing "div"
    (is (= 5 (g/div 20 4)))
    (is (= -14 (reduce g/sub [10 9 8 7]))))

  (testing "invert"
    (is (= 1 (g/invert 1)))
    (is (= (g/div 1 21) (g/invert 21))))

  (testing "expt"
    (is (= 32 (g/expt 2 5))))

  (testing "abs"
    (is (= 1 (g/abs -1)))
    (is (= 1 (g/abs 1))))

  (testing "magnitude"
    (is (= 123 (g/magnitude -123)))
    (is (= 123 (g/magnitude 123))))

  (testing "square/cube"
    (is (= 4 (g/square 2)))
    (is (= 4 (g/square -2)))
    (is (= 27 (g/cube 3)))
    (is (= -27 (g/cube -3))))

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
        (= 10 (g/log (g/exp 10)))))

  (testing "negative?"
    (is (g/negative? (g/negate 4)))
    (is (not (g/negative? (g/negate (g/negate 4))))))

  (testing "quotient"
    (is (= 2 (g/quotient 5 2)))
    (is (= 2 (g/quotient 5N 2)))
    (is (= 2 (g/quotient 5 2N)))
    (is (= 2 (g/quotient 5N 2N))))

  #?(:clj
     (testing "bigint quotient"
       (is (= 2 (g/quotient (BigInteger/valueOf 5) 2)))
       (is (= 2 (g/quotient 5 (BigInteger/valueOf 2))))))

  (testing "remainder"
    (is (= 1 (g/remainder 5 2)))
    (is (= 1 (g/remainder 5N 2)))
    (is (= 1 (g/remainder 5 2N)))
    (is (= 1 (g/remainder 5N 2N)))))

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

#?(:clj
   (deftest with-ratios
     (is (= 13/40 (g/+ 1/5 1/8)))
     (is (= 1/8 (g/- 3/8 1/4)))
     (is (= 5/4 (g/divide 5 4)))
     (is (= 1/2 (g/divide 1 2)))
     (is (= 1/4 (g/divide 1 2 2)))
     (is (= 1/8 (g/divide 1 2 2 2)))))


(defn field-tests
  [from-int]
  (testing "div"
    (is (= 5 (g/div 20 4))))

  (testing "invert"
    (is (= 1 (g/invert 1)))
    (is (= (g/div 1 21) (g/invert 21)))))

(defn misc-tests
  "These seem a little tailored to the number implementation, but let's see. I
  guess, clearly, we should be promoting other types to complex too."
  [from-int]
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
    (is (c/complex? (g/sqrt c/ONE)))) )
