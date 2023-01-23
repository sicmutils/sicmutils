#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.modint-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.modint :as m]
            [emmy.util :as u]
            [emmy.value :as v]))

(deftest modint
  (testing "ModInt throws with non-integer arguments"
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (m/make 12.2 3)))

    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (m/make 12.2 3.2)))

    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (m/make 12 3.2))))

  (testing "value implementation"
    (is (= '(modint 1 2)
           (v/freeze (m/make 1 2)))))

  (checking "v/= can handle non-modint instances" 100
            [m (sg/modint)
             n gen/small-integer]
            (let [incremented (g/+ (m/residue m)
                                   (g/* n (m/modulus m)))]
              (is (v/= (m/residue m) m)
                  "v/= can take numbers on the left")

              (is (= m (m/residue m))
                  "= can handle numbers on the right")

              (is (v/= m (m/residue m))
                  "v/= can too")

              (is (= m incremented)
                  "= returns true of the non-ModInt on the right is incremented
                  by a multiple of the modulus")

              (is (v/= m incremented)
                  "v/= can do this too...")

              (is (v/= incremented m)
                  "v/= can handle the number on the left.")))

  (testing "equality unit tests"
    (is (v/= 3 (m/make 3 10))
        "v/= compares numbers with ModInt instances by modding them first")

    (is (v/= 13 (m/make 3 10))
        "If the number is incremented by a multiple of the modulus, v/= returns
        true."))

  (let [m3_7 (m/make 3 7)
        m0_7 (m/make 0 7)
        m1_7 (m/make 1 7)
        m5_7 (m/make 5 7)
        m4_7 (m/make 4 7)
        m12_7 (m/make 12 7)
        m5_7b (m/make 5 7)
        m2_7 (m/make 2 7)
        m5_13 (m/make 5 13)
        m2_4 (m/make 2 4)
        m3_4 (m/make 3 4)]

    (testing "easy"
      (is (= m5_7 m5_7))
      (is (= m12_7 m5_7))
      (is (= m5_7b m5_7))
      (is (not= m2_7 m5_7))
      (is (not= m5_13 m5_7)))

    (testing "add"
      (is (= m3_7 (g/+ m5_7 m5_7))))

    (testing "sub"
      (is (= m2_7 (g/- m5_7 m3_7))))

    (testing "neg"
      (is (= m2_7 (g/negate m5_7))))

    (checking "m^i matches simple implementation" 100
              [m (sg/modint)
               e (gen/fmap g/abs sg/native-integral)]
              (let [i       (m/residue m)
                    modulus (m/modulus m)]
                (is (= (m/residue (g/expt m e))
                       (-> (g/expt (u/bigint i)
                                   (u/bigint e))
                           (g/modulo modulus))))))

    (testing "zero?"
      (is (v/zero? m0_7))
      (is (v/zero? (v/zero-like m5_7))))

    (testing "one?"
      (is (v/one? m1_7))
      (is (v/one? (v/one-like m5_7))))

    (testing "identity?"
      (is (v/identity? m1_7))
      (is (v/identity? (v/identity-like m5_7))))

    (testing "compatibility"
      (is 4 (g/integer-part m4_7))
      (is 0 (g/fractional-part m4_7))
      (is 4 (g/floor m4_7))
      (is 4 (g/ceiling m4_7)))

    (testing "inv"
      (is (= m3_7 (g/invert m5_7)))
      (is (= m5_7 (g/invert m3_7)))
      (is (= m4_7 (g/invert m2_7)))
      (is (= m2_7 (g/invert m4_7)))
      (is (thrown? #?(:clj ArithmeticException :cljs js/Error) (g/invert m2_4)))
      (is (= m3_4 (g/invert m3_4))))

    (testing "Chinese Remainder Algorithm"
      (let [a1 (m/make 2 5)
            a2 (m/make 3 13)
            cr (m/chinese-remainder a1 a2)]
        (is (= 42 cr))
        (is (= (m/residue a1) (mod cr (m/modulus a1))))
        (is (= (m/residue a2) (mod cr (m/modulus a2))))))))

(defn div-mul-inverse-test
  "Test that:

  - Modular division and exponentiation are inverses
  - solve-linear-left and solve-linear-right behave appropriately

  For some p, for all $1 < i < p$, $1 < j < p$ such that $i \\mod p \\neq 0$,
  the modular quotient b of j and i times b gets back to j."
  [p]
  (is (every? true?
              (for [i (range 1 (inc p))
                    j (range 1 (inc p))
                    :when (not (zero? (mod i p)))
                    :let [jp (m/make j p)
                          ip (m/make i p)]]
                (and (= (g// jp ip)
                        (g/solve-linear-right jp ip))
                     (= (g// jp ip)
                        (g/solve-linear-left ip jp))
                     (= jp (-> (g// jp ip)
                               (g/* ip))))))))

(deftest div-mul-tests
  (testing "Modular division and exponentiation are inverses."
    (div-mul-inverse-test 47)))
