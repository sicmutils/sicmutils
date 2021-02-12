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

(ns sicmutils.modint-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.generic-test :as gt]
            [sicmutils.modint :as m]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(deftest modint
  (testing "value implementation"
    (is (= '(modint 1 2)
           (v/freeze (m/make 1 2)))))

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
              (let [i       (:i m)
                    modulus (:m m)]
                (is (= (:i (g/expt m e))
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

    (testing "inv"
      (is (= m3_7 (g/invert m5_7)))
      (is (= m5_7 (g/invert m3_7)))
      (is (= m4_7 (g/invert m2_7)))
      (is (= m2_7 (g/invert m4_7)))
      (is (thrown? #?(:clj ArithmeticException :cljs js/Error) (g/invert m2_4)))
      (is (= m3_4 (g/invert m3_4))))))

(defn div-mul-inverse-test
  "Test that Modular division and exponentiation are inverses.

  For some p, for all $1 < i < p$, $1 < j < p$ such that $i \\mod p \\neq 0$,
  the modular quotient b of j and i times b gets back to j."
  [p]
  (doall
   (for [i (range 1 (inc p))
         j (range 1 (inc p))
         :when (not (zero? (mod i p)))
         :let [jp (m/make j p)
               ip (m/make i p)]]
     (is (= jp (-> (g// jp ip)
                   (g/* ip)))))))

(deftest div-mul-tests
  (testing "Modular division and exponentiation are inverses."
    (div-mul-inverse-test 47)))
