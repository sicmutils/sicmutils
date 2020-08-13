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

(ns sicmutils.modint-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.value :as v]
            [sicmutils.generic :as g]
            [sicmutils.generic-test :as gt]
            [sicmutils.modint :as m]))

(deftest modint
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

    (testing "nullity?"
      (is (v/nullity? m0_7)))

    (testing "unity?"
      (is (v/unity? m1_7)))

    (testing "inv"
      (is (= m3_7 (g/invert m5_7)))
      (is (= m5_7 (g/invert m3_7)))
      (is (= m4_7 (g/invert m2_7)))
      (is (= m2_7 (g/invert m4_7)))
      (is (thrown? #?(:clj ArithmeticException :cljs js/Error) (g/invert m2_4)))
      (is (= m3_4 (g/invert m3_4))))))
