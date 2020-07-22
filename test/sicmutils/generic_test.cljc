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

(ns sicmutils.generic-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.util :as u]))

(defmulti s* v/argument-kind)
(defmulti s+ v/argument-kind)

(defn multiply-string
  [n s]
  (apply str (repeat n s)))

(defn product-string
  [s t]
  (apply str (for [cs s ct t] (str cs ct))))

(def string #?(:clj String :cljs js/String))

(defmethod s* [u/numtype string] [n s] (multiply-string n s))
(defmethod s* [string u/numtype] [s n] (multiply-string n s))
(defmethod s* [string string] [s t] (product-string s t))
(defmethod s+ [string string] [s t] (str s t))

(deftest handler-fn
  (testing "multiply-string"
    (is (= "foofoofoo" (multiply-string 3 "foo")))
    (is (= "" (multiply-string 0 "bar")))
    (is (= "" (multiply-string -2 "bar"))))
  (testing "mul"
    (is (= "bazbaz" (s* 2 "baz")))
    (is (= "quxquxqux" (s* 3 "qux")))
    (is (= "quxquxqux" (s* "qux" 3)))
    (is (= "cecrcicnoeoroionlelrlilnieiriiinnenrninn" (s* "colin" "erin")))
    (is (= "eceoeleienrcrorlrirnicioiliiinncnonlninn" (s* "erin" "colin"))))
  (testing "add"
    (is (= "foobar" (s+ "foo" "bar")))
    (is (= "zzz" (s+ "" "zzz")))
    ))

(deftest type-assigner
  (testing "types"
    (is (= #?(:clj Long :cljs js/Number) (v/kind 9)))
    (is (= #?(:clj Double :cljs js/Number) (v/kind 99.0)))))

(deftest generic-plus
  (is (= 0 (g/+)) "no args returns additive identity")
  (is (= 3.14 (g/+ 3.14)) "single arg should return itself")
  (is (= 10 (g/+ 0 10 0.0 0 0)) "multi-arg works, as long as zeros appear.")
  (is (= "happy" (g/+ 0 "happy" 0.0 0 0)) "returns really anything."))

(deftest generic-minus
  (is (= 0 (g/-)) "no-arity returns the additive identity.")
  (is (= 10 (g/- 10 0)) "Subtracting a zero works, with no implementations registered.")
  (is (= "face" (g/- "face" 0))))

(deftest generic-times
  (is (= 1 (g/*)) "No args returns the multiplicative identity.")
  (is (= 2 (g/* 2)) "single arg returns itself.")
  (is (= 5 (g/* 5 1) (g/* 1 5)) "Anything times a 1 returns itself.")
  (is (= "face" (g/* "face" 1) (g/* 1 "face")) "works for really anything."))

(deftest generic-divide
  (is (= 1 (g/divide)) "division with no args returns multiplicative identity")
  (is (= 20 (g/divide 20 1)) "dividing by one a single time returns the input")
  (is (= "face" (g/divide "face" 1 1 1 1.0 1)) "dividing by 1 returns the input"))
