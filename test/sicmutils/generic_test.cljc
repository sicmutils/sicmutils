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
            [sicmutils.value :as v]
            [sicmutils.generic :as g]))

(defmulti s* v/argument-kind)
(defmulti s+ v/argument-kind)

(defn multiply-string
  [n s]
  (apply str (repeat n s)))

(defn product-string
  [s t]
  (apply str (for [cs s ct t] (str cs ct))))

(def number #?(:clj Number :cljs js/Number))
(def string #?(:clj String :cljs js/String))

(defmethod s* [number string] [n s] (multiply-string n s))
(defmethod s* [string number] [s n] (multiply-string n s))
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

(deftest generic-plus
  (testing "simple"
    (is (= 0 (g/+)))
    (is (= 7 (g/+ 7)))
    (is (= 7 (g/+ 3 4))))
  (testing "many"
    (is (= 33 (g/+ 3 4 5 6 7 8)))))

(deftest type-assigner
  (testing "types"
    (is (= #?(:clj Long :cljs js/Number) (v/kind 9)))
    (is (= #?(:clj Double :cljs js/Number) (v/kind 99.0)))))
