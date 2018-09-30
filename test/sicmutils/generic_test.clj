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
  (:require [clojure.test :refer :all]
            [sicmutils
             [value :as v]
             [generic :refer :all]]))

(defmulti s* v/argument-kind)
(defmulti s+ v/argument-kind)

(defn multiply-string
  [n s]
  (apply str (repeat n s)))

(defn product-string
  [s t]
  (apply str (for [cs s ct t] (str cs ct))))

(defmethod s* [Number String] [n s] (multiply-string n s))
(defmethod s* [String Number] [s n] (multiply-string n s))
(defmethod s* [String String] [s t] (product-string s t))
(defmethod s+ [String String] [s t] (str s t))

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
    (is (= [Long] (v/argument-kind 9)))
    (is (= [Double] (v/argument-kind 99.0)))))

(deftest joint-arities
  (let [exactly (fn [n] {:arity [:exactly n]})
        at-least (fn [n] {:arity [:at-least n]})
        between (fn [m n] {:arity [:between m n]})]
    (is (= [:exactly 1] (joint-arity [(exactly 1) (exactly 1)])))
    (is (= [:exactly 5] (joint-arity [(exactly 5) (exactly 5)])))
    (is (thrown? IllegalArgumentException (joint-arity [(exactly 2) (exactly 1)])))
    (is (thrown? IllegalArgumentException (joint-arity [(exactly 1) (exactly 2)])))
    (is (= [:exactly 3] (joint-arity [(exactly 3) (at-least 2)])))
    (is (= [:exactly 3] (joint-arity [(exactly 3) (at-least 3)])))
    (is (= [:exactly 3] (joint-arity [(at-least 1) (exactly 3)])))
    (is (= [:exactly 3] (joint-arity [(at-least 3) (exactly 3)])))
    (is (thrown? IllegalArgumentException (joint-arity [(exactly 1) (at-least 2)])))
    (is (thrown? IllegalArgumentException (joint-arity [(at-least 2) (exactly 1)])))
    (is (= [:at-least 3] (joint-arity [(at-least 2) (at-least 3)])))
    (is (= [:at-least 3] (joint-arity [(at-least 3) (at-least 2)])))
    (is (= [:between 2 3] (joint-arity [(between 1 3) (between 2 5)])))
    (is (= [:between 2 3] (joint-arity [(between 2 5) (between 1 3)])))
    (is (thrown? IllegalArgumentException (joint-arity [(between 1 3) (between 4 6)])))
    (is (thrown? IllegalArgumentException (joint-arity [(between 4 6) (between 1 3)])))
    (is (= [:exactly 3] (joint-arity [(between 1 3) (between 3 4)])))
    (is (= [:exactly 3] (joint-arity [(between 3 4) (between 1 3)])))
    (is (= [:between 2 4] (joint-arity [(at-least 2) (between 1 4)])))
    (is (= [:between 2 4] (joint-arity [(between 1 4) (at-least 2)])))
    (is (thrown? IllegalArgumentException (joint-arity [(at-least 4) (between 1 3)])))
    (is (thrown? IllegalArgumentException (joint-arity [(between 1 3) (at-least 4)])))
    (is (= [:exactly 2] (joint-arity [(exactly 2) (between 2 3)])))
    (is (= [:exactly 2] (joint-arity [(between 2 3) (exactly 2)])))
    (is (thrown? IllegalArgumentException (joint-arity [(between 2 3) (exactly 1)])))
    (is (thrown? IllegalArgumentException (joint-arity [(exactly 1) (between 2 3)])))))
