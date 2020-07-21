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

(deftest generic-plus
  (testing "simple"
    (is (= 7 (g/+ 3 4)))
    (is (= 4 (g/+ 2 2)))
    (is (= 3.5 (g/+ 1.5 2))))

  (testing "many"
    (is (= 0 (g/+)))
    (is (= 3.14 (g/+ 3.14)))
    (is (= 7 (g/+ 7)))
    (is (= 10 (g/+ 1 2 3 4)))
    (is (= 33 (g/+ 3 4 5 6 7 8)))))

(deftest generic-minus
  "Subtraction with a single arg doesn't work since we currently lack a default
  implementation for negate."
  (testing "simple"
    (is (= 2.14 (g/- 3.14 1))))

  (testing "many"
    (is (= 0 (g/-)))
    (is (= -14 (g/- 10 9 8 7)))))

(deftest generic-times
  (testing "simple"
    (is (= 20 (g/* 5 4))))

  (testing "many"
    (is (= 1 (g/*)))
    (is (= 2 (g/* 2)))
    (is (= 4 (g/* 2 2)))
    (is (= 8 (g/* 2 2 2)))))

(deftest generic-divide
  "division with a single argument doesn't work, since there's currently no
  default implementation for invert."
  (testing "simple"
    (is (= 5 (g/divide 20 4))))

  (testing "many"
    (is (= 1 (g/divide)))
    (is (= 2 (g/divide 8 2 2)))))

#?(:clj
   (deftest with-ratios
     (is (= 13/40 (g/+ 1/5 1/8)))
     (is (= 1/8 (g/- 3/8 1/4)))
     (is (= 5/4 (g/divide 5 4)))
     (is (= 1/2 (g/divide 1 2)))
     (is (= 1/4 (g/divide 1 2 2)))
     (is (= 1/8 (g/divide 1 2 2 2)))))

(deftest type-assigner
  (testing "types"
    (is (= #?(:clj Long :cljs js/Number) (v/kind 9)))
    (is (= #?(:clj Double :cljs js/Number) (v/kind 99.0)))))
