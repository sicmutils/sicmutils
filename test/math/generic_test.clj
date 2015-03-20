;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.generic-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.value :as v]
            [math.generic :refer :all]))

(def T1 (dtree-insert
         (dtree-insert
          (dtree-insert empty-dtree :op1 [:p1 :p2])
          :op2 [:p2 :p1])
         :op3 [:p2 :p1 :p3]))

(deftest dtree-1
  (testing "lookup"
    (is (= :op1 (dtree-lookup T1 :op [#{:p1} #{:p2}])))
    (is (= :op2 (dtree-lookup T1 :op [#{:p2} #{:p1}])))
    (is (= :op3 (dtree-lookup T1 :op [#{:p2} #{:p1} #{:p3}])))
    (is (not (dtree-lookup T1 :op [#{:p2} #{:p1} #{:p3} #{:p1}])))
    (is (not (dtree-lookup T1 :op [])))
    (is (not (dtree-lookup T1 :op [#{:p2}#{:p2}])))))

(defhandler :y [integer? integer?] :intint)
(defhandler :y [integer? float?]   :intfloat)
(defhandler :y [string? string?]   :strstr)
(defhandler :z [number? string?]   :numstr)

(deftest handler-map
  (testing "lookup"
    (is (= :intint (findhandler :y [1 1])))
    (is (not (findhandler :y [1 "2"])))
    (is (= :intfloat (findhandler :y [1 3.13])))
    (is (= :numstr (findhandler :z [1e10, "baz"])))
    (is (not (findhandler :y [1e10, "baz"])))))

(defn multiply-string
  [n s]
  (apply str (repeat n s)))

(defn product-string
  [s t]
  (apply str(for [cs s ct t] (str cs ct))))

(extend-protocol v/Value
  String
  (nullity? [s] (= s ""))
  (unity? [_] false)
  (zero-like [_] "")
  (sort-key [_] 25)
  (compound? [_] false)
  (kind [_] (class "")))

(defhandler :s* [number? string?] multiply-string)
(defhandler :s* [string? number?] #(multiply-string %2 %1))
(defhandler :s* [string? string?] product-string)
(defhandler :s+ [string? string?] str)

(def s+ (make-operation :s+ 2))
(def s* (make-operation :s* 2))

(deftest handler-fn
  (testing "multiply-string"
    (is (= "foofoofoo" (multiply-string 3 "foo")))
    (is (= "" (multiply-string 0 "bar")))
    (is (= "" (multiply-string -2 "bar")))
    (is (= "barbarbar" (let [args [3 "bar"]
                             h (findhandler :s* args)]
                         (apply h args)))))
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
    (is (= 0 (+)))
    (is (= 7 (+ 7)))
    (is (= 7 (+ 3 4))))
  (testing "many"
    (is (= 33 (+ 3 4 5 6 7 8)))))

(deftest type-assigner
  (testing "types"
    (is (= java.lang.Long (v/kind 9)))
    (is (= java.lang.Double (v/kind 99.0)))))
