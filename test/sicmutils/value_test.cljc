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

(ns sicmutils.value-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            #?(:cljs [cljs.reader :refer [read-string]])
            #?(:cljs [goog.array :as garray])
            [sicmutils.generators :as sg]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang PersistentVector))))

(deftest bigint-literal
  (testing "u/parse-bigint can round-trip Bigint instances in clj or cljs. "
    (is (= #?(:clj 10N
              :cljs '(sicmutils.util/bigint 10))
           (read-string {:readers {'sicm/bigint u/parse-bigint}}
                        (pr-str #sicm/bigint 10))))
    (let [one-e-40 (apply str "1" (repeat 40 "0"))]
      (is (= #?(:clj (bigint 1e40)
                :cljs (list 'sicmutils.util/bigint one-e-40))
             (read-string {:readers {'sicm/bigint u/parse-bigint}}
                          (pr-str #sicm/bigint one-e-40)))
          "Parsing #sicm/bigint works with big strings too."))))

(deftest vector-value-impl
  (testing "zero?"
    (is (v/zero? []))
    (is (v/zero? [0 0]))
    (is (not (v/zero? [1 2 3]))))

  (testing "zero-like"
    (is (= [0 0 0] (v/zero-like [1 2 3])))
    (is (= [] (v/zero-like [])))
    (is (= [0 [0 0] [0 0]] (v/zero-like [1 [2 3] [4 5]])))
    (is (= [(u/long 0) (u/int 0) 0]
           (v/zero-like [(u/long 1) (u/int 2) 3]))))

  (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
               (v/one-like [1 2 3])))

  (testing "exact?"
    (is (v/exact? [1 2 3 4]))
    (is (not (v/exact? [1.2 3 4])))
    (is (v/exact? [0 1 #sicm/ratio 3/2]))
    (is (not (v/exact? [0 0 0.00001]))))

  (testing "freeze"
    (is (= [1 2 3] (v/freeze [1 2 3]))))

  (testing "kind"
    (is (= PersistentVector (v/kind [1 2])))))

(deftest numeric-value-protocol-tests
  (checking "*-like properly coerce" 100
            [n sg/number]
            (is (v/zero? (v/zero-like n)))
            (is (not (v/zero? (v/one-like n))))

            (is (v/one? (v/one-like n)))
            (is (not (v/one? (v/zero-like n))))

            (is (v/identity? (v/identity-like n))))

  (testing "zero-like sticks with precision"
    (is (= 0 (v/zero-like 2)))
    (is (= 0.0 (v/zero-like 3.14))))

  (testing "one-like sticks with precision"
    (is (= 1 (v/one-like 1)))
    (is (= 1.0 (v/one-like 1.2))))

  (checking "on non-rational reals, v/freeze is identity" 100
            [n (gen/one-of [sg/any-integral (sg/reasonable-double)])]
            (is (= n (v/freeze n))))

  (checking "all numbers are numerical" 100
            [n sg/number]
            (is (v/numerical? n)))

  (is (v/numerical? 'x)
      "Symbols are abstract numerical things.")

  (is (isa? (v/kind 10) ::v/real))
  (is (v/exact? 10))
  (is (not (v/exact? 10.1))))

(deftest numeric-comparison-tests
  (checking "v/compare matches <, >, = behavior for reals" 1000
            [l sg/real, r sg/real]
            (let [compare-bit (v/compare l r)]
              (cond (< l r) (is (neg? compare-bit))
                    (> l r) (is (pos? compare-bit))
                    :else   (is (zero? compare-bit))))))

(deftest zero-tests
  (is (v/zero? 0))
  (is (v/zero? 0.0))
  (is (not (v/zero? 1)))
  (is (not (v/zero? 0.1))))

(deftest one-tests
  (is (v/one? 1))
  (is (v/one? 1.0))
  (is (not (v/one? 0)))
  (is (not (v/one? 0.0))))

(deftest kinds
  (is (= #?(:clj Long :cljs ::v/native-integral) (v/kind 1)))
  (is (= #?(:clj Double :cljs ::v/native-integral) (v/kind 1.0)))
  (is (= PersistentVector (v/kind [1 2]))))

(deftest exactness
  (is (v/exact? 1))
  (is (v/exact? 4N))
  (is (not (v/exact? 1.1)))
  (is (not (v/exact? :a)))
  (is (not (v/exact? "a")))
  (is (v/exact? #sicm/ratio 3/2))
  (is (v/exact? (u/biginteger 111))))

(deftest argument-kinds
  (let [L #?(:clj Long :cljs ::v/native-integral)
        V PersistentVector]
    (is (= [L] (v/argument-kind 1)))
    (is (= [L L L] (v/argument-kind 1 2 3)))
    (is (= [V] (v/argument-kind [2 3])))
    (is (= [V V] (v/argument-kind [1] [3 4]))))

  (checking "kind-predicate" 100 [l gen/any r gen/any]
            (let [l-kind  (v/kind l)
                  r-kind  (v/kind r)
                  l-kind? (v/kind-predicate l)
                  r-kind? (v/kind-predicate r)]
              ;; each item is its own kind.
              (is (l-kind? l))
              (is (r-kind? r))

              ;; they only respond true if they match kinds (or one inherits
              ;; from the other), false otherwise.
              (cond (= l-kind r-kind)
                    (do (is (l-kind? r))
                        (is (r-kind? l)))

                    (isa? l-kind r-kind)
                    (do (is (not (l-kind? r)))
                        (is (r-kind? l)))

                    (isa? r-kind l-kind)
                    (do (is (l-kind? r))
                        (is (not (r-kind? l))))

                    :else (do (is (not (l-kind? r)))
                              (is (not (r-kind? l))))))))
