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
  (:require [clojure.test :refer :all]
            [sicmutils.value :refer :all])
  (:import (clojure.lang PersistentVector)))

(deftest arities
  (is (= [:exactly 0] (arity (fn [] 42))))
  (is (= [:exactly 1] (arity (fn [x] (+ x 1)))))
  (is (= [:exactly 2] (arity (fn [x y] (+ x y)))))
  (is (= [:exactly 3] (arity (fn [x y z] (* x y z)))))
  (is (= [:at-least 0] (arity (fn [& xs] (reduce + 0 xs)))))
  (is (= [:at-least 1] (arity (fn [x & xs] (+ x (reduce * 1 xs))))))
  (is (= [:at-least 2] (arity (fn [x y & zs] (+ x y (reduce * 1 zs))))))
  (is (= [:exactly 0] (arity 'x)))
  (is (= [:at-least 0] (arity (constantly 42))))
  ;; the following is dubious until we attach arity metadata to MultiFns
  (is (= [:exactly 1] (arity [1 2 3])))
  (let [f (fn [x] (+ x x))
        g (fn [y] (* y y))]
    (is (= [:exactly 1] (arity (comp f g))))))

(deftest nullity
  (is (nullity? 0))
  (is (nullity? 0.0))
  (is (not (nullity? 1)))
  (is (not (nullity? 0.1))))

(deftest unity
  (is (unity? 1))
  (is (unity? 1.0))
  (is (not (unity? 0)))
  (is (not (unity? 0.0))))

(deftest kinds
  (is (= Long (kind 1)))
  (is (= Double (kind 1.0)))
  (is (= PersistentVector (kind [1 2]))))

(deftest exactness
  (is (exact? 1))
  (is (exact? 3/2))
  (is (exact? 4N))
  (is (exact? (BigInteger/valueOf 111)))
  (is (not (exact? 1.1)))
  (is (not (exact? 'a)))
  (is (not (exact? :a)))
  (is (not (exact? "a"))))

(deftest argument-kinds
  (let [L Long
        V PersistentVector]
    (is (= [L] (argument-kind 1)))
    (is (= [L L L] (argument-kind 1 2 3)))
    (is (= [V] (argument-kind [2 3])))
    (is (= [V V] (argument-kind [1] [3 4])))))

(deftest joint-arities
  (is (= [:exactly 1] (joint-arity [[:exactly 1] [:exactly 1]])))
  (is (= [:exactly 5] (joint-arity [[:exactly 5] [:exactly 5]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:exactly 2] [:exactly 1]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:exactly 1] [:exactly 2]])))
  (is (= [:exactly 3] (joint-arity [[:exactly 3] [:at-least 2]])))
  (is (= [:exactly 3] (joint-arity [[:exactly 3] [:at-least 3]])))
  (is (= [:exactly 3] (joint-arity [[:at-least 1] [:exactly 3]])))
  (is (= [:exactly 3] (joint-arity [[:at-least 3] [:exactly 3]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:exactly 1] [:at-least 2]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:at-least 2] [:exactly 1]])))
  (is (= [:at-least 3] (joint-arity [[:at-least 2] [:at-least 3]])))
  (is (= [:at-least 3] (joint-arity [[:at-least 3] [:at-least 2]])))
  (is (= [:between 2 3] (joint-arity [[:between 1 3] [:between 2 5]])))
  (is (= [:between 2 3] (joint-arity [[:between 2 5] [:between 1 3]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:between 1 3] [:between 4 6]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:between 4 6] [:between 1 3]])))
  (is (= [:exactly 3] (joint-arity [[:between 1 3] [:between 3 4]])))
  (is (= [:exactly 3] (joint-arity [[:between 3 4] [:between 1 3]])))
  (is (= [:between 2 4] (joint-arity [[:at-least 2] [:between 1 4]])))
  (is (= [:between 2 4] (joint-arity [[:between 1 4] [:at-least 2]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:at-least 4] [:between 1 3]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:between 1 3] [:at-least 4]])))
  (is (= [:exactly 2] (joint-arity [[:exactly 2] [:between 2 3]])))
  (is (= [:exactly 2] (joint-arity [[:between 2 3] [:exactly 2]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:between 2 3] [:exactly 1]])))
  (is (thrown? IllegalArgumentException (joint-arity [[:exactly 1] [:between 2 3]]))))
