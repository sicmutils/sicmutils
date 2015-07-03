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

(ns net.littleredcomputer.math.value-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math.value :refer :all]))

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
  (is (= [:at-least 3] (joint-arity [[:at-least 3] [:at-least 2]]))))
