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

(ns sicmutils.expression-test
  (:require [clojure.test :refer :all]
            [sicmutils.expression :refer :all]))

(deftest expressions
  (testing "variables-in"
    (is (= '#{a b c d x y * +} (variables-in '(+ x (* 3 y) [a [b 9 c] [3 4 5 d]]))))
    (is (= '#{x} (variables-in 'x))))
  #_(testing "weight"
      (is (= {:type :sicmutils.expression/numerical-expression
              :weight 3
              :expression '(+ a b)} (fmap identity (->expression '(+ a b))))))
  (testing "walk"
    (is (= 12 (walk-expression '(+ 3 4 x) {'x 5} {'+ +} )))
    (is (= 0 (walk-expression '(+ 3 (* 4 y) x) {'x 5 'y -2} {'* * '+ +})))
    (is (thrown? Exception
                 (walk-expression '(+ 3 (* 4 y) x) {'x 5 'y -2} {'+ +})))))

(deftest foo
  (testing "foo"
    (is (= 1 1))))
