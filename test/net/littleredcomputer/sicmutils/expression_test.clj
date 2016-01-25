;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns net.littleredcomputer.sicmutils.expression-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.sicmutils.expression :refer :all]))

(deftest expressions
  (testing "variables-in"
    (is (= '#{a b c d x y * +} (variables-in '(+ x (* 3 y) [a [b 9 c] [3 4 5 d]]))))
    (is (= '#{x} (variables-in 'x)))
    )
  (testing "walk"
    (is (= 12 ((walk-expression {'+ + 'x 5}) '(+ 3 4 x))))
    (is (= 0 ((walk-expression {'* * '+ + 'x 5 'y -2}) '(+ 3 (* 4 y) x))))
    (is (thrown? IllegalArgumentException
                 ((walk-expression {'+ + 'x 5 'y -2}) '(+ 3 (* 4 y) x))))))

(deftest foo
  (testing "foo"
    (is (= 1 1))))
