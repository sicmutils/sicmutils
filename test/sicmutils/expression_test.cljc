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
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.expression :as e]))

(deftest expressions
  (testing "variables-in"
    (is (= '#{a b c d x y * +} (e/variables-in '(+ x (* 3 y) [a [b 9 c] [3 4 5 d]]))))
    (is (= '#{x} (e/variables-in 'x)))
    )
  (testing "walk"
    (is (= 12 (e/walk-expression '(+ 3 4 x) {'x 5} {'+ +})))
    (is (= 0 (e/walk-expression '(+ 3 (* 4 y) x) {'x 5 'y -2} {'* * '+ +})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (e/walk-expression '(+ 3 (* 4 y) x) {'x 5 'y -2} {'+ +})))))

(deftest is-expression
  (is (e/expression?
       (->> (e/literal-number '(* 4 3)))))
  (is (not (e/expression? "face"))))
