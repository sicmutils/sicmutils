;;
;; Copyright © 2020 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.abstract.number-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.abstract.number :as an]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(deftest abstract-number-tests
  (testing "literal-number"
    (is (= '(+ 1 (cos 12))
           (v/freeze
            (g/+ 1 (g/cos (an/literal-number 12)))))
        "When literal-number wraps an actual number, it attempts to keep the
        result exact instead of evaluating the fns.")

    (is (=  (+ 1 (Math/cos 12))
            (g/+ 1 (g/cos 12)))
        "You get a floating-point inexact result by calling generic fns on a
    number directly, by comparison.")

    (is (= (g/+ 1 (g/cos 12.2))
           (x/expression-of
            (g/+ 1 (g/cos (an/literal-number 12.2)))))
        "Passing an already-inexact number forces evaluation.")))
