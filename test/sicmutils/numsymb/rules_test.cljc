;;
;; Copyright © 2021 Adam Haber.
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

(ns sicmutils.numsymb.rules_test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generators :as sg]
            [sicmutils.numsymb :as sym]
            [sicmutils.numsymb.rules :as sr]
            [sicmutils.util.aggregate :as ua]))

(deftest compare-implementations
  (testing "not"
    (doseq [v [true false 1 0 'a '() ()]]
      (is (= (sr/sym:not v) ((sym/symbolic-operator 'not) v)))))
  
  (testing "gcd"
    (doseq [v1 [-1 0 1 2 3 4 () nil true false 'a]
            v2 [-1 0 1 2 3 4 () nil true false 'a]]
      (is (= ((ua/monoid sr/sym:gcd 0) v1 v2) ((sym/symbolic-operator 'gcd) v1 v2)))))
  
  (testing "tan"
    (doseq [v [-0.1 0 0.1 (/ sym/pi 4) (/ (* 3 sym/pi) 4) 0/1 1/3 -1.2 () nil true false 'a]]
      (is (= (sr/tan v) ((sym/symbolic-operator 'tan) v)))))
)