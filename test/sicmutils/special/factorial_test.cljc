;;
;; Copyright © 2022 Sam Ritchie.
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

(ns sicmutils.special.factorial-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.special.factorial :as sf]))

(deftest factorial-tests
  (testing "factorial"
    (is (= (apply g/* (range 1 8))
           (sf/factorial 7)))

    (is (= #sicm/bigint "15511210043330985984000000"
           (sf/factorial 25))
        "factorial can handle `n` that triggers overflow in cljs and clj."))

  (testing "falling-factorial"
    (is (g/infinite?
         (sf/falling-factorial -10 -10)))
    (is (= -1320 (sf/falling-factorial -10 3)))
    (is (= #sicm/ratio -1/504 (sf/falling-factorial -10 -3)))
    (is (= #sicm/ratio 1/1716 (sf/falling-factorial 10 -3))))

  (testing "double-factorial"
    ;; confirmed via wolfram
    (is (= 1 (sf/double-factorial 0)))
    (is (= 1 (sf/double-factorial -1)))
    (is (= -1 (sf/double-factorial -3)))
    (is (= #sicm/ratio 1/3 (sf/double-factorial -5)))
    (is (= #sicm/ratio -1/15 (sf/double-factorial -7)))
    (is (= #sicm/ratio 1/105 (sf/double-factorial -9))))

  (testing "subfactorial"
    (is (= 1 (sf/subfactorial 0)))
    (is (= 0 (sf/subfactorial 1)))
    (is (= 1 (sf/subfactorial 2)))
    (is (= 2 (sf/subfactorial 3)))
    (is (= 9 (sf/subfactorial 4)))
    (is (= 44 (sf/subfactorial 5)))
    (is (= 265 (sf/subfactorial 6)))))
