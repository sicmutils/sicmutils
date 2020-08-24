;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.ratio-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.generic :as g]
            [sicmutils.generic-test :as gt]
            [sicmutils.value :as v]
            [sicmutils.numbers :as n]))

(deftest ratio-value-implementation
  ;; TODO, test freeze!
  ;;
  ;; TODO exact-divide, quot, rem, all of the ops for the actual strangeness
  ;; with these types.

  ;; TODO fix tests changed by freeze.

  ;; TODO ticket to fix equality, interop with complex numbers.
  )

(deftest ratio-generics
  (testing "rational generics"
    (gt/integral-tests r/rationalize)
    (gt/integral-a->b-tests r/rationalize identity)
    (gt/floating-point-tests
     r/rationalize :eq #(= (r/rationalize %1)
                           (r/rationalize %2))))

  (testing "ratio-operations"
    (is (= #sicm/ratio 13/40
           (g/add #sicm/ratio 1/5
                  #sicm/ratio 1/8)))
    (is (= #sicm/ratio 1/8
           (g/sub #sicm/ratio 3/8
                  #sicm/ratio 1/4)))

    (is (= #sicm/ratio 5/4 (g/div 5 4)))
    (is (= 25 (g/exact-divide #sicm/ratio 10/2
                              #sicm/ratio 2/10)))
    (is (= 1 (g/exact-divide #sicm/ratio 2/10
                             #sicm/ratio 2/10)))
    (is (= #sicm/ratio 1/2 (g/div 1 2)))
    (is (= #sicm/ratio 1/4 (reduce g/div [1 2 2])))
    (is (= #sicm/ratio 1/8 (reduce g/div [1 2 2 2])))
    (is (= #sicm/ratio 1/8 (g/invert 8)))))

(deftest with-ratio-literals
  (is (= #sicm/ratio 13/40 (g/+ #sicm/ratio 1/5
                                #sicm/ratio 1/8)))
  (is (= #sicm/ratio 1/8 (g/- #sicm/ratio 3/8
                              #sicm/ratio 1/4)))
  (is (= #sicm/ratio 5/4 (g/divide 5 4)))
  (is (= #sicm/ratio 1/2 (g/divide 1 2)))
  (is (= #sicm/ratio 1/4 (g/divide 1 2 2)))
  (is (= #sicm/ratio 1/8 (g/divide 1 2 2 2))))
