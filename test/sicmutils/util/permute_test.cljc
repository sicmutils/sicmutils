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

(ns sicmutils.util.permute-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.generic :as g]
            [sicmutils.util.permute :as p]))

(deftest misc-tests
  (testing "factorial"
    (is (= (apply g/* (range 1 8))
           (p/factorial 7)))))

(deftest permutation-test
  (testing "permutation-sequence"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (p/permutation-sequence 0)))

    (is (= '[[a]]
           (p/permutation-sequence '[a])))

    (is (= '[[a b] [b a]]
           (p/permutation-sequence '(a b))))

    (is (= [[0 1 2]
            [0 2 1]
            [2 0 1]
            [2 1 0]
            [1 2 0]
            [1 0 2]]
           (p/permutation-sequence [0 1 2])))

    (is (= [[[0 1 2] 1]
            [[0 2 1] -1]
            [[2 0 1] 1]
            [[2 1 0] -1]
            [[1 2 0] 1]
            [[1 0 2] -1]]
           (map vector
                (p/permutation-sequence (range 3))
                (cycle [1 -1]))))

    (is (= [[0 1 2 3] [0 1 3 2] [0 3 1 2] [3 0 1 2]
            [3 0 2 1] [0 3 2 1] [0 2 3 1] [0 2 1 3]
            [2 0 1 3] [2 0 3 1] [2 3 0 1] [3 2 0 1]
            [3 2 1 0] [2 3 1 0] [2 1 3 0] [2 1 0 3]
            [1 2 0 3] [1 2 3 0] [1 3 2 0] [3 1 2 0]
            [3 1 0 2] [1 3 0 2] [1 0 3 2] [1 0 2 3]]
           (p/permutation-sequence (range 4))))))
