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

(ns math.rational-function-test
  (:require [clojure.test :refer :all]
            [math.rational-function :refer :all]
            [math.generic :as g]
            [math.value :as v]
            [math.polynomial :as p]
            [math.numbers]
            [math.simplify]))

(deftest make-test
  (let [R (make (p/make [2]) (p/make [3]))
        S (make (p/make [4]) (p/make [2]))
        x+1 (p/make [1 1])
        x-1 (p/make [-1 1])
        x+1:x-1 (make x+1 x-1)
        x-1:x+1 (make x-1 x+1)
        one (make (p/make [1]) (p/make [1]))]
    (is (= one (make x+1 x+1)))
    (is (= one (mul x+1:x-1 x-1:x+1)))
    (is (= one (mul x-1:x+1 x+1:x-1)))
    (is (= x+1:x-1 (invert x-1:x+1)))
    (is (= one (mul x-1:x+1 (invert x-1:x+1))))
    (is (= (make (p/make [2 0 2]) (p/make [-1 0 1])) (add x-1:x+1 x+1:x-1)))
    (is (= (make (p/make [2 0 2]) (p/make [-1 0 1])) (add x+1:x-1 x-1:x+1)))))
