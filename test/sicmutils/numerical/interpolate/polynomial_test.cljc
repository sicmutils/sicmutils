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

(ns sicmutils.numerical.interpolate.polynomial-test
  "Tests of the various sequence convergence and generation utilities in the SICM
  library."
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.numerical.interpolate.polynomial :as ip]
            [sicmutils.generic :as g]
            [sicmutils.numsymb]))

(deftest equivalent-tests
  (testing "Neville and Lagrange interpolation are equivalent"
    (let [points [['x_1 'y_1] ['x_2 'y_2]]]
      (is (zero?
           (g/simplify
            (g/- (ip/lagrange points 'x)
                 (ip/neville points 'x))))))))

(deftest polynomial-tests
  (testing "the methods match."
    (let [points   [[0 1] [2 1] [5 2] [8 10]]
          expected 1.0829333333333333]
      (is (ish? expected (ip/neville points 1.2)))
      (is (ish? expected (ip/lagrange points 1.2)))))

  ;; TODO test that all permutations of points give the same result.
  )

(deftest neville-reductions-tests
  (testing "this function returns a sequence of successive approximations. The
  approximation around 1.2 gets better the more points we add in."
    (let [points   [[0 1] [2 1] [5 2] [8 10]]
          expected [1 1.0 0.9359999999999999 1.0829333333333333]]
      (is (ish? expected (ip/neville-reductions* points 1.2)))
      (is (ish? expected (ip/neville-reductions points 1.2)))
      (is (ish? expected (ip/incremental-by-cl points 1.2)))
      (is (ish? expected ((ip/poly-scan 1.2) points))))))
