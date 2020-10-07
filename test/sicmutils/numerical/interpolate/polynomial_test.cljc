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
            [same :refer [zeroish? ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.numerical.interpolate.polynomial :as ip]
            [sicmutils.generic :as g]
            [sicmutils.numsymb]))

(deftest symbolic-tests
  (letfn [(lagrange-incremental [points x]
            (let [n (count points)]
              (map (fn [i]
                     (ip/lagrange (take i points) x))
                   (range 1 (inc n)))))
          (diff [l r]
            (g/simplify (g/- l r)))]

    (testing "Neville and Lagrange interpolation are equivalent"
      (let [points [['x_1 'y_1] ['x_2 'y_2]]]
        (is (zeroish?
             (diff (ip/lagrange points 'x)
                   (ip/neville-top-down points 'x))))))

    (testing "points ordering doesn't matter for the final value. (Should test
    all permutations...)"
      (is (zeroish?
           (diff
            (ip/lagrange [['x_1 'y_1] ['x_2 'y_2] ['x_3 'y_3]] 'x)
            (ip/lagrange [['x_2 'y_2] ['x_1 'y_1] ['x_3 'y_3]] 'x))))

      (is (zeroish?
           (diff
            (ip/lagrange [['x_2 'y_2] ['x_1 'y_1] ['x_3 'y_3]] 'x)
            (ip/lagrange [['x_3 'y_3] ['x_2 'y_2] ['x_1 'y_1]] 'x)))))

    (testing "symbolic incremental methods should be identical to the full
  lagrange method at each point prefix."
      (let [points [['x_1 'y_1] ['x_2 'y_2] ['x_3 'y_3] ['x_4 'y_4]]
            diffs  (map diff
                        (lagrange-incremental points 'x)
                        (ip/neville-incremental* points 'x))]
        (is (ish? [0 0 0 0] diffs))))))

(deftest performance-tests
  (let [points [[0 1] [2 1] [5 2] [8 10]]
        expected [1 1.0 0.9359999999999999 1.0829333333333333]]
    (testing "each function returns a sequence of successive approximations. The
  approximation around 1.2 gets better the more points we add in."

      (is (ish? (last expected) (ip/lagrange points 1.2))
          "Lagrange only returns the final value.")

      (is (ish? (last expected) (ip/neville-top-down points 1.2))
          "Lagrange only returns the final value.")

      (is (ish? expected (ip/neville-incremental* points 1.2))
          "This is the initial, unabstracted version.")

      (is (ish? expected (ip/neville points 1.2))
          "incremental calculation via full Neville's algorithm")

      (is (ish? expected (ip/modified-neville points 1.2))
          "incremental calculation via modified Neville"))

    (testing "folding points in reverse should match column-wise processing."
      (is (ish? expected ((ip/neville-fold 1.2) (reverse points))))
      (is (ish? expected ((ip/modified-neville-fold 1.2) (reverse points)))))

    (testing "scan should process successive rows of the tableau; the diagonal
    of the tableau processed with a fold should match the first row of
    column-wise processing."
      (is (ish? expected (map last ((ip/neville-scan 1.2) points))))
      (is (ish? expected (map last ((ip/modified-neville-scan 1.2) points)))))

    (testing "the tableau processed with a fold should match the first row of
    column-wise processing."
      (is (ish? ((ip/neville-fold 1.2) points)
                (last ((ip/neville-scan 1.2) points))))

      (is (ish?  ((ip/modified-neville-fold 1.2) points)
                 (last ((ip/modified-neville-scan 1.2) points)))))))
