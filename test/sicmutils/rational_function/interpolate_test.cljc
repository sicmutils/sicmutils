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

(ns sicmutils.rational-function.interpolate-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.rational-function.interpolate :as ri]))

(deftest rational-interpolation-tests
  (let [points [[0 1] [2 1.4] [5 2] [8 10]]
        expected [1.0 1.206896551724138 1.24 1.191835569511847]]

    (testing "The algo can handle a zero denominator case, though I don't get
    what it means!"
      (is (ish? 1.126760563380282
                ((ri/modified-bulirsch-stoer-sum 1.2) [[0 1] [0 1] [0 2.5] [8 4]]))))

    (testing "each function returns a sequence of successive approximations. The
  approximation around 1.2 gets better the more points we add in."
      (is (ish? (last expected) (ri/bulirsch-stoer-recursive points 1.2))
          "the recursive version only gives the final.")

      (is (ish? expected (ri/bulirsch-stoer points 1.2))
          "the tableau-based version gives you everything.")

      (is (ish? expected (ri/modified-bulirsch-stoer points 1.2))
          "The incremental, modified version works the same way."))

    (testing "folding points should match the final estimate received through
              column-wise processing."
      (is (ish? (last expected) ((ri/modified-bulirsch-stoer-sum 1.2) points))))

    (testing "scan should process successive rows of the tableau; the diagonal
    of the tableau processed with a fold should match the first row of
    column-wise processing."
      (is (ish? expected ((ri/modified-bulirsch-stoer-scan 1.2) points))))

    (testing "both folds should get the correct value"
      (is (ish? (last expected) ((ri/bulirsch-stoer-sum 1.2) points)))
      (is (ish? (last expected) ((ri/modified-bulirsch-stoer-sum 1.2) points))))

    (testing "both scans should generate the correct sequence of values."
      (is (ish? expected ((ri/bulirsch-stoer-scan 1.2) points)))
      (is (ish? expected ((ri/modified-bulirsch-stoer-scan 1.2) points))))

    (testing "the tableau processed with a fold should match the first row of
    column-wise processing."
      (is (ish? ((ri/modified-bulirsch-stoer-sum 1.2) points)
                (last ((ri/modified-bulirsch-stoer-scan 1.2) points))))

      (is (ish?  ((ri/modified-bulirsch-stoer-sum 1.2) points)
                 (last ((ri/modified-bulirsch-stoer-scan 1.2) points)))))))
