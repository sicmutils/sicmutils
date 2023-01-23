#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.rational-function.interpolate-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [emmy.rational-function.interpolate :as ri]))

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
