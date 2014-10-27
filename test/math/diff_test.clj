(ns math.diff-test
  (:require [clojure.test :refer :all]
            [math.diff :refer :all]
            [math.generic :as g]
            )
  (:import [math.diff Differential DifferentialTerm]))

(deftest diff-test-1
  (testing "add-dtl"
    (let [d (Differential. [(DifferentialTerm. (sorted-set 0) 'foo)
                              (DifferentialTerm. (sorted-set 1) 'bar)])
          d2 (Differential. [(DifferentialTerm. (sorted-set 0) (g/+ 'foo 'foo))
                             (DifferentialTerm. (sorted-set 1) (g/+ 'bar 'bar))])]
      (is (= d2 (add-differential d d)))
      )
    ))
