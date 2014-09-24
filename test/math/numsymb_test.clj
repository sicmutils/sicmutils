(ns math.numsymb-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.numbers :as n]
            [math.numsymb :as ns]))


(deftest numsymb-test
  (testing "with vars"
    (is (= `(g/add 15 ~'x) (g/g:+ 10 3 2 'x)))
    (is (= `(g/add 2 3 10 ~'x) (g/g:+ 10 'x 3 2)))
    (is (= `(g/add 1 2 3 10 ~'x) (g/g:+ 10 'x 3 2 1)))
    (is (= `(g/add 1 2 3 30 ~'x) (g/g:+ 10 20 'x 3 2 1)))
    ))
