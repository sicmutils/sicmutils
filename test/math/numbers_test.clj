(ns math.numbers-test
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.numbers]))

(prn (add 4 'x))
(deftest arithmetic
  (testing "with-numbers"
    (is (= 4 (add 2 2)))
    (is (= 3.5 (add 1.5 2)))
    (is (= 13/40 (add 1/5 1/8)))
    (is (= 20 (mul 5 4))))
  (testing "with-symbols"
    (is (= '(math.generic/add 4 x) (add 4 'x)))
    (is (= '(math.generic/add 5 y) (add 'y 5)))
    ))

