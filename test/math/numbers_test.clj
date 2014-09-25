(ns math.numbers-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.numbers]))

(deftest arithmetic
  (testing "with-numbers"
    (is (= 4 (g/+ 2 2)))
    (is (= 3.5 (g/+ 1.5 2)))
    (is (= 13/40 (g/+ 1/5 1/8)))
    (is (= 1/8 (g/- 3/8 1/4)))
    (is (= 20 (g/mul 5 4)))
    (is (= 5 (g/div 20 4)))
    (is (= 5/4 (g/div 5 4)))
    (is (= 1/8 (g/div 8))))
  (testing "more-numbers"
    (is (= 10 (g/+ 1 2 3 4)))
    (is (= -14 (g/- 10 9 8 7)))
    (is (= 0 (g/+)))
    (is (= 3.14 (g/+ 3.14)))
    (is (= 2.14 (g/- 3.14 1))))
  (testing "with-symbols"
    (is (= '(math.generic/+ 4 x) (g/+ 4 'x)))
    (is (= '(math.generic/+ 5 y) (g/+ 'y 5)))
    (is (= '(math.generic/div 5 y) (g/div 5 'y)))
    (is (= '(math.generic/div x y) (g/div 'x 'y)))
    )
  (testing "zero/one elimination"
    (is (= 'x (g/+ 0 'x)))
    (is (= 'x (g/mul 1 'x)))
    (is (= '(math.generic/- x) (g/- 0 'x)))
    (is (= 'x (g/+ 'x 0)))
    (is (= 'x (g/mul 'x 1)))
    (is (= 'x (g/- 'x 0)))
    (is (= 'x (g/+ 0.0 'x)))
    (is (= 'x (g/mul 1.0 'x)))
    (is (= 'x (g/+ 'x 0.0)))
    (is (= 'x (g/mul 'x 1.0)))
    (is (= 'x (g/div 'x 1.0)))
    (is (= 'x (g/div 'x 1)))
    (is (= 0 (g/div 0 'x)))
    (is (thrown? IllegalArgumentException (g/div 'x 0)))
    )
  (testing "neg"
    (is (= -4 (g/- 0 4)))
    (is (= '(math.generic/- x) (g/- 0 'x)))
    (is (= -4 (g/- 4)))
    (is (= -4.2 (g/- 4.2)))
    ))

