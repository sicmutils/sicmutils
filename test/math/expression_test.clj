(ns math.expression-test
  (:require [clojure.test :refer :all]
            [math.expression :refer :all]))

(deftest expressions
  (testing "variables-in"
    (is (= '#{a b c d x y * +} (variables-in (make '(+ x (* 3 y) [a [b 9 c] [3 4 5 d]])))))
    )
  (testing "walk"
    (is (= 12 (walk-expression {'+ + 'x 5} (make '(+ 3 4 x)))))
    (is (= 0 (walk-expression {'* * '+ + 'x 5 'y -2} (make '(+ 3 (* 4 y) x)))))
    (is (thrown? IllegalArgumentException
                 (walk-expression {'+ + 'x 5 'y -2} (make '(+ 3 (* 4 y) x)))))
    )
  (testing "print"
    (is (= 3 (print-expression 3)))
    (is (= :x (print-expression :x))))
 )

(deftest foo
  (testing "foo"
    (is (= 1 1))))
