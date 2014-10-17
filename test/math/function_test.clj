(ns math.function-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.function :refer :all]
            ))

(deftest function-basic
  (let [f (literal-function 'F)]
    (testing "a"
      (is (= '(F x) (f 'x)))
      (is (= '(F 7) (f (g/+ 3 4))))
      )
    )
  )
