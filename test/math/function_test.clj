(ns math.function-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.expression :as x]
            [math.function :refer :all]
            ))

(defmacro mx [x] `(x/make '~x))

(deftest function-basic
  (let [f (literal-function 'F)]
    (testing "a"
      (is (= (mx (F x)) (f 'x)))
      (is (= (mx (F 7)) (f (g/+ 3 4))))
      )
    )
  )
