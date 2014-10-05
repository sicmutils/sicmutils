(ns math.modint-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.modint :refer :all]
            ))

(deftest foo
  (let [m3_7 (make 3 7)
        m5_7 (make 5 7)
        m12_7 (make 12 7)
        m5_7b (make 5 7)
        m2_7 (make 2 7)
        m5_13 (make 5 13)]
    (testing "easy"
      (is (= m5_7 m5_7))
      (is (= m12_7 m5_7))
      (is (= m5_7b m5_7))
      (is (not= m2_7 m5_7))
      (is (not= m5_13 m5_7))
      )
    (testing "add"
      (is (= m3_7 (g/+ m5_7 m5_7)))
      )
    (testing "sub"
      (is (= m2_7 (g/- m5_7 m3_7)))
      )
    (testing "neg"
      (is (= m2_7 (g/negate m5_7))))
    )
  )
