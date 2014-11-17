(ns math.modint-test
  (:require [clojure.test :refer :all]
            [math.value :as v]
            [math.generic :as g]
            [math.modint :refer :all]
            ))

(deftest modint
  (let [m3_7 (make 3 7)
        m0_7 (make 0 7)
        m5_7 (make 5 7)
        m4_7 (make 4 7)
        m12_7 (make 12 7)
        m5_7b (make 5 7)
        m2_7 (make 2 7)
        m5_13 (make 5 13)
        m2_4 (make 2 4)
        m3_4 (make 3 4)]
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
   (testing "zero?"
     (is (v/zero? m0_7)))
   (testing "inv"
     (is (= m3_7 (g/invert m5_7)))
     (is (= m5_7 (g/invert m3_7)))
     (is (= m4_7 (g/invert m2_7)))
     (is (= m2_7 (g/invert m4_7)))
     (is (thrown? ArithmeticException (g/invert m2_4)))
     (is (= m3_4 (g/invert m3_4)))
     )
   ))
