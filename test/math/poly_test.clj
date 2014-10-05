(ns math.poly-test
  (:require [clojure.test :refer :all]
            [math.poly :refer :all]))

(deftest poly-core
  (testing "degree"
    (is (= (degree (make)) 0))
    (is (= (degree (make [1 1] [0 -1])) 1))
    (is (= (degree (make [1 1])) 1))
    (is (= (degree (make [2 1] [0 -1])) 2))
    (is (= (degree (make [2 0] [1 2] [0 -1])))))
  (testing "add"
    (is (= (make) (add (make [2 2]) (make [2 -2]))))
    (is (= (make [1 1] [0 -1]) (add (make [1 1]) (make [0 -1]))))
    (is (= (make) (sub (make [2 2]) (make [2 2])))))
  (testing "mul"
    (is (= (make [2 1] [0 -1]) (mul (make [1 1] [0 1]) (make [1 1] [0 -1])))))
 )
