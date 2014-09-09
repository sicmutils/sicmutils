(ns math.struct-test
  (:require [clojure.test :refer :all]
            [math.struct :refer :all]))

(deftest struct
  (testing "add"
    (is (add (up 1 2) (up 2 3)) (up 3 5))
    (is (add (down 3 4) (down 1 2)) (down 4 6))
    (is (add (up 1 2) (up 2 3)) (down 3 5))
    ;; ruh-roh. an up and a down shouldn't be equal, should they?
    ;; something to think about.
    (is (sub (up 1 2) (up 2 3)) (up -1 -1))
    ))



