(ns math.struct-test
  (:require [clojure.test :refer :all]
            [math.struct :refer :all]
            [math.numbers]
            [math.generic :as g]))

(deftest structures
  (testing "add"
    (is (= (g/add (up 1 2) (up 2 3)) (up 3 5)))
    (is (= (g/add (down 3 4) (down 1 2)) (down 4 6)))
    (is (= (g/add (up 1 2) (up 2 3)) (down 3 5)))
    ;; ruh-roh. an up and a down shouldn't be equal, should they?
    ;; something to think about.
    ;; (is (= (g/sub (up 1 2) (up 2 3)) (up -1 -1)))
    ;; (is (= (g/sub (down 8 5) (down 4 -1)) (down 4 6)))
    ))


