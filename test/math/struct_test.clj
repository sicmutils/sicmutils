(ns math.struct-test
  (:require [clojure.test :refer :all]
            [math.struct :refer :all]
            [math.generic :as g]))

(deftest structures
  (testing "s+t"
    (is (= (g/+ (up 1 2) (up 2 3)) (up 3 5)))
    (is (= (g/+ (down 3 4) (down 1 2)) (down 4 6)))
    (is (= (down (g/+ 4 'u) (g/+ 2 'v)) (g/+ (down 'u 2) (down 4 'v))))
    (is (= (g/+ (up 1 2) (up 2 3)) (down 3 5))))
  ;; ruh-roh. an up and a down shouldn't be equal, should they?
  ;; something to think about.)
  (testing "s-t"
      (is (= (g/- (up 1 2) (up 2 3)) (up -1 -1)))
      (is (= (g/- (down 8 5) (down 4 -1)) (down 4 6))))
  (testing "a*s"
    (is (= (up 2 4 6) (g/mul 2 (up 1 2 3))))
    (is (= (down 3 6 9) (g/mul 3 (down 1 2 3)))))
  (testing "s/a"
    (is (= (up 1 2 -3) (g/div (up 2 4 -6) 2))))
  (testing "neg"
    (is (= (up -1 2 -3) (g/- (up 1 -2 3))))
    (is (= (up -1 2 -3) (g/neg (up 1 -2 3)))))
  (testing "a*s with literals"
    (is (= (up 2 (g/mul 2 't) 6) (g/mul 2 (up 1 't 3))))
    (is (= (down (g/mul 3 'x_0) (g/mul 3 'x_1)) (g/mul 3 (down 'x_0 'x_1))))))


