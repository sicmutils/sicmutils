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
      (is (= (g/- (down 8 5) (down 4 -1)) (down 4 6)))
      (is (= (g/- (down 8 5)) (down -8 -5)))
      (is (= (g/- (up 10 10) (up 2 3) (up 3 4)) (up 5 3)))
      )
  (testing "s +/- t mixed"
    (is (= (g/+ (up (down 1 2) (down 3 4))
                (up (down 2 3) (down -7 2)))
           (up (down 3 5) (down -4 6))))
    (is (= (g/- (up (down 1 2) (down 3 4))
                (up (down 2 3) (down -7 2)))
           (up (down -1 -1) (down 10 2))))
    (is (= (g/+ (down (up 1 2) (up 3 4))
                (down (up 2 3) (up -7 2)))
           (down (up 3 5) (up -4 6))))
    (is (= (g/- (down (up 1 2) (up 3 4))
                (down (up 2 3) (up -7 2)))
           (down (up -1 -1) (up 10 2))))
    )
  (testing "a*s"
    (is (= [2 4 6] (g/* 2 [1 2 3])))
    (is (= (down 3 6 9) (g/* 3 (down 1 2 3))))
    (is (= (down 12 24 36) (g/* 3 4 (down 1 2 3))))
    )
  (testing "s/a"
    (is (= (up 1 2 -3) (g// (up 2 4 -6) 2))))
  (testing "neg"
    (is (= [-1 2 -3] (g/- (up 1 -2 3))))
    (is (= (up -1 2 -3) (g/negate (up 1 -2 3))))
    )
  (testing "a*s with literals"
    (is (= (up 2 (g/* 2 't) 6) (g/* 2 (up 1 't 3))))
    (is (= (down (g/* 3 'x_0) (g/* 3 'x_1)) (g/* 3 (down 'x_0 'x_1))))
    )
  (testing "s*t outer simple"
    (is (= (up [3 6] [4 8])
           (g/* (up 1 2) (up 3 4))))
    (is (= (down (down 3 6) (down 4 8))
           (g/* (down 1 2) (down 3 4))))
    (is (= (down [3 6] [4 8] [5 10])
           (g/* (up 1 2) (down 3 4 5))))
    )
  (testing "s*t inner simple"
    (is (= 11 (g/* (up 1 2) (down 3 4))))
    (is (= 22 (g/* (down 2 3) (up 5 4))))
    )
  (testing "s*t inner with vars"
    (is (= (g/+ 'y (g/* 'x 4)) (g/* (up 1 'x) (down 'y 4))))
    )
  (testing "examples from refman"
    (is (= 652 (g/* (up (up 2 3) (down 5 7 11))
                    (down (down 13 17) (up 19 23 29)))))
    (is (= [[10 15] [14 21] [22 33]] (g/* (up 2 3) (up 5 7 11))))
    (is (= [[10 14 22] [15 21 33]] (g/* (up 5 7 11) (up 2 3)))))
  (testing "zero?"
    (is (g/zero? (up)))
    (is (g/zero? (up 0)))
    (is (g/zero? (up 0 0)))
    (is (g/zero? (down)))
    (is (g/zero? (down 0)))
    (is (g/zero? (down 0 0)))
    (is (g/zero? []))
    (is (g/zero? [0]))
    (is (g/zero? [0 0]))
    )
  (testing "zero-like"
    (is (= (up 0 0 0) (g/zero-like (up 1 2 3))))
    (is (= [] (g/zero-like (up))))
    (is (= (down 0 0 0) (g/zero-like (down 1 2 3))))
    (is (= [] (g/zero-like (down))))
    )
  (testing "exact?"
    (is (g/exact? (up 0 0 0)))
    (is (not (g/exact? (up 0 0 0.00001))))
    )
  (testing "function"
    (defn Rx [angle]
      (fn [[x y z]]
        (let [ca (g/cos angle)
              sa (g/sin angle)]
          (up x
              (g/- (g/* ca y) (g/* sa z))
              (g/+ (g/* sa y) (g/* ca z))))))
    (is (= [0 0 1] ((Rx 'pi-over-2) [0 1 0])))
    (is (= '[x (math.generic/- z) y] ((Rx 'pi-over-2) (up 'x 'y 'z))))
    )
  )

