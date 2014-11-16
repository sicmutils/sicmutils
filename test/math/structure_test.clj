(ns math.structure-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.structure :refer :all]
            [math.generic :refer :all]))

(deftest structures
  (testing "s+t"
    (is (= (+ (up 1 2) (up 2 3)) (up 3 5)))
    (is (= (+ (down 3 4) (down 1 2)) (down 4 6)))
    (is (= (down (+ 4 'u) (+ 2 'v)) (+ (down 'u 2) (down 4 'v))))
    )
  (testing "s-t"
      (is (= (- (up 1 2) (up 2 3)) (up -1 -1)))
      (is (= (- (down 8 5) (down 4 -1)) (down 4 6)))
      (is (= (- (down 8 5)) (down -8 -5)))
      (is (= (- (up 10 10) (up 2 3) (up 3 4)) (up 5 3)))
      )
  (testing "s +/- t mixed"
    (is (= (+ (up (down 1 2) (down 3 4))
                (up (down 2 3) (down -7 2)))
           (up (down 3 5) (down -4 6))))
    (is (= (- (up (down 1 2) (down 3 4))
                (up (down 2 3) (down -7 2)))
           (up (down -1 -1) (down 10 2))))
    (is (= (+ (down (up 1 2) (up 3 4))
                (down (up 2 3) (up -7 2)))
           (down (up 3 5) (up -4 6))))
    (is (= (- (down (up 1 2) (up 3 4))
                (down (up 2 3) (up -7 2)))
           (down (up -1 -1) (up 10 2))))
    )
  (testing "a*s"
    (is (= (up 2 4 6) (* 2 [1 2 3])))
    (is (= (down 3 6 9) (* 3 (down 1 2 3))))
    (is (= (down 12 24 36) (* 3 4 (down 1 2 3))))
    )
  (testing "s/a"
    (is (= (up 1 2 -3) (/ (up 2 4 -6) 2))))
  (testing "neg"
    (is (= (up -1 2 -3) (- (up 1 -2 3))))
    (is (= (up -1 2 -3) (negate (up 1 -2 3))))
    )
  (testing "a*s with literals"
    (is (= (up 2 (* 2 't) 6) (* 2 (up 1 't 3))))
    (is (= (down (* 3 'x_0) (* 3 'x_1)) (* 3 (down 'x_0 'x_1))))
    )
  (testing "s*t outer simple"
    (is (= (up (up 3 6) (up 4 8))
           (* (up 1 2) (up 3 4))))
    (is (= (down (down 3 6) (down 4 8))
           (* (down 1 2) (down 3 4))))
    (is (= (down (up 3 6) (up 4 8) (up 5 10))
           (* (up 1 2) (down 3 4 5))))
    )
  (testing "s*t inner simple"
    (is (= 11 (* (up 1 2) (down 3 4))))
    (is (= 22 (* (down 2 3) (up 5 4))))
    )
  (testing "s*t inner with vars"
    (is (= (+ 'y (* 'x 4)) (* (up 1 'x) (down 'y 4))))
    )
  (testing "examples from refman"
    (is (= 652 (* (up (up 2 3) (down 5 7 11))
                    (down (down 13 17) (up 19 23 29)))))
    (is (= (up (up 10 15) (up 14 21) (up 22 33)) (* (up 2 3) (up 5 7 11))))
    (is (= (up (up 10 14 22) (up 15 21 33)) (* (up 5 7 11) (up 2 3)))))
  (testing "zero?"
    (is (zero? (up)))
    (is (zero? (up 0)))
    (is (zero? (up 0 0)))
    (is (zero? (down)))
    (is (zero? (down 0)))
    (is (zero? (down 0 0)))
    (is (zero? []))
    (is (zero? [0]))
    (is (zero? [0 0]))
    )
  (testing "zero-like"
    (is (= (up 0 0 0) (zero-like (up 1 2 3))))
    (is (= (up) (zero-like (up))))
    (is (= (down 0 0 0) (zero-like (down 1 2 3))))
    (is (= (down) (zero-like (down))))
    )
  (testing "exact?"
    (is (exact? (up 0 1 3/2)))
    (is (not (exact? (up 0 0 0.00001))))
    )
  (testing "function - rotate about x axis"
    (defn Rx [θ]
      (fn [[x y z]]
        (let [c (cos θ) s (sin θ)]
          (up x
              (- (* c y) (* s z))
              (+ (* s y) (* c z))))))
    (is (= (up 0 0 1) ((Rx 'pi-over-2) (up 0 1 0))))
    (is (= (up 'x (math.generic/- 'z) 'y) ((Rx 'pi-over-2) (up 'x 'y 'z))))
    )
  (testing "square/cube"
    (is (= 14 (square (up 1 2 3))))
    (is (= (up (up (up 1 2 3) (up 2 4 6) (up 3 6 9))
               (up (up 2 4 6) (up 4 8 12) (up 6 12 18))
               (up (up 3 6 9) (up 6 12 18) (up 9 18 27))) (cube (up 1 2 3)))))
  (testing "matrix-like"
    (let [M (down (up 'a 'c) (up 'b 'd))
          x (up 'x 'y)
          xt (down 'x 'y)]
      (is (= (up (+ (* 'x 'a) (* 'y 'b))
                 (+ (* 'x 'c) (* 'y 'd))) (* M x)))
      (is (= (down (+ (* 'x 'a) (* 'y 'c))
                 (+ (* 'x 'b) (* 'y 'd))) (* xt M)))
      (is (= (+ (* 'x (+ (* 'x 'a) (* 'y 'c)))
                (* 'y (+ (* 'x 'b) (* 'y 'd)))) (* xt M x))))
    (let [M (up (down 'a 'b) (down 'c 'd))
          x (down 'x 'y)
          xt (up 'x 'y)]
      (is (= (down (+ (* 'x 'a) (* 'y 'c))
                   (+ (* 'x 'b) (* 'y 'd))) (* M x)))
      (is (= (down (+ (* 'x 'a) (* 'y 'c))
                   (+ (* 'x 'b) (* 'y 'd))) (* x M))))
    (let [M (up (down 'a 'c) (down 'b 'd))]
      (is (= (up (down (+ (* 'a 'a) (* 'c 'b))
                       (+ (* 'a 'c) (* 'c 'd)))
                 (down (+ (* 'b 'a) (* 'd 'b))
                       (+ (* 'b 'c) (* 'd 'd)))) (* M M))))
    )
  (testing "fibonacci-matrix"
    (let [n 20
          fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))
          fib (fn [i] (nth fibs i))
          M (down (up 1 1) (up 1 0))]
      (is (= (fib n) (-> (expt M n) first second)))))
  (testing "expt"
    (is (= (up
            (up
             (up (up 1 2) (up 2 4))
             (up (up 2 4) (up 4 8)))
            (up
             (up (up 2 4) (up 4 8))
             (up (up 4 8) (up 8 16))))
           (expt (up 1 2) 4)))
    (is (= (* (up 1 2) (up 1 2) (up 1 2) (up 1 2))
           (expt (up 1 2) 4))))
  (testing "mapr"
    (let [S0 (up 2)
          S1 (up 2 3)
          S2 (down (up 1 2) (up 3 4))
          S3 (up (down 1 2) (down 3 4))]
      (is (= (up 4) (mapr square S0)))
      (is (= (up 4 9) (mapr square S1)))
      (is (= (down (up 1 4) (up 9 16)) (mapr square S2)))
      (is (= (up (down 1 4) (down 9 16)) (mapr square S3)))
      ))
  )
