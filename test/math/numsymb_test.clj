(ns math.numsymb-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.numbers :as n]
            [math.numsymb :as ns]))

(deftest numsymb-test
  (testing "+/- with vars"
    (is (= (g/+ 15 'x) (g/+ 10 3 2 'x)))
    (is (= (g/+ 15 'x) (g/+ 10 'x 3 2)))
    (is (= (g/+ 16 'x) (g/+ 10 'x 3 2 1)))
    (is (= (g/+ 36 'x) (g/+ 10 20 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 5 'x)) (g/- 10 3 2 'x)))
    (is (= (g/- 10 (g/+ 5 'x)) (g/- 10 'x 3 2)))
    (is (= (g/- 10 (g/+ 6 'x)) (g/- 10 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 26 'x)) (g/- 10 20 'x 3 2 1)))
    )
  (testing "* with vars"
    (is (= (g/* 60 'x) (g/* 10 3 2 'x)))
    (is (= (g/* 60 'x) (g/* 10 'x 3 2)))
    (is (= (g/* 60 'x) (g/* 10 'x 3 2 1)))
    (is (= (g/* 60 'x 'x) (g/* 'x 10 'x 3 2 1)))
    (is (= (g/* 1200 'x) (g/* 10 20 'x 3 2 1)))
    )
  (testing "trig shortcuts - sin"
    (is (= 0.0 (g/sin 0)))
    (is (= 0 (g/sin 'pi)))
    (is (= 0 (g/sin 'two-pi)))
    (is (= 0 (g/sin '-pi)))
    ;(is (= 0.0 (g/sin Math/PI)))
    ;(is (= 0.0 (g/sin (* 2 Math/PI))))
    ;(is (= 0.0 (g/sin (- Math/PI))))
    (is (= 1 (g/sin 'pi-over-2)))
    ;(is (= 1.0 (g/sin (/ Math/PI 2))))
    )
  (testing "trig shortcuts - cos"
    (is (= 1.0 (g/cos 0)))
    (is (= -1 (g/cos 'pi)))
    (is (= 1 (g/cos 'two-pi)))
    (is (= -1 (g/cos '-pi)))
    (is (= 0 (g/cos 'pi-over-2)))
    )
  )
