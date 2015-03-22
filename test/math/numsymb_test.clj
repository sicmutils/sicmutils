;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.numsymb-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.value :as v]
            [math.numbers]
            [math.numsymb]))

(def ^:private near (v/within 1e-12))

(deftest numsymb-test
  (testing "+/- with vars"
    (is (= (g/+ 15 'x) (g/+ 10 3 2 'x)))
    (is (= (g/+ 10 'x 3 2) (g/+ 10 'x 3 2)))
    (is (= (g/+ 10 'x 3 2 1) (g/+ 10 'x 3 2 1)))
    (is (= (g/+ 30 'x 3 2 1) (g/+ 10 20 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 5 'x)) (g/- 10 3 2 'x)))
    (is (= (g/- 10 (g/+ 'x 3 2)) (g/- 10 'x 3 2)))
    (is (= (g/- 10 (g/+ 'x 3 2 1)) (g/- 10 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 20 'x 3 2 1)) (g/- 10 20 'x 3 2 1)))
    )
  (testing "* with vars"
    (is (= (g/* 60 'x) (g/* 10 3 2 'x)))
    (is (= (g/* 10 'x 3 2) (g/* 10 'x 3 2)))
    (is (= (g/* 10 'x 3 2 1) (g/* 10 'x 3 2 1)))
    (is (= (g/* 'x 10 'x 3 2 1) (g/* 'x 10 'x 3 2 1)))
    (is (= (g/* 200 'x 3 2) (g/* 10 20 'x 3 2 1))))
  (testing "trig shortcuts - sin"
    (is (= 0.0 (g/sin 0)))
    (is (= 0 (g/sin 'pi)))
    (is (= 0 (g/sin 'two-pi)))
    (is (= 0 (g/sin '-pi)))
    (is (near 0.0 (g/sin Math/PI)))
    (is (near 0.0 (g/sin (* 2 Math/PI))))
    (is (near 0.0 (g/sin (- Math/PI))))
    (is (= 1 (g/sin 'pi-over-2)))
    (is (= 1.0 (g/sin (/ Math/PI 2)))))
  (testing "trig shortcuts - cos"
    (is (= 1.0 (g/cos 0)))
    (is (= -1 (g/cos 'pi)))
    (is (near -1.0 (g/cos Math/PI)))
    (is (= 1 (g/cos 'two-pi)))
    (is (near 1.0 (g/cos (* 2 Math/PI))))
    (is (= -1 (g/cos '-pi)))
    (is (= 0 (g/cos 'pi-over-2))))

  (testing "trig shortcuts - tan"
    (is (= 0.0 (g/tan 0)))
    (is (= 1 (g/tan 'pi-over-4)))
    (is (= -1 (g/tan '-pi-over-4)))
    (is (thrown? IllegalArgumentException (g/tan 'pi-over-2)))))
