#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.euclid-test
  (:require [clojure.test :refer [is deftest testing]]
            [emmy.euclid :as e]
            [emmy.generic :as g]
            [emmy.numbers]))

(defn ^:private ok
  "Compute the extended Euclid data; ensure that the gcd returned
  divides x and y, and that the GCD is the linear combination of x and
  y with the returned Bézout coefficients"
  [x y]
  (let [[g a b] (e/extended-gcd x y)]
    (and (= 0 (g/modulo x g))
         (= 0 (g/modulo y g))
         (= g (g/+ (g/* a x) (g/* b y)))
         g)))

(deftest euclid-test
  (testing "gcd"
    (doseq [gcd [e/gcd (fn [x y] (first (e/extended-gcd x y)))]]
      (is (= 7 (gcd 21 35)))
      (is (= 7 (gcd -21 35)))
      (is (= 7 (gcd 21 -35)))
      (is (= 7 (gcd -21 -35)))
      (is (= 1 (gcd 8 7)))
      (is (= 1 (gcd -8 7) 1))
      (is (= 1 (gcd 8 -7) 1))
      (is (= 1 (gcd -8 -7) 1))

      (testing "EQUAL floats, each sign combo causes identity result"
        (is (= 1.2 (gcd -1.2 1.2)))
        (is (= 1.2 (gcd -1.2 -1.2)))
        (is (= 1.2 (gcd 1.2 -1.2)))
        (is (= 1.2 (gcd 1.2 1.2))))))

  (testing "generic-gcd"
    (is (= (* 2 5 7) (g/gcd (* 2 3 5 7) (* 2 5 7 11))))
    (is (= 4 (g/gcd 4 0)))
    (is (= 4 (g/gcd 0 4)))
    (is (= 1 (g/gcd 1 4)))
    (is (= 1 (g/gcd 4 1))))

  (testing "extended-gcd"
    (is (= (ok 8 7) 1))
    (is (= (ok 927 632) 1))
    (is (= [3 0 1] (e/extended-gcd 0 3)))
    (is (= [323 1 0] (e/extended-gcd 323 0)))
    (is (= (ok 934132 (* 934132 71)) 934132))
    (is (= [2 -9 47] (e/extended-gcd 240 46)))
    (is (= [2 1 0] (e/extended-gcd 2 4))))

  (testing "lcm"
    (is (= 21 (g/lcm 3 7)))
    (is (= 6 (g/lcm 2 6)))
    (is (= 8 (g/lcm 2 8)))
    (is (= 30 (g/lcm 6 15)))
    (is (= 12 (reduce g/lcm [2 3 4])))
    (is (= 30 (reduce g/lcm [2 3 5]))))

  (testing "high precision gcd"
    (is (= (ok #sicm/bigint 37279462087332
               #sicm/bigint 366983722766)
           564958))
    (is (= (ok #sicm/bigint 4323874085395
               #sicm/bigint "586898689868986900219865")
           85))))
