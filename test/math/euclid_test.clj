(ns math.euclid-test
  (:require [clojure.test :refer :all]
            [math.euclid :refer :all]))

(testing "gcd"
  (let [gcd (fn [x y] (first (extended-euclid x y)))]
    (is (= 1 (gcd 8 7)))
    (is (= 7 (gcd 21 35)))))

(defn- ok
  "Compute the extended Euclid data; ensure that the gcd returned
  divides x and y, and that the GCD is the linear combination of x and
  y with the returned Bézout coefficients"
  [x y]
  (let [[g a b] (extended-euclid x y)]
    (and (= 0 (mod x g))
         (= 0 (mod y g))
         (= g (+ (* a x) (* b y)))
         g)))

(testing "extended-gcd"
  (is (= (ok 8 7) 1))
  (is (= (ok 927 632) 1))
  (is (= (ok 934132 (* 934132 71)) 934132)))
