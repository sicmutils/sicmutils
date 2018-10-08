;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.euclid-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sicmutils.euclid :refer :all]))

(defn ^:private ok
  "Compute the extended Euclid data; ensure that the gcd returned
  divides x and y, and that the GCD is the linear combination of x and
  y with the returned Bézout coefficients"
  [x y]
  (let [[g a b] (extended-gcd x y)]
    (and (= 0 (mod x g))
         (= 0 (mod y g))
         (= g (+ (* a x) (* b y)))
         g)))

(deftest euclid-test
  (testing "gcd"
    (let [gcd (fn [x y] (first (extended-gcd x y)))]
      (is (= 7 (gcd 21 35)))
      (is (= 7 (gcd -21 35)))
      (is (= 7 (gcd 21 -35)))
      (is (= 7 (gcd -21 -35)))
      (is (= 1 (gcd 8 7)))
      (is (= 1 (gcd -8 7) 1))
      (is (= 1 (gcd 8 -7) 1))
      (is (= 1 (gcd -8 -7) 1))))
  (testing "extended-gcd"
    (is (= (ok 8 7) 1))
    (is (= (ok 927 632) 1))
    (is (= (ok 934132 (* 934132 71)) 934132))
    (is (= (ok 37279462087332 366983722766) 564958))
    (is (= (ok 4323874085395 586898689868986900219865) 85))
    (is (= [2 -9 47] (extended-gcd 240 46)))
    (is (= [2 1 0] (extended-gcd 2 4))))
  (testing "lcm"
    (is (= 21 (lcm 3 7)))
    (is (= 6 (lcm 2 6)))
    (is (= 8 (lcm 2 8)))
    (is (= 30 (lcm 6 15)))
    (is (= 12 (reduce lcm [2 3 4])))
    (is (= 30 (reduce lcm [2 3 5])))))

(defspec gcd-divides-both-evenly 1000
  (prop/for-all [a gen/s-pos-int
                 b gen/s-pos-int]
                (let [g (gcd a b)]
                  (and (= 0 (mod a g))
                       (= 0 (mod b g))))))

(defspec modular-inverses-work 1000
  (gen/let [p (gen/elements [2 3 5 7 11 13 17 19 23 29 31])]
    (prop/for-all [a (gen/such-that #(not= 0 (mod % p)) gen/s-pos-int)]
                  ;; a · ai ≡ 1 (mod p)
                  (= 1 (mod (* (modular-inverse p a) a)
                            p)))))

(defspec bezout-coefficients-work 1000
  (prop/for-all [a gen/s-pos-int
                 b gen/s-pos-int]
                (let [[g m n] (extended-gcd a b)]
                  (= g (+ (* m a) (* n b))))))
