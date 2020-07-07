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
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.euclid :as e]))

(defn ^:private ok
  "Compute the extended Euclid data; ensure that the gcd returned
  divides x and y, and that the GCD is the linear combination of x and
  y with the returned Bézout coefficients"
  [x y]
  (let [[g a b] (e/extended-gcd x y)]
    (and (= 0 (mod x g))
         (= 0 (mod y g))
         (= g (+ (* a x) (* b y)))
         g)))

(deftest euclid-test
  (testing "gcd"
    (let [gcd (fn [x y] (first (e/extended-gcd x y)))]
      (is (= 7 (e/gcd 21 35)))
      (is (= 7 (e/gcd -21 35)))
      (is (= 7 (e/gcd 21 -35)))
      (is (= 7 (e/gcd -21 -35)))
      (is (= 1 (e/gcd 8 7)))
      (is (= 1 (e/gcd -8 7) 1))
      (is (= 1 (e/gcd 8 -7) 1))
      (is (= 1 (e/gcd -8 -7) 1))))
  (testing "extended-gcd"
    (is (= (ok 8 7) 1))
    (is (= (ok 927 632) 1))
    (is (= [3 0 1] (e/extended-gcd 0 3)))
    (is (= [323 1 0] (e/extended-gcd 323 0)))
    (is (= (ok 934132 (* 934132 71)) 934132))
    (is (= (ok 37279462087332 366983722766) 564958))
    (is (= (ok 4323874085395 586898689868986900219865) 85))
    (is (= [2 -9 47] (e/extended-gcd 240 46)))
    (is (= [2 1 0] (e/extended-gcd 2 4))))
  (testing "lcm"
    (is (= 21 (e/lcm 3 7)))
    (is (= 6 (e/lcm 2 6)))
    (is (= 8 (e/lcm 2 8)))
    (is (= 30 (e/lcm 6 15)))
    (is (= 12 (reduce e/lcm [2 3 4])))
    (is (= 30 (reduce e/lcm [2 3 5])))))
