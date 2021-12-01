;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.euclid-test
  (:require [clojure.test :refer [is deftest testing]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.numbers]
            [sicmutils.euclid :as e]
            [sicmutils.value :as v]
            [sicmutils.complex :as c]
            [sicmutils.generic :as g]
            [sicmutils.generators :as sg]
            [same :refer [ish?]]))

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

(defn ^:private fourth-power-is-one?
  "Checks if x^4 is 1+0i. This is needed because the gcd-complex function returns
   solutions that are determined up to a factor of ±1 and ±i."
  [x]
  (let [x4 (g/expt x 4)
        re (g/real-part x4)
        im (g/imag-part x4)]
    (and (ish? re 1)
         (ish? im 0))))

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

  (testing "gcd-complex"
    (checking "GCD of anything with itself is itself."
              10 [z sg/complex]
              (let [gaussian-z (e/round-complex z)]
                (is (= gaussian-z (e/gcd-complex gaussian-z gaussian-z)))))

    (checking "GCD of anything with 0 is itself, also for non-gaussian complex numbers (by definition)"
              10 [z sg/complex]
              (is (= z (e/gcd-complex z (c/complex 0 0))))
              (is (= z (e/gcd-complex (c/complex 0 0) z))))

    (checking "GCD of anything with 1 is 1."
              10 [z sg/complex]
              (is (fourth-power-is-one? (e/gcd-complex (e/round-complex z) (c/complex 1 0))))
              (is (fourth-power-is-one? (e/gcd-complex (c/complex 1 0) (e/round-complex z)))))

    (checking "dividing out the GCD gives coprime results"
              10 [l sg/complex r sg/complex]
              (let [gaussian-l (e/round-complex l)
                    gaussian-r (e/round-complex r)
                    z (e/gcd-complex gaussian-l gaussian-r)]
                (is (fourth-power-is-one? (e/gcd-complex
                                          (g// gaussian-l z)
                                          (g// gaussian-r z)))))))
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
