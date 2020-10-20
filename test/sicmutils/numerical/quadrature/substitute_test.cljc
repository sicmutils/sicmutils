;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.quadrature.substitute-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.numerical.quadrature.simpson :as simp]
            [sicmutils.numerical.quadrature.romberg :as qr]
            [sicmutils.numerical.quadrature.substitute :as qs]
            [sicmutils.value :as v]))

(deftest infinitize-tests
  (testing "1 => inf calculation of Euler's constant"
    ;; https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant
    (let [f (fn [x] (* (Math/log x)
                      (Math/exp (- x))))]
      (is (ish? {:converged? true
                 :terms-checked 7
                 :result 0.2193839343960799}
                ((qs/infinitize qr/open-integral) f 1 ##Inf))
          "This is a piece of the proper calculation of Euler's constant.")

      (with-comparator (v/within 1e-8)
        (is (ish? (:result ((qs/infinitize qr/open-integral) f 1 10))
                  (:result (qr/open-integral f 1 10)))
            "This is a piece of the proper calculation of Euler's constant.")))))

(deftest power-law-tests
  (let [f     (fn [x] (Math/sin x))
        [a b] [0 10]]
    (testing "Substitution methods shouldn't change results."
      (let [expected {:converged? true
                      :terms-checked 9
                      :result 1.8390715305632985}]
        (is (ish? expected (simp/integral f a b))
            "Baseline Simpson's rule test, with no variable substitution.")

        (with-comparator (v/within 1e-8)
          (is (ish? (:result expected)
                    (:result
                     ((qs/inverse-power-law-lower simp/integral 0.8) f a b))
                    (:result
                     ((qs/inverse-power-law-lower simp/integral 0.2) f a b)))
              "lower inverse power law singularities with different gamma values
            ~equal the original, untransformed result."))

        (with-comparator (v/within 1e-7)
          (is (ish? (:result expected)
                    (:result
                     ((qs/inverse-power-law-upper simp/integral 0.8) f a b)))
              "UPPER inverse power law singularities with different gamma values ~equal
            the original, untransformed result."))))

    (testing "inverse-sqrt is a general inverse power law with gamma=0.5"
      (is (ish? ((qs/inverse-sqrt-lower simp/integral) f a b)
                ((qs/inverse-power-law-lower simp/integral  0.5) f a b))
          "lower inverse sqrt")

      (is (ish? ((qs/inverse-sqrt-upper simp/integral) f a b)
                ((qs/inverse-power-law-upper simp/integral 0.5) f a b))
          "upper inverse sqrt"))))

(deftest exponential-tests
  (testing "Exponentially decaying upper endpoint"
    (let [f (fn [x] (Math/exp (- x)))]
      (is (ish? {:converged? true
                 :terms-checked 5
                 :result 0.6321205588285578}
                ((qs/exponential-upper qr/open-integral) f 0 ##Inf))
          "Calculation converges."))))
