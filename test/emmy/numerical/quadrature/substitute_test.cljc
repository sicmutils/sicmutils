#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.substitute-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.numerical.quadrature.romberg :as qr]
            [emmy.numerical.quadrature.simpson :as simp]
            [emmy.numerical.quadrature.substitute :as qs]
            [emmy.value :as v]))

(deftest infinitize-tests
  (testing "volume of Gabriel's Horn (https://en.wikipedia.org/wiki/Gabriel%27s_Horn)"
    (let [slice-area (fn [x] (let [y (/ 1 x)]
                              (* Math/PI (* y y))))
          integrate  (qs/infinitize qr/open-integral)]
      (is (ish? {:converged? true
                 :terms-checked 2
                 :result Math/PI}
                (integrate slice-area 1 ##Inf)))))

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
                 :terms-checked 2
                 :result 1.0}
                ((qs/exponential-upper qr/open-integral) f 0 ##Inf))
          "Calculation converges."))))
