#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.bulirsch-stoer-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [emmy.numerical.quadrature.bulirsch-stoer :as bs]
            [emmy.numerical.quadrature.romberg-test :as rom]))

(deftest open-bulirsch-stoer-tests
  (testing "Bulirsch-Stoer integration over an open interval converges."
    (let [expected {:converged? true
                    :terms-checked 5
                    :result 0.42135039647834527}
          terms (bs/open-sequence rom/gaussian 0 1)]

      (is (ish? expected (bs/open-integral rom/gaussian 0 1))
          "The sequence converges.")

      (is (= (:result (bs/open-integral rom/gaussian 0 1))
             (nth terms 4))
          "The sequence converged on the 5 term (index 4).")

      (is (ish? [0.42573629964283044
                 0.42134340779993334
                 0.42135014080568106
                 0.4213504053817121
                 0.42135039647834527
                 0.42135039647485945]
                (take 6 terms))
          "here's a look at the first six terms.")))

  (testing "open Bulirsch-Stoer won't touch the endpoints."
    (is (= (bs/open-integral rom/gaussian 0 1)
           (-> (rom/exploding rom/gaussian 0 1)
               (bs/open-integral 0 1)))))

  (testing "open Bulirsch-Stoer passes options through."
    (is (ish? {:converged? false
               :terms-checked 3
               :result 0.42135014080568106}
              (bs/open-integral rom/gaussian 0 1 {:maxterms 3})))))

(deftest closed-bulirsch-stoer-tests
  (testing "Bulirsch-Stoer integration over a closed interval converges."
    (is (ish? {:converged? true
               :terms-checked 5
               :result 0.42135039646230377}
              (bs/closed-integral rom/gaussian 0.0 1.0))))

  (testing "Bulirsch-Stoer integration over a closed interval DOES evaluate the
  endpoints."
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (-> (rom/exploding rom/gaussian 0 1)
                     (bs/closed-integral 0 1)))))

  (testing "closed Bulirsch-Stoer passes options through."
    (is (ish? {:converged? false
               :terms-checked 3
               :result 0.42135062439636956}
              (bs/closed-integral rom/gaussian 0 1 {:maxterms 3})))))

(deftest polynomial-extrapolation-tests
  (testing "Bulirsch-Stoer integration using polynomial extrapolation."
    (is (ish? {:converged? true
               :terms-checked 5
               :result 0.4213503965205771}
              (bs/closed-integral rom/gaussian 0.0 1.0 {:bs-extrapolator :polynomial}))
        "closed interval")

    (is (not (ish? (bs/closed-integral rom/gaussian 0.0 1.0)
                   (bs/closed-integral rom/gaussian 0.0 1.0 {:bs-extrapolator :polynomial})))
        "the extrapolator change has an effect!")

    (is (ish? {:converged? true
               :terms-checked 5
               :result 0.42135039642921074}
              (bs/open-integral rom/gaussian 0.0 1.0 {:bs-extrapolator :polynomial}))
        "open interval")))
