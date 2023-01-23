#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.infinite-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? zeroish?] :include-macros true]
            [emmy.numerical.quadrature.adaptive :as qa]
            [emmy.numerical.quadrature.bulirsch-stoer :as bs]
            [emmy.numerical.quadrature.infinite :as qi]))

(def ^:private integrator
  (qa/adaptive bs/open-integral
               bs/closed-integral))

(deftest improper-tests
  (binding [qa/*neighborhood-width* 0]
    (testing "Euler's constant"
      ;; https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant
      (let [f (fn [x] (* (Math/log x)
                        (Math/exp (- x))))]
        (is (ish? {:converged? true
                   :result -0.5772156418405041}
                  ((qi/improper integrator) f 0 ##Inf))
            "The improper integral converges.")

        (is (ish? {:converged? true
                   :result -2.7965685938222346E-9}
                  ((qi/improper integrator) f 0 ##Inf {:infinite-breakpoint 100}))
            "A breakpoint that's too big ruins the calculation by pushing the
            variable-change region to the right.")

        (is (= (integrator f 0 10)
               ((qi/improper integrator) f 0 10))
            "With non-infinite bounds, integration passes through.")))

    (testing "full integration"
      (let [f (fn [x] (* x (Math/exp (- (* x x)))))
            integrate (qi/improper integrator)]
        (is (zeroish?
             (+ (:result (integrate f ##-Inf 0))
                (:result (integrate f 0 ##Inf)))))

        (is (ish? {:converged? true :result 0}
                  (integrate f ##-Inf ##Inf))
            "We can handle the full range all at once.")))))
