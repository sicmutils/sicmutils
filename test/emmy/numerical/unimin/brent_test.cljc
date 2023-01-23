#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.unimin.brent-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish? zeroish? with-comparator] :include-macros true]
            [emmy.calculus.derivative :refer [D]]
            [emmy.generic :as g]
            [emmy.numerical.unimin.bracket :as brack]
            [emmy.numerical.unimin.brent :as b]
            [emmy.util :as u]
            [emmy.value :as v]))

(deftest brent-tests
  (with-comparator (v/within 1e-8)
    (checking "brent quadratic minimization, commons tests."
              100
              [lower  gen/small-integer
               upper  gen/small-integer
               offset gen/small-integer]
              (let [f (fn [x] (g/square (- x offset)))
                    upper (if (= lower upper) (inc lower) upper)
                    {:keys [lo hi]} (brack/bracket-min f {:xa lower :xb upper})
                    {:keys [result value converged? iterations fncalls]
                     :as #?(:clj m :cljs _m)}
                    (b/brent-min f (first lo) (first hi))]

                (is converged? "The result converges to the supplied offset.")
                (is (ish? result offset) "The result converges to the supplied offset.")
                (is (zeroish? value) "The result converges to the supplied offset.")
                (is (= fncalls (inc iterations))
                    "we only need 1 additional fn call (for the first interior
                    point) in addition to 1 per iteration.")

                #?(:clj
                   (is (= m (b/brent-min-commons f (first lo) (first hi)))
                       "The result is identical to the Commons implementation's results.")))))

  (testing "basic brent minimization"
    (is (ish?
         {:result 2.000000000000032
          :value 0
          :iterations 10
          :fncalls 11
          :converged? true}
         (-> (fn [x] (g/square (- x 2)))
             (b/brent-min -1000 10)))
        "minimize a quadratic and test that the iterations compare to the
        BrentOptimizer jvm implementation.")

    (is (ish?
         {:result -35.53582159348451
          :value 1408.9379026978984
          :iterations 6
          :converged? false
          :fncalls 7}
         (-> (fn [x] (g/square (- x 2)))
             (b/brent-min -1000 10 {:maxiter 5})))
        "maxiter limits the number of iterations allowed.")))

(deftest commons-ported-brent-tests
  (with-comparator (v/within 1e-7)
    (testing "g/sin converges to a minimum in (4, 5)"
      (is (ish? {:result (* 3 (/ Math/PI 2))
                 :value -1.0
                 :converged? true
                 :iterations 15
                 :fncalls 16}
                (b/brent-min g/sin 4 5 {:relative-threshold 1e-10
                                        :absolute-threshold 1e-14
                                        :maxfun 200}))))

    (testing "g/sin converges to a minimum in (1, 5)"
      (is (ish? {:result (* 3 (/ Math/PI 2))
                 :value -1.0
                 :converged? true
                 :iterations 20
                 :fncalls 21}
                (b/brent-min g/sin 1 5 {:relative-threshold 1e-10
                                        :absolute-threshold 1e-14
                                        :maxfun 200}))))

    (testing "g/sin converges to a minimum in (4, 5)"
      (is (ish? {:result (* 3 (/ Math/PI 2))
                 :value -1.0
                 :converged? false
                 :iterations 10
                 :fncalls 11}
                (b/brent-min g/sin 4 5 {:relative-threshold 1e-10
                                        :absolute-threshold 1e-14
                                        :maxfun 10}))))

    (testing "more relaxed relative threshold"
      (let [expected {:result (* 3 (/ Math/PI 2))
                      :value -1.0
                      :converged? true
                      :iterations 6
                      :fncalls 7}
            actual (b/brent-min g/sin 4 5 {:relative-threshold 1e-5
                                           :absolute-threshold 1e-14})]
        (is (not (ish? (:result expected)
                       (:result actual))))
        (with-comparator (v/within 1e-5)
          (is (ish? expected actual)))))

    (testing "boundaries don't get evaluated"
      (let [lower -1
            upper 1
            f (fn [x]
                (cond (<= x lower) (u/illegal "Too small!")
                      (>= x upper) (u/illegal "Too small!")
                      :else x))]
        (is (ish? -1 (:result (b/brent-min f -1 1 {})))
            "the endpoints are never evaluated, but the minimizer gets close.")

        (is (ish? 1 (:result (b/brent-max f -1 1 {})))
            "the endpoints are never evaluated, but the maximizer gets close.")))))

(deftest quintic-tests
  (with-comparator (v/within 1e-8)
    (let [f (fn [x] (g/* (g/- x 1)
                        (g/- x 0.5)
                        x
                        (g/+ x 0.5)
                        (g/+ x 1)))]

      (testing "quintic minimum"
        (let [min1 -0.27195613
              min2 0.82221643]
          ;; The function has local minima at -0.27195613 and 0.82221643.
          (is (zeroish? ((D f) min1)) "verify min1 is a local minimum.")
          (is (zeroish? ((D f) min2)) "verify min2 is a local minimum.")

          (is (ish? {:result min1
                     :value (f min1)
                     :converged? true
                     :iterations 8
                     :fncalls 9}
                    (b/brent-min f -0.3 -0.2 {:maxeval 200}))
              "First local minimum for the quintic.")

          (is (ish? {:result min2
                     :value (f min2)
                     :converged? true
                     :iterations 11
                     :fncalls 12}
                    (b/brent-min f 0.3 0.9 {:maxeval 200}))
              "Second local minimum for the quintic.")

          (is (ish? {:result min1
                     :value (f min1)
                     :converged? true
                     :iterations 10
                     :fncalls 11}
                    (b/brent-min f -1 0.2 {:maxeval 200}))
              "Search in a larger interval.")))

      (testing "quintic minimum"
        (let [max1 0.27195613]
          ;; The function has local minima at -0.27195613 and 0.82221643.
          (is (zeroish? ((D f) max1)) "verify max1 is a local maximum.")

          (is (ish? {:result max1
                     :value (f max1)
                     :converged? true
                     :iterations 8
                     :fncalls 9}
                    (b/brent-max f 0.2 0.3 {:maxeval 200}))
              "local max for the quintic."))))))
