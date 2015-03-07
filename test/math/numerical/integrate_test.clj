(ns math.numerical.integrate-test
  (:require [clojure.test :refer :all]
            [math.value :as v]
            [math.numerical.integrate :refer :all]))

(def ^:private near (v/within 1e-6))

(def ^:private natural-log (partial integrate / 1.))

(def ^:private sine (partial integrate #(Math/cos %) 0.))

(defn bessel-j0 [x]
  (/ (integrate #(Math/cos (- (* x (Math/sin %)))) 0. Math/PI) Math/PI))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (integrate #(* % %) 0. 1.)))
    (is (near 0.5 (integrate identity 0. 1.)))
    (is (near 3 (integrate (constantly 1.0) 0. 3.)))
    (is (near 0 (integrate (constantly 0.0) 0. 1000.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 0 (sine Math/PI)))
    (is (near 1 (sine (/ Math/PI 2))))
    (is (near 0.7651976 (bessel-j0 1)))
    (is (near -0.2459358 (bessel-j0 10)))
    ))
