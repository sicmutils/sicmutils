(ns math.integral-test
  (:require [clojure.test :refer :all]
            [math.integral :refer :all]
            ))

(defn- within [ε]
  (fn [x y] (< (Math/abs (- x y)) ε)))

(def ^:private near (within 1e-6))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (integrate 0. 1. #(* % %))))
    (is (near 1.0 (integrate 1. (Math/exp 1.) #(/ %))))
    (is (near 2.302585 (integrate 1. 10. #(/ %))))
    )

   )
