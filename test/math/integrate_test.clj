(ns math.integrate-test
  (:require [clojure.test :refer :all]
            [math.integrate :refer :all]
            ))

(defn- within [ε]
  (fn [x y] (< (Math/abs (- x y)) ε)))

(def ^:private near (within 1e-6))
(def ^:private natural-log (partial integrate / 1.))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (integrate #(* % %) 0. 1.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 2.302585 (natural-log 10.))))
    )
