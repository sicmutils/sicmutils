(ns math.numerical.integrate-test
  (:require [clojure.test :refer :all]
            [math.numerical.integrate :refer :all]
            ))

(defn- within [ε]
  (fn [x y] (< (Math/abs (- x y)) ε)))

(def ^:private near (within 1e-6))
(def ^:private natural-log (partial integrate / 1.))
(def ^:private sine (partial integrate #(Math/cos %) 0.))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (integrate #(* % %) 0. 1.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 0 (sine Math/PI)))
    (is (near 1 (sine (/ Math/PI 2))))
    ))

;; (deftest with-timing
;;   (testing "easy"
;;     (prn (time (integrate #(* % %) 0. 1.)))
;;     (prn (time (natural-log (Math/exp 1.))))
;;     (prn (time (sine Math/PI)))
;;     (prn (time (sine (/ Math/PI 2))))
;;     ))
