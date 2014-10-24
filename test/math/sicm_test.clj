(ns math.sicm-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.struct :refer :all]
            [math.function :refer :all]))

(defn velocity [local] (nth 2 local))

(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* 1/2 mass (square v)))))

(def q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

;; next step: q must be applicable!

;; (deftest sicm
;;   (testing "a"
;;     (is (= 'foo (q 't)))))
