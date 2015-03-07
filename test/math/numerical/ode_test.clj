(ns math.numerical.ode-test
  (:require [clojure.test :refer :all]
            [math.structure :refer :all]
            [math.value :as v]
            [math.numerical.ode :refer :all]))

(deftest simple-odes
  (testing "y' = y"
    (let [result ((state-advancer (constantly identity)) (up 1.) 1. 1.e-10)]
      (is ((v/within 1e-8) 2.718281828 (nth result 0))))))
