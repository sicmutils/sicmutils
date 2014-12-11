(ns math.lagrange.lagrange-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.mechanics.lagrange :refer :all]))

(deftest interpolation
  (testing "lagrange interpolation"
    (is (= [1/6 1/3 1/2 2/3 5/6] (linear-interpolants 0 1 5)))
    (let [f (Lagrange-interpolation-function [3 2 5 1] [1 2 3 4])]
      (is (= (f 1) 3))
      (is (= (f 2) 2))
      (is (= (f 3) 5))
      (is (= (f 4) 1)))
    ;; this works, but since we don't have simplification yet it leaves
    ;; behind a horrible expression that works out to 'a.
    ;; (let [f (Lagrange-interpolation-function '[a b c] '[w x y])]
    ;;   (is (= 'a (f 'w))))
    ))