(ns math.lagrange.lagrange-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.mechanics.lagrange :refer :all]
            [math.function :refer :all]
            [math.numbers]
            [math.expression :refer :all]
            [math.structure :refer :all]))

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

(deftest lagrange-equations
  (testing "basics"
    (let [Le (Lagrange-equations (L-free-particle 'm))
          generic-path (up (literal-function 'x)
                           (literal-function 'y)
                           (literal-function 'z))
          LeP (Le generic-path)]
      (is (= 'foo (print-expression (LeP 't)))))))