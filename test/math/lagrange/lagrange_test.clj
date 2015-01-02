(ns math.lagrange.lagrange-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
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
          literal-path (literal-function 'q)
          generic-path (up (literal-function 'x)
                           (literal-function 'y)
                           (literal-function 'z))
          LeQ (Le literal-path)
          LeP (Le generic-path)]
      (is (= '(* 1/2 m (+ (((expt D 2) q) t) (((expt D 2) q) t)))
             (print-expression (LeQ 't))))
      (is (= '(down
                (* 1/2 m (+ (((expt D 2) x) t) (((expt D 2) x) t)))
                (* 1/2 m (+ (((expt D 2) y) t) (((expt D 2) y) t)))
                (* 1/2 m (+ (((expt D 2) z) t) (((expt D 2) z) t))))
             (print-expression (LeP 't))))))
  (testing "derivations"
    (let [test-path (fn [t]
                      (up (+ (* 'a t) 'a0)
                          (+ (* 'b t) 'b0)
                          (+ (* 'c t) 'c0)))]
      (is (= (down 0 0 0)
             (((Lagrange-equations (L-free-particle 'm))
                test-path)
               't))))
    ;; TODO: for the present, we just ensure the following expressions
    ;; compile & execute without checking their values. This is because
    ;; simplification isn't online yet, so the values are going to change
    ;; soon enough, and the current forms of the values are pretty big.
    (is (((Lagrange-equations (L-free-particle 'm))
           (literal-function 'q))
          't))
    (let [proposed-solution (fn [t]
                              (* 'a (cos (+ (* 'omega t) 'phi))))]
      (is (((Lagrange-equations (L-harmonic 'm 'k))
             proposed-solution)
            't)))
    (is (((Lagrange-equations (L-uniform-acceleration 'm 'g))
           (up (literal-function 'x) (literal-function 'y)))
          't))
    (is (((Lagrange-equations (L-central-rectangular 'm (literal-function 'U)))
           (up (literal-function 'x)
               (literal-function 'y)))
          't))
    (is (((Lagrange-equations (L-central-polar 'm (literal-function 'U)))
           (up (literal-function 'r)
               (literal-function 'phi)))
          't))

    (is (velocity ((F->C p->r)
                    (->local 't (up 'r 'phi) (up 'rdot 'phidot)))))
    ))

