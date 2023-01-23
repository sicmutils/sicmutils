#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch3-test
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ - *
                                    literal-function
                                    literal-manifold-function
                                    literal-vector-field
                                    up down
                                    square exp
                                    point chart
                                    define-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp e/freeze e/simplify))

(define-coordinates [x y] e/R2-rect)
(define-coordinates [r theta] e/R2-polar)

(def R2-rect-point ((point R2-rect) (up 'x0 'y0)))
(def R2->R '(-> (UP Real Real) Real))

(deftest section-3-1
  (let [v (e/components->vector-field
           (up (literal-function 'b↑0 R2->R)
               (literal-function 'b↑1 R2->R))
           R2-rect)
        v2 (literal-vector-field 'b R2-rect)]
    (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            ((v (literal-manifold-function 'f-rect R2-rect)) R2-rect-point))))

    (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            ((v2 (literal-manifold-function 'f-rect R2-rect)) R2-rect-point)))
        "This is just demonstrating the shorthand for making vector fields.")

    (is (= '(up (b↑0 (up x0 y0)) (b↑1 (up x0 y0)))
           (simplify
            ((v (chart R2-rect)) R2-rect-point))))

    (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            (((e/coordinatize v R2-rect) (literal-function 'f-rect R2->R))
             (up 'x0 'y0)))))))

(deftest section-3-2
  (testing "page 27"
    (is (= '(* 2 x0)
           (simplify
            ((d:dx (square r)) R2-rect-point))))

    (is (= '(+ (* 2 x0) (* 4 y0) 3)
           (simplify
            (((+ d:dx (* 2 d:dy)) (+ (square r) (* 3 x)))
             R2-rect-point))))))

(deftest section-3-3
  (let [circular (- (* x d:dy) (* y d:dx))]
    (is (= '((up 1 0)
             (up 0 t)
             (up (* (/ -1 2) (expt t 2)) 0)
             (up 0 (* (/ -1 6) (expt t 3)))
             (up (* (/ 1 24) (expt t 4)) 0)
             (up 0 (* (/ 1 120) (expt t 5))))
           (simplify
            (take 6
                  (seq
                   (((exp (* 't circular)) (chart R2-rect))
                    ((point R2-rect) (up 1 0))))))))
    (is (= '(up
             (+ (* (/ -1 720) (expt delta-t 6))
                (* (/ 1 24) (expt delta-t 4))
                (* (/ -1 2) (expt delta-t 2))
                1)
             (+ (* (/ 1 120) (expt delta-t 5))
                (* (/ -1 6) (expt delta-t 3))
                delta-t))
           (simplify
            ((((e/evolution 6) 'delta-t circular) (chart R2-rect))
             ((point R2-rect) (up 1 0))))))))

(deftest section-3-5
  (let [omega (e/components->oneform-field
               (down (literal-function 'a_0 R2->R)
                     (literal-function 'a_1 R2->R))
               R2-rect)
        omega2 (e/literal-oneform-field 'a R2-rect)
        circular (- (* x d:dy) (* y d:dx))]
    (is (= '(oneform-field (down a_0 a_1))
           (v/freeze omega))
        "TODO - why does this freeze into this form?")

    (testing "page 35, with and without literal-oneform-field shorthand"
      (is (= '(down (a_0 (up x0 y0)) (a_1 (up x0 y0)))
             (simplify
              ((omega (down d:dx d:dy)) R2-rect-point))))

      (is (= '(down (a_0 (up x0 y0)) (a_1 (up x0 y0)))
             (simplify
              ((omega2 (down d:dx d:dy)) R2-rect-point)))))

    (is (= '(down (((partial 0) f-rect) (up x0 y0))
                  (((partial 1) f-rect) (up x0 y0)))
           (simplify
            (((e/d (literal-manifold-function 'f-rect R2-rect))
              (e/coordinate-system->vector-basis R2-rect))
             R2-rect-point)))
        "p35")

    (is (= '(down
             (/ (+ (* r (cos theta) (((partial 0) f-polar) (up r theta)))
                   (* -1 (sin theta) (((partial 1) f-polar) (up r theta))))
                r)
             (/ (+ (* r (sin theta) (((partial 0) f-polar) (up r theta)))
                   (* (cos theta) (((partial 1) f-polar) (up r theta))))
                r))
           (simplify
            (((e/d (literal-manifold-function 'f-polar R2-polar))
              (e/coordinate-system->vector-basis R2-rect))
             ((point R2-polar) (up 'r 'theta)))))
        "page 36")

    (is (= 0 ((dx d:dy) R2-rect-point)))
    (is (= 1 ((dx d:dx) R2-rect-point)))

    (is (= '(* -1 y0)
           (simplify
            ((dx circular) R2-rect-point))))

    (is (= 'x0 ((dy circular) R2-rect-point)))

    (testing "page 37"
      (is (= 0 (simplify ((dr circular) R2-rect-point))))
      (is (= 1 (simplify ((dtheta circular) R2-rect-point))))

      (let [f (literal-manifold-function 'f-rect R2-rect)]
        (is (zero?
             (simplify
              (((- circular d:dtheta) f) R2-rect-point))))))

    (let [v (literal-vector-field 'b R2-rect)]
      (is (= '(+ (* (a_0 (up x0 y0)) (b↑0 (up x0 y0)))
                 (* (a_1 (up x0 y0)) (b↑1 (up x0 y0))))
             (simplify
              ((omega v) R2-rect-point)))
          "page 38"))))
