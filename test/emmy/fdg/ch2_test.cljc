#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch2-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ - *
                                    D compose
                                    literal-function
                                    literal-manifold-function
                                    up cos square cube sqrt atan
                                    point chart
                                    S2-spherical S2-Riemann
                                    define-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp e/freeze e/simplify))

(define-coordinates (up x y) e/R2-rect)
(define-coordinates (up r theta) e/R2-polar)

(def R2-rect-chi (chart R2-rect))
(def R2-rect-chi-inverse (point R2-rect))
(def R2-polar-chi (chart R2-polar))
(def R2-polar-chi-inverse (point R2-polar))

(deftest section-2-1
  (testing "coordinate functions"
    (is (= '(up (sqrt (+ (expt x0 2) (expt y0 2)))
                (atan y0 x0))
           (simplify
            ((compose R2-polar-chi R2-rect-chi-inverse)
             (up 'x0 'y0))))
        "p14: the polar coordinates of a rectangular point")

    (is (= '(up (* r0 (cos theta0))
                (* r0 (sin theta0)))
           (simplify
            ((compose R2-rect-chi R2-polar-chi-inverse)
             (up 'r0 'theta0))))
        "p14: the rectangular coordinates of a polar point")

    (is (= '(down (up (cos theta0)
                      (sin theta0))
                  (up (* -1 r0 (sin theta0))
                      (* r0 (cos theta0))))
           (simplify
            ((D (compose R2-rect-chi R2-polar-chi-inverse))
             (up 'r0 'theta0))))
        "And we can obtain the Jacobian of the polar-to-rectangular transformation
      by taking its derivative")))

(deftest section-2-2
  (testing "manifold functions"
    (let [R2->R '(-> (UP Real Real) Real)
          f (compose (literal-function 'f-rect R2->R) R2-rect-chi)

          ;; Show that you can also use the `literal-manifold-function`
          ;; shorthand...
          g (literal-manifold-function 'g-rect R2-rect)
          R2-rect-point (R2-rect-chi-inverse (up 'x0 'y0))
          corresponding-polar-point (R2-polar-chi-inverse
                                     (up (sqrt (+ (square 'x0) (square 'y0)))
                                         (atan 'y0 'x0)))]
      (is (= '(f-rect (up x0 y0)) (simplify (f R2-rect-point))))
      (is (= '(f-rect (up x0 y0)) (simplify (f corresponding-polar-point))))
      (is (= '(g-rect (up x0 y0)) (simplify (g R2-rect-point))))
      (is (= '(g-rect (up x0 y0)) (simplify (g corresponding-polar-point))))

      (testing "page 17"
        (is (= 'x0
               (x (R2-rect-chi-inverse (up 'x0 'y0)))))

        (is (= '(* r0 (cos theta0))
               (simplify
                (x (R2-polar-chi-inverse (up 'r0 'theta0))))))

        (is (= 'r0
               (simplify
                (r (R2-polar-chi-inverse (up 'r0 'theta0))))))

        (is (= '(sqrt (+ (expt x0 2) (expt y0 2)))
               (simplify
                (r (R2-rect-chi-inverse (up 'x0 'y0))))))

        (is (= '(atan y0 x0)
               (simplify
                (theta (R2-rect-chi-inverse (up 'x0 'y0)))))))

      ;; We can work with the coordinate functions in a natural manner,
      ;; defining new manifold functions in terms of them:
      (let [h (+ (* x (square r)) (cube y))]
        (is (= '(+ (expt x0 3) (* x0 (expt y0 2))
                   (expt y0 3))
               (simplify
                (h R2-rect-point))))

        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify
                (h (R2-polar-chi-inverse
                    (up 'r0 'theta0)))))))

      (is (= '(/ (+ (* -2 a x)
                    (* -2 a (sqrt (+ (expt x 2) (expt y 2))))
                    (expt x 2)
                    (expt y 2))
                 (sqrt (+ (expt x 2) (expt y 2))))
             (simplify
              ((- r (* 2 'a (+ 1 (cos theta))))
               ((point R2-rect) (up 'x 'y))))))

      (testing "Setup for ex2.1, p18"
        (is (= '(/ (+ (* -2 a x)
                      (* -2 a (sqrt (+ (expt x 2) (expt y 2))))
                      (expt x 2) (expt y 2))
                   (sqrt (+ (expt x 2) (expt y 2))))
               (simplify
                ((- r (* 2 'a (+ 1 (cos theta))))
                 ((point R2-rect) (up 'x 'y)))))))

      (testing "ex2.2"
        (is (= '(up (acos (/ (+ (expt rho 2) -1)
                             (+ (expt rho 2) 1)))
                    theta)
               (simplify
                ((compose
                  (chart S2-spherical)
                  (point S2-Riemann)
                  (chart R2-rect)
                  (point R2-polar))
                 (up 'rho 'theta)))))))))
