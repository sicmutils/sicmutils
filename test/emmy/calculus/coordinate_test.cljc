#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.coordinate-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.calculus.coordinate :as c
             :refer [let-coordinates
                     using-coordinates]]
            [emmy.calculus.manifold :as m
             :refer [R2-rect R2-polar
                     R3-rect R3-cyl]]
            [emmy.function :refer [compose]]
            [emmy.generic :as g :refer [+ * /]]
            [emmy.simplify :as s :refer [hermetic-simplify-fixture]]
            [emmy.structure :refer [up]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest smoke
  (let-coordinates [[x y]     R2-rect
                    [r theta] R2-polar]
    (let [p ((m/point R2-rect) (up 1 2))]
      (is (= '[x y] (m/coordinate-prototype R2-rect)))
      (is (= '[r theta] (m/coordinate-prototype R2-polar)))
      (is (= 1 (x p)))
      (is (= 2 (y p)))
      (is (= (g/sqrt 5) (r p)))
      (is (= (g/atan 2) (theta p))))))

(deftest coordinates
  (testing "using coordinates"
    (using-coordinates
     (up x y) R2-rect
     (using-coordinates
      (up r theta) R2-polar
      (let [R2-polar-chi-inverse (m/point R2-polar)
            h (+ (* x (g/square r)) (g/cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))))))

  (testing "let-coordinates"
    (let-coordinates [(up x y) R2-rect
                      (up r theta) R2-polar]
      (let [R2-polar-chi-inverse (m/point R2-polar)
            h (+ (* x (g/square r)) (g/cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))))
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      (let [R2-rect-chi-inverse  (m/point R2-rect)
            R2-polar-chi-inverse (m/point R2-polar)
            R2-rect-point  (R2-rect-chi-inverse (up 'x0 'y0))
            R2-polar-point (R2-polar-chi-inverse (up 'r0 'theta0))
            h (+ (* x (g/square r)) (g/cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify (h R2-polar-point))))
        (is (= 'x0 (simplify (x (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(* r0 (cos theta0)) (simplify (x R2-polar-point))))
        (is (= 'r0 (simplify (r R2-polar-point))))
        (is (= '(sqrt (+ (expt x0 2) (expt y0 2))) (simplify (r R2-rect-point))))
        (is (= '(atan y0 x0) (simplify (theta R2-rect-point))))))))

(deftest literal-tests
  ;; These come from the bottom of manifold.scm.
  ;;
  ;; A scalar field can be defined by combining coordinate functions:
  (let-coordinates [[x y z] R3-rect
                    [r theta zeta] R3-cyl]
    (let [h (+ 5
               (g/square x)
               (* -1 x (g/cube y))
               (/ 1 y))]
      ;; The field, however defined, can be seen as independent of
      ;; coordinate system:

      (is (= -177.75 (h ((m/point R3-rect) (up 3.0 4.0 'z)))))
      (is (= -177.74999999999997
             (h ((m/point m/R3-cyl)
                 (up 5.0 (g/atan 4 3) 'z))))))

    ;; However this may be too clever, producing a traditional notation
    ;; that is hard to understand deeply.  Perhaps it is better to be
    ;; explicit about what is coordinate-system independent.  For
    ;; example, we can define a coordinate-free function h by composing a
    ;; definition in terms of coordinates with a coordinate function.
    (let [h-concrete (fn [[x y]]
                       (+ 5
                          (g/square x)
                          (* -1 x (g/cube y))
                          (/ 1 y)))
          h (compose h-concrete (m/chart m/R3-rect))]
      (is (= -177.75 (h ((m/point m/R3-rect)
                         (up 3.0 4 5))))))))
