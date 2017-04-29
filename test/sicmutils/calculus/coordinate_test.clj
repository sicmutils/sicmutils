(ns sicmutils.calculus.coordinate-test
  (:refer-clojure :exclude [+ - * / ref zero? partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]))

(deftest coordinates
  (testing "using coordinates"
    (using-coordinates (up x y) R2-rect
                       (using-coordinates (up r theta) R2-polar
                                          (let [R2-polar-chi-inverse (point R2-polar)
                                                h (+ (* x (square r)) (cube y))]
                                            (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                                                       (* (expt r0 3) (cos theta0)))
                                                   (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))))))

  (testing "let-coordinates"
    (let-coordinates [(up x y) R2-rect
                      (up r theta) R2-polar]
      (let [R2-polar-chi-inverse (point R2-polar)
            h (+ (* x (square r)) (cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))))
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      (let [R2-rect-chi (chart R2-rect)
            R2-rect-chi-inverse (point R2-rect)
            R2-polar-chi (chart R2-polar)
            R2-polar-chi-inverse (point R2-polar)
            R2-rect-point (R2-rect-chi-inverse (up 'x0 'y0))
            h (+ (* x (square r)) (cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= 'x0 (simplify (x (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(* r0 (cos theta0)) (simplify (x (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= 'r0 (simplify (r (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= '(sqrt (+ (expt x0 2) (expt y0 2))) (simplify (r (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(atan y0 x0) (simplify (theta (R2-rect-chi-inverse (up 'x0 'y0))))))))))

(deftest various-manifold-operations
  ;; being the test material found in the comments of manifold.scm of scmutils
  (let-coordinates [[x y] R2-rect
                    [r theta] R2-polar]
    (let [mr ((point R2-rect) (up 'x0 'y0))
          mp ((point R2-polar) (up 'r0 'theta0))
          circular (- (* x d:dy) (* y d:dx))]
      (is (= 1 (simplify ((circular theta) mr))))
      ;; (is (= 0 (simplify ((dr circular) mr)))) TODO(colin.smith): implement one-form fields
      ;; (is (= what (((d r) d:dr) mr)))) TODO(colin.smith): implement little d
      ;; (is (= 0 (simplify ((dr d:dr) mr))))
      ;; (is (= 0 (simplify ((dr (literal-vector-field 'v R2-polar)) mr))))
      ;; (((d r) (literal-vector-field 'v R2-polar)) mr)
      ;; (is (= 0 (simplify ((dr (literal-vector-field 'v R2-rect)) mr))))
      ;; (((d r) (literal-vector-field 'v R2-rect)) mr)

      ))

  )
