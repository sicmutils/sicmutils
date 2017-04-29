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
        (is (= '(atan y0 x0) (simplify (theta (R2-rect-chi-inverse (up 'x0 'y0))))))

        (testing "coordinate vector fields"
          (is (= 1 ((d:dx x) R2-rect-point)))
          ;; (is (= 0 (simplify ((d:dr x) R2-rect-point))))
          )
        ))))
