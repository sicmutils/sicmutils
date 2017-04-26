(ns sicmutils.fdg.ch2-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(def ^:private R2-rect-chi (chart R2-rect))
(def ^:private R2-rect-chi-inverse (point R2-rect))
(def ^:private R2-polar-chi (chart R2-polar))
(def ^:private R2-polar-chi-inverse (point R2-polar))

(deftest section-2-1
  (is (= '(up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0))
         (simplify ((compose R2-polar-chi R2-rect-chi-inverse) (up 'x0 'y0)))))
  (is (= '(up (* r0 (cos theta0)) (* r0 (sin theta0)))
         (simplify ((compose R2-rect-chi R2-polar-chi-inverse) (up 'r0 'theta0)))))
  (is (= '(down (up (cos theta0) (sin theta0))
                (up (* -1 r0 (sin theta0)) (* r0 (cos theta0))))
         (simplify ((D (compose R2-rect-chi R2-polar-chi-inverse)) (up 'r0 'theta0))))))

(deftest section-2-2
  (let [f (compose (literal-function 'f-rect (-> (UP Real Real) Real)) R2-rect-chi)
        R2-rect-point (R2-rect-chi-inverse (up 'x0 'y0))
        corresponding-polar-point (R2-polar-chi-inverse
                                    (up (sqrt (+ (square 'x0) (square 'y0)))
                                        (atan 'y0 'x0)))]
    (is (= 0 R2-rect-point))
    (is (= 0 corresponding-polar-point))
    (is (= 0 (R2-rect-chi corresponding-polar-point)))
    (is (= 0 (simplify (f R2-rect-point))))
    (is (= 0 (simplify (f corresponding-polar-point))))
    ))
