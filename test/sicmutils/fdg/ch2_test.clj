(ns sicmutils.fdg.ch2-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-2-1
  (let [R2-rect-chi (chart R2-rect)
        R2-rect-chi-inverse (point R2-rect)
        R2-polar-chi (chart R2-polar)
        R2-polar-chi-inverse (point R2-polar)]
    (is (= '(up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0))
           (simplify ((compose R2-polar-chi R2-rect-chi-inverse) (up 'x0 'y0)))))
    (is (= '(up (* r0 (cos theta0)) (* r0 (sin theta0)))
           (simplify ((compose R2-rect-chi R2-polar-chi-inverse) (up 'r0 'theta0)))))
    (is (= '(down (up (cos theta0) (sin theta0)) (up (* -1 r0 (sin theta0)) (* r0 (cos theta0))))
           (simplify ((D (compose R2-rect-chi R2-polar-chi-inverse)) (up 'r0 'theta0)))))))
