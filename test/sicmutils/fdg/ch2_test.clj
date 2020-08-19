;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.fdg.ch2-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
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
  (let [R2->R '(-> (UP Real Real) Real)
        f (compose (literal-function 'f-rect R2->R) R2-rect-chi)
        g (literal-manifold-function 'g-rect R2-rect)
        R2-rect-point (R2-rect-chi-inverse (up 'x0 'y0))
        corresponding-polar-point (R2-polar-chi-inverse
                                   (up (sqrt (+ (square 'x0) (square 'y0)))
                                       (atan 'y0 'x0)))]
    (is (= '(f-rect (up x0 y0)) (simplify (f R2-rect-point))))
    (is (= '(f-rect (up x0 y0)) (simplify (f corresponding-polar-point))))
    (is (= '(g-rect (up x0 y0)) (simplify (g R2-rect-point))))
    (is (= '(g-rect (up x0 y0)) (simplify (g corresponding-polar-point))))
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      (is (= 'x0 (x (R2-rect-chi-inverse (up 'x0 'y0)))))
      (is (= '(* r0 (cos theta0)) (simplify (x (R2-polar-chi-inverse (up 'r0 'theta0))))))
      (is (= 'r0 (simplify (r (R2-polar-chi-inverse (up 'r0 'theta0))))))
      (is (= '(sqrt (+ (expt x0 2) (expt y0 2))) (simplify (r (R2-rect-chi-inverse (up 'x0 'y0))))))
      (is (= '(atan y0 x0) (simplify (theta (R2-rect-chi-inverse (up 'x0 'y0))))))
      (let [h (+ (* x (square r)) (cube y))]
        (is (= '(+ (expt x0 3) (* x0 (expt y0 2)) (expt y0 3))
               (simplify (h R2-rect-point))))
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3)) (* (expt r0 3) (cos theta0)))
               (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0)))))))
      (is (= '(/ (+ (* -2 a x)
                    (* -2 a (sqrt (+ (expt x 2) (expt y 2))))
                    (expt x 2)
                    (expt y 2))
                 (sqrt (+ (expt x 2) (expt y 2))))
             (simplify ((- r (* 2 'a (+ 1 (cos theta))))
                        ((point R2-rect) (up 'x 'y)))))))
    (testing "ex2.2"
      (is (= '(up (acos (/ (+ (expt rho 2) -1) (+ (expt rho 2) 1))) theta)
             (simplify ((compose
                         (chart S2-spherical)
                         (point S2-Riemann)
                         (chart R2-rect)
                         (point R2-polar))
                        (up 'rho 'theta))))))))
