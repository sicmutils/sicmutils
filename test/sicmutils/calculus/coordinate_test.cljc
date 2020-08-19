;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.calculus.coordinate-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.coordinate :as c
             #?(:clj :refer :cljs :refer-macros) [let-coordinates
                                                  using-coordinates]]
            [sicmutils.calculus.manifold :as m :refer [R2-rect R2-polar]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest smoke
  (let-coordinates [[x y] R2-rect
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
               (g/simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))))))

  (testing "with-coordinate-prototype"
    (let [A R2-rect
          B (m/with-coordinate-prototype R2-rect '[X Y])]
      (is (= '[d:dx0 d:dx1] (map :name (c/coordinate-system->vector-basis A))))
      (is (= '[d:dX d:dY] (map :name (c/coordinate-system->vector-basis B))))))

  (testing "let-coordinates"
    (let-coordinates [(up x y) R2-rect
                      (up r theta) R2-polar]
      (let [R2-polar-chi-inverse (m/point R2-polar)
            h (+ (* x (g/square r)) (g/cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (g/simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))))
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      (let [R2-rect-chi (m/chart R2-rect)
            R2-rect-chi-inverse (m/point R2-rect)
            R2-polar-chi (m/chart R2-polar)
            R2-polar-chi-inverse (m/point R2-polar)
            R2-rect-point (R2-rect-chi-inverse (up 'x0 'y0))
            h (+ (* x (g/square r)) (g/cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (g/simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= 'x0 (g/simplify (x (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(* r0 (cos theta0)) (g/simplify (x (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= 'r0 (g/simplify (r (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= '(sqrt (+ (expt x0 2) (expt y0 2))) (g/simplify (r (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(atan y0 x0) (g/simplify (theta (R2-rect-chi-inverse (up 'x0 'y0))))))))))

(deftest various-manifold-operations
  ;; being the test material found in the comments of manifold.scm of scmutils
  (let-coordinates [[x y] R2-rect
                    [r theta] R2-polar]
    (let [mr ((m/point R2-rect) (up 'x0 'y0))
          mp ((m/point R2-polar) (up 'r0 'theta0))
          circular (- (* x d:dy) (* y d:dx))
          g-polar (fn [u v] (+ (* (dr u) (dr v))
                              (* (* r (dtheta u)) (* r (dtheta v)))))
          g-rect (fn [u v] (+ (* (dx u) (dx v))
                             (* (dy u) (dy v))))
          residual (- g-polar g-rect)
          vp (vf/literal-vector-field 'v R2-polar)
          vr (vf/literal-vector-field 'v R2-rect)]
      (is (= 0 (g/simplify ((dr circular) mr))))
      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (g/simplify ((dr vr) mr))))
      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (g/simplify (((ff/d r) vr) mr))))
      (is (= 0 (g/simplify ((residual vr vr) mr))))
      (is (= 0 (g/simplify ((residual vp vp) mr))))
      (is (= 0 (g/simplify ((residual vp vp) mp))))
      (is (= 0 (g/simplify ((residual vr vr) mp))))
      (is (= 1 (g/simplify ((circular theta) mr))))

      ;; TODO(colin.smith): implement little d
      (is (= 1 (((ff/d r) d:dr) mr)))
      (is (= 1 (g/simplify ((dr d:dr) mr))))
      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (g/simplify ((dr vp) mr))))
      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (g/simplify (((ff/d r) vp) mr)))))))
