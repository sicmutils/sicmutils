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
            [sicmutils.calculus.manifold :as m :refer [R2-rect R2-polar
                                                       R3-rect R3-cyl]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :refer [compose]]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.operator :as o]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

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
      (let [R2-rect-chi (m/chart R2-rect)
            R2-rect-chi-inverse (m/point R2-rect)
            R2-polar-chi (m/chart R2-polar)
            R2-polar-chi-inverse (m/point R2-polar)
            R2-rect-point (R2-rect-chi-inverse (up 'x0 'y0))
            h (+ (* x (g/square r)) (g/cube y))]
        (is (= '(+ (* (expt r0 3) (expt (sin theta0) 3))
                   (* (expt r0 3) (cos theta0)))
               (simplify (h (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= 'x0 (simplify (x (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(* r0 (cos theta0)) (simplify (x (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= 'r0 (simplify (r (R2-polar-chi-inverse (up 'r0 'theta0))))))
        (is (= '(sqrt (+ (expt x0 2) (expt y0 2))) (simplify (r (R2-rect-chi-inverse (up 'x0 'y0))))))
        (is (= '(atan y0 x0) (simplify (theta (R2-rect-chi-inverse (up 'x0 'y0))))))))))

(deftest various-manifold-operations
  ;; These comprise the first block of test material in manifold.scm of
  ;; scmutils.
  (let-coordinates [[x y]     R2-rect
                    [r theta] R2-polar]
    (let [mr ((m/point R2-rect) (up 'x0 'y0))
          mp ((m/point R2-polar) (up 'r0 'theta0))
          circular (- (* x d:dy) (* y d:dx))
          g-polar (fn [u v]
                    (+ (* (dr u) (dr v))
                       (* (* r (dtheta u)) (* r (dtheta v)))))
          g-rect (fn [u v]
                   (+ (* (dx u) (dx v))
                      (* (dy u) (dy v))))
          residual (- g-polar g-rect)
          vp (vf/literal-vector-field 'v R2-polar)
          vr (vf/literal-vector-field 'v R2-rect)]
      (is (= '(+ (* 3 x0) (* -2 y0))
             (simplify
              ((circular (+ (* 2 x) (* 3 y))) mr))))

      (is (= 1 (simplify ((circular theta) mr))))
      (is (= 0 (simplify ((dr circular) mr))))
      (is (= 1 (((ff/d r) d:dr) mr)))
      (is (= 1 ((dr d:dr) mr)))

      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (simplify
              ((dr vp) mr))))

      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (simplify
              ((dr vr) mr))))

      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (simplify
              (((ff/d r) vp) mr))))

      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (simplify
              (((ff/d r) vr) mr))))

      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (simplify (((ff/d r) vr) mr))))

      (is (= 0 (simplify ((residual vr vr) mr))))
      (is (= 0 (simplify ((residual vp vp) mr))))
      (is (= 0 (simplify ((residual vp vp) mp))))
      (is (= 0 (simplify ((residual vr vr) mp)))))))

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
