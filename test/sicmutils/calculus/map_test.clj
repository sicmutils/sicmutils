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

(ns sicmutils.calculus.map-test
  (:refer-clojure :exclude [+ - * / ref zero? partial])
  (:require [clojure.test :refer :all]
            [sicmutils.calculus.form-field :refer [permutation-sequence]]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.env :refer :all]))

(deftest map-tests
  (testing "the basics"
    (let-coordinates [[x y z] R3-rect
                      [r theta zeta] R3-cyl]
      (let [R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))
            R3-cyl-point ((point R3-cyl) (up 'r0 'theta0 'zeta0))
            counter-clockwise (- (* x d:dy) (* y d:dx))
            outward (+ (* x d:dx) (* y d:dy))]
        (is (= '(* -1 y0) (simplify ((dx counter-clockwise) R3-rect-point))))
        (is (= '(* -1 y0) (simplify ((((differential x) counter-clockwise) identity) R3-rect-point))))
        (is (= 'x0 (simplify ((dx outward) R3-rect-point))))
        (is (= 'x0 (simplify ((((differential x) outward) identity) R3-rect-point))))
        (is (= 'x0 ((dy counter-clockwise) R3-rect-point)))
        (is (= 'x0 ((((differential y) counter-clockwise) identity) R3-rect-point)))
        (is (= 'y0 ((dy outward) R3-rect-point)))
        (is (= 'y0 ((((differential y) outward) identity) R3-rect-point)))
        (is (= '0 (simplify ((dr counter-clockwise) R3-cyl-point))))
        (is (= '0 (simplify ((((differential r) counter-clockwise) identity) R3-cyl-point))))
        (is (= 'r0 (simplify ((dr outward) R3-cyl-point))))
        (is (= 'r0 (simplify ((((differential r) outward) identity) R3-cyl-point))))
        (is (= 1 (simplify ((dtheta counter-clockwise) R3-cyl-point))))
        (is (= 1 (simplify ((((differential theta) counter-clockwise) identity) R3-cyl-point))))
        (is (= 0 (simplify ((dtheta outward) R3-cyl-point))))
        (is (= 0 (simplify ((((differential theta) outward) identity) R3-cyl-point)))))))
  (testing "literal manifold map"
    (let-coordinates [[x y] R2-rect
                      t R1-rect]
      (let [R2-rect-point ((point R2-rect) (up 'x0 'y0))
            μ (literal-manifold-map 'μ R1-rect R2-rect)
            f (literal-manifold-function 'f R2-rect)]

        (is (= '(+ (* (((∂ 0) f) (up (μ↑0 τ) (μ↑1 τ))) ((D μ↑0) τ))
                   (* (((∂ 1) f) (up (μ↑0 τ) (μ↑1 τ))) ((D μ↑1) τ)))
               (simplify ((((differential μ) d:dt) f)
                          ((point R1-rect) 'τ)))))
        (is (= '((D μ↑0) τ)
               (simplify ((dx ((differential μ) d:dt))
                          ((point R1-rect) 'τ)))))
        (let [e0 (literal-vector-field 'e0 R2-rect)
              e1 (literal-vector-field 'e1 R2-rect)
              edual (vector-basis->dual (down e0 e1) R2-rect)]
          ;; "But this is a fraud... Note that if we have a non-coordinate
          ;; basis the dual does not work on the transported vector."  [–- MIT]
          (is (thrown? AssertionError (((nth edual 0) ((differential μ) d:dt))
                                       ((point R1-rect) 'τ))))
          ;; "However, if we kludge the correct argument it gives the expected
          ;; answer."
          (is (= '(/ (+ (* ((D μ↑0) τ) (e1↑1 (up x0 y0)))
                        (* -1 ((D μ↑1) τ) (e1↑0 (up x0 y0))))
                     (+ (* (e1↑1 (up x0 y0)) (e0↑0 (up x0 y0)))
                        (* -1 (e1↑0 (up x0 y0)) (e0↑1 (up x0 y0)))))
                 (simplify (((nth edual 0)
                             (vf/procedure->vector-field
                              (fn [f]
                                (fn [m]
                                  ((((differential μ) d:dt) f)
                                   ((point R1-rect) 'τ))))))
                            R2-rect-point))))))))
  (testing "general path on the sphere"
    (let-coordinates [t R1-rect]
      (let [μ (compose (point S2-spherical)
                       (up (literal-function 'θ)
                           (literal-function 'φ))
                       (chart R1-rect))
            f (compose (literal-function 'f (-> (UP Real Real) Real))
                       (chart S2-spherical))]
        (is (= '(+ (* (((∂ 0) f) (up (θ τ) (φ τ))) ((D θ) τ))
                   (* (((∂ 1) f) (up (θ τ) (φ τ))) ((D φ) τ)))
               (simplify ((((differential μ) d:dt) f)
                          ((point R1-rect) 'τ)))))
        (let-coordinates [[θ φ] S2-spherical]
          (is (= '(((∂ 0) f) (up (θ τ) (φ τ)))
                 (simplify ((((vector-field->vector-field-over-map μ) d:dθ) f)
                            ((point R1-rect) 'τ)))))
          (is (= '((D θ) τ)
                 (simplify ((((form-field->form-field-over-map μ) dθ)
                             ((differential μ) d:dt))
                            ((point R1-rect) 'τ)))))
          (let [foo (basis->basis-over-map μ (coordinate-system->basis S2-spherical))]
            (is (= '(up (down 1 0) (down 0 1))
                   (simplify (((basis->oneform-basis foo)
                               (basis->vector-basis foo))
                              ((point R1-rect) 'τ)))))
            (is (= '(up ((D θ) τ) ((D φ) τ))
                   (simplify (((basis->oneform-basis foo)
                               ((differential μ) d:dt))
                              ((point R1-rect) 'τ)))))))
        )))
  )
