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
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.abstract.function :as af]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :as c
             :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.map :as m]
            [sicmutils.calculus.manifold :as man
             :refer [R1-rect R2-rect R3-rect R3-cyl S2-spherical]]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.structure :refer [up down]]))

(deftest map-tests
  (testing "the basics"
    (let-coordinates [[x y z] R3-rect
                      [r theta zeta] R3-cyl]
      (let [R3-rect-point ((man/point R3-rect) (up 'x0 'y0 'z0))
            R3-cyl-point ((man/point R3-cyl) (up 'r0 'theta0 'zeta0))
            counter-clockwise (- (* x d:dy) (* y d:dx))
            outward (+ (* x d:dx) (* y d:dy))]
        (is (= '(* -1 y0) (g/simplify ((dx counter-clockwise) R3-rect-point))))
        (is (= '(* -1 y0) (g/simplify ((((m/differential x) counter-clockwise) identity) R3-rect-point))))
        (is (= 'x0 (g/simplify ((dx outward) R3-rect-point))))
        (is (= 'x0 (g/simplify ((((m/differential x) outward) identity) R3-rect-point))))
        (is (= 'x0 ((dy counter-clockwise) R3-rect-point)))
        (is (= 'x0 ((((m/differential y) counter-clockwise) identity) R3-rect-point)))
        (is (= 'y0 ((dy outward) R3-rect-point)))
        (is (= 'y0 ((((m/differential y) outward) identity) R3-rect-point)))
        (is (= '0 (g/simplify ((dr counter-clockwise) R3-cyl-point))))
        (is (= '0 (g/simplify ((((m/differential r) counter-clockwise) identity) R3-cyl-point))))
        (is (= 'r0 (g/simplify ((dr outward) R3-cyl-point))))
        (is (= 'r0 (g/simplify ((((m/differential r) outward) identity) R3-cyl-point))))
        (is (= 1 (g/simplify ((dtheta counter-clockwise) R3-cyl-point))))
        (is (= 1 (g/simplify ((((m/differential theta) counter-clockwise) identity) R3-cyl-point))))
        (is (= 0 (g/simplify ((dtheta outward) R3-cyl-point))))
        (is (= 0 (g/simplify ((((m/differential theta) outward) identity) R3-cyl-point)))))))

  (testing "literal manifold map"
    (let-coordinates [[x y] R2-rect
                      t R1-rect]
      (let [R2-rect-point ((man/point R2-rect) (up 'x0 'y0))
            μ (m/literal-manifold-map 'μ R1-rect R2-rect)
            f (man/literal-manifold-function 'f R2-rect)]

        (is (= '(+ (* (((partial 0) f) (up (μ↑0 τ) (μ↑1 τ))) ((D μ↑0) τ))
                   (* (((partial 1) f) (up (μ↑0 τ) (μ↑1 τ))) ((D μ↑1) τ)))
               (g/simplify ((((m/differential μ) d:dt) f)
                            ((man/point R1-rect) 'τ)))))
        (is (= '((D μ↑0) τ)
               (g/simplify ((dx ((m/differential μ) d:dt))
                            ((man/point R1-rect) 'τ)))))
        (let [e0 (vf/literal-vector-field 'e0 R2-rect)
              e1 (vf/literal-vector-field 'e1 R2-rect)
              edual (c/vector-basis->dual (down e0 e1) R2-rect)]
          ;; "But this is a fraud... Note that if we have a non-coordinate
          ;; basis the dual does not work on the transported vector."  [–- MIT]
          (is (thrown? #?(:clj AssertionError :cljs js/Error)
                       (((nth edual 0) ((m/differential μ) d:dt))
                        ((man/point R1-rect) 'τ))))

          ;; "However, if we kludge the correct argument it gives the expected
          ;; answer."
          (is (= '(/ (+ (* ((D μ↑0) τ) (e1↑1 (up x0 y0)))
                        (* -1 ((D μ↑1) τ) (e1↑0 (up x0 y0))))
                     (+ (* (e1↑1 (up x0 y0)) (e0↑0 (up x0 y0)))
                        (* -1 (e1↑0 (up x0 y0)) (e0↑1 (up x0 y0)))))
                 (g/simplify (((nth edual 0)
                               (vf/procedure->vector-field
                                (fn [f]
                                  (fn [m]
                                    ((((m/differential μ) d:dt) f)
                                     ((man/point R1-rect) 'τ))))))
                              R2-rect-point))))))))

  (testing "general path on the sphere"
    (let-coordinates [t R1-rect]
      (let [μ (f/compose (man/point S2-spherical)
                         (up (af/literal-function 'θ)
                             (af/literal-function 'φ))
                         (man/chart R1-rect))
            f (f/compose (af/literal-function 'f '(-> (UP Real Real) Real))
                         (man/chart S2-spherical))]
        (is (= '(+ (* (((partial 0) f) (up (θ τ) (φ τ))) ((D θ) τ))
                   (* (((partial 1) f) (up (θ τ) (φ τ))) ((D φ) τ)))
               (g/simplify ((((m/differential μ) d:dt) f)
                            ((man/point R1-rect) 'τ)))))
        (let-coordinates [[θ φ] S2-spherical]
          (is (= '(((partial 0) f) (up (θ τ) (φ τ)))
                 (g/simplify ((((m/vector-field->vector-field-over-map μ) d:dθ) f)
                              ((man/point R1-rect) 'τ)))))
          (is (= '((D θ) τ)
                 (g/simplify ((((m/form-field->form-field-over-map μ) dθ)
                               ((m/differential μ) d:dt))
                              ((man/point R1-rect) 'τ)))))
          (let [foo (m/basis->basis-over-map μ (c/coordinate-system->basis S2-spherical))]
            (is (= '(up (down 1 0) (down 0 1))
                   (g/simplify (((b/basis->oneform-basis foo)
                                 (b/basis->vector-basis foo))
                                ((man/point R1-rect) 'τ)))))
            (is (= '(up ((D θ) τ) ((D φ) τ))
                   (g/simplify (((b/basis->oneform-basis foo)
                                 ((m/differential μ) d:dt))
                                ((man/point R1-rect) 'τ)))))))
        )))
  )
