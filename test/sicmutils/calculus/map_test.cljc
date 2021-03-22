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
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

(def simplify
  (comp v/freeze g/simplify))

(deftest map-tests
  (testing "the basics: explanation of the connection between the basis forms
and the differentials of coordinate functions."
    (let-coordinates [[x y z]        R3-rect
                      [r theta zeta] R3-cyl]
      (let [R3-rect-point ((man/point R3-rect) (up 'x0 'y0 'z0))
            R3-cyl-point ((man/point R3-cyl) (up 'r0 'theta0 'zeta0))
            counter-clockwise (- (* x d:dy) (* y d:dx))
            outward (+ (* x d:dx) (* y d:dy))]
        (is (= '(* -1 y0)
               (simplify
                ((dx counter-clockwise) R3-rect-point))))

        (is (= '(* -1 y0)
               (simplify
                ((((m/differential x) counter-clockwise) identity)
                 R3-rect-point))))

        (is (= 'x0 (simplify
                    ((dx outward) R3-rect-point))))

        (is (= 'x0 (simplify
                    ((((m/differential x) outward) identity)
                     R3-rect-point))))

        (is (= 'x0 ((dy counter-clockwise)
                    R3-rect-point)))

        (is (= 'x0 ((((m/differential y) counter-clockwise) identity)
                    R3-rect-point)))

        (is (= 'y0 ((dy outward) R3-rect-point)))

        (is (= 'y0 ((((m/differential y) outward) identity)
                    R3-rect-point)))

        (is (= 0 (simplify ((dr counter-clockwise)
                            R3-cyl-point))))

        (is (= 0 (simplify
                  ((((m/differential r) counter-clockwise) identity)
                   R3-cyl-point))))

        (is (= 'r0 (simplify
                    ((dr outward) R3-cyl-point))))

        (is (= 'r0 (simplify
                    ((((m/differential r) outward) identity)
                     R3-cyl-point))))

        (is (= 1 (simplify
                  ((dtheta counter-clockwise) R3-cyl-point))))

        (is (= 1 (simplify
                  ((((m/differential theta) counter-clockwise) identity)
                   R3-cyl-point))))

        (is (= 0 (simplify
                  ((dtheta outward) R3-cyl-point))))

        (is (= 0 (simplify
                  ((((m/differential theta) outward) identity)
                   R3-cyl-point)))))))

  (testing "literal manifold map"
    (let-coordinates [[x y] R2-rect
                      t R1-rect]
      (let [R2-rect-point ((man/point R2-rect) (up 'x0 'y0))
            μ (m/literal-manifold-map 'μ R1-rect R2-rect)
            f (man/literal-manifold-function 'f R2-rect)]

        (is (= '(+ (* (((partial 0) f) (up (μ↑0 τ) (μ↑1 τ)))
                      ((D μ↑0) τ))
                   (* (((partial 1) f) (up (μ↑0 τ) (μ↑1 τ)))
                      ((D μ↑1) τ)))
               (simplify
                ((((m/differential μ) d:dt) f)
                 ((man/point R1-rect) 'τ)))))

        (is (= '((D μ↑0) τ)
               (simplify
                ((dx ((m/differential μ) d:dt))
                 ((man/point R1-rect) 'τ)))))

        (is (= '((D μ↑1) τ)
               (simplify
                ((dy ((m/differential μ) d:dt))
                 ((man/point R1-rect) 'τ)))))

        (let [e0 (vf/literal-vector-field 'e0 R2-rect)
              e1 (vf/literal-vector-field 'e1 R2-rect)
              edual (b/vector-basis->dual (down e0 e1) R2-rect)]
          ;; NOTE from GJS: "but this is a fraud... Note that if we have a
          ;; non-coordinate basis the dual does not work on the transported
          ;; vector."
          (is (thrown? #?(:clj AssertionError :cljs js/Error)
                       (((nth edual 0) ((m/differential μ) d:dt))
                        ((man/point R1-rect) 'τ))))

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
                                  ((((m/differential μ) d:dt) f)
                                   ((man/point R1-rect) 'τ))))))
                            R2-rect-point))))))))

  (testing "general path on the sphere"
    (let-coordinates [[θ φ] S2-spherical
                      t     R1-rect]
      (let [μ (f/compose (man/point S2-spherical)
                         (up (af/literal-function 'θ)
                             (af/literal-function 'φ))
                         (man/chart R1-rect))
            f (f/compose (af/literal-function 'f '(-> (UP Real Real) Real))
                         (man/chart S2-spherical))]
        (is (= '(+ (* (((partial 0) f) (up (θ τ) (φ τ))) ((D θ) τ))
                   (* (((partial 1) f) (up (θ τ) (φ τ))) ((D φ) τ)))
               (simplify ((((m/differential μ) d:dt) f)
                          ((man/point R1-rect) 'τ)))))

        (is (= '(((partial 0) f) (up (θ τ) (φ τ)))
               (simplify
                ((((m/vector-field->vector-field-over-map μ) d:dθ) f)
                 ((man/point R1-rect) 'τ)))))

        (is (= '((D θ) τ)
               (simplify
                ((((m/form-field->form-field-over-map μ) dθ)
                  ((m/differential μ) d:dt))
                 ((man/point R1-rect) 'τ)))))

        (let [foo (m/basis->basis-over-map
                   μ (b/coordinate-system->basis S2-spherical))]
          (is (= '(up (down 1 0)
                      (down 0 1))
                 (simplify
                  (((b/basis->oneform-basis foo)
                    (b/basis->vector-basis foo))
                   ((man/point R1-rect) 'τ)))))

          (is (= '(up ((D θ) τ)
                      ((D φ) τ))
                 (simplify
                  (((b/basis->oneform-basis foo)
                    ((m/differential μ) d:dt))
                   ((man/point R1-rect) 'τ))))))

        (testing "pullback"
          (is (= '(f (up (θ t) (φ t)))
                 (v/freeze
                  (((m/pullback μ) f)
                   ((man/point R1-rect) 't)))))

          (is (= '((D θ) t)
                 (v/freeze
                  ((((m/pullback μ) dθ) d:dt)
                   ((man/point R1-rect) 't)))))

          (is (= 0 (simplify
                    ((((m/pullback μ)
                       (ff/wedge dθ dφ))
                      d:dt d:dt)
                     ((man/point R1-rect) 't)))))))))

  (let-coordinates [[x y z]        R3-rect
                    [r theta zeta] R3-cyl]
    (let [mu (f/compose
              (man/point R3-cyl)
              (up (af/literal-function 'mu↑r
			                                 '(-> (UP Real Real Real) Real))
                  (af/literal-function 'mu↑theta
			                                 '(-> (UP Real Real Real) Real))
                  (af/literal-function 'mu↑zeta
			                                 '(-> (UP Real Real Real) Real)))
              (man/chart R3-rect))]
      (is (= '(((partial 0) mu↑theta) (up x y z))
             (v/freeze
              ((((m/pullback mu) dtheta) d:dx)
               ((man/point R3-rect) (up 'x 'y 'z))))))

      (is (= '(((partial 1) mu↑theta) (up x y z))
             (v/freeze
              ((((m/pullback mu) dtheta) d:dy)
               ((man/point R3-rect) (up 'x 'y 'z))))))

      (is (= '(((partial 0) mu↑r) (up x y z))
             (v/freeze
              ((((m/pullback mu) dr) d:dx)
               ((man/point R3-rect) (up 'x 'y 'z))))))

      (is (= '(((partial 1) mu↑r) (up x y z))
             (v/freeze
              ((((m/pullback mu) dr) d:dy)
               ((man/point R3-rect) (up 'x 'y 'z))))))

      (is (= '(+ (* (((partial 0) mu↑r) (up x y z))
                    (((partial 1) mu↑theta) (up x y z)))
                 (* -1
                    (((partial 1) mu↑r) (up x y z))
                    (((partial 0) mu↑theta) (up x y z))))
             (simplify
              ((((m/pullback mu)
                 (ff/wedge dr dtheta))
                d:dx d:dy)
               ((man/point R3-rect)
                (up 'x 'y 'z))))))

      (comment
        (define m ((R2-rect '->point) (up 3 4)))

        (install-coordinates R2-rect (up 'x 'y))

        (define phi
          (compose (R2-rect '->point)
	                 (up square cube)
	                 (R1-rect '->coords)))

        (pec ((((m/pullback phi) (* x dy)) d:dt)
              ((R1-rect '->point) 't0)))
        ;; Result:
        (* 3 (expt t0 4))


        (define psi
          (compose (R1-rect '->point)
	                 (lambda (v)
	                         (let ((x (ref v 0))
		                             (y (ref v 1)))
	                           (- x y)))
	                 (R2-rect '->coords)))

        (pec ((((m/pullback psi) dt)
               (literal-vector-field 'u R2-rect))
              ((R2-rect '->point) (up 'x0 'y0))))
        ;; Result:
        (+ (u↑0 (up x0 y0)) (* -1 (u↑1 (up x0 y0))))))
    )

  (testing "pullback commutes with exterior derivative"
    (let-coordinates [[x y z] R3-rect]
      (comment
        (define R3-rect-chi (R3-rect '->coords))
        (define R3-rect-chi-inverse (R3-rect '->point))
        (define R3-rect->R (-> (UP Real Real Real) Real))
        (define m3 ((R3-rect '->point) (up 'x0 'y0 'z0)))

        (define alpha (literal-function 'alpha R3-rect->R))
        (define beta (literal-function 'beta R3-rect->R))
        (define gamma (literal-function 'gamma R3-rect->R))

        (define theta
          (+ (* (compose alpha R3-rect-chi) dx)
             (* (compose beta R3-rect-chi) dy)
             (* (compose gamma R3-rect-chi) dz)))

        (define R2-chi (R2-rect '->coords))
        (define R2-chi-inverse (R2-rect '->point))
        (define R2-rect->R (-> (UP Real Real) Real))
        (define X2 (literal-vector-field 'X R2-rect))
        (define Y2 (literal-vector-field 'Y R2-rect))
        (define m2 ((R2-rect '->point) (up 'u0 'v0)))

        (define mu
          (compose R3-rect-chi-inverse
	                 (up (literal-function 'mu↑x R2-rect->R)
	                     (literal-function 'mu↑y R2-rect->R)
	                     (literal-function 'mu↑z R2-rect->R))
	                 R2-chi))

        ;; first pullback a function
        (let [f (f/compose (literal-function 'f R3-rect->R)
	                         R3-rect-chi)]
          (is (= 0 (((- ((m/pullback mu) (d f))
                        (d ((m/pullback mu) f)))
                     X2)
                    m2))))

        ;; now pullback a form
        (is (= '(up (mu↑x (up u0 v0))
                    (mu↑y (up u0 v0))
                    (mu↑z (up u0 v0)))
               (R3-rect-chi (mu m2))))

        (is (= '(+ (* (((partial 0) mu↑x) (up u0 v0))
                      (alpha (up (mu↑x (up u0 v0))
                                 (mu↑y (up u0 v0))
                                 (mu↑z (up u0 v0))))
                      (X↑0 (up u0 v0)))
                   (* (((partial 1) mu↑x) (up u0 v0))
                      (alpha (up (mu↑x (up u0 v0))
                                 (mu↑y (up u0 v0))
                                 (mu↑z (up u0 v0))))
                      (X↑1 (up u0 v0)))
                   (* (((partial 0) mu↑y) (up u0 v0))
                      (beta (up (mu↑x (up u0 v0))
                                (mu↑y (up u0 v0))
                                (mu↑z (up u0 v0))))
                      (X↑0 (up u0 v0)))
                   (* (((partial 1) mu↑y) (up u0 v0))
                      (beta (up (mu↑x (up u0 v0))
                                (mu↑y (up u0 v0))
                                (mu↑z (up u0 v0))))
                      (X↑1 (up u0 v0)))
                   (* (((partial 0) mu↑z) (up u0 v0))
                      (X↑0 (up u0 v0))
                      (gamma (up (mu↑x (up u0 v0))
                                 (mu↑y (up u0 v0))
                                 (mu↑z (up u0 v0)))))
                   (* (((partial 1) mu↑z) (up u0 v0))
                      (gamma (up (mu↑x (up u0 v0))
                                 (mu↑y (up u0 v0))
                                 (mu↑z (up u0 v0))))
                      (X↑1 (up u0 v0))))
               ((((m/pullback mu) theta) X2) m2)))

        (is (= 0 (((- ((m/pullback mu) (d theta))
                      (d ((m/pullback mu) theta)))
                   X2 Y2)
                  m2))))))

  (testing "pullback commutes with wedge"
    (comment
      (let [theta (ff/literal-oneform-field 'theta R3-rect)
            phi (ff/literal-oneform-field 'phi R3-rect)]
        (is (= 0 (((- (ff/wedge ((m/pullback mu) theta)
                                ((m/pullback mu) phi))
	                    ((m/pullback mu) (ff/wedge theta phi)))
                   X2
                   Y2)
                  m2))))

      (let [theta (man/literal-manifold-function 'f R3-rect)
            phi (ff/literal-oneform-field 'phi R3-rect)]
        (is (= 0 (((- (ff/wedge ((m/pullback mu) theta)
                                ((m/pullback mu) phi))
	                    ((m/pullback mu) (ff/wedge theta phi)))
                   X2)
                  m2)))))))
