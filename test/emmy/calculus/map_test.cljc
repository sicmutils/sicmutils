#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.map-test
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing]]
            [emmy.abstract.function :as af]
            [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :as c :refer [let-coordinates]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as man
             :refer [R1-rect R2-rect R3-rect R3-cyl S2-spherical]]
            [emmy.calculus.map :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.expression :as x]
            [emmy.function :as f]
            [emmy.generic :as g :refer [+ - *]]
            [emmy.structure :refer [up down]]
            [emmy.value :as v]))

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
                 (simplify
                  (((nth edual 0)
                    (vf/procedure->vector-field
                     (fn [f]
                       (fn [_]
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
    (let [R3-rect->R '(-> (UP Real Real Real) Real)
          mu (f/compose
              (man/point R3-cyl)
              (up (af/literal-function 'mu↑r R3-rect->R)
                  (af/literal-function 'mu↑theta R3-rect->R)
                  (af/literal-function 'mu↑zeta R3-rect->R))
              (man/chart R3-rect))
          R3-rect-point ((man/point R3-rect) (up 'x 'y 'z))]
      (is (= '(((partial 0) mu↑theta) (up x y z))
             (v/freeze
              ((((m/pullback mu) dtheta) d:dx)
               R3-rect-point))))

      (is (= '(((partial 1) mu↑theta) (up x y z))
             (v/freeze
              ((((m/pullback mu) dtheta) d:dy)
               R3-rect-point))))

      (is (= '(((partial 0) mu↑r) (up x y z))
             (v/freeze
              ((((m/pullback mu) dr) d:dx)
               R3-rect-point))))

      (is (= '(((partial 1) mu↑r) (up x y z))
             (v/freeze
              ((((m/pullback mu) dr) d:dy)
               R3-rect-point))))

      (is (= '(+ (* (((partial 0) mu↑r) (up x y z))
                    (((partial 1) mu↑theta) (up x y z)))
                 (* -1
                    (((partial 1) mu↑r) (up x y z))
                    (((partial 0) mu↑theta) (up x y z))))
             (simplify
              ((((m/pullback mu)
                 (ff/wedge dr dtheta))
                d:dx d:dy)
               R3-rect-point)))))

    (let-coordinates [[x y] R2-rect
                      t     R1-rect]
      (let [phi (f/compose (man/point R2-rect)
	                         (up g/square g/cube)
	                         (man/chart R1-rect))
            psi (f/compose (man/point R1-rect)
	                         (fn [[x y]] (- x y))
	                         (man/chart R2-rect))]
        (is (= '(* 3 (expt t0 4))
               (simplify
                ((((m/pullback phi) (* x dy)) d:dt)
                 ((man/point R1-rect) 't0)))))

        (is  (= '(+ (u↑0 (up x0 y0)) (* -1 (u↑1 (up x0 y0))))
                (simplify
                 ((((m/pullback psi) dt)
                   (vf/literal-vector-field 'u R2-rect))
                  ((man/point R2-rect) (up 'x0 'y0)))))))))

  (testing "pullback commutes with exterior derivative"
    (let-coordinates [[x y z] R3-rect]
      (let [R3-rect-chi (man/chart R3-rect)
            R3-rect-chi-inverse (man/point R3-rect)
            R3-rect->R '(-> (UP Real Real Real) Real)

            alpha (af/literal-function 'alpha R3-rect->R)
            beta (af/literal-function 'beta R3-rect->R)
            gamma (af/literal-function 'gamma R3-rect->R)

            theta
            (+ (* (f/compose alpha R3-rect-chi) dx)
               (* (f/compose beta R3-rect-chi) dy)
               (* (f/compose gamma R3-rect-chi) dz))

            R2-chi (man/chart R2-rect)
            R2-rect->R '(-> (UP Real Real) Real)
            X2 (vf/literal-vector-field 'X R2-rect)
            Y2 (vf/literal-vector-field 'Y R2-rect)
            m2 ((man/point R2-rect) (up 'u0 'v0))

            mu
            (f/compose R3-rect-chi-inverse
	                     (up (af/literal-function 'mu↑x R2-rect->R)
	                         (af/literal-function 'mu↑y R2-rect->R)
	                         (af/literal-function 'mu↑z R2-rect->R))
	                     R2-chi)]
        ;; first pullback a function
        (let [f (f/compose (af/literal-function 'f R3-rect->R)
	                         R3-rect-chi)]
          (is (= 0 (v/freeze
                    (((- ((m/pullback mu) (ff/d f))
                         (ff/d ((m/pullback mu) f)))
                      X2)
                     m2)))))

        ;; now pullback a form
        (is (= '(up (mu↑x (up u0 v0))
                    (mu↑y (up u0 v0))
                    (mu↑z (up u0 v0)))
               (v/freeze
                (R3-rect-chi (mu m2)))))

        (let [present (fn [expr]
                        (-> (simplify expr)
                            (x/substitute '(up u0 v0) 'p)))]

          (is (= '(+ (* (alpha (up (mu↑x p) (mu↑y p) (mu↑z p)))
                        (((partial 0) mu↑x) p)
                        (X↑0 p))
                     (* (alpha (up (mu↑x p) (mu↑y p) (mu↑z p)))
                        (((partial 1) mu↑x) p)
                        (X↑1 p))
                     (* (X↑0 p)
                        (beta (up (mu↑x p) (mu↑y p) (mu↑z p)))
                        (((partial 0) mu↑y) p))
                     (* (X↑0 p)
                        (gamma (up (mu↑x p) (mu↑y p) (mu↑z p)))
                        (((partial 0) mu↑z) p))
                     (* (X↑1 p)
                        (beta (up (mu↑x p) (mu↑y p) (mu↑z p)))
                        (((partial 1) mu↑y) p))
                     (* (X↑1 p)
                        (gamma (up (mu↑x p) (mu↑y p) (mu↑z p)))
                        (((partial 1) mu↑z) p)))
                 (present
                  ((((m/pullback mu) theta) X2) m2)))))

        (is (= 0 (simplify
                  (((- ((m/pullback mu) (ff/d theta))
                       (ff/d ((m/pullback mu) theta)))
                    X2 Y2)
                   m2))))

        (testing "pullback commutes with wedge"
          (let [theta (ff/literal-oneform-field 'theta R3-rect)
                phi   (ff/literal-oneform-field 'phi R3-rect)]
            (is (= 0 (simplify
                      (((- (ff/wedge ((m/pullback mu) theta)
                                     ((m/pullback mu) phi))
	                         ((m/pullback mu) (ff/wedge theta phi)))
                        X2
                        Y2)
                       m2)))))

          (let [theta (man/literal-manifold-function 'f R3-rect)
                phi (ff/literal-oneform-field 'phi R3-rect)]
            (is (= 0 (simplify
                      (((- (ff/wedge ((m/pullback mu) theta)
                                     ((m/pullback mu) phi))
	                         ((m/pullback mu) (ff/wedge theta phi)))
                        X2)
                       m2))))))))))
