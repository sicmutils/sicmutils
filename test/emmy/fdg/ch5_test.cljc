#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch5-test
  (:refer-clojure :exclude [+ - * / = partial zero?])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ - * = zero?
                                    d simplify partial compose
                                    literal-function
                                    literal-manifold-function
                                    literal-vector-field
                                    up
                                    point chart wedge
                                    R2-rect
                                    define-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(define-coordinates [x y z] e/R3-rect)
(define-coordinates [r theta z-cyl] e/R3-cyl)

(def R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0)))
(def R3-cyl-point  ((point R3-cyl) (up 'r0 'theta0 'z0)))

(deftest section-5-1
  (let [u (+ (* 'u↑0 d:dx) (* 'u↑1 d:dy))
        v (+ (* 'v↑0 d:dx) (* 'v↑1 d:dy))
        a (+ (* 'a↑0 d:dr) (* 'a↑1 d:dtheta))
        b (+ (* 'b↑0 d:dr) (* 'b↑1 d:dtheta))]

    (is (= '(+ (* u↑0 v↑1) (* -1 u↑1 v↑0))
           (simplify
            (((wedge dx dy) u v) R3-rect-point)))
        "Page 60")

    (is (= '(+ (* a↑0 b↑1) (* -1 a↑1 b↑0))
           (simplify
            (((wedge dr dtheta) a b) R3-cyl-point)))
        "Page 60"))

  (let [u (+ (* 'u↑0 d:dx) (* 'u↑1 d:dy) (* 'u↑2 d:dz))
        v (+ (* 'v↑0 d:dx) (* 'v↑1 d:dy) (* 'v↑2 d:dz))
        w (+ (* 'w↑0 d:dx) (* 'w↑1 d:dy) (* 'w↑2 d:dz))]
    (testing "page 61"
      (is (= '(+ (* u↑0 v↑1 w↑2)
                 (* -1 u↑0 v↑2 w↑1)
                 (* -1 u↑1 v↑0 w↑2)
                 (* u↑1 v↑2 w↑0)
                 (* u↑2 v↑0 w↑1)
                 (* -1 u↑2 v↑1 w↑0))
             (simplify
              (((wedge dx dy dz) u v w) R3-rect-point))))

      (is (zero?
           (simplify
            (- (((wedge dx dy dz) u v w) R3-rect-point)
               (e/determinant
                (e/matrix-by-rows '[u↑0 u↑1 u↑2]
                                  '[v↑0 v↑1 v↑2]
                                  '[w↑0 w↑1 w↑2])))))
          "This last expression is the determinant of a 3×3 matrix"))

    (is (= 1 (((wedge dx dy dz) d:dx d:dy d:dz)
              R3-rect-point)))))

(deftest section-5-2
  ;; Fun with unicode variable names!
  (let [a (literal-manifold-function 'α R3-rect)
        b (literal-manifold-function 'β R3-rect)
        c (literal-manifold-function 'γ R3-rect)
        θ (+ (* a dx) (* b dy) (* c dz))
        X (literal-vector-field 'X-rect R3-rect)
        Y (literal-vector-field 'Y-rect R3-rect)
        ω (+ (* a (wedge dy dz))
             (* b (wedge dz dx))
             (* c (wedge dx dy)))
        Z (literal-vector-field 'Z-rect R3-rect)]
    (is (zero?
         (simplify
          (((- (d θ)
               (+ (wedge (d a) dx)
                  (wedge (d b) dy)
                  (wedge (d c) dz)))
            X Y)
           R3-rect-point)))
        "page 63")

    (is (zero?
         (simplify
          (((- (d ω)
               (+ (wedge (d a) dy dz)
                  (wedge (d b) dz dx)
                  (wedge (d c) dx dy)))
            X Y Z)
           R3-rect-point)))
        "page 64")

    (is (zero?
         (simplify
          (((d (d θ)) X Y Z) R3-rect-point)))
        "page 65")))

(deftest section-5-4
  (let [v (literal-vector-field 'v-rect R2-rect)
        w (literal-vector-field 'w-rect R2-rect)
        α (literal-function 'α (-> (UP Real Real) Real))
        β (literal-function 'β (-> (UP Real Real) Real))
        R2-rect-basis (e/coordinate-system->basis R2-rect)
        [dx dy] (e/basis->oneform-basis R2-rect-basis)
        R2-rect-point ((point R2-rect) (up 'x0 'y0))]
    (is (zero?
         (simplify
          (((- (d (+ (* (compose α (chart R2-rect)) dx)
                     (* (compose β (chart R2-rect)) dy)))
               (* (compose (- ((partial 0) β)
                              ((partial 1) α))
                           (chart R2-rect))
                  (wedge dx dy)))
            v w)
           R2-rect-point)))
        "page 67"))

  (let [a (literal-manifold-function 'a-rect R3-rect)
        b (literal-manifold-function 'b-rect R3-rect)
        c (literal-manifold-function 'c-rect R3-rect)
        flux-through-boundary-element (+ (* a (wedge dy dz))
                                         (* b (wedge dz dx))
                                         (* c (wedge dx dy)))
        production-in-volume-element (* (+ (d:dx a) (d:dy b) (d:dz c))
                                        (wedge dx dy dz))
        X (literal-vector-field 'X-rect R3-rect)
        Y (literal-vector-field 'Y-rect R3-rect)
        Z (literal-vector-field 'Z-rect R3-rect)]
    (is (zero?
         (simplify
          (((- production-in-volume-element
               (d flux-through-boundary-element))
            X Y Z)
           R3-rect-point)))
        "page 69")))
