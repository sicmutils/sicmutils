#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch4-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ - * / zero?
                                    D
                                    literal-manifold-function
                                    literal-vector-field
                                    up down
                                    sin cos
                                    point chart
                                    R2-rect R2-polar R3-rect
                                    Euler-angles
                                    let-coordinates]]
            [emmy.mechanics.rotation :refer [rotate-x-matrix rotate-z-matrix]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp e/freeze e/simplify))

(deftest section-4-0
  (let-coordinates [[x y] R2-rect]
    (let [e0 (+ (* (literal-manifold-function 'e0x R2-rect) d:dx)
                (* (literal-manifold-function 'e0y R2-rect) d:dy))
          e1 (+ (* (literal-manifold-function 'e1x R2-rect) d:dx)
                (* (literal-manifold-function 'e1y R2-rect) d:dy))
          F (literal-manifold-function 'F R2-rect)
          e-vector-basis (down e0 e1)
          e-dual-basis (e/vector-basis->dual e-vector-basis R2-polar)
          R2-rect-chi-inverse (point R2-rect)
          v (* (up (literal-manifold-function 'b↑0 R2-rect)
                   (literal-manifold-function 'b↑1 R2-rect))
               e-vector-basis)
          p (R2-rect-chi-inverse (up 'X 'Y))]

      (is (= '(up (down 1 0)
                  (down 0 1))
             (simplify
              ((e-dual-basis e-vector-basis) p)))
          "p44")

      (is (= '(up (b↑0 (up X Y))
                  (b↑1 (up X Y)))
             (simplify
              ((e-dual-basis v) p)))
          "p44, bottom")

      (testing "e0, 1 are vector fields. They act on manifold functions to
                produce directional derivatives... which are manifold
                functions."
        (is (= '(+
                 (* (e0x (up X Y)) (((partial 0) F) (up X Y)))
                 (* (e0y (up X Y)) (((partial 1) F) (up X Y))))
               (simplify
                ((e0 F) p))))
        (is (= '(+
                 (* (((partial 0) F) (up X Y)) (e1x (up X Y)))
                 (* (((partial 1) F) (up X Y)) (e1y (up X Y))))
               (simplify
                ((e1 F) p))))
        (is (= '(down
                 (+
                  (* (e0x (up X Y)) (((partial 0) F) (up X Y)))
                  (* (e0y (up X Y)) (((partial 1) F) (up X Y))))
                 (+
                  (* (((partial 0) F) (up X Y)) (e1x (up X Y)))
                  (* (((partial 1) F) (up X Y)) (e1y (up X Y)))))
               (simplify
                ((e-vector-basis F) p))))))))

(deftest section-4-1
  (let [b-rect ((e/coordinate-system->oneform-basis R2-rect)
                (literal-vector-field 'b R2-rect))
        b-polar (* (e/Jacobian
                    (e/coordinate-system->basis R2-polar)
                    (e/coordinate-system->basis R2-rect))
                   b-rect)
        p ((point R2-rect) (up 'X 'Y))
        q ((point R2-polar) (up 'R 'Θ))]
    (is (= '(up (/
                 (+ (* x0 (b↑0 (up x0 y0))) (* y0 (b↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
                (/
                 (+ (* x0 (b↑1 (up x0 y0))) (* -1 y0 (b↑0 (up x0 y0))))
                 (+ (expt x0 2) (expt y0 2))))
           (simplify
            (b-polar
             ((point R2-rect) (up 'x0 'y0)))))
        "page 46")

    (is (= '(up
             (/
              (+ (* x0 (b↑0 (up x0 y0))) (* y0 (b↑1 (up x0 y0))))
              (sqrt (+ (expt x0 2) (expt y0 2))))
             (/
              (+ (* x0 (b↑1 (up x0 y0))) (* -1 y0 (b↑0 (up x0 y0))))
              (+ (expt x0 2) (expt y0 2))))
           (simplify
            (((e/coordinate-system->oneform-basis R2-polar)
              (literal-vector-field 'b R2-rect))
             ((point R2-rect) (up 'x0 'y0)))))
        "p46: We can also get the polar components directly")

    (is (= '(up 0 0)
           (simplify
            (- (b-polar ((point R2-rect) (up 'x0 'y0)))
               (((e/coordinate-system->oneform-basis R2-polar)
                 (literal-vector-field 'b R2-rect))
                ((point R2-rect) (up 'x0 'y0))))))
        "Showing that these are equivalent")

    (is (= '(up (down (/ X (sqrt (+ (expt X 2) (expt Y 2))))
                      (* -1 Y))
                (down (/ Y (sqrt (+ (expt X 2) (expt Y 2))))
                      X))
           (simplify
            (((e/coordinate-system->oneform-basis R2-rect)
              (e/coordinate-system->vector-basis R2-polar))
             p))))

    (is (= '(up (down (cos Θ) (* -1 R (sin Θ)))
                (down (sin Θ) (* R (cos Θ))))
           (simplify
            (((e/coordinate-system->oneform-basis R2-rect)
              (e/coordinate-system->vector-basis R2-polar))
             q))))

    (is (= '(up (down (/ X (sqrt (+ (expt X 2) (expt Y 2))))
                      (/ Y (sqrt (+ (expt X 2) (expt Y 2)))))
                (down (/ (* -1 Y) (+ (expt X 2) (expt Y 2)))
                      (/ X (+ (expt X 2) (expt Y 2)))))
           (simplify
            (((e/coordinate-system->oneform-basis R2-polar)
              (e/coordinate-system->vector-basis R2-rect))
             p))))

    (is (= '(up (down (cos Θ) (sin Θ))
                (down (/ (* -1N (sin Θ)) R) (/ (cos Θ) R)))
           (simplify
            (((e/coordinate-system->oneform-basis R2-polar)
              (e/coordinate-system->vector-basis R2-rect))
             q))))

    (let [p (up 'theta 'phi 'psi)
          q (up 'a 'b 'c)
          f #(+ p (* % q))
          M (fn [[theta phi psi]]
              (* (rotate-z-matrix phi)
                 (rotate-x-matrix theta)
                 (rotate-z-matrix psi)))]
      (is (= '(up
               (+ (* a epsilon) theta)
               (+ (* b epsilon) phi)
               (+ (* c epsilon) psi))
             (simplify
              (f 'epsilon))))

      (is (= '(up
               (+ (* a epsilon) theta)
               (+ (* b epsilon) phi)
               (+ (* c epsilon) psi))
             (simplify
              ((comp (chart e/Euler-angles)
                     (point e/Euler-angles)
                     f)
               'epsilon))))

      (is (= '(up a b c)
             (simplify ((D f) 'epsilon))))

      (let [g (comp M f)
            h #(* (rotate-x-matrix %)
                  (M (up 'theta 'phi 'psi)))]
        ;; The next two expressions show the LHS and RHS of the "linear
        ;; equations" alluded to on p.48 of FDG. By equating corresponding
        ;; entries, we may verify the solution of a, b, c given there.
        (is (= '(matrix-by-rows
                 (up
                  (+ (* a (sin phi) (sin psi) (sin theta))
                     (* -1 b (sin psi) (cos phi) (cos theta))
                     (* -1 c (sin phi) (cos theta) (cos psi))
                     (* -1 b (sin phi) (cos psi))
                     (* -1 c (sin psi) (cos phi)))
                  (+
                   (* a (sin phi) (sin theta) (cos psi))
                   (* -1 b (cos phi) (cos theta) (cos psi))
                   (* c (sin phi) (sin psi) (cos theta))
                   (* b (sin phi) (sin psi))
                   (* -1 c (cos phi) (cos psi)))
                  (+ (* a (sin phi) (cos theta)) (* b (sin theta) (cos phi))))
                 (up
                  (+ (* -1 a (sin psi) (sin theta) (cos phi))
                     (* -1 b (sin phi) (sin psi) (cos theta))
                     (* c (cos phi) (cos theta) (cos psi))
                     (* b (cos phi) (cos psi))
                     (* -1 c (sin phi) (sin psi)))
                  (+ (* -1 a (sin theta) (cos phi) (cos psi))
                     (* -1 b (sin phi) (cos theta) (cos psi))
                     (* -1 c (sin psi) (cos phi) (cos theta))
                     (* -1 b (sin psi) (cos phi))
                     (* -1 c (sin phi) (cos psi)))
                  (+ (* -1 a (cos phi) (cos theta)) (* b (sin phi) (sin theta))))
                 (up
                  (+ (* a (sin psi) (cos theta)) (* c (sin theta) (cos psi)))
                  (+ (* a (cos theta) (cos psi)) (* -1 c (sin psi) (sin theta)))
                  (* -1 a (sin theta))))
               (simplify ((D g) 0))))

        (is (= '(matrix-by-rows
                 (up 0 0 0)
                 (up
                  (* -1 (sin psi) (sin theta))
                  (* -1 (sin theta) (cos psi))
                  (* -1 (cos theta)))
                 (up
                  (+ (* (sin psi) (cos phi) (cos theta)) (* (sin phi) (cos psi)))
                  (+ (* (cos phi) (cos theta) (cos psi)) (* -1 (sin phi) (sin psi)))
                  (* -1 (sin theta) (cos phi))))
               (simplify ((D h) 0))))))))

(deftest section-4-3
  (let-coordinates [[x y] R2-rect]
    (let [e0 (+ (* (literal-manifold-function 'e0x R2-rect) d:dx)
                (* (literal-manifold-function 'e0y R2-rect) d:dy))
          e1 (+ (* (literal-manifold-function 'e1x R2-rect) d:dx)
                (* (literal-manifold-function 'e1y R2-rect) d:dy))
          polar-basis (e/coordinate-system->basis R2-polar)
          polar-vector-basis (e/basis->vector-basis polar-basis)
          polar-dual-basis (e/basis->oneform-basis polar-basis)
          f (literal-manifold-function 'f-rect R2-rect)
          p ((point R2-rect) (up 'X 'Y))]
      (is (zero?
           (simplify
            ((- ((e/commutator e0 e1) f)
                (* (- (e0 (polar-dual-basis e1))
                      (e1 (polar-dual-basis e0)))
                   (polar-vector-basis f)))
             p)))
          "page 50")))

  (let-coordinates [[x y z] R3-rect]
    (let [R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))
          g (literal-manifold-function 'g-rect R3-rect)
          Jz (- (* x d:dy) (* y d:dx))
          Jx (- (* y d:dz) (* z d:dy))
          Jy (- (* z d:dx) (* x d:dz))]
      (testing "page 51"
        (is (zero?
             (simplify
              (((+ (e/commutator Jx Jy) Jz) g) R3-rect-point))))

        (is (zero?
             (simplify
              (((+ (e/commutator Jy Jz) Jx) g) R3-rect-point))))

        (is (zero?
             (simplify
              (((+ (e/commutator Jz Jx) Jy) g) R3-rect-point)))))))

  (let-coordinates [[theta phi psi] Euler-angles]
    (let [;; equation 4.29
          e_x (+ (* (cos phi) d:dtheta)
                 (* -1 (/ (* (sin phi) (cos theta)) (sin theta)) d:dphi)
                 (* (/ (sin phi) (sin theta)) d:dpsi))

          ;; equation 4.30
          e_y (+ (/ (* (cos phi) (cos theta) d:dphi) (sin theta))
                 (* (sin phi) d:dtheta)
                 (* -1 (/ (cos phi) (sin theta)) d:dpsi))

          ;; equation 4.31
          e_z d:dphi

          f (literal-manifold-function 'f-Euler Euler-angles)
          SO3-point((point Euler-angles) (up 'theta 'phi 'psi))]
      (testing "page 51, part 2"
        (is (zero?
             (simplify
              (((+ (e/commutator e_x e_y) e_z) f) SO3-point))))

        (is (zero?
             (simplify
              (((+ (e/commutator e_y e_z) e_x) f) SO3-point))))

        (is (zero?
             (simplify
              (((+ (e/commutator e_z e_x) e_y) f) SO3-point))))))))
