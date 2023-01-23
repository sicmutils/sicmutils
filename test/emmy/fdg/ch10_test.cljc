#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch10-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ - * /
                                    zero?
                                    square sin expt
                                    wedge
                                    down up
                                    point
                                    define-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(def spherical e/R3-rect)
(define-coordinates [r theta phi] spherical)

(def SR e/R4-rect)
(define-coordinates [ct x y z] SR)

(deftest ch10-tests
  (testing "spherical coordinates, p156"
    (let [R3-spherical-point ((point spherical) (up 'r0 'theta0 'phi0))
          spherical-metric (fn [v1 v2]
                             (+ (* (dr v1) (dr v2))
                                (* (square r)
                                   (+ (* (dtheta v1) (dtheta v2))
                                      (* (expt (sin theta) 2)
                                         (dphi v1) (dphi v2))))))
          e_0 d:dr
          e_1 (* (/ 1 r) d:dtheta)
          e_2 (* (/ 1 (* r (sin theta))) d:dphi)
          orthonormal-spherical-vector-basis (down e_0 e_1 e_2)

          orthonormal-spherical-oneform-basis
          (e/vector-basis->dual orthonormal-spherical-vector-basis
                                spherical)

          orthonormal-spherical-basis
          (e/make-basis orthonormal-spherical-vector-basis
                        orthonormal-spherical-oneform-basis)]
      (is (= '(up (((partial 0) f) (up r0 theta0 phi0))
                  (/ (((partial 1) f) (up r0 theta0 phi0))
                     r0)
                  (/ (((partial 2) f) (up r0 theta0 phi0))
                     (* r0 (sin theta0))))
             (simplify
              ((orthonormal-spherical-oneform-basis
                ((e/gradient spherical-metric orthonormal-spherical-basis)
                 (e/literal-manifold-function 'f spherical)))
               R3-spherical-point)))
          "The components of the gradient of a scalar field are obtained
            using the dual basis (p157)")

      (let [v (+ (* (e/literal-manifold-function 'v↑0 spherical) e_0)
                 (* (e/literal-manifold-function 'v↑1 spherical) e_1)
                 (* (e/literal-manifold-function 'v↑2 spherical) e_2))]
        (is (= '(up
                 (/ (+ (* (sin theta0) (((partial 1) v↑2) (up r0 theta0 phi0)))
                       (* (v↑2 (up r0 theta0 phi0)) (cos theta0))
                       (* -1 (((partial 2) v↑1) (up r0 theta0 phi0))))
                    (* r0 (sin theta0)))
                 (/ (+ (* -1 r0 (sin theta0) (((partial 0) v↑2) (up r0 theta0 phi0)))
                       (* -1 (sin theta0) (v↑2 (up r0 theta0 phi0)))
                       (((partial 2) v↑0) (up r0 theta0 phi0)))
                    (* r0 (sin theta0)))
                 (/ (+ (* r0 (((partial 0) v↑1) (up r0 theta0 phi0)))
                       (v↑1 (up r0 theta0 phi0))
                       (* -1 (((partial 1) v↑0) (up r0 theta0 phi0))))
                    r0))
               (simplify
                ((orthonormal-spherical-oneform-basis
                  ((e/curl spherical-metric orthonormal-spherical-basis) v))
                 R3-spherical-point)))
            "The curl is a bit complicated (p158)")

        (testing "But the divergence and Laplacian are simpler"
          (is (= '(/ (+ (* r0 (sin theta0) (((partial 0) v↑0) (up r0 theta0 phi0)))
                        (* 2 (sin theta0) (v↑0 (up r0 theta0 phi0)))
                        (* (sin theta0) (((partial 1) v↑1) (up r0 theta0 phi0)))
                        (* (cos theta0) (v↑1 (up r0 theta0 phi0)))
                        (((partial 2) v↑2) (up r0 theta0 phi0)))
                     (* r0 (sin theta0)))
                 (simplify
                  (((e/divergence spherical-metric orthonormal-spherical-basis) v)
                   R3-spherical-point)))
              "divergence")

          (is (= '(/ (+ (* (expt r0 2)
                           (expt (sin theta0) 2)
                           (((expt (partial 0) 2) f) (up r0 theta0 phi0)))
                        (* 2 r0 (((partial 0) f) (up r0 theta0 phi0)) (expt (sin theta0) 2))
                        (* (((partial 1) f) (up r0 theta0 phi0)) (sin theta0) (cos theta0))
                        (* (expt (sin theta0) 2)
                           (((expt (partial 1) 2) f) (up r0 theta0 phi0)))
                        (((expt (partial 2) 2) f) (up r0 theta0 phi0)))
                     (* (expt r0 2) (expt (sin theta0) 2)))
                 (simplify
                  (((e/Laplacian spherical-metric orthonormal-spherical-basis)
                    (e/literal-manifold-function 'f spherical))
                   R3-spherical-point)))
              "Laplacian"))))))

(deftest wave-equation-tests
  (testing "The wave equation (p159)"
    (let [an-event ((point SR) (up 'ct0 'x0 'y0 'z0))

          a-vector (+ (* (e/literal-manifold-function 'v↑t SR) d:dct)
                      (* (e/literal-manifold-function 'v↑x SR) d:dx)
                      (* (e/literal-manifold-function 'v↑y SR) d:dy)
                      (* (e/literal-manifold-function 'v↑z SR) d:dz))

          ;; NOTE that this suite of tests differs from the book in that we
          ;; include the c^2 term in the code, and track it all the way
          ;; through. The book assumes c==1.
          g-Minkowski (fn [u v]
                        (+ (* -1 (square 'c) (dct u) (dct v))
                           (* (dx u) (dx v))
                           (* (dy u) (dy v))
                           (* (dz u) (dz v))))
          SR-vector-basis (down (* (/ 1 'c) d:dct) d:dx d:dy d:dz)
          SR-oneform-basis (up (* 'c dct) dx dy dz)
          SR-basis
          (e/make-basis SR-vector-basis
                        SR-oneform-basis)]
      (is (= '(+ (* -1 (expt c 2) (expt (v↑t (up ct0 x0 y0 z0)) 2))
                 (expt (v↑x (up ct0 x0 y0 z0)) 2)
                 (expt (v↑y (up ct0 x0 y0 z0)) 2)
                 (expt (v↑z (up ct0 x0 y0 z0)) 2))
             (simplify
              ((g-Minkowski a-vector a-vector) an-event)))
          "p159")

      (is (= '(down (down -1 0 0 0)
                    (down  0 1 0 0)
                    (down  0 0 1 0)
                    (down  0 0 0 1))
             (simplify
              ((g-Minkowski SR-vector-basis SR-vector-basis) an-event)))
          "We check that it is orthonormal with respect to the metric (p160)")

      (let [phi (e/literal-manifold-function 'phi SR)]
        (is (= '(/ (+ (* -1 (expt c 2) (((expt (partial 1) 2) phi) (up ct0 x0 y0 z0)))
                      (* -1 (expt c 2) (((expt (partial 2) 2) phi) (up ct0 x0 y0 z0)))
                      (* -1 (expt c 2) (((expt (partial 3) 2) phi) (up ct0 x0 y0 z0)))
                      (((expt (partial 0) 2) phi) (up ct0 x0 y0 z0)))
                   (expt c 2))
               (simplify
                (((e/Laplacian g-Minkowski SR-basis) phi) an-event)))
            "So, the Laplacian of a scalar field is the wave equation! (p160)"))

      (testing "Electrodynamics (p160)"
        (let [Faraday (fn [Ex Ey Ez Bx By Bz]
                        (+ (* Ex 'c (wedge dx dct))
                           (* Ey 'c (wedge dy dct))
                           (* Ez 'c (wedge dz dct))
                           (* Bx (wedge dy dz))
                           (* By (wedge dz dx))
                           (* Bz (wedge dx dy))))

              Maxwell (fn [Ex Ey Ez Bx By Bz]
                        (+ (* -1 Bx 'c (wedge dx dct))
                           (* -1 By 'c (wedge dy dct))
                           (* -1 Bz 'c (wedge dz dct))
                           (* Ex (wedge dy dz))
                           (* Ey (wedge dz dx))
                           (* Ez (wedge dx dy))))
              SR-star (e/Hodge-star g-Minkowski SR-basis)]
          (is (zero?
               (simplify
                (((- (SR-star (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
                     (Maxwell 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
                  (e/literal-vector-field 'u SR)
                  (e/literal-vector-field 'v SR))
                 an-event)))
              "And indeed, it transforms the Faraday tensor into the Maxwell
                   tensor (p161)")

          (testing "Defining the 4-current density J."
            ;; Charge density is a manifold function.  Current density is a
            ;; vector field having only spatial components
            (let [J (fn [charge-density Ix Iy Iz]
                      (- (* (/ 1 'c) (+ (* Ix dx) (* Iy dy) (* Iz dz)))
                         (* charge-density 'c dct)))

                  F (Faraday (e/literal-manifold-function 'Ex SR)
                             (e/literal-manifold-function 'Ey SR)
                             (e/literal-manifold-function 'Ez SR)
                             (e/literal-manifold-function 'Bx SR)
                             (e/literal-manifold-function 'By SR)
                             (e/literal-manifold-function 'Bz SR))

                  four-current (J (e/literal-manifold-function 'rho SR)
                                  (e/literal-manifold-function 'Ix SR)
                                  (e/literal-manifold-function 'Iy SR)
                                  (e/literal-manifold-function 'Iz SR))]
              ;; Maxwell's equations in the form language ar:  dF=0, d(*F)=fourpi *J
              (is (= '(+ (((partial 1) Bx) (up ct0 x0 y0 z0))
                         (((partial 2) By) (up ct0 x0 y0 z0))
                         (((partial 3) Bz) (up ct0 x0 y0 z0)))
                     (simplify
                      (((e/d F) d:dx d:dy d:dz) an-event)))
                  "div B = 0")

              (testing "The three mixed space and time components of
                            equation 10.13 are equation 10.16 (p163)"
                (is (= '(/ (+ (* c (((partial 2) Ez) (up ct0 x0 y0 z0)))
                              (* -1 c (((partial 3) Ey) (up ct0 x0 y0 z0)))
                              (((partial 0) Bx) (up ct0 x0 y0 z0)))
                           c)
                       (simplify
                        (((e/d F) (* (/ 1 'c) d:dct) d:dy d:dz) an-event)))
                    "curl E = -1/c dB/dt")

                (is (= '(/ (+ (* c (((partial 3) Ex) (up ct0 x0 y0 z0)))
                              (* -1 c (((partial 1) Ez) (up ct0 x0 y0 z0)))
                              (((partial 0) By) (up ct0 x0 y0 z0)))
                           c)
                       (simplify
                        (((e/d F) (* (/ 1 'c) d:dct) d:dz d:dx) an-event))))

                (is (= '(/ (+ (* c (((partial 1) Ey) (up ct0 x0 y0 z0)))
                              (* -1 c (((partial 2) Ex) (up ct0 x0 y0 z0)))
                              (((partial 0) Bz) (up ct0 x0 y0 z0)))
                           c)
                       (simplify
                        (((e/d F) (* (/ 1 'c) d:dct) d:dx d:dy) an-event)))))

              (is (= '(+ (* -4 pi (rho (up ct0 x0 y0 z0)))
                         (((partial 1) Ex) (up ct0 x0 y0 z0))
                         (((partial 2) Ey) (up ct0 x0 y0 z0))
                         (((partial 3) Ez) (up ct0 x0 y0 z0)))
                     (simplify
                      (((- (e/d (SR-star F)) (* 4 'pi (SR-star four-current)))
                        d:dx d:dy d:dz)
                       an-event)))
                  "div E = fourpi rho, The purely spatial component of
                       equation 10.14 is equation 10.17")

              (testing "And finally, the three mixed time and space
                            components of equation 10.14 are equation
                            10.18 (p164)"
                (is (= '(/ (+ (* -1 c (((partial 2) Bz) (up ct0 x0 y0 z0)))
                              (* c (((partial 3) By) (up ct0 x0 y0 z0)))
                              (* 4 pi (Ix (up ct0 x0 y0 z0)))
                              (((partial 0) Ex) (up ct0 x0 y0 z0)))
                           c)
                       (simplify
                        (((- (e/d (SR-star F)) (* 4 'pi (SR-star four-current)))
                          (* (/ 1 'c) d:dct) d:dy d:dz)
                         an-event)))
                    "curl B = 1/c dE/dt + fourpi I")

                (is (= '(/ (+ (* -1 c (((partial 3) Bx) (up ct0 x0 y0 z0)))
                              (* c (((partial 1) Bz) (up ct0 x0 y0 z0)))
                              (* 4 pi (Iy (up ct0 x0 y0 z0)))
                              (((partial 0) Ey) (up ct0 x0 y0 z0)))
                           c)
                       (simplify
                        (((- (e/d (SR-star F)) (* 4 'pi (SR-star four-current)))
                          (* (/ 1 'c) d:dct) d:dz d:dx)
                         an-event))))

                (is (= '(/ (+ (* -1 c (((partial 1) By) (up ct0 x0 y0 z0)))
                              (* c (((partial 2) Bx) (up ct0 x0 y0 z0)))
                              (* 4 pi (Iz (up ct0 x0 y0 z0)))
                              (((partial 0) Ez) (up ct0 x0 y0 z0)))
                           c)
                       (simplify
                        (((- (e/d (SR-star F)) (* 4 'pi (SR-star four-current)))
                          (* (/ 1 'c) d:dct) d:dx d:dy)
                         an-event)))))

              (testing "Lorentz force (p164)"
                (let [E (up (e/literal-manifold-function 'Ex SR)
                            (e/literal-manifold-function 'Ey SR)
                            (e/literal-manifold-function 'Ez SR))
                      B (up (e/literal-manifold-function 'Bx SR)
                            (e/literal-manifold-function 'By SR)
                            (e/literal-manifold-function 'Bz SR))
                      V (up 'V_x 'V_y 'V_z)]
                  (is (= '(up (+ (* V_y q (Bz (up ct0 x0 y0 z0)))
                                 (* -1 V_z q (By (up ct0 x0 y0 z0)))
                                 (* q (Ex (up ct0 x0 y0 z0))))
                              (+ (* -1 V_x q (Bz (up ct0 x0 y0 z0)))
                                 (* V_z q (Bx (up ct0 x0 y0 z0)))
                                 (* q (Ey (up ct0 x0 y0 z0))))
                              (+ (* V_x q (By (up ct0 x0 y0 z0)))
                                 (* -1 V_y q (Bx (up ct0 x0 y0 z0)))
                                 (* q (Ez (up ct0 x0 y0 z0)))))
                         (simplify
                          (* 'q (+ (E an-event) (e/cross-product V (B an-event))))))
                      "The 3-space force that results is a mess (p165)")

                  ;; TODO eta-inverse is not defined, so these will not run.
                  (comment
                    (let [Force (fn [charge F four-velocity component]
                                  (* -1 charge
                                     (e/contract
                                      (fn [a b]
                                        (e/contract
                                         (fn [e w]
                                           (* (w four-velocity)
                                              (F e a)
                                              (eta-inverse b component)))
                                         SR-basis))
                                      SR-basis)))
                          Ux (fn [beta]
                               (+ (* (/ 1 (sqrt (- 1 (square beta)))) d:dct)
                                  (* (/ beta (sqrt (- 1 (square beta)))) d:dx)))]
                      (is (= '(* q (Ex (up ct0 x0 y0 z0)))
                             (simplify
                              ((Force 'q F d:dct dx) an-event)))
                          "the force in the ˆx direction for a stationary
                            particle is...")

                      (is (= '(/ (+ (* -1 q v/c (Bz (up ct0 x0 y0 z0)))
                                    (* q (Ey (up ct0 x0 y0 z0))))
                                 (sqrt (+ 1 (* -1 (expt v:c 2)))))
                             ((Force 'q F (Ux 'v:c) dy) an-event))
                          "If we give a particle a more general timelike
                             4-velocity in the xˆ direction we can see how the
                             ˆy component of the force involves both the
                             electric and magnetic field")

                      (is (= '(/ (* q v:c (Ex (up ct0 x0 y0 z0)))
                                 (sqrt (+ 1 (* -1 (expt v:c 2)))))
                             ((Force ’q F (Ux 'v:c) dct) an-event))
                          "From ex 10.1b"))))))))))))
