#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.vector-calculus-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.function :as af]
            [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :refer [let-coordinates]]
            [emmy.calculus.derivative :refer [D]]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-calculus :as vc]
            [emmy.generic :as g :refer [+ - * / sin cos]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest old-vector-calculus-tests
  (let [f (fn [[x y z]]
            (s/up (identity x) (sin y) (cos z)))
        xyz (s/up 'x 'y 'z)]
    (is (= '(down
             (up 1 0 0)
             (up 0 (cos y) 0)
             (up 0 0 (* -1 (sin z))))
           (simplify ((D f) xyz))))
    (is (= '(up
             (up 1 0 0)
             (up 0 (cos y) 0)
             (up 0 0 (* -1 (sin z))))
           (simplify ((vc/Grad f) xyz))))

    (is (= '(up 0 (* -1 (sin y)) (* -1 (cos z)))
           (simplify ((vc/Lap f) xyz))))

    (is (= '(+ (cos y) (* -1 (sin z)) 1)
           (simplify ((vc/Div f) (s/up 'x 'y 'z)))))))

(deftest non-fdg-vector-operator-tests
  (testing "symbolic representations of Div, Curl, Grad, Lap are correct"
    (let [F (af/literal-function 'F '(-> (UP Real Real Real) Real))
          A (af/literal-function 'A '(-> (UP Real Real Real)
                                         (UP Real Real Real)))]
      (is (= '(up (((partial 0) F) (up x y z))
                  (((partial 1) F) (up x y z))
                  (((partial 2) F) (up x y z)))
             (simplify
              ((vc/Grad F) (s/up 'x 'y 'z)))))

      (is (= '(+ (((partial 0) A↑0) (up x y z))
                 (((partial 1) A↑1) (up x y z))
                 (((partial 2) A↑2) (up x y z)))
             (simplify
              ((vc/Div A) (s/up 'x 'y 'z)))))

      (is (= '(up (+ (((partial 1) A↑2) (up x y z))
                     (* -1 (((partial 2) A↑1) (up x y z))))
                  (+ (((partial 2) A↑0) (up x y z))
                     (* -1 (((partial 0) A↑2) (up x y z))))
                  (+ (((partial 0) A↑1) (up x y z))
                     (* -1 (((partial 1) A↑0) (up x y z)))))
             (simplify
              ((vc/Curl A) (s/up 'x 'y 'z)))))

      (is (= '(+ (((expt (partial 0) 2) F) (up x y z))
                 (((expt (partial 1) 2) F) (up x y z))
                 (((expt (partial 2) 2) F) (up x y z)))
             (simplify
              ((vc/Lap F) (s/up 'x 'y 'z)))))))

  (testing "Div, Curl, Grad, Lap identities"
    (let [F (af/literal-function 'F '(-> (UP Real Real Real) Real))
          G (af/literal-function 'G '(-> (UP Real Real Real) Real))
          A (af/literal-function 'A '(-> (UP Real Real Real)
                                         (UP Real Real Real)))]
      (is (= '(up 0 0 0)
             (simplify
              ((vc/Curl (vc/Grad F)) (s/up 'x 'y 'z))))
          "Curl of the gradient is zero!")

      (is (= 0 (simplify
                ((vc/Div (vc/Curl A)) (s/up 'x 'y 'z))))
          "divergence of curl is 0.")

      (is (= 0 (simplify
                ((- (vc/Div (vc/Grad F))
                    (vc/Lap F))
                 (s/up 'x 'y 'z))))
          "The Laplacian of a scalar field is the div of its gradient.")

      (is (= '(up 0 0 0)
             (simplify
              ((- (vc/Curl (vc/Curl A))
                  (- (vc/Grad (vc/Div A)) (vc/Lap A)))
               (s/up 'x 'y 'z)))))

      (is (= 0 (simplify
                ((- (vc/Div (* F (vc/Grad G)))
                    (+ (* F (vc/Lap G))
                       (g/dot-product (vc/Grad F)
                                      (vc/Grad G))))
                 (s/up 'x 'y 'z))))))))

(deftest vector-operator-tests
  (let [spherical m/R3-rect]
    (let-coordinates [[r theta phi] spherical]
      (let [spherical-point ((m/point spherical) (up 'r 'theta 'phi))
            spherical-basis (b/coordinate-system->basis spherical)
            spherical-metric (fn [v1 v2]
                               (+ (* (dr v1) (dr v2))
                                  (* (g/square r)
                                     (+ (* (dtheta v1) (dtheta v2))
                                        (* (g/expt (sin theta) 2)
                                           (dphi v1) (dphi v2))))))

            ;; normalized spherical basis
            e_0 d:dr
            e_1 (* (/ 1 r) d:dtheta)
            e_2 (* (/ 1 (* r (sin theta))) d:dphi)

            orthonormal-spherical-vector-basis (down e_0 e_1 e_2)

            orthonormal-spherical-oneform-basis
            (b/vector-basis->dual orthonormal-spherical-vector-basis
                                  spherical)

            orthonormal-spherical-basis
            (b/make-basis orthonormal-spherical-vector-basis
                          orthonormal-spherical-oneform-basis)

            v (+ (* (m/literal-manifold-function 'v↑0 spherical) e_0)
                 (* (m/literal-manifold-function 'v↑1 spherical) e_1)
                 (* (m/literal-manifold-function 'v↑2 spherical) e_2))]

        (testing "testing that basis is normalized"
          (is (= 1 ((spherical-metric e_0 e_0) spherical-point)))
          (is (= 1 (simplify
                    ((spherical-metric e_1 e_1) spherical-point))))
          (is (= 1 (simplify
                    ((spherical-metric e_2 e_2) spherical-point))))

          (is (= 0 ((spherical-metric e_0 e_1) spherical-point)))
          (is (= 0 ((spherical-metric e_0 e_2) spherical-point)))
          (is (= 0 ((spherical-metric e_1 e_2) spherical-point))))

        (testing "Test of gradient"
          (is (= '(up (((partial 0) f) (up r theta phi))
                      (/ (((partial 1) f) (up r theta phi)) r)
                      (/ (((partial 2) f) (up r theta phi)) (* r (sin theta))))
                 (simplify
                  ((orthonormal-spherical-oneform-basis
                    ((vc/gradient spherical-metric spherical-basis)
                     (m/literal-manifold-function 'f spherical)))
                   spherical-point)))))

        (testing "curl"
          (is (= '(up (/ (+ (* (sin theta) (((partial 1) v↑2) (up r theta phi)))
                            (* (v↑2 (up r theta phi)) (cos theta))
                            (* -1 (((partial 2) v↑1) (up r theta phi))))
                         (* r (sin theta)))
                      (/ (+ (* -1 r (sin theta) (((partial 0) v↑2) (up r theta phi)))
                            (* -1 (sin theta) (v↑2 (up r theta phi)))
                            (((partial 2) v↑0) (up r theta phi)))
                         (* r (sin theta)))
                      (/ (+ (* r (((partial 0) v↑1) (up r theta phi)))
                            (v↑1 (up r theta phi))
                            (* -1 (((partial 1) v↑0) (up r theta phi))))
                         r))
                 (simplify
                  ((orthonormal-spherical-oneform-basis
                    ((vc/curl spherical-metric orthonormal-spherical-basis) v))
                   spherical-point)))))

        (testing "divergence"
          (is (= '(/ (+ (* r (sin theta) (((partial 0) v↑0) (up r theta phi)))
                        (* 2 (sin theta) (v↑0 (up r theta phi)))
                        (* (sin theta) (((partial 1) v↑1) (up r theta phi)))
                        (* (cos theta) (v↑1 (up r theta phi)))
                        (((partial 2) v↑2) (up r theta phi)))
                     (* r (sin theta)))
                 (simplify
                  (((vc/divergence spherical-metric orthonormal-spherical-basis) v)
                   spherical-point)))))

        (let [phi (m/literal-manifold-function 'phi spherical)]
          (testing "laplacian"
            (is (= '(/ (+ (* (expt r 2) (expt (sin theta) 2) (((expt (partial 0) 2) phi) (up r theta phi)))
                          (* 2 r (expt (sin theta) 2) (((partial 0) phi) (up r theta phi)))
                          (* (expt (sin theta) 2) (((expt (partial 1) 2) phi) (up r theta phi)))
                          (* (sin theta) (cos theta) (((partial 1) phi) (up r theta phi)))
                          (((expt (partial 2) 2) phi) (up r theta phi)))
                       (* (expt r 2) (expt (sin theta) 2)))
                   (simplify
                    (((vc/Laplacian spherical-metric orthonormal-spherical-basis)
                      phi)
                     spherical-point))))))))))

(deftest wave-equation-tests
  (testing "Obtaining the wave equation."
    (let [SR m/R4-rect]
      (let-coordinates [[t x y z] SR]
        (let [c 'c
              an-event ((m/point SR) (up 't0 'x0 'y0 'z0))
              g-Minkowski (fn [u v]
                            (+ (* -1 (g/square c) (dt u) (dt v))
                               (* (dx u) (dx v))
                               (* (dy u) (dy v))
                               (* (dz u) (dz v))))
              SR-vector-basis
              (down (* (/ 1 c) d:dt) d:dx d:dy d:dz)

              SR-oneform-basis
              (up (* c dt) dx dy dz)

              SR-basis
              (b/make-basis SR-vector-basis
                            SR-oneform-basis)]
          (is (= '(down (down -1 0 0 0)
                        (down  0 1 0 0)
                        (down  0 0 1 0)
                        (down  0 0 0 1))
                 (simplify
                  (s/mapr
                   (fn [u]
                     (s/mapr (fn [v]
                               ((g-Minkowski u v) an-event))
                             SR-vector-basis))
                   SR-vector-basis))))

          (let [phi (m/literal-manifold-function 'phi SR)]
            (is (= '(/ (+ (* -1 (expt c 2) (((expt (partial 1) 2) phi) (up t0 x0 y0 z0)))
                          (* -1 (expt c 2) (((expt (partial 2) 2) phi) (up t0 x0 y0 z0)))
                          (* -1 (expt c 2) (((expt (partial 3) 2) phi) (up t0 x0 y0 z0)))
                          (((expt (partial 0) 2) phi) (up t0 x0 y0 z0)))
                       (expt c 2))
                   (simplify
                    (((vc/Laplacian g-Minkowski SR-basis) phi) an-event))))))))))
