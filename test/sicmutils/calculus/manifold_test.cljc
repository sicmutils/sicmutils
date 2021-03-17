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

(ns sicmutils.calculus.manifold-test
  (:refer-clojure :exclude [* - / +])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.function :as f :refer [compose]]
            [sicmutils.generic :as g :refer [cos sin * - / +]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def s-freeze
  (comp v/freeze g/simplify))

(defn rt
  "Round trip the supplied coordinates through the supplied `coordinate-system`."
  [coordinate-system coords]
  (->> coords
       (m/coords->point coordinate-system)
       (m/point->coords coordinate-system)))

(defn cacheless-rt
  "Round trip the supplied coordinates through the `coordinate-system` without
  touching any internal cache."
  [coordinate-system coords]
  (let [p (-> (m/coords->point coordinate-system coords)
              (assoc :coordinate-representation (atom {})))]
    (m/point->coords coordinate-system p)))

(deftest Rn-manifold-tests
  (testing "R1-rect"
    (testing "check-coordinates"
      (is (m/check-coordinates m/R1-rect 1))
      (is (m/check-coordinates m/R1-rect (up 1))))

    (let [Tp (m/coords->point m/R1-rect 3)]
      (is (m/check-point m/R1-rect Tp))
      (is (= 99 (m/point->coords
                 m/R1-rect
                 (m/coords->point m/R1-rect 99))))))

  (testing "R2-rect"
    (testing "check-coordinates"
      (is (not (m/check-coordinates m/R2-rect (up 1))))
      (is (m/check-coordinates m/R2-rect (up 1 2)))
      (is (not (m/check-coordinates m/R2-rect (up 1 2 3))))
      (is (not (m/check-coordinates m/R2-rect 99))))

    (testing "coords->point"
      (let [m (m/coords->point m/R2-rect (up 3 4))]
        (is (= (up 5 (g/atan 4 3))
               (m/point->coords m/R2-polar m))
            "TODO make a generative test."))

      (let [p (m/coords->point m/R2-rect (up 3 4))]
        (is (= (up 3 4)
               (m/get-coordinates p m/R2-rect #(u/exception "")))
            "the throw continuation is meant to assert that the thunk is not
        called when retrieving the coordinates from the system with which the
        manifold-point was created.")

        (is (m/check-point m/R2-rect p))
        (is (= ::m/manifold-point (v/kind p)))
        (is (m/manifold-point? p))
        (is (not (v/numerical? p))))))

  (testing "R2-polar"
    (testing "check-coordinates"
      (is (not (m/check-coordinates m/R2-polar (up 1))))
      (is (m/check-coordinates m/R2-polar (up 1 2)))
      (is (not (m/check-coordinates m/R2-polar (up 1 2 3))))
      (is (not (m/check-coordinates m/R2-polar 99))))

    (testing "coords->point"
      (let [p (m/coords->point m/R2-polar (up 1 2))]
        (is (= (up 1 2)
               (m/get-coordinates p m/R2-polar #(u/exception "")))))))

  (testing "R2-rect <-> R2-polar"
    (let [Pr (m/coords->point m/R2-rect (up (Math/sqrt 2) (Math/sqrt 2)))
          xy (m/coords->point m/R2-rect (up 'x 'y))
          rt (m/coords->point m/R2-polar (up 'ρ 'θ))]
      (is (= (up 'x 'y) (m/point->coords m/R2-rect xy)))
      (is (ish? (up 2 (/ Math/PI 4)) (m/point->coords m/R2-polar Pr)))
      (is (= (up 'ρ 'θ) (m/point->coords m/R2-polar rt)))
      (is (= (up (g/sqrt (+ (g/square 'x) (g/square 'y)))
                 (g/atan 'y 'x)) (m/point->coords m/R2-polar xy)))
      (is (= (up (* 'ρ (cos 'θ)) (* 'ρ (sin 'θ))) (m/point->coords m/R2-rect rt))))))

(defn run-S2-tests
  [prefix S2-spherical S2-tilted S2-Riemann S2-gnomonic S2-stereographic]
  (testing prefix
    (testing "tilted -> spherical"
      (let [point (m/coords->point m/S2p-spherical (up 'theta 'phi))]
        (is (= '(up (* (cos phi) (sin theta))
                    (* (sin theta) (sin phi))
                    (cos theta))
               (v/freeze
                (m/manifold-point-representation point))))

        (is (= '(up theta phi)
               (s-freeze
                ((compose (m/chart S2-spherical)
                          (m/point S2-spherical))
                 (up 'theta 'phi))))
            "round trip back to coords")

        (is (= (up 1 0)
               ((compose (m/chart S2-spherical)
                         (m/point S2-spherical))
                (up 1 0))))

        (is (= (up 0 1) (rt S2-spherical (up 0 1)))
            "Even though this point is singular, the cache takes care of getting
            the right result.")

        (is (= (up 0 0) (cacheless-rt S2-spherical (up 0 1)))
            "Even though this point is singular, the cache takes care of getting
            the right result.")))

    (testing "Riemann"
      (let [point (m/coords->point S2-Riemann (up 'x 'y))]
        (is (= '(up (/ (* 2 x)
                       (+ (expt x 2) (expt y 2) 1))
                    (/ (* 2 y)
                       (+ (expt x 2) (expt y 2) 1))
                    (/ (+ (expt x 2) (expt y 2) -1)
                       (+ (expt x 2) (expt y 2) 1)))
               (s-freeze
                (m/manifold-point-representation point))))

        (is  (= '(up (/ (* 2 x)
                        (+ (expt x 2) (expt y 2) 1))
                     (/ (* 2 y)
                        (+ (expt x 2) (expt y 2) 1))
                     (/ (+ (expt x 2) (expt y 2) -1)
                        (+ (expt x 2) (expt y 2) 1)))
                (s-freeze
                 (m/manifold-point-representation
                  ((compose (m/point S2-Riemann)
                            (m/chart S2-Riemann))
                   point))))))

      (is (= (up 'x 'y)
             ((compose (m/chart S2-Riemann)
                       (m/point S2-Riemann))
              (up 'x 'y))))

      (is (= '(up (cos theta) (sin theta) 0)
             (s-freeze
              (m/manifold-point-representation
               ((m/point S2-Riemann)
                (up (cos 'theta) (sin 'theta))))))
          "The equator is invariant."))

    (testing "gnomonic"
      (let [point (m/coords->point S2-gnomonic (up 'x 'y))]
        (is (= '(up (/ x (sqrt (+ (expt x 2) (expt y 2) 1)))
                    (/ y (sqrt (+ (expt x 2) (expt y 2) 1)))
                    (/ 1 (sqrt (+ (expt x 2) (expt y 2) 1))))
               (s-freeze
                (m/manifold-point-representation point))))

        (is  (= '(up (/ x (sqrt (+ (expt x 2) (expt y 2) 1)))
                     (/ y (sqrt (+ (expt x 2) (expt y 2) 1)))
                     (/ 1 (sqrt (+ (expt x 2) (expt y 2) 1))))
                (s-freeze
                 (m/manifold-point-representation
                  ((compose (m/point S2-gnomonic)
                            (m/chart S2-gnomonic))
                   point))))))

      (is (= (up 'x 'y)
             ((compose (m/chart S2-gnomonic)
                       (m/point S2-gnomonic))
              (up 'x 'y))))

      (comment
        ;; TODO this is busted until we get the simplifier fine with expressions
        ;; like `(sqrt 2)`.
        (is (= '(up (/ (cos theta) (sqrt 2))
                    (/ (sin theta) (sqrt 2))
                    (/ 1 (sqrt 2)))
               (s-freeze
                (m/manifold-point-representation
                 ((m/point S2-gnomonic)
                  (up (g/cos 'theta) (g/sin 'theta))))))))

      ;; The unit circle on the plane represents the intersection of S2 and z
      ;; = (/ 1 (sqrt 2))

      ;; Straight lines in the gnomonic coordinates are geodesics. We compute a
      ;; straight line, then transform it back to stereographic coordinates.
      (let [q ((m/point S2-stereographic) (up -1.5 1.5))
            p ((m/point S2-stereographic) (up 1.5 0))]
        (is (= '(up
                 (/ (+ (* 3.257142857142857 t) -0.8571428571428571)
                    (+ (sqrt (+ (* 11.343673469387754 (expt t 2))
                                (* -7.053061224489795 t)
                                2.4693877551020407))
                       -1))
                 (/ (+ (* -0.8571428571428571 t) 0.8571428571428571)
                    (+ (sqrt (+ (* 11.343673469387754 (expt t 2))
                                (* -7.053061224489795 t)
                                2.4693877551020407))
                       -1)))
               (s-freeze
                ((m/chart S2-stereographic)
                 ((m/point S2-gnomonic)
                  (+ (* 't ((m/chart S2-gnomonic) p))
                     (* (- 1 't) ((m/chart S2-gnomonic) q))))))))))))

(deftest S1-tests
  (testing "S1"
    (let [point (m/coords->point m/S1-circular 'theta)]
      (is (= (up (g/cos 'theta) (g/sin 'theta))
             (m/manifold-point-representation point))))

    (is (v/= 'theta
             (g/simplify
              ((compose (m/chart m/S1-circular)
                        (m/point m/S1-circular))
               'theta))))

    (is (v/= '(atan (cos theta) (* -1 (sin theta)))
             (g/simplify
              ((compose (m/chart m/S1-circular)
                        (m/point m/S1-tilted))
               'theta)))))

  (testing "S1-slope"
    (let [point ((m/point m/S1-slope) 's)]
      (is (= '(up (/ (* 2 s)
                     (+ (expt s 2) 1))
                  (/ (+ (expt s 2) -1)
                     (+ (expt s 2) 1)))
             (v/freeze
              (g/simplify
               (m/manifold-point-representation point)))))

      (is (= '(up (/ (* 2 s)
                     (+ (expt s 2) 1))
                  (/ (+ (expt s 2) -1)
                     (+ (expt s 2) 1)))
             (v/freeze
              (g/simplify
               (m/manifold-point-representation
                ((compose (m/point m/S1-slope)
                          (m/chart m/S1-slope))
                 point)))))))

    (is (= 's ((compose (m/chart m/S1-slope)
                        (m/point m/S1-slope))
               's)))))

(deftest S2-tests
  (run-S2-tests "S2"
                m/S2-spherical m/S2-tilted m/S2-Riemann
                m/S2-gnomonic m/S2-stereographic)

  (run-S2-tests "S2p"
                m/S2p-spherical m/S2p-tilted m/S2p-Riemann
                m/S2p-gnomonic m/S2p-stereographic)

  (testing "The simplifier can't show that these are the same, yet, but they
  are! S2 and S2p behave differently so we have two separate tests."
    (testing "S2p-tilted->spherical and back"
      (is (= '(up (atan (sqrt (+ (* (expt (sin theta) 2)
                                    (expt (cos phi) 2))
                                 (expt (cos theta) 2)))
                        (* (sin theta) (sin phi)))
                  (atan (* -1 (cos theta))
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2p-spherical)
                        (m/point m/S2p-tilted))
               (up 'theta 'phi)))))

      (is (= '(up (atan (sqrt (+ (* (expt (sin theta) 2)
                                    (expt (cos phi) 2))
                                 (expt (cos theta) 2)))
                        (* -1 (sin theta) (sin phi)))
                  (atan (cos theta)
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2p-tilted)
                        (m/point m/S2p-spherical))
               (up 'theta 'phi))))))

    (testing "S2-tilted->spherical and back"
      (is (= '(up (acos (* (sin theta)
                           (sin phi)))
                  (atan (* -1 (cos theta))
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2-spherical)
                        (m/point m/S2-tilted))
               (up 'theta 'phi)))))

      (is (= '(up (acos (* -1 (sin theta)
                           (sin phi)))
                  (atan (cos theta)
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2-tilted)
                        (m/point m/S2-spherical))
               (up 'theta 'phi))))))))

(deftest S3-tests
  (testing "S3-{spherical,tilted}"
    (is (= '(up a b c)
           (s-freeze
            ((compose (m/chart m/S3-spherical)
                      (m/point m/S3-spherical))
             (up 'a 'b 'c)))))

    (is (= (up 0 0 0)
           (g/simplify
            (- (up (g/atan
                    (g/sqrt
                     (+ (* (g/expt (g/sin 'b) 2) (g/expt (g/sin 'c) 2) (g/expt (g/cos 'a) 2))
                        (* (g/expt (g/sin 'c) 2) (g/expt (g/cos 'b) 2))
                        (g/expt (g/cos 'c) 2)))
                    (* (g/sin 'c) (g/sin 'b) (g/sin 'a)))
                   (g/atan (g/sqrt (+ (* (g/expt (g/sin 'b) 2) (g/expt (g/sin 'a) 2) (g/expt (g/cos 'c) 2))
                                      (g/expt (g/cos 'a) 2)))
                           (* (g/sin 'a) (g/cos 'b)))
                   (g/atan (* -1 (g/cos 'a)) (* (g/sin 'a) (g/cos 'c) (g/sin 'b))))
               ((compose (m/chart m/S3-spherical)
                         (m/point m/S3-tilted))
                (up 'a 'b 'c))))))

    (is (= (up 0 0 0)
           ((compose (m/chart m/S3-spherical)
                     (m/point m/S3-spherical))
            (up 0 0 0)))
        "NOTE: Should be warned singular!"))

  (testing "S3-{gnomonic,stereographic}"
    ;; S3 is one-to-one with the quaternions.
    ;; We interpret the first three components of the embedding space as the
    ;; i,j,k imaginary party and the 4th component as the real part.
    ;; The gnomonic projection removes the double-cover of quaternions to rotations.
    ;; The solid unit-sphere of the stereographic projection from the south pole likewise.
    (is (= '(up (/ (* 2 x) (+ (expt x 2) (expt y 2) (expt z 2) -1))
                (/ (* 2 y) (+ (expt x 2) (expt y 2) (expt z 2) -1))
                (/ (* 2 z) (+ (expt x 2) (expt y 2) (expt z 2) -1)))
           (s-freeze
            ((m/chart m/S3-gnomonic)
             ((m/point m/S3-stereographic)
              (up 'x 'y 'z))))))

    (is  (= '(up (/ x (+ (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)) -1))
                 (/ y (+ (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)) -1))
                 (/ z (+ (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)) -1)))
            (s-freeze
             ((m/chart m/S3-stereographic)
              ((m/point m/S3-gnomonic)
               (up 'x 'y 'z))))))

    (is (= '(sqrt (/ (+ (expt x 2) (expt y 2) (expt z 2))
                     (+ (expt x 2) (expt y 2) (expt z 2)
                        (* -2 (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)))
                        2)))
           (s-freeze
            (g/abs
             ((m/chart m/S3-stereographic)
              ((m/point m/S3-gnomonic)
               (up 'x 'y 'z)))))))))

(deftest SO3-tests
  (testing "SO(3)"
    (is (= '(up theta phi psi)
           (v/freeze
            (g/simplify
             ((f/compose (m/chart m/Euler-angles)
                         (m/point m/alternate-angles)
                         (m/chart m/alternate-angles)
                         (m/point m/Euler-angles))
              (up 'theta 'phi 'psi))))))

    (is (= '(up (asin (* (sin theta) (cos psi)))
                (atan (+ (* (cos psi) (cos theta) (sin phi))
                         (* (cos phi) (sin psi)))
                      (+ (* (cos psi) (cos phi) (cos theta))
                         (* -1 (sin psi) (sin phi))))
                (atan (* -1 (sin theta) (sin psi)) (cos theta)))
           (v/freeze
            (g/simplify
             ((f/compose (m/chart m/alternate-angles)
                         (m/point m/Euler-angles))
              (up 'theta 'phi 'psi))))))))
