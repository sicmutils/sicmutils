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

(ns sicmutils.fdg.ch7-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [sicmutils.env :as e :refer [+ - * / zero?
                                         D d freeze simplify partial
                                         up down exp
                                         point chart wedge
                                         R2-rect R2-polar R3-rect
                                         R1-rect S2-spherical
                                         let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.curvature-test :refer [S2-Christoffel]]
            [sicmutils.operator :as o]
            [sicmutils.value :as v]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(defn F->directional-derivative
  [F]
  (fn [v]
    (fn [u]
      (fn [f]
        (fn [m]
          (let [g (fn [delta]
                    (- ((u f) m) (((((F v) delta) u) f) m)))]
            ((D g) 0)))))))

(defn F-Lie
  [phi]
  (fn [v]
    (fn [delta]
      (e/pushforward-vector ((phi v) delta) ((phi v) (- delta))))))

(defn phi
  [coordinate-system order]
  (fn [v]
    (fn [delta]
      (fn [m]
        ((point coordinate-system)
         (e/series:sum (((exp (* delta v)) (chart coordinate-system)) m)
                       order))))))

(defn Lie-directional
  [coordinate-system order]
  (let [Phi (phi coordinate-system order)]
    (F->directional-derivative (F-Lie Phi))))

(deftest section-7-1
  (let [v (e/literal-vector-field 'v-rect R3-rect)
        w (e/literal-vector-field 'w-rect R3-rect)
        f (e/literal-manifold-function 'f-rect R3-rect)]
    (is (zero?
         (simplify
          ((- ((((Lie-directional R3-rect 2) v) w) f)
              ((e/commutator v w) f))
           ((point R3-rect) (up 'x0 'y0 'z0)))))))

  (e/let-coordinates
   [[x y z] R3-rect]
   (let [a (e/literal-manifold-function 'alpha R3-rect)
         b (e/literal-manifold-function 'beta R3-rect)
         c (e/literal-manifold-function 'gamma R3-rect)
         theta (+ (* a dx) (* b dy) (* c dz))
         omega (+ (* a (wedge dy dz))
                  (* b (wedge dz dx))
                  (* c (wedge dx dy)))
         X (e/literal-vector-field 'X-rect R3-rect)
         Y (e/literal-vector-field 'Y-rect R3-rect)
         Z (e/literal-vector-field 'Z-rect R3-rect)
         V (e/literal-vector-field 'V-rect R3-rect)
         R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))]
     (is (= :sicmutils.calculus.vector-field/vector-field (v/kind V)))
     (is (= :sicmutils.operator/operator (v/kind (e/Lie-derivative V))))
     (is (= :sicmutils.calculus.form-field/oneform-field (v/kind theta)))
     (is (= 1 (:rank (o/context theta))))
     (is (= 2 (:rank (o/context omega))))

     ;; if you look at the LH and RH of the subtraction, you will observe that
     ;; this is nontrivial :)
     (is (zero?
          (simplify
           (((- ((e/Lie-derivative V) (d theta))
                (d ((e/Lie-derivative V) theta)))
             X Y)
            R3-rect-point))))

     (is (zero?
          (simplify (((- ((e/Lie-derivative V) (d omega))
                         (d ((e/Lie-derivative V) omega)))
                      X Y Z)
                     R3-rect-point))))
     (is (zero?
          (simplify
           ((((- (e/commutator (e/Lie-derivative X) (e/Lie-derivative Y))
                 (e/Lie-derivative (e/commutator X Y)))
              theta)
             Z)
            R3-rect-point))))

     (is (zero?
          (simplify
           ((((- (e/commutator (e/Lie-derivative X) (e/Lie-derivative Y))
                 (e/Lie-derivative (e/commutator X Y)))
              omega)
             Z V)
            R3-rect-point)))))))

(deftest section-7-1b
  (e/let-coordinates
   [[x y z] R3-rect]
   (let [Jz (- (* x d:dy) (* y d:dx))]
     (is (= '((((partial 1) f-rect) (up 1 0 0))
              (* a (((partial 0) f-rect) (up 1 0 0)))
              (* (/ -1 2) (expt a 2) (((partial 1) f-rect) (up 1 0 0)))
              (* (/ -1 6) (expt a 3) (((partial 0) f-rect) (up 1 0 0)))
              (* (/ 1 24) (expt a 4) (((partial 1) f-rect) (up 1 0 0))))
            (freeze
             (simplify
              (take 5
                    (seq
                     ((((exp (* 'a (e/Lie-derivative Jz))) d:dy)
                       (e/literal-manifold-function 'f-rect R3-rect))
                      ((point R3-rect) (up 1 0 0))))))))))))

(deftest section-7-1c
  (e/let-coordinates
   [[x y z] R3-rect]
   (let [X (e/literal-vector-field 'X-rect R3-rect)
         Y (e/literal-vector-field 'Y-rect R3-rect)
         Z (e/literal-vector-field 'Z-rect R3-rect)
         a (e/literal-manifold-function 'alpha R3-rect)
         b (e/literal-manifold-function 'beta R3-rect)
         c (e/literal-manifold-function 'gamma R3-rect)
         omega (+ (* a (wedge dx dy))
                  (* b (wedge dy dz))
                  (* c (wedge dz dx)))
         L1 (fn [X]
              (fn [omega]
                (+ ((e/interior-product X) (d omega))
                   (d ((e/interior-product X) omega)))))]
     (is (v/zero?
          (simplify
           ((- (((e/Lie-derivative X) omega) Y Z)
               (((L1 X) omega) Y Z))
            ((point R3-rect) (up 'x0 'y0 'z0)))))))))

(defn F-parallel [omega phi coordinate-system]
  (fn [v]
    (fn [delta]
      (fn [u]
        (fn [f]
          (fn [m]
            (let [basis (e/coordinate-system->basis coordinate-system)
                  etilde (e/basis->oneform-basis basis)
                  e (e/basis->vector-basis basis)
                  m0 (((phi v) (- delta)) m)
                  Aij (+ (v/one-like ((omega v) m0))
                         (* delta (- ((omega v) m0))))
                  ui ((etilde u) m0)]
              (* ((e f) m)
                 (* Aij ui)))))))))

(deftest section-7-2
  (e/let-coordinates
   [[x y] R2-rect
    [r theta] R2-polar]
   (let [R2-rect-basis (e/coordinate-system->basis R2-rect)
         R2-polar-basis (e/coordinate-system->basis R2-polar)
         R2-rect-Christoffel (e/make-Christoffel
                              (let [zero (fn [m] 0)]
                                (down (down (up zero zero)
                                            (up zero zero))
                                      (down (up zero zero)
                                            (up zero zero))))
                              R2-rect-basis)
         R2-rect-Cartan (e/Christoffel->Cartan R2-rect-Christoffel)
         R2-polar-Cartan (e/Cartan-transform R2-rect-Cartan R2-polar-basis)
         circular (- (* x d:dy) (* y d:dx))
         f (e/literal-manifold-function 'f-rect R2-rect)
         R2-rect-point ((point R2-rect) (up 'x0 'y0))]

     (is (e/= '(((partial 1) f-rect) (up x0 y0))
              (simplify
               (((((e/covariant-derivative R2-rect-Cartan) d:dx)
                  circular)
                 f)
                R2-rect-point))))

     (is (e/= '(((partial 1) f-rect) (up x0 y0))
              (simplify
               ((d:dy f) R2-rect-point))))
     (is (e/= '(((partial 1) f-rect) (up x0 y0))
              (simplify
               (((((e/covariant-derivative R2-polar-Cartan) d:dx) circular) f)
                R2-rect-point))))

     (let [V (e/literal-vector-field 'V-rect R2-rect)
           W (e/literal-vector-field 'W-rect R2-rect)]
       (is (zero?
            (simplify
             (((((- (e/covariant-derivative R2-rect-Cartan)
                    (e/covariant-derivative R2-polar-Cartan))
                 V)
                W)
               f)
              R2-rect-point))))))))

(defn transform
  "P.109"
  [tilt]
  (fn [[colat long]]
    (let [x (* (e/sin colat) (e/cos long))
          y (* (e/sin colat) (e/sin long))
          z    (e/cos colat)
          [vp0 vp1 vp2] ((e/rotate-x tilt) (up x y z))
          colatp (e/acos vp2)
          longp (e/atan vp1 vp0)]
      (up colatp longp))))

(defn tilted-path
  "P.110. Use `letfn` internally so we don't bind `coords` in the namespace; this
  keeps it a named private definition."
  [source target tilt]
  (let [xform (transform tilt)]
    (letfn [(coords [t]
              (xform (up (/ e/pi 2) t)))]
      (e/compose (e/point target)
                 coords
                 (e/chart source)))))

(def gamma
  (e/compose (e/point S2-spherical)
             (up (e/literal-function 'alpha)
                 (e/literal-function 'beta))
             (e/chart R1-rect)))

(deftest section-7-3
  (testing "Parallel transport on a sphere, p106"
    (let-coordinates [[theta phi] S2-spherical
                      t           R1-rect]
      (let [sphere   e/S2
            S2-basis (e/coordinate-system->basis S2-spherical)
            basis-over-gamma (e/basis->basis-over-map gamma S2-basis)
            u_gamma (* (up (e/compose (e/literal-function 'u↑0)
                                      (e/chart R1-rect))
                           (e/compose (e/literal-function 'u↑1)
                                      (e/chart R1-rect)))
                       (e/basis->vector-basis basis-over-gamma))
            S2C           (S2-Christoffel S2-basis theta)
            sphere-Cartan (e/Christoffel->Cartan S2C)]
        (is (= '(up (+ (* -1
                          (cos (alpha tau))
                          (sin (alpha tau))
                          ((D beta) tau)
                          (u↑1 tau))
                       ((D u↑0) tau))
                    (/ (+ (* (cos (alpha tau)) ((D beta) tau) (u↑0 tau))
                          (* (cos (alpha tau)) (u↑1 tau) ((D alpha) tau))
                          (* (sin (alpha tau)) ((D u↑1) tau)))
                       (sin (alpha tau))))
               (e/freeze
                (simplify
                 (e/mapr
                  (fn [omega]
                    ((omega
                      (((e/covariant-derivative sphere-Cartan gamma)
                        d:dt)
                       u_gamma))
                     ((e/point R1-rect) 'tau)))
                  (e/basis->oneform-basis basis-over-gamma)))))))))

  (testing "on a great circle, p109"
    (let-coordinates [[theta phi] S2-spherical
                      t           R1-rect]
      (let [S2-basis       (e/coordinate-system->basis S2-spherical)
            S2-Christoffel (S2-Christoffel S2-basis theta)
            sphere-Cartan  (e/Christoffel->Cartan S2-Christoffel)
            g (fn [gamma Cartan]
                (let [omega ((e/Cartan->forms
                              (e/Cartan->Cartan-over-map Cartan gamma))
                             ((e/differential gamma) d:dt))]
                  (fn []
                    (fn [[t u]]
                      (let [t-point ((e/point R1-rect) t)]
                        (up 1 (* -1 (omega t-point) u)))))))
            integrator    (e/state-advancer
                           (g (tilted-path R1-rect S2-spherical 1)
                              sphere-Cartan))
            initial-state (up 0 (* ((D (transform 1)) (up (/ e/pi 2) 0)) (up 1 0)))]
        (is (ish? (up 1.570796326794894
                      (up 0.9999999999545687 -1.676680708092223E-10))
                  (integrator initial-state (/ e/pi 2))))

        (is (ish? (up 1.0 (up 0.7651502649161671 0.9117920274079562))
                  (integrator initial-state 1)))

        (is (ish? (up 0.7651502649370375 0.9117920272004736)
                  (* ((D (transform 1)) (up (/ e/pi 2) 1)) (up 1 0))))))))

(deftest section-7-4
  (let-coordinates [[theta phi] S2-spherical
                    t           R1-rect]
    (let [S2-basis       (e/coordinate-system->basis S2-spherical)
          S2-Christoffel (S2-Christoffel S2-basis theta)
          sphere-Cartan  (e/Christoffel->Cartan S2-Christoffel)]
      (testing "geodesic motion, p111"
        (is (= '(up (+ (* -1 (cos (alpha t0)) (sin (alpha t0)) (expt ((D beta) t0) 2))
                       (((expt D 2) alpha) t0))
                    (/ (+ (* 2 (cos (alpha t0)) ((D beta) t0) ((D alpha) t0))
                          (* (sin (alpha t0)) (((expt D 2) beta) t0)))
                       (sin (alpha t0))))
               (freeze
                (simplify
                 (((((e/covariant-derivative sphere-Cartan gamma)
                     d:dt)
                    ((e/differential gamma) d:dt))
                   (e/chart S2-spherical))
                  ((e/point R1-rect) 't0)))))))))

  (testing "Lagrange equation on unit sphere, p112"
    (let [Lfree      (fn [[_ _ v]]
                       (* (/ 1 2) (e/square v)))
          sphere->R3 (fn [[_ [theta phi]]]
                       (up (* (e/sin theta) (e/cos phi))
                           (* (e/sin theta) (e/sin phi))
                           (e/cos theta)))
          Lsphere (e/compose Lfree (e/F->C sphere->R3))]
      (is (= '(down (+ (* -1
                          (cos (alpha t))
                          (sin (alpha t))
                          (expt ((D beta) t) 2))
                       (((expt D 2) alpha) t))
                    (+ (* 2
                          (cos (alpha t))
                          ((D alpha) t)
                          (sin (alpha t))
                          ((D beta) t))
                       (* (expt (sin (alpha t)) 2)
                          (((expt D 2) beta) t))))
             (freeze
              (simplify
               (((e/Lagrange-equations Lsphere)
                 (up (e/literal-function 'alpha)
                     (e/literal-function 'beta)))
                't))))))))
