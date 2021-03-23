;;
;; Copyright © 2021 Sam Ritchie.
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

(ns sicmutils.calculus.curvature-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.abstract.function :as af]
            [sicmutils.abstract.number :as an]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.curvature :as c]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.map :as cm]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.mechanics.lagrange :refer [osculating-path]]
            [sicmutils.expression :as x]
            [sicmutils.function :refer [compose]]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.operator :as o]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest curvature-tests
  (testing "tests from curvature.scm"
    ;; General torsion is not too complicated to compute
    (let-coordinates [[x y] m/R2-rect]
      (let [R2-rect-basis (b/coordinate-system->basis R2-rect)
            R2-rect-point ((m/point R2-rect) (up 'x0 'y0))

            Gijk (fn [i j k]
                   (m/literal-manifold-function
                    (symbol (str "G↑" i "_" j k))
                    R2-rect))
            G (down (down (up (Gijk 0 0 0)
                              (Gijk 1 0 0))
                          (up (Gijk 0 1 0)
                              (Gijk 1 1 0)))
                    (down (up (Gijk 0 0 1)
                              (Gijk 1 0 1))
                          (up (Gijk 0 1 1)
                              (Gijk 1 1 1))))

            CG (cov/make-Christoffel G R2-rect-basis)
            CF (cov/Christoffel->Cartan CG)

            v (vf/literal-vector-field 'v R2-rect)
            w (vf/literal-vector-field 'w R2-rect)
            f (m/literal-manifold-function 'f R2-rect)
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up x0 y0) 'p)))]
        (is (= '(+ (* -1 (((partial 0) f) p) (v↑0 p) (G↑0_01 p) (w↑1 p))
                   (* (((partial 0) f) p) (v↑0 p) (w↑1 p) (G↑0_10 p))
                   (* (((partial 0) f) p) (v↑1 p) (w↑0 p) (G↑0_01 p))
                   (* -1 (((partial 0) f) p) (v↑1 p) (w↑0 p) (G↑0_10 p))
                   (* -1 (v↑0 p) (w↑1 p) (((partial 1) f) p) (G↑1_01 p))
                   (* (v↑0 p) (w↑1 p) (((partial 1) f) p) (G↑1_10 p))
                   (* (v↑1 p) (w↑0 p) (((partial 1) f) p) (G↑1_01 p))
                   (* -1 (v↑1 p) (w↑0 p) (((partial 1) f) p) (G↑1_10 p)))
               (present
                ((((c/torsion-vector
                    (cov/covariant-derivative CF)) v w) f)
                 R2-rect-point))))

        ;; Unfortunately, this says only that the
        ;; Christoffel symbols are symmetric in the
        ;; lower two indices iff the torsion is zero.
        ))

    (let-coordinates [[theta phi] m/S2-spherical]
      (let [S2-spherical-basis (b/coordinate-system->basis S2-spherical)
            a-point ((m/point S2-spherical) (up 'theta 'phi))
            a-function (m/literal-scalar-field 'f S2-spherical)

            ;; the Christoffel symbols (for r=1) (p.341 mtw) are:
            ;; (the up-down-down Christoffel symbols do not depend on R)
            G-S2-1 (cov/make-Christoffel
                    (let [zero m/zero-manifold-function]
                      (down (down (up zero zero)
                                  (up zero (/ 1 (g/tan theta))))
                            (down (up zero (/ 1 (g/tan theta)))
                                  (up (- (* (g/sin theta)
                                            (g/cos theta)))
                                      zero))))
                    S2-spherical-basis)
            nabla (cov/covariant-derivative
                   (cov/Christoffel->Cartan G-S2-1))]
        (is (= 0 (simplify
                  (((o/commutator d:dtheta d:dphi) a-function)
                   a-point))))

        (is (= '(/ (* (((partial 1) f) (up theta phi))
                      (cos theta))
                   (sin theta))
               (simplify
                ((((nabla d:dtheta) d:dphi)
                  a-function)
                 a-point))))

        (is (= '(* -1 (expt (cos theta) 2)
                   (((partial 0) f) (up theta phi)))
               (simplify
                ((((nabla d:dphi) ((nabla d:dtheta) d:dphi))
                  a-function)
                 a-point))))

        (doall
         (for [x [d:dtheta d:dphi]
               y [d:dtheta d:dphi]]
           (is (= 0 (simplify
                     ((((c/torsion-vector nabla) x y)
                       a-function)
                      a-point))))))

        (is (= 1 (simplify
                  (((c/Riemann nabla)
                    dphi d:dtheta d:dphi d:dtheta)
                   a-point))))))

    ;; We can work without embedding the sphere in R↑3
    ;; We need another copy of R2...
    (let [M (m/make-manifold m/Rn 2)
          M-rect (m/coordinate-system-at M :rectangular :origin)
          M-polar (m/coordinate-system-at M :polar-cylindrical :origin)]
      (let-coordinates [(up theta phi) M-rect]
        (let [M-basis (b/coordinate-system->basis M-rect)
              a-point ((m/point M-rect) (up 'theta↑0 'phi↑0))
              a-function (m/literal-scalar-field 'f M-rect)
              G-S2-1 (cov/make-Christoffel
                      (let [zero m/zero-manifold-function]
                        (down (down
                               (up zero zero)
                               (up zero (/ 1 (g/tan theta))))
                              (down
                               (up zero (/ 1 (g/tan theta)))
                               (up (- (* (g/sin theta)
                                         (g/cos theta)))
                                   zero))))
                      M-basis)
              nabla (cov/covariant-derivative
                     (cov/Christoffel->Cartan G-S2-1))]
          (doall
           (for [x [d:dtheta d:dphi]
                 y [d:dtheta d:dphi]]
             (is (= 0 (simplify
                       ((((c/torsion-vector nabla) x y)
                         a-function)
                        a-point))))))

          (is (= 1 (simplify
                    (((c/Riemann nabla)
                      dphi d:dtheta d:dphi d:dtheta)
                     a-point))))

          (testing "p351 MTW has efficient method for computing curvature (eq
          14.18)"
            ;; R↑alpha_{beta gamma delta}
            (letfn [(check! [expected alpha beta gamma delta]
                      (is (= expected
                             (simplify
                              (((c/Riemann nabla)
                                alpha beta gamma delta)
                               a-point)))))]
              (check! 0 dtheta d:dtheta d:dtheta d:dtheta)
              (check! 0 dtheta d:dtheta d:dtheta d:dphi)
              (check! 0 dtheta d:dtheta d:dphi d:dtheta)
              (check! 0 dtheta d:dtheta d:dphi d:dphi)
              (check! 0 dtheta d:dphi d:dtheta d:dtheta)

              (check! '(expt (sin theta↑0) 2)
                      dtheta d:dphi d:dtheta d:dphi)

              (check! '(* -1 (expt (sin theta↑0) 2))
                      dtheta d:dphi d:dphi d:dtheta)

              (check! 0 dtheta d:dphi d:dphi d:dphi)
              (check! 0 dphi d:dtheta d:dtheta d:dtheta)
              (check! -1 dphi d:dtheta d:dtheta d:dphi)
              (check! 1 dphi d:dtheta d:dphi d:dtheta)
              (check! 0 dphi d:dtheta d:dphi d:dphi)
              (check! 0 dphi d:dphi d:dtheta d:dtheta)
              (check! 0 dphi d:dphi d:dtheta d:dphi)
              (check! 0 dphi d:dphi d:dphi d:dtheta)
              (check! 0 dphi d:dphi d:dphi d:dphi))))))

    ;; The Christoffel symbols (for R=1) (p.341 MTW) are:
    ;; (the up-down-down Christoffel symbols do not depend on R)
    (let [M (m/make-manifold m/Rn 2)
          M-rect (m/coordinate-system-at M :rectangular :origin)]
      (let-coordinates [[t n] m/R2-rect
                        [theta phi] M-rect]
        (let [M-basis (b/coordinate-system->basis M-rect)
              G-S2-1 (cov/make-Christoffel
                      (let [zero m/zero-manifold-function]
                        (down (down
                               (up zero zero)
                               (up zero (/ 1 (g/tan theta))))
                              (down
                               (up zero (/ 1 (g/tan theta)))
                               (up (- (* (g/sin theta)
                                         (g/cos theta)))
                                   zero))))
                      M-basis)

              f↑theta (af/literal-function 'f↑theta '(-> (UP Real Real) Real))
              f↑phi (af/literal-function 'f↑phi '(-> (UP Real Real) Real))

              s0 (simplify
                  (let [mu:N->M (compose (m/point M-rect)
                                         (up f↑theta f↑phi)
                                         (m/chart R2-rect))
                        basis-over-mu (cm/basis->basis-over-map mu:N->M M-basis)
                        oneform-basis (b/basis->oneform-basis basis-over-mu)
                        Cartan (cov/Christoffel->Cartan G-S2-1)
                        nabla (cov/covariant-derivative Cartan mu:N->M)
                        nablau (nabla d:dt)
                        d1 (nablau (nablau ((cm/differential mu:N->M) d:dn)))
                        d2 (((c/Riemann-curvature nabla) d:dn d:dt)
                            ((cm/differential mu:N->M) d:dt))
                        deviation (+ d1 d2)]
                    (s/mapr
                     (fn [w]
                       ((w deviation) ((m/point R2-rect)
                                       (up 'tau 0))))
                     oneform-basis)))

              s12
              (x/substitute
               s0 {'(((* (expt (partial 0) 2) (partial 1)) f↑theta) (up tau 0)) 'xidotdot
                   '(((* (expt (partial 0) 2) (partial 1)) f↑phi) (up tau 0)) 'etadotdot
                   '(((expt (partial 0) 2) f↑phi) (up tau 0)) 'phidotdot
                   '(((expt (partial 0) 2) f↑theta) (up tau 0)) 'thetadotdot
                   '(((* (partial 0) (partial 1)) f↑phi) (up tau 0)) 'etadot
                   '(((* (partial 0) (partial 1)) f↑theta) (up tau 0)) 'xidot
                   '(((partial 1) f↑theta) (up tau 0)) 'xi
                   '(((partial 1) f↑phi) (up tau 0)) 'eta
                   '(((partial 0) f↑theta) (up tau 0)) 'thetadot
                   '(((partial 0) f↑phi) (up tau 0)) 'phidot
                   '(f↑theta (up tau 0)) 'theta
                   '(f↑phi (up tau 0)) 'phi})

              ;; Substituting from the geodesic equation (equation of motion) to
              ;; make make use of the fact that the trajectory is a geodesic.
              s13
              (x/substitute s12 'phidotdot '(* -2 thetadot phidot (/ (cos theta) (sin theta))))

              s14
              (x/substitute s13 'thetadotdot '(* phidot phidot (cos theta) (sin theta)))]

          ;; These geodesic deviation equations are the variational equations
          ;; driven by the geodesic equation.
          (is (= '(up (+ (* -2 (expt phidot 2) xi (expt (cos theta) 2))
                         (* -2 etadot phidot (sin theta) (cos theta))
                         (* (expt phidot 2) xi)
                         xidotdot)
                      (/ (+ (* 2 etadot thetadot (sin theta) (cos theta))
                            (* 2 phidot xidot (sin theta) (cos theta))
                            (* etadotdot (expt (sin theta) 2))
                            (* -2 phidot thetadot xi))
                         (expt (sin theta) 2)))
                 (simplify
                  (an/literal-number s14)))))

        ))
    ))

(comment
  ;; Testing equation 3 on MTW p272
  (define s0
    (simplify
     (let* ( ;; d:dt and d:dn exist
            (mu:N->M (compose
                      (m/point M-rect)
                      (up f↑theta f↑phi)
                      (m/chart R2-rect)))
            (basis-over-mu (cm/basis->basis-over-map mu:N->M M-basis))
            (oneform-basis (b/basis->oneform-basis basis-over-mu))
            (Cartan (Christoffel->Cartan G-S2-1))
            (nabla (cov/covariant-derivative Cartan mu:N->M))
            (nablau (nabla d:dt))
            (nablan (nabla d:dn))
            (deviation (nablan (nablau ((differential mu:N->M) d:dt)))))
       (s/mapr
        (fn [w]
          ((w deviation) ((m/point R2-rect) (up 'tau 0))))
        oneform-basis))))

  ;; do all substitutions again...
  s12
  ;; ;; Result:
  (up
   (+ (* -2 eta phidot thetadot (expt (cos theta) 2))
      (* -2 (expt phidot 2) xi (expt (cos theta) 2))
      (* -1 eta phidotdot (cos theta) (sin theta))
      (* -2 etadot phidot (cos theta) (sin theta))
      (* (expt phidot 2) xi)
      xidotdot)
   (/
    (+ (* -1 eta (expt phidot 2) (expt (cos theta) 2) (sin theta))
       (* -2 phidot thetadot xi (sin theta))
       (* eta thetadotdot (cos theta))
       (* 2 etadot thetadot (cos theta))
       (* 2 phidot xidot (cos theta))
       (* phidotdot xi (cos theta))
       (* etadotdot (sin theta)))
    (sin theta)))


  s14
  ;; ;; Result:
  (up
   (+ (* -2 (expt phidot 2) xi (expt (cos theta) 2))
      (* -2 etadot phidot (cos theta) (sin theta))
      (* (expt phidot 2) xi)
      xidotdot)
   (/
    (+ (* 2 etadot thetadot (cos theta) (sin theta))
       (* 2 phidot xidot (cos theta) (sin theta))
       (* etadotdot (expt (sin theta) 2))
       (* -2 phidot thetadot xi))
    (expt (sin theta) 2)))


  ;; agrees with Riemann calculation
  ;;
  ;; shouldn't this be zero?
  ;;
  ;; parallel transport of vector about a loop

  (define-coordinates t the-real-line)

  ;; The coordinates on the unit sphere

  (define-coordinates (up theta phi) S2-spherical)

  (define S2-spherical-basis (coordinate-system->basis S2-spherical))

  ;; The Christoffel symbols (for r=1) (p.341 MTW) are:

  (define G-S2-1
    (cov/make-Christoffel
     (let [zero m/zero-manifold-function]
       (down (down (up zero zero)
                   (up zero (/ 1 (g/tan theta))))
             (down (up zero (/ 1 (g/tan theta)))
                   (up (- (* (g/sin theta) (g/cos theta))) zero))))
     S2-spherical-basis))


  ;; Ordinary Lagrange Equations (= Geodesic Equations)

  (let [U d:dt
        mu:N->M (compose (m/point S2-spherical)
                         (up (af/literal-function 'f↑theta)
                             (af/literal-function 'f↑phi))
                         (m/chart the-real-line))
        basis-over-mu (cm/basis->basis-over-map mu:N->M S2-spherical-basis)
        oneform-basis (b/basis->oneform-basis basis-over-mu)
        Cartan (Christoffel->Cartan G-S2-1)]
    (s/mapr
     (fn [w]
       ((w (((cov/covariant-derivative Cartan mu:N->M) U)
            ((differential mu:N->M) U)))
        ((m/point the-real-line) 'tau)))
     oneform-basis))
  ;; ;; Result:
  (up
   (+ (((expt D 2) f↑theta) tau)
      (* -1 (cos (f↑theta tau)) (sin (f↑theta tau)) (expt ((D f↑phi) tau) 2)))
   (/ (+ (* (sin (f↑theta tau)) (((expt D 2) f↑phi) tau))
         (* 2 (cos (f↑theta tau)) ((D f↑phi) tau) ((D f↑theta) tau)))
      (sin (f↑theta tau))))


  ;; Parallel transport of vector W over path mu

  (let ((U d:dt)
        (mu:N->M (compose (m/point S2-spherical)
                          (up (af/literal-function 'f↑theta)
                              (af/literal-function 'f↑phi))
                          (m/chart the-real-line))))
    (let* ((basis-over-mu
            (cm/basis->basis-over-map mu:N->M S2-spherical-basis))
           (oneform-basis (b/basis->oneform-basis basis-over-mu))
           (vector-basis (basis->vector-basis basis-over-mu))
           (Cartan (Christoffel->Cartan G-S2-1))
           (transported-vector-over-map
            (basis-components->vector-field
             (up (compose (af/literal-function 'w↑0)
                          (m/chart the-real-line))
                 (compose (af/literal-function 'w↑1)
                          (m/chart the-real-line)))
             vector-basis)))
      (s/mapr
       (fn [w]
         ((w
           (((cov/covariant-derivative Cartan mu:N->M) U)
            transported-vector-over-map))
          ((m/point the-real-line) 'tau)))
       oneform-basis)))

  ;; Result:
  (up
   (+ ((D w↑0) tau)
      (* -1 (cos (f↑theta tau)) ((D f↑phi) tau) (w↑1 tau) (sin (f↑theta tau))))
   (/ (+ (* (sin (f↑theta tau)) ((D w↑1) tau))
         (* (cos (f↑theta tau)) ((D f↑phi) tau) (w↑0 tau))
         (* (cos (f↑theta tau)) (w↑1 tau) ((D f↑theta) tau)))
      (sin (f↑theta tau))))


  ;; was  ...  looks like right hand side

  (up (* (sin (theta tau)) (cos (theta tau)) (w↑1 tau)
         ((D phi) tau))
      (/ (+ (* -1 (w↑0 tau) (cos (theta tau)) ((D phi) tau))
            (* -1 ((D theta) tau) (cos (theta tau)) (w↑1 tau)))
         (sin (theta tau))))


  ;; To set up for solving for the derivatives, we lift off of the path

  (let [U d:dt
        mu:N->M (compose (m/point S2-spherical)
                         (up (af/literal-function 'f↑theta)
                             (af/literal-function 'f↑phi))
                         (m/chart the-real-line))
        basis-over-mu (cm/basis->basis-over-map mu:N->M S2-spherical-basis)
        oneform-basis (b/basis->oneform-basis basis-over-mu)
        vector-basis (b/basis->vector-basis basis-over-mu)
        Cartan (cov/Christoffel->Cartan G-S2-1)
        transported-vector-over-map
        (vf/basis-components->vector-field
         (up (compose (osculating-path (up 'tau 'w↑0 'dw↑0:dt))
                      (m/chart the-real-line))
             (compose (osculating-path (up 'tau 'w↑1 'dw↑1:dt))
                      (m/chart the-real-line)))
         vector-basis)]
    (let*
        (s/mapr
         (fn [w]
           ((w
             (((cov/covariant-derivative Cartan mu:N->M)
               U)
              transported-vector-over-map))
            ((m/point the-real-line) 'tau)))
         oneform-basis)))

  ;; Result:
  (up (+ dw↑0:dt
         (* -1 (cos (f↑theta tau)) ((D f↑phi) tau) (sin (f↑theta tau)) w↑1))
      (/ (+ (* (sin (f↑theta tau)) dw↑1:dt)
            (* (cos (f↑theta tau)) ((D f↑phi) tau) w↑0)
            (* (cos (f↑theta tau)) ((D f↑theta) tau) w↑1))
         (sin (f↑theta tau))))


  ;; Loaded solve by (load "/usr/local/scmutils/src/solve/linreduce")
  (let [tau 'tau
        theta (af/literal-function 'f↑theta)
        phi (af/literal-function 'f↑phi)
        w↑0 (af/literal-function 'w↑0)
        w↑1 (af/literal-function 'w↑1)]
    (solve
     (fn [v]
       (let [dw↑0:dt (ref v 0)
             dw↑1:dt (ref v 1)]
         (up (+ (* -1
                   (w↑1 tau)
                   (g/sin (theta tau))
                   (g/cos (theta tau))
                   ((D phi) tau))
                dw↑0:dt)
             (+ (/ (* (w↑0 tau) (g/cos (theta tau)) ((D phi) tau))
                   (g/sin (theta tau)))
                (/ (* (w↑1 tau) ((D theta) tau) (g/cos (theta tau)))
                   (g/sin (theta tau)))
                dw↑1:dt))))
     2 2))

  ;; ;; Result:
  (up (* (w↑1 tau) (sin (f↑theta tau)) (cos (f↑theta tau)) ((D f↑phi) tau))
      (/ (+ (* -1 (w↑1 tau) (cos (f↑theta tau)) ((D f↑theta) tau))
            (* -1 (cos (f↑theta tau)) ((D f↑phi) tau) (w↑0 tau)))
         (sin (f↑theta tau))))


  (let [U d:dt
        mu:N->M (compose
                 (m/point S2-spherical)
                 (up (af/literal-function 'f↑theta)
                     (af/literal-function 'f↑phi))
                 (m/chart the-real-line))]
    (solve
     (fn [v]
       (let [dw↑0:dt (ref v 0)
             dw↑1:dt (ref v 1)
             basis-over-mu (cm/basis->basis-over-map mu:N->M S2-spherical-basis)
             oneform-basis (b/basis->oneform-basis basis-over-mu)
             vector-basis (b/basis->vector-basis basis-over-mu)
             Cartan (cov/Christoffel->Cartan G-S2-1)
             transported-vector-over-map
             (vf/basis-components->vector-field
              (up (compose (osculating-path (up 'tau 'w↑0 dw↑0:dt))
                           (m/chart the-real-line))
                  (compose (osculating-path (up 'tau 'w↑1 dw↑1:dt))
                           (m/chart the-real-line)))
              vector-basis)]
         (s/mapr
          (fn [w]
            ((w
              (((cov/covariant-derivative Cartan mu:N->M)
                U)
               transported-vector-over-map))
             ((m/point the-real-line) 'tau)))
          oneform-basis)))
     (S2-spherical 'dimension)
     (S2-spherical 'dimension)))
  ;; ;; Result:
  (up
   (* w↑1 (cos (f↑theta tau)) (sin (f↑theta tau)) ((D f↑phi) tau))
   (/
    (+ (* -1 w↑0 (cos (f↑theta tau)) ((D f↑phi) tau))
       (* -1 w↑1 ((D f↑theta) tau) (cos (f↑theta tau))))
    (sin (f↑theta tau))))

  ;; Computing parallel transport without the embedding
  (let-coordinates [t m/the-real-line
                    [theta phi] M-rect]
    (let [M-basis (b/coordinate-system->basis M-rect)
          G-S2-1  (cov/make-Christoffel
                   (let [zero m/zero-manifold-function]
                     (down
                      (down (up zero zero)
                            (up zero (/ 1 (g/tan theta))))
                      (down (up zero (/ 1 (g/tan theta)))
                            (up (- (* (g/sin theta)
                                      (g/cos theta))) zero))))
                   M-basis)]
      ))
  ;; Parallel transport of vector w over path mu

  (define mu:N->M
    (compose (m/point M-rect)
             (up (af/literal-function 'mu↑theta)
                 (af/literal-function 'mu↑phi))
             (m/chart the-real-line)))

  (define basis-over-mu
    (cm/basis->basis-over-map mu:N->M M-basis))

  (define w
    (basis-components->vector-field
     (up (compose (af/literal-function 'w↑0)
                  (m/chart the-real-line))
         (compose (af/literal-function 'w↑1)
                  (m/chart the-real-line)))
     (basis->vector-basis basis-over-mu)))

  (let [Cartan (Christoffel->Cartan G-S2-1)]
    (s/mapr
     (fn [omega]
       ((omega
         (((cov/covariant-derivative Cartan mu:N->M) d:dt) w))
        ((m/point the-real-line) 'tau)))
     (b/basis->oneform-basis basis-over-mu)))
  ;; ;; Result:
  (up
   (+ (* -1 (w↑1 tau) ((D mu↑phi) tau) (cos (mu↑theta tau)) (sin (mu↑theta tau)))
      ((D w↑0) tau))
   (/
    (+ (* (w↑1 tau) (cos (mu↑theta tau)) ((D mu↑theta) tau))
       (* (w↑0 tau) ((D mu↑phi) tau) (cos (mu↑theta tau)))
       (* ((D w↑1) tau) (sin (mu↑theta tau))))
    (sin (mu↑theta tau))))

  )

(deftest final-tests
  ;; NOTE: Working from here on out!

  ;; These are the equations of the coordinates of a vector being
  ;; parallel transported along the path defined by f.
  ;;
  ;; To integrate these equations of the coordinates of the vector
  ;; being transported along a path (mu↑theta(tau), mu↑phi(tau)), defined
  ;; by differential equations we need to make a state space that
  ;; represents both the path and the coordinates of the vector being
  ;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
  ;; and the differential equations for the path are Dsigma(tau) =
  ;; b(sigma(tau)).  The differential equations for the coordinates of
  ;; the vector are driven by this path.
  ;;
  ;; To represent these states we make a new manifold with 4
  ;; coordinates.  The first two coordinates are tha coordinates of the
  ;; path.  The second two coordinates are the components of the vector
  ;; to be transported, relative to the coordinate directions in the
  ;; original manifold.  The right-hand side of the composite
  ;; differential equation is a vector field on this manifold.
  (let [R4 (m/make-manifold m/Rn 4)
        states (m/coordinate-system-at R4 :rectangular :origin)]
    (let-coordinates [[theta phi w0 w1] states]
      (let [initial-state-d:dphi
            ((m/point states) (up 'theta0 'phi0 0 1))

            initial-state-d:dtheta
            ((m/point states) (up 'theta0 'phi0 1 0))

            ;; Assuming that the paths are integral curves of a vector field v,
            ;; we supply the vector field:
            G (fn [v]
                (let [alphadot (dtheta v)
                      betadot (dphi v)]
                  (+ v
                     (* (compose (* g/sin g/cos) theta)
                        betadot w1 d:dw0)
                     (* -1
                        (compose (/ g/cos g/sin) theta)
                        (+ (* w0 betadot)
                           (* w1 alphadot))
                        d:dw1))))

            Gu (G d:dtheta)
            Gv (G d:dphi)

            initial-state
            (fn [[theta0 phi0] w]
              (let [dummy
                    ((m/point states)
                     (up theta0 phi0 'foo 'bar))]
                ((m/point states)
                 (up theta0 phi0
                     ((dw0 w) dummy)
                     ((dw1 w) dummy)))))]
        (is (= '(* -1 (expt (sin theta0) 2))
               (simplify
                ((dw0 (o/commutator Gu Gv))
                 (initial-state (up 'theta0 'phi0) d:dw1)))))

        ;; Gee, this gets the right answer.
        (is (= 1 (simplify
                  ((dw1 (o/commutator Gu Gv))
                   (initial-state (up 'theta0 'phi0) d:dw0)))))))

    ;; NOW, a duplicate! Note that it's the same, except we have a second set of
    ;; coordinates.
    ;;
    ;; To integrate these equations of the coordinates of the vector
    ;; being transported along a path (mu↑theta(tau), mu↑phi(tau)), defined
    ;; by differential equations we need to make a state space that
    ;; represents both the path and the coordinates of the vector being
    ;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
    ;; and the differential equations for the path are Dsigma(tau) =
    ;; b(sigma(tau)).  The differential equations for the coordinates of
    ;; the vector are driven by this path.

    ;; To represent these states we make a new manifold with 4
    ;; coordinates.  The first two coordinates are tha coordinates of the
    ;; path.  The second two coordinates are the components of the vector
    ;; to be transported, relative to the coordinate directions in the
    ;; original manifold.  The right-hand side of the composite
    ;; differential equation is a vector field on this manifold.
    (let [M (m/make-manifold m/Rn 2)
          M-rect (m/coordinate-system-at M :rectangular :origin)]
      (let-coordinates [[theta phi] M-rect
                        [Theta Phi w0 w1] states]
        (let [initial-state-d:dphi
              ((m/point states) (up 'theta0 'phi0 0 1))

              initial-state-d:dtheta
              ((m/point states) (up 'theta0 'phi0 1 0))

              ;; Assuming that the paths are integral curves of a vector field v,
              ;; we supply the vector field:
              G (fn [v]
                  (let [alphadot (dTheta v)
                        betadot (dPhi v)]
                    (+ v
                       (* (compose (* g/sin g/cos) Theta)
                          betadot w1 d:dw0)
                       (* -1
                          (compose (/ g/cos g/sin) Theta)
                          (+ (* w0 betadot)
                             (* w1 alphadot))
                          d:dw1))))

              Gu (G d:dTheta)
              Gv (G d:dPhi)

              initial-state
              (fn [[Theta0 Phi0] w]
                (let [m ((m/point M-rect) (up Theta0 Phi0))]
                  ((m/point states)
                   (up Theta0 Phi0
                       ((dtheta w) m) ((dphi w) m)))))]
          (is (= '(* -1 (expt (sin Theta0) 2))
                 (simplify
                  ((dw0 (o/commutator Gu Gv))
                   (initial-state (up 'Theta0 'Phi0) d:dphi)))))

          ;; Gee, this gets the right answer.
          (is (= 1 (simplify
                    ((dw1 (o/commutator Gu Gv))
                     (initial-state (up 'theta0 'phi0) d:dtheta)))))))))
  ;;----------------------------------------------------------------
  ;; try to improve this
  ;;
  ;; let gamma be the path that we are transporting along
  ;; gamma(t)->M
  ;;
  ;; dgamma(d:dt)(f)(t) is the velocity vector, a vector over the map gamma
  ;;
  ;; when gamma is an integral curve of v, then
  ;; v(f)(gamma(t)) = dgamma(d:dt)(f)(t)
  ;;
  ;; let w be an arbitrary vector over the map
  ;; w(f)(t) = d:dtheta (f)(gamma(t)) a_0(t) + d:dphi (f)(gamma(t)) a_1(t)
  )
