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

(ns sicmutils.calculus.connection-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            ;; [sicmutils.abstract.function :as af]
            ;; [sicmutils.abstract.number :as an]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.connection :as conn]
            ;; [sicmutils.calculus.curvature :as c]
            [sicmutils.calculus.manifold :as m]
            ;; [sicmutils.calculus.map :as cm]
            ;; [sicmutils.calculus.vector-field :as vf]
            ;; [sicmutils.mechanics.lagrange :refer [osculating-path]]
            ;; [sicmutils.expression :as x]
            [sicmutils.function :refer [compose]]
            [sicmutils.generic :as g :refer [+ - * /]]
            ;; [sicmutils.operator :as o]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest connection-tests
  (let [two-sphere m/R2-rect]
    (let-coordinates [[theta phi] two-sphere]
      (let [g-sphere (fn [R]
                       (fn [u v]
                         (* (g/square R)
                            (+ (* (dtheta u) (dtheta v))
                               (* (compose (g/square g/sin) theta)
                                  (dphi u)
                                  (dphi v))))))]
        (is (= '(down
                 (down (down 0 0)
                       (down 0 (* (expt R 2) (sin theta0) (cos theta0))))
                 (down (down 0 (* (expt R 2) (sin theta0) (cos theta0)))
                       (down (* -1N (expt R 2) (sin theta0) (cos theta0)) 0)))
               (simplify
                ((cov/Christoffel->symbols
                  (conn/metric->Christoffel-1 (g-sphere 'R)
                                              (b/coordinate-system->basis two-sphere)))
                 ((m/point two-sphere) (up 'theta0 'phi0)))
                ))))))

  (testing "Test with general 2d metric"
    (comment
      (define fa
        (compose (literal-function 'a (-> (UP Real Real) Real))
                 (R2-rect '->coords)))
      (define fb
        (compose (literal-function 'b (-> (UP Real Real) Real))
                 (R2-rect '->coords)))
      (define fc
        (compose (literal-function 'c (-> (UP Real Real) Real))
                 (R2-rect '->coords)))

      (define ((g-R2 g_00 g_01 g_11) u v)
        (+ (* g_00 (dx u) (dx v))
           (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
           (* g_11 (dy u) (dy v))))

      (pec (((g-R2 fa fb fc)
             (literal-vector-field 'u R2-rect)
             (literal-vector-field 'v R2-rect))
            ((R2-rect '->point) (up 'x0 'y0))))
      ;; Result:
      (+ (* (v↑1 (up x0 y0)) (u↑1 (up x0 y0)) (c (up x0 y0)))
         (* (v↑0 (up x0 y0)) (b (up x0 y0)) (u↑1 (up x0 y0)))
         (* (u↑0 (up x0 y0)) (b (up x0 y0)) (v↑1 (up x0 y0)))
         (* (a (up x0 y0)) (u↑0 (up x0 y0)) (v↑0 (up x0 y0))))


      (define R2-basis (coordinate-system->basis R2-rect))

      (pec ((Christoffel->symbols
             (metric->Christoffel-1 (g-R2 fa fb fc) R2-basis))
            ((R2-rect '->point) (up 'x0 'y0))))
      ;; Result:
      (down
       (down
        (down (* 1/2 (((partial 0) a) (up x0 y0)))
              (+ (* -1/2 (((partial 1) a) (up x0 y0)))
                 (((partial 0) b) (up x0 y0))))
        (down (* 1/2 (((partial 1) a) (up x0 y0)))
              (* 1/2 (((partial 0) c) (up x0 y0)))))
       (down
        (down (* 1/2 (((partial 1) a) (up x0 y0)))
              (* 1/2 (((partial 0) c) (up x0 y0))))
        (down (+ (((partial 1) b) (up x0 y0))
                 (* -1/2 (((partial 0) c) (up x0 y0))))
              (* 1/2 (((partial 1) c) (up x0 y0))))))



      (pec ((Christoffel->symbols
             (metric->Christoffel-2 (g-sphere 'R)
                                    (coordinate-system->basis two-sphere)))
            ((two-sphere '->point) (up 'theta0 'phi0))))
      ;; Result:
      (down
       (down (up 0 0) (up 0 (/ (cos theta0) (sin theta0))))
       (down (up 0 (/ (cos theta0) (sin theta0)))
             (up (* -1 (sin theta0) (cos theta0)) 0)))
      )

    )

  (testing "next block"
    (let [polar m/R2-polar]
      (let-coordinates [[r theta] polar]

        (comment
          (define polar-point
            ((polar '->point) (up 'r 'theta)))

          (define polar-basis
            (coordinate-system->basis polar))

          (define (polar-metric v1 v2)
            (+ (* (dr v1) (dr v2))
               (* (square r)
                  (* (dtheta v1) (dtheta v2)))))

          (define foo
            ((Christoffel->symbols
              (metric->Christoffel-2 polar-metric polar-basis))
             polar-point))

          (pec foo)
          ;; Result:
          (down
           (down (up 0 0)
                 (up 0 (/ 1 r)))
           (down (up 0 (/ 1 r))
                 (up (* -1 r) 0)))

          )
        )))

  (testing "faster, a simplified version"
    (let [polar m/R2-rect]
      (let-coordinates [[r theta] polar]
        (comment
          (define polar-point
            ((polar '->point) (up 'r 'theta)))

          (define polar-Gamma
            (make-Christoffel
             (let ((O (lambda x 0)))
               (down
                (down (up O O)
                      (up O (/ 1 r)))
                (down (up O (/ 1 r))
                      (up (* -1 r) O))))
             (coordinate-system->basis polar)))

          ;; Now look at curvature
          (let [nabla (cov/covariant-derivative (Christoffel->Cartan polar-Gamma))
                curvature (Riemann nabla)]
            (doall
             (for [alpha [dr dtheta]
                   beta [d:dr d:dtheta]
                   gamma [d:dr d:dtheta]
                   delta [d:dr d:dtheta]]
               (is (= 0 (simplify
                         ((curvature alpha beta gamma delta)
                          polar-point)))))))
          )
        ))
    )

  (testing "sphere block"
    (let [spherical m/R3-rect]
      (let-coordinates [[r theta phi] spherical]
        (comment

          (define spherical-point
            ((spherical '->point) (up 'r 'theta 'phi)))

          (define spherical-basis
            (coordinate-system->basis spherical))

          (define (spherical-metric v1 v2)
            (+ (* (dr v1) (dr v2))
               (* (square r)
                  (+ (* (dtheta v1) (dtheta v2))
                     (* (expt (sin theta) 2)
                        (dphi v1) (dphi v2))))))

          (define foo
            ((Christoffel->symbols
              (metric->Christoffel-2 spherical-metric spherical-basis))
             spherical-point))

          (pec foo)
          ;; Result:
          (down
           (down (up 0 0 0) (up 0 (/ 1 r) 0) (up 0 0 (/ 1 r)))
           (down (up 0 (/ 1 r) 0) (up (* -1 r) 0 0) (up 0 0 (/ (cos theta) (sin theta))))
           (down (up 0 0 (/ 1 r))
                 (up 0 0 (/ (cos theta) (sin theta)))
                 (up (* -1 r (expt (sin theta) 2)) (* -1 (sin theta) (cos theta)) 0)))


          ;; Thus, make simplified version.

          (define spherical-Gamma
            (make-Christoffel
             (let ((O (lambda x 0)))
               (down
                (down (up O O O) (up O (/ 1 r) O) (up O O (/ 1 r)))
                (down (up O (/ 1 r) O) (up (* -1 r) O O) (up O O (/ (cos theta) (sin theta))))
                (down (up O O (/ 1 r))
                      (up O O (/ (cos theta) (sin theta)))
                      (up (* -1 r (expt (sin theta) 2)) (* -1 (sin theta) (cos theta)) O))))
             (coordinate-system->basis spherical)))

          ;; Now look at curvature
          (let [nabla (covariant-derivative (Christoffel->Cartan spherical-Gamma))
                curvature (Riemann nabla)]
            (doall
             (for [alpha [dr dtheta dphi]
                   beta [d:dr d:dtheta]
                   gamma [d:dr d:dtheta]
                   delta [d:dr d:dtheta]]
               (is (= 0 (simplify
                         ((curvature alpha beta gamma delta)
                          spherical-point)))))))

          )
        )))

  (testing "MTW p205 spherical flat lorentz"
    (let [spherical-Lorentz m/R4-rect]
      (let-coordinates [[t r theta phi] spherical-Lorentz]
        (comment

          (define spherical-Lorentz-basis
            (coordinate-system->basis spherical-Lorentz))

          (define ((spherical-Lorentz-metric c↑2) v1 v2)
            (+ (* -1 c↑2 (* (dt v1) (dt v2)))
               (* (dr v1) (dr v2))
               (* (square r)
                  (+ (* (dtheta v1) (dtheta v2))
                     (* (square (sin theta))
                        (* (dphi v1) (dphi v2)))))))

          (define spherical-Lorentz-point
            ((spherical-Lorentz '->point) (up 't 'r 'theta 'phi)))

          (define (orthonormal-spherical-Lorentz-vector-basis c↑2)
            (down (* (/ 1 (sqrt c↑2)) d:dt)
                  d:dr
                  (* (/ 1 r) d:dtheta)
                  (* (/ 1 (* r (sin theta))) d:dphi)))

          (define (orthonormal-spherical-Lorentz-oneform-basis c↑2)
            (let ((orthonormal-spherical-Lorentz-vectors
                   (orthonormal-spherical-Lorentz-vector-basis c↑2)))
              (vector-basis->dual orthonormal-spherical-Lorentz-vectors
                                  spherical-Lorentz)))

          (define (orthonormal-spherical-Lorentz-basis c↑2)
            (make-basis (orthonormal-spherical-Lorentz-vector-basis c↑2)
                        (orthonormal-spherical-Lorentz-oneform-basis c↑2)))

          (pec ((s/mapr (orthonormal-spherical-Lorentz-oneform-basis 'c↑2)
                        (orthonormal-spherical-Lorentz-vector-basis 'c↑2))
                spherical-Lorentz-point))
          ;; Result:
          (down (up 1 0 0 0) (up 0 1 0 0) (up 0 0 1 0) (up 0 0 0 1))


          (pec (((spherical-Lorentz-metric 'c↑2)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 0)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 0))
                spherical-Lorentz-point))
          ;; Result:
          -1


          (pec (((spherical-Lorentz-metric 'c↑2)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 1)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 1))
                spherical-Lorentz-point))
          ;; Result:
          1


          (pec (((spherical-Lorentz-metric 'c↑2)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 2)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 2))
                spherical-Lorentz-point))
          ;; Result:
          1


          (pec (((spherical-Lorentz-metric 'c↑2)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 3)
                 (ref (orthonormal-spherical-Lorentz-vector-basis 'c↑2) 3))
                spherical-Lorentz-point))
          ;; Result:
          1


          (pec ((Christoffel->symbols
                 (metric->connection-1 (spherical-Lorentz-metric 'c↑2)
                                       (orthonormal-spherical-Lorentz-basis 'c↑2)))
                spherical-Lorentz-point))
          ;; Result:
          (down
           (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
           (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
           (down (down 0 0 0 0) (down 0 0 (/ 1 r) 0) (down 0 (/ -1 r) 0 0) (down 0 0 0 0))
           (down (down 0 0 0 0)
                 (down 0 0 0 (/ 1 r))
                 (down 0 0 0 (/ (cos theta) (* r (sin theta))))
                 (down 0 (/ -1 r) (/ (* -1 (cos theta)) (* r (sin theta))) 0)))


          (define foo
            (show-time
             (lambda ()
                     ((Christoffel->symbols
                       (metric->connection-2 (spherical-Lorentz-metric 'c↑2)
                                             (orthonormal-spherical-Lorentz-basis 'c↑2)))
                      spherical-Lorentz-point))))

          (pec foo)
          ;; Result:
          (down
           (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
           (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
           (down (up 0 0 0 0) (up 0 0 (/ 1 r) 0) (up 0 (/ -1 r) 0 0) (up 0 0 0 0))
           (down (up 0 0 0 0)
                 (up 0 0 0 (/ 1 r))
                 (up 0 0 0 (/ (cos theta) (* r (sin theta))))
                 (up 0 (/ -1 r) (/ (* -1 (cos theta)) (* r (sin theta))) 0)))

          ;; The last two are essentially the same.  Is this correct?


          ;; Check answers from MTW p.213
          ;; t r theta phi
          ;; 0 1 2     3

          (pe (ref foo 3 2 3))
          (/ (cos theta) (* r (sin theta)))

          (pe (ref foo 3 3 2))
          (/ (* -1 (cos theta)) (* r (sin theta)))

          (pe (ref foo 2 1 2))
          (/ 1 r)

          (pe (ref foo 3 1 3))
          (/ 1 r)

          (pe (ref foo 2 2 1))
          (/ -1 r)

          (pe (ref foo 3 3 1))
          (/ -1 r)


          (define (orthonormal-spherical-Lorentz-second-connection c↑2)
            (make-Christoffel
             (let ((zero (lambda (point) 0)))
               (down
                (down (up zero zero zero zero)
                      (up zero zero zero zero)
                      (up zero zero zero zero)
                      (up zero zero zero zero))
                (down (up zero zero zero zero)
                      (up zero zero zero zero)
                      (up zero zero zero zero)
                      (up zero zero zero zero))
                (down (up zero zero zero zero)
                      (up zero zero (/ 1 r) zero)
                      (up zero (/ -1 r) zero zero)
                      (up zero zero zero zero))
                (down (up zero zero zero zero)
                      (up zero zero zero (/ 1 r))
                      (up zero zero zero (/ (cos theta) (* r (sin theta))))
                      (up zero
                          (/ -1 r)
                          (/ (* -1 (cos theta)) (* r (sin theta)))
                          zero))))
             (orthonormal-spherical-Lorentz-basis c↑2)))

          ;; Look at curvature
          (doall
           (for [alpha [dt dr dtheta dphi]
                 beta [d:dt d:dr d:dtheta]
                 gamma [d:dt d:dr d:dtheta]
                 delta [d:dt d:dr d:dtheta]]
             (is (= 0 (simplify
                       (((Riemann
                          (Christoffel->Cartan
                           (orthonormal-spherical-Lorentz-second-connection 'c↑2)))
                         alpha beta gamma delta)
                        spherical-Lorentz-point)))))))))))
