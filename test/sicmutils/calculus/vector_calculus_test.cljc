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

(ns sicmutils.calculus.vector-calculus-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-calculus :as vc]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(comment
  ;; Test setup for spherical system

  (define spherical R3-rect)

  (define-coordinates (up r theta phi) spherical)

  (define spherical-point
    ((m/point spherical) (up 'r 'theta 'phi)))


  (define spherical-basis
    (coordinate-system->basis spherical))


  (define (spherical-metric v1 v2)
    (+ (* (dr v1) (dr v2))
       (* (square r)
          (+ (* (dtheta v1) (dtheta v2))
             (* (expt (sin theta) 2)
                (dphi v1) (dphi v2))))))

  (define spherical-Gamma
    (make-Christoffel
     (let ((O (fn x 0)))
       (down
        (down (up O O O)
              (up O (/ 1 r) O)
              (up O O (/ 1 r)))
        (down (up O (/ 1 r) O)
              (up (* -1 r) O O)
              (up O O (/ (cos theta) (sin theta))))
        (down (up O O (/ 1 r))
              (up O O (/ (cos theta) (sin theta)))
              (up (* -1 r (expt (sin theta) 2))
                  (* -1 (sin theta) (cos theta))
                  O))))
     (coordinate-system->basis spherical)))


  (define spherical-Cartan
    (Christoffel->Cartan spherical-Gamma))

  ;; normalized spherical basis
  (define e_0 d:dr)
  (define e_1 (* (/ 1 r) d:dtheta))
  (define e_2 (* (/ 1 (* r (sin theta))) d:dphi))

  ;; ((spherical-metric e_0 e_0) spherical-point)
  ;; ((spherical-metric e_1 e_1) spherical-point)
  ;; ((spherical-metric e_2 e_2) spherical-point)
  ;; all 1

  ;; ((spherical-metric e_0 e_1) spherical-point)
  ;; ((spherical-metric e_0 e_2) spherical-point)
  ;; ((spherical-metric e_1 e_2) spherical-point)
  ;; all 0



  (define orthonormal-spherical-vector-basis
    (down e_0 e_1 e_2))

  (define orthonormal-spherical-oneform-basis
    (vector-basis->dual orthonormal-spherical-vector-basis
                        spherical))

  (define orthonormal-spherical-basis
    (b/make-basis orthonormal-spherical-vector-basis
                  orthonormal-spherical-oneform-basis))

  (define v
    (+ (* (m/literal-manifold-function 'v↑0 spherical) e_0)
       (* (m/literal-manifold-function 'v↑1 spherical) e_1)
       (* (m/literal-manifold-function 'v↑2 spherical) e_2)))


  ;; Test of Gradient

  ((orthonormal-spherical-oneform-basis
    ((gradient spherical-metric spherical-basis)
     (m/literal-manifold-function 'f spherical)))
   spherical-point)

  (up (((partial 0) f) (up r theta phi))
      (/ (((partial 1) f) (up r theta phi)) r)
      (/ (((partial 2) f) (up r theta phi)) (* r (sin theta))))


  ;; Test of Curl

  ((orthonormal-spherical-oneform-basis
    ((vc/curl spherical-metric orthonormal-spherical-basis) v))
   spherical-point)

  (up
   (/ (+ (* (sin theta) (((partial 1) v↑2) (up r theta phi)))
         (* (cos theta) (v↑2 (up r theta phi)))
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


  ;; Test of Divergence

  (((vc/divergence spherical-metric orthonormal-spherical-basis) v)
   spherical-point)

  (+ (((partial 0) v↑0) (up r theta phi))
     (/ (* 2 (v↑0 (up r theta phi))) r)
     (/ (((partial 1) v↑1) (up r theta phi)) r)
     (/ (* (v↑1 (up r theta phi)) (cos theta)) (* r (sin theta)))
     (/ (((partial 2) v↑2) (up r theta phi)) (* r (sin theta))))



  (define phi
    (m/literal-manifold-function 'phi spherical))
  phi

  (((vc/Laplacian spherical-metric orthonormal-spherical-basis)
    phi)
   spherical-point)

  (+ (((partial 0) ((partial 0) phi)) (up r theta phi))
     (/ (* 2 (((partial 0) phi) (up r theta phi)))
        r)
     (/ (((partial 1) ((partial 1) phi)) (up r theta phi))
        (expt r 2))
     (/ (* (cos theta) (((partial 1) phi) (up r theta phi)))
        (* (expt r 2) (sin theta)))
     (/ (((partial 2) ((partial 2) phi)) (up r theta phi))
        (* (expt r 2) (expt (sin theta) 2))))
  ;; Obtaining the wave equation.

  (define SR R4-rect)
  (define-coordinates (up t x y z) SR)
  (define an-event ((m/point SR) (up 't0 'x0 'y0 'z0)))
  (define c 'c)         ; We like units.

  (define (g-Minkowski u v)
    (+ (* -1 (square c) (dt u) (dt v))
       (* (dx u) (dx v))
       (* (dy u) (dy v))
       (* (dz u) (dz v))))


  (define SR-vector-basis
    (down (* (/ 1 c) d:dt) d:dx d:dy d:dz))

  (define SR-oneform-basis
    (up (* c dt) dx dy dz))

  (define SR-basis
    (b/make-basis SR-vector-basis
                  SR-oneform-basis))


  (s/mapr
   (fn (u)
     (s/mapr (fn (v)
               ((g-Minkowski u v) an-event))
             SR-vector-basis))
   SR-vector-basis)

  (down (down -1 0 0 0)
        (down  0 1 0 0)
        (down  0 0 1 0)
        (down  0 0 0 1))


  (define phi
    (m/literal-manifold-function 'phi SR))

  (((vc/Laplacian g-Minkowski SR-basis) phi) an-event)

  (+ (* -1 (((partial 1) ((partial 1) phi)) (up t0 x0 y0 z0)))
     (* -1 (((partial 2) ((partial 2) phi)) (up t0 x0 y0 z0)))
     (* -1 (((partial 3) ((partial 3) phi)) (up t0 x0 y0 z0)))
     (/ (((partial 0) ((partial 0) phi)) (up t0 x0 y0 z0)) (expt c 2)))
  )
