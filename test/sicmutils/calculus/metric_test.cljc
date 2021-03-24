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

(ns sicmutils.calculus.metric-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            ;; [sicmutils.abstract.function :as af]
            ;; [sicmutils.abstract.number :as an]
            ;; [sicmutils.calculus.basis :as b]
            ;; [sicmutils.calculus.coordinate :refer [let-coordinates]
            ;;  #?@(:cljs [:include-macros true])]
            ;; [sicmutils.calculus.covariant :as cov]
            ;; [sicmutils.calculus.curvature :as c]
            ;; [sicmutils.calculus.manifold :as m]
            ;; [sicmutils.calculus.map :as cm]
            ;; [sicmutils.calculus.vector-field :as vf]
            ;; [sicmutils.mechanics.lagrange :refer [osculating-path]]
            ;; [sicmutils.expression :as x]
            ;; [sicmutils.function :refer [compose]]
            [sicmutils.generic :as g :refer [+ - * /]]
            ;; [sicmutils.operator :as o]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(comment
  ;; Example: natural metric on a sphere of radius R
  (define two-sphere R2-rect)

  (install-coordinates two-sphere (up 'theta 'phi))

  (define ((g-sphere R) u v)
    (* (square R)
       (+ (* (dtheta u) (dtheta v))
          (* (compose (square sin) theta)
             (dphi u)
             (dphi v)))))

  (define u (literal-vector-field 'u two-sphere))
  (define v (literal-vector-field 'v two-sphere))

  (pec (((g-sphere 'R) u v)
        ((two-sphere '->point) (up 'theta0 'phi0))))
  ;; Result:
  (* (+ (* (v↑0 (up theta0 phi0))
           (u↑0 (up theta0 phi0)))
        (* (expt (sin theta0) 2)
           (v↑1 (up theta0 phi0))
           (u↑1 (up theta0 phi0))))
     (expt R 2))


;;; Example: Lorentz metric on R↑4

  (define SR R4-rect)
  (install-coordinates SR (up 't 'x 'y 'z))

  (define ((g-Lorentz c) u v)
    (+ (* (dx u) (dx v))
       (* (dy u) (dy v))
       (* (dz u) (dz v))
       (* -1 (square c) (dt u) (dt v))))


;;; Example: general metric on R↑2

  (install-coordinates R2-rect (up 'x 'y))
  (define R2-basis (coordinate-system->basis R2-rect))

  (define ((g-R2 g_00 g_01 g_11) u v)
    (+ (* g_00 (dx u) (dx v))
       (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
       (* g_11 (dy u) (dy v))))

  (pec (((g-R2 'a 'b 'c)
         (literal-vector-field 'u R2-rect)
         (literal-vector-field 'v R2-rect))
        ((R2-rect '->point) (up 'x0 'y0))))
  ;; Result:
  (+ (* (u↑0 (up x0 y0)) (v↑0 (up x0 y0)) a)
     (* (+ (* (v↑0 (up x0 y0)) (u↑1 (up x0 y0)))
           (* (u↑0 (up x0 y0)) (v↑1 (up x0 y0))))
        b)
     (* (v↑1 (up x0 y0)) (u↑1 (up x0 y0)) c))



  ((coordinate-system->metric-components R3-spherical) (up 'r 'theta 'phi))
  ;; result:
  (down (down 1 0 0)
        (down 0 (expt r 2) 0)
        (down 0 0 (* (expt r 2) (expt (sin theta) 2))))


  (s:map/r (lambda (v1)
                   (s:map/r (lambda (v2)
                                    (((coordinate-system->metric R3-spherical) v1 v2)
                                     ((point R3-spherical) (up 'r 'theta 'phi))))
                            (coordinate-system->vector-basis R3-spherical)))
           (coordinate-system->vector-basis R3-spherical))
  ;; result:
  (down (down 1 0 0)
        (down 0 (expt r 2) 0)
        (down 0 0 (* (expt r 2) (expt (sin theta) 2))))



  (s:map/r (lambda (w1)
                   (s:map/r (lambda (w2)
                                    (((coordinate-system->inverse-metric R3-spherical) w1 w2)
                                     ((point R3-spherical) (up 'r 'theta 'phi))))
                            (coordinate-system->1form-basis R3-spherical)))
           (coordinate-system->1form-basis R3-spherical))
  ;; result
  (up (up 1 0 0)
      (up 0 (/ 1 (expt r 2)) 0)
      (up 0 0 (/ 1 (* (expt r 2) (expt (sin theta) 2)))))


  (install-coordinates R3-rect (up 'x 'y 'z))

  (pec (((literal-metric 'g R3-rect)
         (literal-vector-field 'u R3-rect)
         (literal-vector-field 'v R3-rect))
        ((R3-rect '->point) (up 'x0 'y0 'z0))))
  ;; Result:
  (+ (* (v↑0 (up x0 y0 z0)) (u↑0 (up x0 y0 z0)) (g_00 (up x0 y0 z0)))
     (* (v↑0 (up x0 y0 z0)) (g_01 (up x0 y0 z0)) (u↑1 (up x0 y0 z0)))
     (* (v↑0 (up x0 y0 z0)) (g_02 (up x0 y0 z0)) (u↑2 (up x0 y0 z0)))
     (* (u↑0 (up x0 y0 z0)) (v↑1 (up x0 y0 z0)) (g_01 (up x0 y0 z0)))
     (* (u↑0 (up x0 y0 z0)) (v↑2 (up x0 y0 z0)) (g_02 (up x0 y0 z0)))
     (* (v↑1 (up x0 y0 z0)) (u↑1 (up x0 y0 z0)) (g_11 (up x0 y0 z0)))
     (* (v↑1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)) (u↑2 (up x0 y0 z0)))
     (* (v↑2 (up x0 y0 z0)) (u↑1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)))
     (* (v↑2 (up x0 y0 z0)) (u↑2 (up x0 y0 z0)) (g_22 (up x0 y0 z0))))




  (install-coordinates R2-rect (up 'x 'y))
  (define R2-basis (coordinate-system->basis R2-rect))

  (define ((g-R2 g_00 g_01 g_11) u v)
    (+ (* g_00 (dx u) (dx v))
       (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
       (* g_11 (dy u) (dy v))))

  (pec (((metric:invert (g-R2 'a 'b 'c) R2-basis)
         (literal-1form-field 'omega R2-rect)
         (literal-1form-field 'theta R2-rect))
        ((R2-rect '->point) (up 'x0 'y0))))
  ;; Result:
  (/ (+ (* a (theta_1 (up x0 y0)) (omega_1 (up x0 y0)))
        (* -1 b (theta_1 (up x0 y0)) (omega_0 (up x0 y0)))
        (* -1 b (omega_1 (up x0 y0)) (theta_0 (up x0 y0)))
        (* c (omega_0 (up x0 y0)) (theta_0 (up x0 y0))))
     (+ (* a c) (* -1 (expt b 2))))


  ;; Test of inversion

  (pec
   (let* ((g (g-R2 'a 'b 'c))
          (gi (metric:invert g R2-basis))
          (vector-basis (list d/dx d/dy))
          (dual-basis (list dx dy))
          (m ((R2-rect '->point) (up 'x0 'y0))))
     (matrix:generate 2 2
                      (lambda (i k)
                              (sigma (lambda (j)
                                             (* ((gi (ref dual-basis i) (ref dual-basis j)) m)
                                                ((g  (ref vector-basis j) (ref vector-basis k)) m)))
                                     0 1)))))
  ;; Result:
  (matrix-by-rows (list 1 0) (list 0 1))



  ;; Note: raise needs an extra argument -- the basis -- why?
  (pec
   ((((lower (g-R2 'a 'b 'c))
      (literal-vector-field 'v R2-rect))
     (literal-vector-field 'w R2-rect))
    ((R2-rect '->point) (up 'x0 'y0))))
  ;; Result:
  (+ (* a (v↑0 (up x0 y0)) (w↑0 (up x0 y0)))
     (* b (v↑0 (up x0 y0)) (w↑1 (up x0 y0)))
     (* b (v↑1 (up x0 y0)) (w↑0 (up x0 y0)))
     (* c (v↑1 (up x0 y0)) (w↑1 (up x0 y0))))

  (pec
   ((((raise (g-R2 'a 'b 'c) R2-basis)
      ((lower (g-R2 'a 'b 'c)) (literal-vector-field 'v R2-rect)))
     (compose (literal-function 'w (-> (UP Real Real) Real))
	            (R2-rect '->coords)))
    ((R2-rect '->point) (up 'x0 'y0))))
  ;;Result:
  (+ (* (v↑0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
     (* (v↑1 (up x0 y0)) (((partial 1) w) (up x0 y0))))

  (pec
   ((((sharpen (g-R2 'a 'b 'c) R2-basis ((R2-rect '->point) (up 'x0 'y0)))
      ((lower (g-R2 'a 'b 'c)) (literal-vector-field 'v R2-rect)))
     (compose (literal-function 'w (-> (UP Real Real) Real))
	            (R2-rect '->coords)))
    ((R2-rect '->point) (up 'x0 'y0))))

  ;; Result:
  (up (* (v↑0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
      (* (v↑1 (up x0 y0)) (((partial 1) w) (up x0 y0))))

  )
