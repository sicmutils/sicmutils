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

(ns sicmutils.calculus.covariant-test
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.abstract.function :as af]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :as c
             :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.map :as m]
            [sicmutils.calculus.manifold :as man
             :refer [R1-rect R2-rect R3-rect R3-cyl S2-spherical]]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

(deftest lie-derivative-tests
  (testing "Lie derivative."

    (comment
      (install-coordinates R3-rect (up 'x 'y 'z))

      (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

      (install-coordinates R3-cyl (up 'r 'theta 'zeta))

      (define R3-cyl-point ((R3-cyl '->point) (up 'r0 'theta0 'zeta0)))


      (define w (literal-oneform-field 'w R3-rect))
      (define u (literal-oneform-field 'u R3-rect))
      (define v (literal-oneform-field 'v R3-rect))

      (define X (literal-vector-field 'X R3-rect))
      (define Y (literal-vector-field 'Y R3-rect))
      (define Z (literal-vector-field 'Z R3-rect))
      (define W (literal-vector-field 'W R3-rect))

      (define f (literal-scalar-field 'f R3-rect))

      (clear-arguments)
      (suppress-arguments (list '(up x0 y0 z0)))

      (pec ((((Lie-derivative X) w) Y) R3-rect-point)
           (compose arg-suppressor simplify))
      ;; Result:
      (+ (* ((partial 0) w_0) X↑0 Y↑0)
         (* ((partial 0) w_1) X↑0 Y↑1)
         (* ((partial 0) w_2) X↑0 Y↑2)
         (* ((partial 1) w_0) X↑1 Y↑0)
         (* ((partial 1) w_1) X↑1 Y↑1)
         (* ((partial 1) w_2) X↑1 Y↑2)
         (* ((partial 2) w_0) X↑2 Y↑0)
         (* ((partial 2) w_1) X↑2 Y↑1)
         (* ((partial 2) w_2) X↑2 Y↑2)
         (* ((partial 0) X↑0) w_0 Y↑0)
         (* ((partial 1) X↑0) w_0 Y↑1)
         (* ((partial 2) X↑0) w_0 Y↑2)
         (* ((partial 0) X↑1) w_1 Y↑0)
         (* ((partial 1) X↑1) w_1 Y↑1)
         (* ((partial 2) X↑1) w_1 Y↑2)
         (* ((partial 0) X↑2) Y↑0 w_2)
         (* ((partial 1) X↑2) Y↑1 w_2)
         (* ((partial 2) X↑2) w_2 Y↑2))



      (pec ((- ((d ((Lie-derivative X) f)) Y)
               (((Lie-derivative X) (d f)) Y) )
            R3-rect-point)
           (compose arg-suppressor simplify))
      ;; Result:
      0


      (pec ((- ((d ((Lie-derivative X) w)) Y Z)
               (((Lie-derivative X) (d w)) Y Z) )
            ((R3-rect '->point) (up 'x↑0 'y↑0 'z↑0)))
           (compose arg-suppressor simplify))
      ;; Result:
      0




      (install-coordinates R2-rect (up 'x 'y))

      (define R2-rect-point ((R2-rect '->point) (up 'x0 'y0)))

      (define X (literal-vector-field 'X R2-rect))
      (define Y (literal-vector-field 'Y R2-rect))

      (define f (literal-scalar-field 'f R2-rect))

      (clear-arguments)
      (suppress-arguments (list '(up x0 y0)))

      (pec ((((Lie-derivative X) Y) f) R2-rect-point)
           (compose arg-suppressor simplify))
      ;; Result:
      (+ (* ((partial 0) Y↑0) X↑0 ((partial 0) f))
         (* ((partial 0) Y↑1) X↑0 ((partial 1) f))
         (* ((partial 1) Y↑0) X↑1 ((partial 0) f))
         (* ((partial 1) Y↑1) X↑1 ((partial 1) f))
         (* -1 ((partial 0) X↑0) Y↑0 ((partial 0) f))
         (* -1 ((partial 0) X↑1) Y↑0 ((partial 1) f))
         (* -1 ((partial 1) X↑0) ((partial 0) f) Y↑1)
         (* -1 ((partial 1) X↑1) Y↑1 ((partial 1) f)))



      ;; Let phi_t(x) be the integral curve of V from x for interval t

      ;; L_V Y (f) (x) = lim_t->0 ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x))/t
      ;; = D (lambda (t)
      ;;             ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x)))
      ;; (t=0)
      ;; so let g(t) = ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x))
      ;; = ( Y(f) (phi_t (x)) - Y(f circ phi_t)(x))

      ;; we only need linear terms in phi_t(x)


      ;; Perhaps

      ;; phi_t(x) = (I + t v(I))(x)

      ;; Is this correct?  No!, cannot add to a manifold point. ***********

      ;; g(t) = ( Y(f) ((I + t v(I))(x)) - Y(f circ (I + t v(I)))(x))


      (define ((((Lie-test V) Y) f) x)
        (define (g t)
          (- ((Y f) ((+ identity (* t (V identity))) x))
             ((Y (compose f (+ identity (* t (V identity))))) x)))
        ((D g) 0))


      (pec (- ((((Lie-test X) Y) f) R2-rect-point)
              ((((Lie-derivative X) Y) f) R2-rect-point)))
      ;; Result:
      0


      ;; this result is a consequence of confusing manifold points
      ;; with tuples of coordinates in the embedding space.
      (clear-arguments)

      ;; Lie derivative satisfies extended Leibnitz rule

      (define V (literal-vector-field 'V R2-rect))
      (define Y (literal-vector-field 'Y R2-rect))
      (define q_0 (up 'q_x 'q_y))
      (define m ((R2-rect '->point) q_0))
      ;; Value: m


      (define f (literal-manifold-function 'f R2-rect))

      (define e_0 (literal-vector-field 'e_0 R2-rect))
      (define e_1 (literal-vector-field 'e_1 R2-rect))

      (define vector-basis (down e_0 e_1))
      (define oneform-basis (vector-basis->dual (down e_0 e_1) R2-rect))
      (define basis (make-basis vector-basis oneform-basis))

      (define Y↑i (oneform-basis Y))

      (pe ((- (((Lie-derivative V) Y) f)
              (+ (* (s:map/r (Lie-derivative V) Y↑i) (vector-basis f))
                 (* Y↑i ((s:map/r (Lie-derivative V) vector-basis) f))))
           m))
      0

      ;; Computation of Lie derivatives by difference quotient.

      (define X (literal-vector-field 'X R2-rect))
      (define Y (literal-vector-field 'Y R2-rect))

      (define q_0 (up 'q_x 'q_y))

      (define m_0 ((R2-rect '->point) q_0))

      (define ((q coords)  t)
        (+ coords
           (* t
              ((X (R2-rect '->coords))
               ((R2-rect '->point) coords)))))

      (define (gamma initial-point)
        (compose (R2-rect '->point)
                 (q ((R2-rect '->coords) initial-point))))

      (define ((phi↑X t) point)
        ((gamma point) t))

      (define f (literal-manifold-function 'f R2-rect))


      (pe ((D (lambda (t)
                      (- ((Y f) ((phi↑X t) m_0))
                         ((Y (compose f (phi↑X t))) m_0))))
           0))
      (+ (* -1 (((partial 1) X↑0) (up q_x q_y)) (Y↑1 (up q_x q_y)) (((partial 0) f) (up q_x q_y)))
         (* -1 (Y↑1 (up q_x q_y)) (((partial 1) X↑1) (up q_x q_y)) (((partial 1) f) (up q_x q_y)))
         (* (((partial 1) Y↑0) (up q_x q_y)) (((partial 0) f) (up q_x q_y)) (X↑1 (up q_x q_y)))
         (* (((partial 0) f) (up q_x q_y)) (((partial 0) Y↑0) (up q_x q_y)) (X↑0 (up q_x q_y)))
         (* -1 (((partial 0) f) (up q_x q_y)) (((partial 0) X↑0) (up q_x q_y)) (Y↑0 (up q_x q_y)))
         (* (((partial 1) Y↑1) (up q_x q_y)) (((partial 1) f) (up q_x q_y)) (X↑1 (up q_x q_y)))
         (* (((partial 1) f) (up q_x q_y)) (((partial 0) Y↑1) (up q_x q_y)) (X↑0 (up q_x q_y)))
         (* -1 (((partial 1) f) (up q_x q_y)) (((partial 0) X↑1) (up q_x q_y)) (Y↑0 (up q_x q_y))))


      (pe ((((Lie-derivative X) Y) f) m_0))
      (+ (* -1 (((partial 1) X↑0) (up q_x q_y)) (Y↑1 (up q_x q_y)) (((partial 0) f) (up q_x q_y)))
         (* -1 (Y↑1 (up q_x q_y)) (((partial 1) X↑1) (up q_x q_y)) (((partial 1) f) (up q_x q_y)))
         (* (((partial 1) Y↑0) (up q_x q_y)) (((partial 0) f) (up q_x q_y)) (X↑1 (up q_x q_y)))
         (* (((partial 0) f) (up q_x q_y)) (((partial 0) Y↑0) (up q_x q_y)) (X↑0 (up q_x q_y)))
         (* -1 (((partial 0) f) (up q_x q_y)) (((partial 0) X↑0) (up q_x q_y)) (Y↑0 (up q_x q_y)))
         (* (((partial 1) Y↑1) (up q_x q_y)) (((partial 1) f) (up q_x q_y)) (X↑1 (up q_x q_y)))
         (* (((partial 1) f) (up q_x q_y)) (((partial 0) Y↑1) (up q_x q_y)) (X↑0 (up q_x q_y)))
         (* -1 (((partial 1) f) (up q_x q_y)) (((partial 0) X↑1) (up q_x q_y)) (Y↑0 (up q_x q_y))))


      (pe (- ((D (lambda (t)
                         (- ((Y f) ((phi↑X t) m_0))
                            ((Y (compose f (phi↑X t))) m_0))))
              0)
             ((((Lie-derivative X) Y) f) m_0)))
      0

      (pe (- ((D (lambda (t)
                         (- ((Y f) ((phi↑X t) m_0))
                            ((((pushforward-vector (phi↑X t) (phi↑X (- t)))
                               Y)
                              f)
                             ((phi↑X t) m_0)))))
              0)
             ((((Lie-derivative X) Y) f) m_0)))
      0

      (pe (- ((D (lambda (t)
                         ((((pushforward-vector (phi↑X (- t)) (phi↑X t))
                            Y)
                           f)
                          m_0)))
              0)
             ((((Lie-derivative X) Y) f) m_0)))
      0




      (define m ((R2-rect '->point) (up 'x 'y)))
      (define V (literal-vector-field 'V R2-rect))
      (define Y (literal-vector-field 'Y R2-rect))
      (define f (literal-manifold-function  'f R2-rect))


      (define e_0 (literal-vector-field 'e_0 R2-rect))
      (define e_1 (literal-vector-field 'e_1 R2-rect))

      (define vector-basis (down e_0 e_1))
      (define oneform-basis (vector-basis->dual (down e_0 e_1) R2-rect))

      (define e↑0 (ref oneform-basis 0))
      (define e↑1 (ref oneform-basis 1))

      (define basis (make-basis vector-basis oneform-basis))

      (define (Delta↑i_j v)
        (oneform-basis (s:map/r (Lie-derivative v) vector-basis)))

;;; Verifying equation 0.184

      (pec ((- (((Lie-derivative V) Y) f)
               (* (vector-basis f)
                  (+ (V (oneform-basis Y))
                     (* (oneform-basis Y) (Delta↑i_j V)))))
            m))

      ;; Result:
      0

      ;; Indeed, a painful detail:

      (pec ((- (* (oneform-basis Y) ((s:map/r (Lie-derivative V) vector-basis) f))
               (* (oneform-basis Y) (Delta↑i_j V) (vector-basis f)))
            m))
      ;; Result:
      0

      ;; Even simpler
      (pec ((- (* ((s:map/r (Lie-derivative V) vector-basis) f))
               (* (Delta↑i_j V) (vector-basis f)))
            m))

      ;; Result:
      (down 0 0)
      )))

(deftest interior-product-tests

  (comment

    ;; Claim L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)

    (install-coordinates R3-rect (up 'x 'y 'z))

    (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

    (define X (literal-vector-field 'X R3-rect))
    (define Y (literal-vector-field 'Y R3-rect))
    (define Z (literal-vector-field 'Z R3-rect))
    (define W (literal-vector-field 'W R3-rect))

    (define alpha
      (compose (literal-function 'alpha (-> (UP Real Real Real) Real))
	             (R3-rect '->coords)))
    (define beta
      (compose (literal-function 'beta (-> (UP Real Real Real) Real))
	             (R3-rect '->coords)))
    (define gamma
      (compose (literal-function 'gamma (-> (UP Real Real Real) Real))
	             (R3-rect '->coords)))

    (define omega
      (+ (* alpha (wedge dx dy))
         (* beta (wedge dy dz))
         (* gamma (wedge dz dx))))

    (define ((L1 X) omega)
      (+ ((interior-product X) (d omega))
         (d ((interior-product X) omega))))


    (pec ((- (((Lie-derivative X) omega) Y Z)
	           (((L1 X) omega) Y Z))
          ((R3-rect '->point) (up 'x0 'y0 'z0))))
    ;; Result:
    0


    (pec (let ((omega (literal-1form-field 'omega R3-rect)))
           ((- (((Lie-derivative X) omega) Y)
	             (((L1 X) omega) Y))
	          ((R3-rect '->point) (up 'x0 'y0 'z0)))))
    ;; Result:
    0


    (pec (let ((omega (* alpha (wedge dx dy dz))))
           ((- (((Lie-derivative X) omega) Y Z W)
	             (((L1 X) omega) Y Z W))
	          ((R3-rect '->point) (up 'x0 'y0 'z0)))))
    ;; Result:
    0
    )

  )
