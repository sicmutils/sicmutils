;;
;; Copyright © 2022 Sam Ritchie.
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

(ns sicmutils.mechanics.routhian
  (:refer-clojure :exclude [+ - * / partial])
  #_
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f :refer [compose]]
            [sicmutils.generic :as g :refer [cos sin + - * /]]
            [sicmutils.numerical.minimize :as m]
            [sicmutils.numerical.quadrature :as q]
            [sicmutils.polynomial :as p]
            [sicmutils.structure :refer [up]]))


SICM2 p.233...

Routhian equations of motion

;; Assume a Lagrangian of the form L(t; x, y; vx, vy), where x and y may have
;; substructure.
;;
;; We perform a Legendre transform on vy to get the Routhian.
;;
;; The equations of motion are Hamilton's equations for the py, y and Lagrange's
;; equations for vx, x.

(comment
  (define ((Lagrangian->Routhian Lagrangian) R-state)
    (let ((t (time R-state))
          (q (coordinate R-state))
          (vp (ref R-state 2)))
      (let ((x (ref q 0))
            (y (ref q 1))
            (vx (ref vp 0))
            (py (ref vp 1)))
        (define (L vy)
          (Lagrangian (up t q (up vx vy))))
        ((Legendre-transform-procedure L) py))))

  (define (((Routh-equations Routhian) x y py) t)
    (define (L s)
      (let ((tau (time s))
            (q (coordinate s))
            (v (velocity s)))
        (Routhian (up tau (up q (y tau)) (up v (py tau))))))
    (define (H s)
      (let ((tau (time s))
            (q (coordinate s))
            (p (momentum s)))
        (Routhian (up tau (up (x tau) q) (up ((D x) tau) p)))))
    (up
     (((Lagrange-equations L) x) t)
     (((Hamilton-equations H) y py) t)
     ))

  (define (((Routh-equations-bad Routhian) x y py) t)
    (define (L s)
      (let ((tau (time s))
            (q (coordinate s))
            (v (velocity s)))
        (Routhian (up tau (up q (y t)) (up v (py t))))))
    (define (H s)
      (let ((tau (time s))
            (q (coordinate s))
            (p (momentum s)))
        (Routhian (up tau (up (x t) q) (up ((D x) t) p)))))
    (up
     (((Lagrange-equations L) x) t)
     (((Hamilton-equations H) y py) t)
     )))


;;TODO tests

;; #|

;; (define ((Lag mx kx my ky) s)
;;   (let ((t (time s))
;;        (q (coordinate s))
;;        (v (velocity s)))
;;     (let ((x (ref q 0))
;;          (y (ref q 1))
;;          (vx (ref v 0))
;;          (vy (ref v 1)))
;;       (- (+ (* 1/2 mx (square vx))
;;            (* 1/2 my (square vy)))
;;         (+ (* 1/2 kx (square x))
;;            (* 1/2 ky (square y))
;;            (* x y y))))))

;; (pe ((Lagrangian->Routhian (Lag 'mx 'kx 'my 'ky))
;;      (up 't (up 'x 'y) (up 'vx 'py))))
;; (+ (* 1/2 kx (expt x 2))
;;    (* 1/2 ky (expt y 2))
;;    (* -1/2 mx (expt vx 2))
;;    (* x (expt y 2))
;;    (/ (* 1/2 (expt py 2)) my))
;;                                         ; ok

;; (pe (((Routh-equations
;;        (Lagrangian->Routhian (Lag 'mx 'kx 'my 'ky)))
;;       (literal-function 'x)
;;       (literal-function 'y)
;;       (literal-function 'py))
;;      't))

;; (up
;;  (+ (* -1 kx (x t)) (* -1 mx (((expt D 2) x) t)) (* -1 (expt (y t) 2)))
;;  (up 0
;;      (+ ((D y) t) (/ (* -1 (py t)) my))
;;      (+ (* ky (y t)) (* 2 (y t) (x t)) ((D py) t))))
;;                                         ;looks good

;; (define ((Lag2 m k) s)
;;   (let ((t (time s))
;;        (q (coordinate s))
;;        (v (velocity s)))
;;     (let ((x (ref q 0))
;;          (y (ref q 1))
;;          (vx (ref v 0))
;;          (vy (ref v 1)))
;;       (- (+ (* 1/2 m (square vx))
;;            (* 1/2 m (square vy)))
;;         (+ (* 1/2 k (square x))
;;            (* 1/2 k (square y)))))))

;; (pe (((Routh-equations
;;        (Lagrangian->Routhian (Lag2 'm 'k)))
;;       (up (literal-function 'x0) (literal-function 'x1))
;;       (up (literal-function 'y0) (literal-function 'y1))
;;       (down (literal-function 'py1) (literal-function 'py1)))
;;      't))
;; (up
;;  (down (+ (* -1 k (x0 t)) (* -1 m (((expt D 2) x0) t)))
;;        (+ (* -1 k (x1 t)) (* -1 m (((expt D 2) x1) t))))
;;  (up
;;   0
;;   (up (+ ((D y0) t) (/ (* -1 (py1 t)) m))
;;       (+ ((D y1) t) (/ (* -1 (py1 t)) m)))
;;   (down (+ (* k (y0 t)) ((D py1) t))
;;        (+ (* k (y1 t)) ((D py1) t)))))
;; ;;; good

;; |#

(comment
  (define ((Routhian->acceleration R #!optional dissipation-function) s)
    (if (default-object? dissipation-function)
      (let ((minus-P ((partial 2 0) R))
            (minus-F (((partial 1 0) R) s))
            (vy (((partial 2 1) R) s))
            (pyd ((* -1 ((partial 1 1) R)) s)))
        (* (s:inverse (ref s 2 0) (((partial 2 0) minus-P) s) (ref s 2 0))
           (- minus-F
              (+ (((partial 0) minus-P) s)
                 (* (((partial 1 0) minus-P) s) ((component 2 0) s))
                 (* (((partial 1 1) minus-P) s) vy)
                 (* (((partial 2 1) minus-P) s) pyd)
                 ))))
      (let ((minus-P ((partial 2 0) R))
            (minus-F (((partial 1 0) R) s))
            (vy (((partial 2 1) R) s)))
        (let ((minus-F0 (((partial 2 0) dissipation-function)
                         (up (time s)
                             (coordinate s)
                             (up (ref s 2 0) vy))))
              (minus-F1 (((partial 2 1) dissipation-function)
                         (up (time s)
                             (coordinate s)
                             (up (ref s 2 0) vy)))))
          (let ((pyd (- ((* -1 ((partial 1 1) R)) s) minus-F1)))
            (* (s:inverse (ref s 2 0) (((partial 2 0) minus-P) s) (ref s 2 0))
               (+ (- minus-F
                     (+ (((partial 0) minus-P) s)
                        (* (((partial 1 0) minus-P) s) ((component 2 0) s))
                        (* (((partial 1 1) minus-P) s) vy)
                        (* (((partial 2 1) minus-P) s) pyd)
                        ))
                  minus-F0))))))))

;; TODO test

;; (pe ((Routhian->acceleration
;;       (Lagrangian->Routhian (Lag2 'm 'k)))
;;      (up 't
;;         (up (up 'x0 'x1) (up 'y0 'y1))
;;         (up (up 'vx0 'vx1) (down 'py0 'py1)))))
;; (up (/ (* -1 k x0) m) (/ (* -1 k x1) m))



(comment
  (define ((Routhian->state-derivative R #!optional dissipation-function) s)
    (let ((minus-P ((partial 2 0) R))
          (minus-F (((partial 1 0) R) s))
          (vx (ref s 2 0))
          (vy (((partial 2 1) R) s)))
      (if (default-object? dissipation-function)
        (let ((pyd (- (((partial 1 1) R) s))))
          (up 1
              (up vx vy)
              (up
               (* (s:inverse vx (((partial 2 0) minus-P) s) vx)
                  (- minus-F
                     (+ (((partial 0) minus-P) s)
                        (* (((partial 1 0) minus-P) s) vx)
                        (* (((partial 1 1) minus-P) s) vy)
                        (* (((partial 2 1) minus-P) s) pyd)
                        )))
               pyd)))
        (let ((minus-F0 (((partial 2 0) dissipation-function)
                         (up (time s) (coordinate s) (up vx vy))))
              (minus-F1 (((partial 2 1) dissipation-function)
                         (up (time s) (coordinate s) (up vx vy)))))
          (let ((pyd (- ((* -1 ((partial 1 1) R)) s) minus-F1)))
            (up 1
                (up vx vy)
                (up
                 (* (s:inverse vx (((partial 2 0) minus-P) s) vx)
                    (+ (- minus-F
                          (+ (((partial 0) minus-P) s)
                             (* (((partial 1 0) minus-P) s) vx)
                             (* (((partial 1 1) minus-P) s) vy)
                             (* (((partial 2 1) minus-P) s) pyd)
                             ))
                       minus-F0))
                 pyd))))))))

;; (pe ((Routhian->state-derivative
;;       (Lagrangian->Routhian (Lag2 'm 'k)))
;;      (up 't
;;         (up (up 'x0 'x1) (up 'y0 'y1))
;;         (up (up 'vx0 'vx1) (down 'py0 'py1)))))

;; (up
;;  1
;;  (up (up vx0 vx1)
;;      (up (/ py0 m) (/ py1 m)))
;;  (up (up (/ (* -1 k x0) m) (/ (* -1 k x1) m))
;;      (down (* -1 k y0) (* -1 k y1))))



;; a test of the dissipation function

;; ;;; in Lagrangian variables
;; (define ((diss2 delta0 delta1) s)
;;   (+ (* 1/2 delta0 (square (ref s 2 0)))
;;      (* 1/2 delta1 (square (ref s 2 1)))))

;; (pe ((Routhian->state-derivative
;;       (Lagrangian->Routhian (Lag2 'm 'k))
;;       (diss2 'delta0 'delta1))
;;      (up 't
;;         (up (up 'x0 'x1) (up 'y0 'y1))
;;         (up (up 'vx0 'vx1) (down 'py0 'py1)))))
;; (up
;;  1
;;  (up (up vx0 vx1) (up (/ py0 m) (/ py1 m)))
;;  (up
;;   (up (+ (/ (* -1 delta0 vx0) m) (/ (* -1 k x0) m))
;;       (+ (/ (* -1 delta0 vx1) m) (/ (* -1 k x1) m)))
;;   (down (+ (* -1 k y0) (/ (* -1 delta1 py0) m))
;;         (+ (* -1 k y1) (/ (* -1 delta1 py1) m)))))

;; ok

(comment
  (define ((Lagrangian-state->Routhian-state L) s)
    (let ((t (time s))
          (q (coordinate s))
          (v (velocity s)))
      (let ((vx (ref v 0)))
        (let ((py (ref (((partial 2) L) s) 1)))
          (up t
              q
              (up vx py))))))

  (define ((Routhian-state->Lagrangian-state R) s)
    (let ((t (time s))
          (q (coordinate s))
          (v (velocity s)))
      (let ((vx (ref v 0)))
        (let ((vy (ref (((partial 2) R) s) 1)))
          (up t
              q
              (up vx vy)))))))

;; #|;;; Two 2-dimensional particles

;; (define ((L m1 m2 V) s)
;;   (let ((t (time s))
;;        (q (coordinate s))
;;        (v (velocity s)))
;;     (let ((xy1 (ref q 0))
;;          (xy2 (ref q 1))
;;          (v1 (ref v 0))
;;          (v2 (ref v 1)))
;;       (- (+ (* 1/2 m1 (square v1))
;;            (* 1/2 m2 (square v2)))
;;         (V xy1 xy2)))))

;; (pe ((Lagrangian->Routhian
;;       (L 'm1 'm2
;;         (literal-function 'V
;;                           (-> (X (UP Real Real) (UP Real Real)) Real))))
;;      (up 't
;;         (up (up 'x1 'y1) (up 'x2 'y2))
;;         (up (up 'v1x 'v1y) (down 'p2x 'p2y)))))
;; (+ (* -1/2 m1 (expt v1x 2))
;;    (* -1/2 m1 (expt v1y 2))
;;    (V (up x1 y1) (up x2 y2))
;;    (/ (* 1/2 (expt p2x 2)) m2)
;;    (/ (* 1/2 (expt p2y 2)) m2))


;; (pe (((Routh-equations
;;        (Lagrangian->Routhian
;;        (L 'm1 'm2
;;           (literal-function 'V
;;                             (-> (X (UP Real Real) (UP Real Real)) Real)))))
;;       (up (literal-function 'x1) (literal-function 'y1))
;;       (up (literal-function 'x2) (literal-function 'y2))
;;       (down (literal-function 'p2x) (literal-function 'p2y)))
;;      't))
;; (up
;;  (down
;;   (+ (* -1 m1 (((expt D 2) x1) t))
;;      (* -1 (((partial 0 0) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t)))))
;;   (+ (* -1 m1 (((expt D 2) y1) t))
;;      (* -1 (((partial 0 1) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t))))))
;;  (up
;;   0
;;   (up (+ ((D x2) t) (/ (* -1 (p2x t)) m2))
;;       (+ ((D y2) t) (/ (* -1 (p2y t)) m2)))
;;   (down
;;    (+ ((D p2x) t) (((partial 1 0) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t))))
;;    (+ ((D p2y) t) (((partial 1 1) V) (up (x1 t) (y1 t)) (up (x2 t) (y2 t)))))))
;; |#
