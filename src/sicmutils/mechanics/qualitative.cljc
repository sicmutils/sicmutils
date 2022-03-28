#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.qualitative
  (:refer-clojure :exclude [+ - * / partial time])
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f :refer [compose]]
            [sicmutils.generic :as g :refer [cos sin + - * /]]
            [sicmutils.numerical.minimize :as m]
            [sicmutils.numerical.quadrature :as q]
            [sicmutils.operator :as o]
            [sicmutils.polynomial :as p]
            [sicmutils.structure :as s :refer [up up?]]))

;;; Homoclinic tangles

(comment
  (define ((unstable-manifold T xe ye dx dy A eps) param)
    (let ((n (floor->exact (/ (log (/ param eps)) (log A)))))
      ((iterated-map T n) (+ xe (* dx (/ param (expt A n))))
       (+ ye (* dy (/ param (expt A n))))
       cons
       (lambda ()
               (error "Failed"))))))

(comment
  (define (fixed-point-eigen T xe ye eps cont)
    (let ((M00 ((richardson-derivative
                 (lambda (dx)
                         (T (+ xe dx) ye
                            (lambda (x y)
                                    ((principal-value pi) (- x xe)))
                            'failure))
                 eps)
                0.0))
          (M01 ((richardson-derivative
                 (lambda (dx)
                         (T xe (+ ye dx)
                            (lambda (x y)
                                    ((principal-value pi) (- x xe)))
                            'failure))
                 eps)
                0.0))
          (M10 ((richardson-derivative
                 (lambda (dx)
                         (T (+ xe dx) ye
                            (lambda (x y) y)
                            'failure))
                 eps)
                0.0))
          (M11 ((richardson-derivative
                 (lambda (dx)
                         (T xe (+ ye dx)
                            (lambda (x y) y)
                            'failure))
                 eps)
                0.0)))
      (let ((trace (+ M00 M11))
            (determinant (- (* M00 M11) (* M01 M10))))
        (quadratic 1. (- trace) determinant
                   (lambda (root1 root2)
                           (cont root1 M01 (- root1 M00)
                                 root2 M01 (- root2 M00))))))))
;; 
;; #| in open.scm

;; (define (plot-parametric-fill win f a b near?)
;;   (let loop ((a a) (xa (f a)) (b b) (xb (f b)))
;;        (let ((m (/ (+ a b) 2)))
;;          (let ((xm (f m)))
;;            (plot-point win (car xm) (cdr xm))
;;            (if (not (and (near? xa xm) (near? xb xm)))
;;              (begin (loop a xa m xm)
;;                     (loop m xm b xb)))))))

;; (define (cylinder-near? eps)
;;   (let ((eps2 (square eps)))
;;     (lambda (x y)
;;             (< (+ (square ((principal-value pi)
;;                            (- (car x) (car y))))
;;                   (square (- (cdr x) (cdr y))))
;;                eps2))))
;; |#

;;; Poincare-Birkhoff

(comment
  (define (radially-mapping-points map Jmin Jmax phi eps)
    (bisect
     (lambda (J)
             ((principal-value pi)
              (- phi (map phi J (lambda (phip Jp) phip) list))))
     Jmin Jmax eps)))

;;; See indexed/driven-pend-evolution.scm


;;; Invariant Curves

(comment
  (define (find-invariant-curve map rn theta0 Jmin Jmax eps)
    (bisect (lambda (J) (which-way? rn theta0 J map))
            Jmin Jmax eps)))

;; #|

;; (define (which-way? rn theta0 J0 map)
;;   (compare-streams
;;    (position-stream theta0
;;                     (orbit-stream map theta0 J0)
;;                     '())
;;    (position-stream theta0
;;                     (orbit-stream (circle-map rn) theta0 J0)
;;                     '())
;;    0))

;; (define (circle-map rotation-number)
;;   (let ((delta-theta (* :2pi rotation-number)))
;;     (lambda (theta y result fail)
;;             (result ((principal-value :2pi) (+ theta delta-theta))
;;                     y))))

;; (define (orbit-stream the-map x y)
;;   (cons-stream (list x y)
;;                (the-map x y
;;                         (lambda (nx ny)
;;                                 (orbit-stream the-map nx ny))
;;                         (lambda () 'fail))))

;; (define (position-stream cut orbit list)
;;   (insert! ((principal-value cut) (car (head orbit)))
;;            list
;;            (lambda (nlist position)
;;                    (cons-stream
;;                     position
;;                     (position-stream cut (tail orbit) nlist)))))


;; (define (insert! x set cont)
;;   (cond ((null? set)
;;          (cont (list x) 1))
;;         ((< x (car set))
;;          (cont (cons x set) 0))
;;         (else
;;          (let lp ((i 1) (lst set))
;;               (cond ((null? (cdr lst))
;;                      (set-cdr! lst (cons x (cdr lst)))
;;                      (cont set i))
;;                     ((< x (cadr lst))
;;                      (set-cdr! lst (cons x (cdr lst)))
;;                      (cont set i))
;;                     (else
;;                      (lp (+ i 1) (cdr lst))))))))

;; (define (compare-streams s1 s2 count)
;;   (if (= (head s1) (head s2))
;;     (compare-streams (tail s1) (tail s2) (+ count 1))
;;     ((principal-range count) (- (head s2) (head s1)))))

;; (find-invariant-curve (standard-map 0.95)
;;                       (- 1 (/ 1 golden-mean))
;;                       0.0
;;                       2.0
;;                       2.2
;;                       1e-5)
;;                                         ;Value: 2.114462280273437

;; |#

(comment
  (define (which-way? rotation-number x0 y0 map)
    (let ((pv (principal-value (+ x0 pi))))
      (let lp ((n 0)
               (z x0) (zmin (- x0 two-pi)) (zmax (+ x0 two-pi))
               (x x0) (xmin (- x0 two-pi)) (xmax (+ x0 two-pi)) (y y0))
           (let ((nz (pv (+ z (* two-pi rotation-number)))))
             (map x y
                  (lambda (nx ny)
                          (let ((nx (pv nx)))
                            (cond ((< x0 z zmax)
                                   (if (< x0 x xmax)
                                     (lp (+ n 1) nz zmin z nx xmin x ny)
                                     (if (> x xmax) 1 -1)))
                                  ((< zmin z x0)
                                   (if (< xmin x x0)
                                     (lp (+ n 1) nz z zmax nx x xmax ny)
                                     (if (< x xmin) -1 1)))
                                  (else
                                   (lp (+ n 1) nz zmin zmax nx xmin xmax ny)))))
                  (lambda ()
                          (error "Map failed" x y))))))))
