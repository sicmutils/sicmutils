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

(ns sicmutils.mechanics.pendulum
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.generic :as g :refer [sin cos + - * /]]
            [sicmutils.mechanics.hamilton :as h]
            [sicmutils.mechanics.lagrange :as l]
            [sicmutils.numerical.roots.bisect :as bi]
            [sicmutils.special.elliptic :as ell]
            [sicmutils.value :as v]))

;; ## THE PENDULUM

;; definition H = p^2/(2alpha) - beta cos(theta)
;; ASSUME alpha > 0 and beta > 0
;; alpha = ml^2  beta = mgl for ordinary pendulum

(defn Hpendulum [alpha beta]
  (fn [state]
    (let [theta (l/state->q state)
	        ptheta (h/state->p state)]
      (- (/ (g/square ptheta)
            (* 2 alpha))
         (* beta (cos theta))))))

(defn pendulum-sysder [alpha beta]
  (h/Hamiltonian->state-derivative
   (Hpendulum alpha beta)))

(def pendulum-Hamiltonian Hpendulum)

;; ## oscillating case

(defn pendulum-oscillating-frequency [alpha beta E]
  (let [k (g/sqrt (/ (+ E beta)
                     (* 2.0 beta)))
	      omega0 (g/sqrt (g/abs (/ beta alpha)))]
    (/ (* Math/PI omega0)
       (* 2.0 (ell/elliptic-k k)))))

(defn pendulum-oscillating-angle [alpha beta E]
  (let [k (g/sqrt (/ (+ E beta) (* 2.0 beta)))
	      omega-0 (g/sqrt (/ beta alpha))]
    (fn [t]
      (ell/jacobi-elliptic-functions
       (* omega-0 t)
       k
       (fn [sn _cn _dn]
	       (* 2.0 (g/asin (* k sn))))))))

(defn pendulum-oscillating-angular-momentum [alpha beta E]
  (let [k (g/sqrt (/ (+ E beta) (* 2.0 beta)))
	      omega-0 (g/sqrt (/ beta alpha))]
    (fn [t]
      (ell/jacobi-elliptic-functions
       (* omega-0 t)
       k
       (fn [_sn cn _dn]
	       (* 2.0 alpha omega-0 k cn))))))

;; freq = (/ (* pi omega0) (* 2 (ell/elliptic-k k)))
;; period = 4 K / omega0

;; omega0 period = 4 K the period of sn

(defn pendulum-oscillating-action [alpha beta E]
  (let [k**2 (/ (+ beta E) (* 2.0 beta))]
    (if (= k**2 1.0)
	    (* (/ 8.0 Math/PI) (g/sqrt (* beta alpha)))
	    (ell/elliptic-integrals
	     (Math/sqrt k**2)
	     (fn [Kk Ek]
	       (* (/ 8.0 Math/PI)
	          (g/sqrt (* beta alpha))
	          (- Ek (* (- 1.0 k**2) Kk))))))))

(defn pendulum-f [k]
  (if (= k 1.0)
    1.0
    (ell/elliptic-integrals
     k
     (fn [Kk Ek]
	     (- Ek (* (- 1.0 (g/square k)) Kk))))))

(defn pendulum-g [k]
  (/ (ell/elliptic-e k) k))

(defn pendulum-inverse-g [gk]
  (let [inv-gk (/ 1.0 gk)]
    (bi/bisect (fn [k]
	               (if (zero? k)
		               (- inv-gk)
		               (- (/ 1.0 (pendulum-g k)) inv-gk)))
	             0.0 1.0 1.e-10)))

(defn pendulum-inverse-f [fk]
  (let [sfk (g/sqrt fk)]
    (bi/bisect (fn [k]
	               (- (g/sqrt (pendulum-f k)) sfk))
	             0.0 1.0 1e-10)))

(defn pendulum-oscillating-action-to-E [alpha beta]
  (fn [action]
    (let [f (/ action (* (/ 8.0 Math/PI)
                         (g/sqrt (* beta alpha))))
          k (pendulum-inverse-f f)]
      (* beta (- (* 2.0 (g/square k)) 1.0)))))

;; action angle -pi to pi

(define (pendulum-oscillating-phase alpha beta)
  (let [omega-0 (g/sqrt (/ beta alpha))]
    (fn (state)
      (let ((theta (state->q state))
	          (ptheta (state->p state)))
	      (let ((E ((Hpendulum alpha beta) state)))
	        (if (> E (- beta))
	          (let ((k (g/sqrt (/ (+ E beta) (* 2.0 beta))))
		              (period (/ v/twopi (pendulum-frequency alpha beta E))))
		          (let* ((sin-phi (/ (sin (/ theta 2.0)) k))
		                 (dt0 (/ (elliptic-integral-F (asin sin-phi) k) omega-0)))
		            (let ((dt (if (< ptheta 0) (- (/ period 2.0) dt0) dt0)))
		              ((principal-value Math/PI) (* v/twopi (/ dt period))))))
	          (error "at the fixed point the phase is undefined")))))))

;;; time from theta=0 to state
(define ((pendulum-oscillating-dt alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state))
	      (phase ((pendulum-oscillating-phase alpha beta) state)))
    (let ((period (/ v/twopi (pendulum-frequency alpha beta E))))
      (* phase (/ period v/twopi)))))

(define ((pendulum-oscillating-aa-state-to-state alpha beta) aa-state)
  (let ((angle (state->q aa-state))
	      (action (state->p aa-state)))
    (let* ((E ((pendulum-oscillating-action-to-E alpha beta) action))
	         (period (/ v/twopi (pendulum-frequency alpha beta E))))
      (let ((dt (* (/ period v/twopi) angle)))
	      (->H-state (state->t aa-state)
		               ((pendulum-oscillating-angle alpha beta E) dt)
		               ((pendulum-oscillating-angular-momentum alpha beta E) dt))))))

(defn pendulum-oscillating-state-to-aa-state [alpha beta]
  (fn [state]
    (let [E ((Hpendulum alpha beta) state)
          action (pendulum-oscillating-action alpha beta E)
          angle ((pendulum-oscillating-phase alpha beta) state)]
      (h/->H-state (l/state->t state) angle action))))

;;;----------------------------------------------------------------
;;; circulating case

(define (pendulum-circulating-frequency alpha beta E)
  (let ((k (g/sqrt (/ (* 2.0 beta) (+ E beta))))
	      (omegaR (g/sqrt (abs (/ (+ E beta) (* 2.0 alpha))))))
	  (/ (* pi omegaR) (ell/elliptic-k k))))

(define (pendulum-circulating-angle alpha beta E)
  (let ((k (g/sqrt (/ (* 2.0 beta) (+ E beta))))
	      (omega-R (g/sqrt (abs (/ (+ E beta) (* 2.0 alpha)))))
	      (period (/ v/twopi (pendulum-frequency alpha beta E))))
    (fn (t)
            (Jacobi-elliptic-functions
             (* omega-R ((principal-range period) t))
             k
             (fn (sn cn dn)
	                   (* 2.0 (asin sn)))))))

(define (pendulum-circulating-angular-momentum alpha beta E)
  (let ((k (g/sqrt (/ (* 2.0 beta) (+ E beta))))
	      (omega-R (g/sqrt (abs (/ (+ E beta) (* 2.0 alpha)))))
	      (period (/ v/twopi (pendulum-frequency alpha beta E))))
    (fn (t)
      (Jacobi-elliptic-functions
       (* omega-R ((principal-range period) t))
       k
       (fn (sn cn dn)
	       (* 2.0 alpha omega-R dn))))))


;;; Defined in kernel/numeric.scm

;; (define ((principal-range period) t)
;;   (let ((t (- t (* period (floor (/ t period))))))
;;     (if (< t (/ period 2.0))
;; 	    t
;; 	    (- t period))))
;; |#

;; #|

;; omega =	(/ (* pi omegaR) (ell/elliptic-k k)))))
;; period = 2pi / omega = 2 K / omegaR
;; so period*omegaR = 2 K but the period of sn is 4 K
;; so if period*omegaR is in the range 2K to 4K the
;; program would not work without the principal-range call


(define (pendulum-circulating-action alpha beta E)
  (let* ((k (g/sqrt (/ (* 2.0 beta) (+ beta E))))
         (Ek (ell/elliptic-e k)))
    (* (/ 4. pi)
       (g/sqrt (* beta alpha))
       (/ Ek k))))

(define ((pendulum-circulating-action-to-E alpha beta) action)
  (let ((g (/ action (* (/ 4. pi) (g/sqrt (* beta alpha))))))
    (let ((k (pendulum-inverse-g g)))
      (let ((k**2 (square k)))
	      (/ (* beta (- 2.0 k**2)) k**2)))))


(define ((pendulum-circulating-phase alpha beta) state)
  (let ((theta (state->q state))
        (ptheta (state->p state)))
    (let ((E ((Hpendulum alpha beta) state)))
      (let ((k (g/sqrt (/ (* 2.0 beta) (+ E beta))))
	          (omega-R (g/sqrt (abs (/ (+ E beta) (* 2.0 alpha)))))
	          (period (/ v/twopi (pendulum-frequency alpha beta E))))
	      (let ((dt (/ (elliptic-integral-F
		                  (/ ((principal-value pi) theta) 2.0) k)
		                 omega-R)))
	        ((principal-value pi) (* v/twopi (/ dt period))))))))

;;; time from theta=0 to state
(define ((pendulum-circulating-dt alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state))
        (phase ((pendulum-circulating-phase alpha beta) state)))
    (let ((period (/ v/twopi (pendulum-frequency alpha beta E))))
      (* phase (/ period v/twopi)))))

(define ((pendulum-circulating-aa-state-to-state alpha beta) aa-state)
  (let ((angle (state->q aa-state))
        (action (state->p aa-state)))
    (let* ((E ((pendulum-circulating-action-to-E alpha beta) action))
	         (period (/ v/twopi (pendulum-frequency alpha beta E))))
      (let ((dt (* (/ period v/twopi) angle)))
	      (->H-state (state->t aa-state)
		               ((pendulum-circulating-angle alpha beta E) dt)
		               ((pendulum-circulating-angular-momentum alpha beta E) dt))))))

(define ((pendulum-circulating-state-to-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (let ((action (pendulum-circulating-action alpha beta E))
	        (angle ((pendulum-circulating-phase alpha beta) state)))
      (->H-state (state->t state) angle action))))

;;;----------------------------------------------------------------
;;; separatrix case

(define ((pendulum-separatrix-angle alpha beta) t)
  (let ((omega-0 (g/sqrt (abs (/ beta alpha)))))
    (* 2.0 (gudermannian (* omega-0 t)))))

(define ((pendulum-separatrix-angular-momentum alpha beta) t)
  (let ((theta ((pendulum-separatrix-angle alpha beta) t)))
    (g/sqrt (* 2.0 alpha beta (+ 1. (cos theta))))))

(define (gudermannian x)
  (- (* 2.0 (atan (exp x))) pi/2))

(define (inverse-gudermannian x)
  (log (tan (+ (/ x 2.0) pi/4))))


;;; area of "eye"
(define (pendulum-separatrix-action alpha beta)
  (* (/ 8. pi) (g/sqrt (* alpha beta))))

;;;----------------------------------------------------------------
;;; pendulum state advancer

(define (((pendulum-advance alpha beta) state) t)
  (let ((E ((Hpendulum alpha beta) state)))
    (if (< E beta)
	    (let ((dt ((pendulum-oscillating-dt alpha beta) state)))
	      (let ((t' (+ dt (- t (state->t state)))))
	        (->H-state t
		                 ((pendulum-oscillating-angle alpha beta E) t')
		                 ((pendulum-oscillating-angular-momentum alpha beta E) t'))))

	    (if (> (state->p state) 0)
	      (let ((dt ((pendulum-circulating-dt alpha beta) state)))
	        (let ((t' (+ dt (- t (state->t state)))))
		        (->H-state t
			                 ((pendulum-circulating-angle alpha beta E) t')
			                 ((pendulum-circulating-angular-momentum alpha beta E) t'))))
	      (let ((dt ((pendulum-circulating-dt alpha beta)
		               (->H-state (- (state->t state))
				                      (- (state->q state))
				                      (- (state->p state))))))
	        (let ((t' (+ dt (- t (state->t state)))))
		        (->H-state
		         t
		         (- ((pendulum-circulating-angle alpha beta E) t'))
		         (- ((pendulum-circulating-angular-momentum alpha beta E) t')))))))))


(define (((pendulum-integration alpha beta eps) state) t)
  (let ((state2
	       ((state-advancer pendulum-sysder alpha beta)
	        state (- t (state->t state)) eps)))
    (->H-state (state->t state2)
	             ((principal-value pi) (state->q state2))
	             (state->p state2))))



;;;----------------------------------------------------------------
;;; series solutions

;; #|
;; (define ((pendulum-oscillating-solution-series alpha beta E omega eps) t)
;;   (let ((k (g/sqrt (/ (+ E beta) (* 2.0 beta))))
;; 	      (omega-0 (g/sqrt (/ beta alpha))))
;;     (let ((Kp (ell/elliptic-k (g/sqrt (- 1. (square k))))))
;;       (define (term n)
;; 	      (let ((omega-n (* omega (- (* 2.0 n) 1.))))
;; 	        (/ (sin (* omega-n t))
;; 	           (* omega-n (cosh (/ (* omega-n Kp) omega-0))))))
;;       (* 4. omega (sum-series term eps)))))

;; (define ((pendulum-circulating-solution-series alpha beta E omega eps) t)
;;   (let ((k (g/sqrt (/ (* 2.0 beta) (+ E beta))))
;; 	      (omega-R (g/sqrt (abs (/ (+ E beta) (* 2.0 alpha))))))
;;     (let ((Kp (ell/elliptic-k (g/sqrt (- 1. (square k))))))
;;       (define ((term t) n)
;; 	      (let ((omega-n (* omega n)))
;; 	        (/ (sin (* omega-n t))
;; 	           (* omega-n (cosh (/ (* omega-n Kp) omega-R))))))
;;       (+ (* omega t)
;; 	       (* 2.0 omega (sum-series (term t) eps))))))

;; ;;; don't use this without thinking...
;; (define (sum-series term eps)
;;   (let loop ((n 1) (sum 0.) (lastf 1.))
;;        (let ((f (term n)))
;;          (if (and (< (abs f) eps) (< (abs lastf) eps))
;; 	         sum
;; 	         (loop (fix:+ n 1) (+ sum f) f)))))
;; ;;; purpose of checking last two is
;; ;;; to prevent some premature terminations
;; ;;; because a term is "accidently" zero

;; |#


;; #|

;; (define (((pendulum-solution-series alpha beta) state) t)
;;   (let ((E ((Hpendulum alpha beta) state)))
;;     (let ((omega (pendulum-frequency alpha beta E))
;; 	        (beta (abs beta)))
;;       (if (< E beta)
;; 	      (let ((k (g/sqrt (/ (+ E beta) (* 2 beta))))
;; 		          (omega-0 (g/sqrt (abs (/ beta alpha)))))
;; 	        (let ((Kp (ell/elliptic-k (g/sqrt (- 1 (square k))))))
;; 	          (define (term n)
;; 		          (let ((omega-n (* omega (- (* 2 n) 1))))
;; 		            (/ (sin (* omega-n t))
;; 		               (* omega-n (cosh (/ (* omega-n Kp) omega-0))))))
;; 	          (* 4 omega (series:generate (fn (i) (term (+ i 1)))))))
;; 	      (let ((k (g/sqrt (/ (* 2 beta) (+ E beta))))
;; 		          (omega-R (g/sqrt (abs (/ (+ E beta) (* 2 alpha))))))
;; 	        (let ((Kp (ell/elliptic-k (g/sqrt (- 1 (square k))))))
;; 	          (define ((term t) n)
;; 		          (let ((omega-n (* omega n)))
;; 		            (/ (sin (* omega-n t))
;; 		               (* omega-n (cosh (/ (* omega-n Kp) omega-R))))))
;; 	          (+ (* omega t)
;; 		           (* 2 omega (series:generate (fn (i) ((term t) (+ i 1))))))))))))

;; (series:print
;;  (((pendulum-solution-series 1. 9.8)
;;    (->H-state 0. 0. 4.9006733894348145)) 't)
;;  10)
;; (* 1.8349993630564594 (sin (* 2.5043962735932013 t)))
;; (* .03821300344597103 (sin (* 7.513188820779604 t)))
;; (* .00135312864251141 (sin (* 12.521981367966006 t)))
;; (* 5.702944261999213e-5 (sin (* 17.53077391515241 t)))
;; (* 2.617233223741749e-6 (sin (* 22.53956646233881 t)))
;; (* 1.2635138738869227e-7 (sin (* 27.548359009525214 t)))
;; (* 6.308369363000512e-9 (sin (* 32.55715155671162 t)))
;; (* 3.225945107424557e-10 (sin (* 37.56594410389802 t)))
;; (* 1.679527336266625e-11 (sin (* 42.57473665108442 t)))
;; (* 8.866866369088442e-13 (sin (* 47.583529198270824 t)))
;;                                         ;Value: ...

;; |#



;; #|
;; ;;; Check that the canonical transformation is area-preserving.

;; (define (der-qq f state)
;;   ((richardson-derivative
;;     (fn (q)
;;             (state->q
;;              (f (->H-state (state->t state) q (state->p state)))))
;;     1.e-8
;;     .01)
;;    (state->q state)))

;; (define (der-qp f state)
;;   ((richardson-derivative
;;     (fn (p)
;;             (state->q
;;              (f (->H-state (state->t state) (state->q state) p))))
;;     1.e-8
;;     .01)
;;    (state->p state)))

;; (define (der-pq f state)
;;   ((richardson-derivative
;;     (fn (q)
;;             (state->p
;;              (f (->H-state (state->t state) q (state->p state)))))
;;     1.e-8
;;     .01)
;;    (state->q state)))

;; (define (der-pp f state)
;;   ((richardson-derivative
;;     (fn (p)
;;             (state->p
;;              (f (->H-state (state->t state) (state->q state) p))))
;;     1.e-8
;;     .01)
;;    (state->p state)))

;; ;;;----------------------------------------------------------------

;; (let ((f (pendulum-circulating-aa-state-to-state 2.0 9.8))
;;       (g (pendulum-circulating-state-to-aa-state 2.0 9.8)))
;;   (let* ((state (->H-state 1. 1. 15.))
;; 	       (aa-state (g state)))
;;     (- (* (der-qq f aa-state) (der-pp f aa-state))
;;        (* (der-pq f aa-state) (der-qp f aa-state)))))
;;                                         ;Value: 1.0000000000003484

;; (let ((f (pendulum-circulating-aa-state-to-state 2.0 9.8))
;;       (g (pendulum-circulating-state-to-aa-state 2.0 9.8)))
;;   (let* ((state (->H-state 1. 1. 15.))
;; 	       (aa-state (g state)))
;;     (- (* (der-qq g state) (der-pp g state))
;;        (* (der-pq g state) (der-qp g state)))))
;;                                         ;Value: .9999999999986688

;; (let ((f (pendulum-oscillating-aa-state-to-state 2.0 9.8))
;;       (g (pendulum-oscillating-state-to-aa-state 2.0 9.8)))
;;   (let* ((state (->H-state 1. 1. 1.))
;; 	       (aa-state (g state)))
;;     (- (* (der-qq g state) (der-pp g state))
;;        (* (der-pq g state) (der-qp g state)))))
;;                                         ;Value: 1.000000000000521

;; (let ((f (pendulum-oscillating-aa-state-to-state 2.0 9.8))
;;       (g (pendulum-oscillating-state-to-aa-state 2.0 9.8)))
;;   (let* ((state (->H-state 1. 1. 1.))
;; 	       (aa-state (g state)))
;;     (- (* (der-qq f aa-state) (der-pp f aa-state))
;;        (* (der-pq f aa-state) (der-qp f aa-state)))))
;;                                         ;Value: 1.000000000000406

;; |#

;;;----------------------------------------------------------------

(define (pendulum-frequency alpha beta E)
  (cond ((< E beta) (pendulum-oscillating-frequency alpha beta E))
        ((> E beta) (pendulum-circulating-frequency alpha beta E))
        (else
         0.)))

;;;----------------------------------------------------------------
;;; global action angle coordinates for pendulum

;; #|

;; Oscillation region:
;; -pi < phi < pi
;; 0 <  I < Isep

;; Upper circulation region:
;; -pi < phi < pi  ->  -pi/2 < phi' < pi/2   phi' = phi/2
;; Isep < 2I                  Isep < I'      I' = 2I

;; Lower circulation region:
;; ...

;; |#

(define ((pendulum-state-to-global-aa-state alpha beta) state)
  (let ((E ((Hpendulum alpha beta) state)))
    (cond ((< E beta)
	         ((pendulum-oscillating-state-to-aa-state alpha beta) state))
          ((and (> E beta) (> (state->p state) 0.))
           (let ((aa-state
		              ((pendulum-circulating-state-to-aa-state alpha beta)
		               state)))
	           (->H-state (state->t state)
			                  (* 0.5 (state->q aa-state))
			                  (* 2.0 (state->p aa-state)))))
          ((and (> E beta) (< (state->p state) 0.))
           (let ((aa-state
		              ((pendulum-circulating-state-to-aa-state alpha beta)
		               state)))
	           (->H-state (state->t state)
			                  ((principal-value pi)
			                   (- pi (* 0.5 (state->q aa-state))))
			                  (* 2.0 (state->p aa-state)))))
          ((= E beta)
           'go-figure))))

(define (pendulum-global-aa-state-to-state alpha beta)
  (let ((separatrix-action (pendulum-separatrix-action alpha beta)))
    (fn (aa-state)
      (let ((angle (state->q aa-state))
	          (action (state->p aa-state)))
	      (cond ((< action separatrix-action)
	             ((pendulum-oscillating-aa-state-to-state alpha beta) aa-state))
	            ((> action separatrix-action)
	             (if (and (< angle pi/2) (>= angle -pi/2))
		             ((pendulum-circulating-aa-state-to-state alpha beta)
		              (->H-state (state->t aa-state)
			                       (* 2. (state->q aa-state))
			                       (* 0.5 (state->p aa-state))))
		             (let ((state
			                  ((pendulum-circulating-aa-state-to-state alpha beta)
			                   (->H-state (state->t aa-state)
				                            (* 2. (state->q aa-state))
				                            (* 0.5 (state->p aa-state))))))
		               (->H-state (state->t state)
				                      (- (state->q state))
				                      (- (state->p state))))))
	            ((= action separatrix-action)
	             'oh-well))))))
