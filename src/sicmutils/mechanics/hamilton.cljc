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

(ns sicmutils.mechanics.hamilton
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [sicmutils.calculus.derivative :refer [D D-as-matrix partial]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [sin cos + - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :refer [make-operator]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; TODO move to lagrangian where it is in scmutils?
(def momentum-tuple down)

;; Hamiltonian mechanics requires a phase space QxP, and a function H:RxQxP -->
;; R
;;
;; A system has a dynamic state, which has the time, the configuration, and the
;; momenta. Hamiltonian mechanics is formulated in terms of the dynamic state.

(defn ->H-state
  [t q p]
  (up t q p))

(comment
  (define (H-state? s)
    (and (up? s)
         (fix:= (s:length s) 3)
         (numerical-quantity? (ref s 0))
         (or (and (numerical-quantity? (ref s 1))
                  (numerical-quantity? (ref s 2)))
             (and (up? (ref s 1))
                  (down? (ref s 2))
                  (= (s:dimension (ref s 1))
                     (s:dimension (ref s 2)))))))

  (define (compatible-H-state? s)
    (and (down? s)
         (fix:= (s:length s) 3)
         (numerical-quantity? (ref s 0))
         (or (and (numerical-quantity? (ref s 1))
                  (numerical-quantity? (ref s 2)))
             (and (down? (ref s 1))
                  (up? (ref s 2))
                  (= (s:dimension (ref s 1))
                     (s:dimension (ref s 2)))))))

  (define (state->p state)
    (if (not (and (vector? state)
                  (fix:> (vector-length state) 2)))
      (error "Cannot extract momentum from" state))
    (ref state 2)))

(comment
  (def momentum state->p)
  (def momenta state->p)
  (def P state->p))

;; TODO - put a precondition on here like above, make aliases.
(defn momentum
  "See coordinate: this returns the momentum element of a
  Hammilton state tuple (by convention, the element at index 2)."
  [H-state]
  (nth H-state 2))

(comment
  (define (state->qp dynamic-state)
    (vector-tail dynamic-state 1))

  (define (literal-Hamiltonian-state n-dof)
    (up (literal-number (generate-uninterned-symbol 't))
        (s:generate n-dof 'up
		                (lambda (i)
		                        (literal-number (generate-uninterned-symbol 'x))))
        (s:generate n-dof 'down
		                (lambda (i)
		                        (literal-number (generate-uninterned-symbol 'p)))))))

(comment
  (define ((Lstate->Hstate L) Ls)
    (up (time Ls)
        (coordinate Ls)
        (((partial 2) L) Ls)))

  (define ((Hstate->Lstate H) Hs)
    (up (time Hs)
        (coordinate Hs)
        (((partial 2) H) Hs)))

  (define (H-state->matrix s)
    (s->m (compatible-shape s) s 1)))

(comment
  (define (matrix->H-state m s)
    (assert (= (m:num-cols m) 1))
    (assert (and (odd? (m:num-rows m))
	               (> (m:num-rows m) 2)))
    (m->s (compatible-shape s) m 1))

  (define (degrees-of-freedom H-state)
    (assert (= (s:length H-state) 3))
    (assert (= (s:dimension (coordinate H-state))
               (s:dimension (momentum H-state))))
    (s:dimension (coordinate H-state))))

(comment
  (is (= (up t (up x y) (down p_x p_y))
         (matrix->H-state
          (H-state->matrix (up 't (up 'x 'y) (down 'p_x 'p_y)))))))

(comment
  (is (= (matrix-by-rows (list t) (list x) (list y) (list p_x) (list p_y))
         (H-state->matrix
          (matrix->H-state
           (matrix-by-rows (list 't)
                           (list 'x)
                           (list 'y)
                           (list 'p_x)
                           (list 'p_y)))))))

(comment
  (define (make-Hamiltonian kinetic-energy potential-energy)
    (+ kinetic-energy potential-energy))

  (define ((Hamilton-equations Hamiltonian) q p)
    (let ((H-state-path (qp->H-state-path q p))
	        (dH (Hamiltonian->state-derivative Hamiltonian)))
      (- (D H-state-path)
         (compose dH
                  H-state-path)))))

(defn Hamiltonian->state-derivative [Hamiltonian]
  (fn [H-state]
    (->H-state 1
               (((partial 2) Hamiltonian) H-state)
               (- (((partial 1) Hamiltonian) H-state)))))

;; NOTE: For compatibility with 1st edition
(def phase-space-derivative
  Hamiltonian->state-derivative)

(defn qp->H-state-path [q p]
  (fn [t]
    (->H-state t (q t) (p t))))

(defn Hamilton-equations [Hamiltonian]
  (fn [q p]
    (let [H-state-path (qp->H-state-path q p)
          dH (Hamiltonian->state-derivative Hamiltonian)]
      (- (D H-state-path)
         (f/compose dH H-state-path)))))

(defn D-phase-space [H]
  (fn [s]
    (up 0
        (((partial 2) H) s)
        (- (((partial 1) H) s)))))

;; If we express the energy in terms of t,Q,P we have the Hamiltonian.
;; A Hamiltonian is an example of an H-function: an H-function takes
;; 2 vector arguments and a scalar argument (t, Q, P).  It produces a
;; scalar result.

(defn H-rectangular
  [m V]
  (fn [[_ q p]]  ;; H-state
    (+ (/ (g/square p) (* 2 m))
       (apply V q))))

;; TODO move to tests:
(comment
  (is (= (up 0
             (up (+ ((D x) t) (/ (* -1 (p_x t)) m))
                 (+ ((D y) t) (/ (* -1 (p_y t)) m)))
             (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                   (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
         (((Hamilton-equations
            (H-rectangular
             'm
             (literal-function 'V (-> (X Real Real) Real))))
           (coordinate-tuple (literal-function 'x)
                             (literal-function 'y))
           (momentum-tuple (literal-function 'p_x)
                           (literal-function 'p_y)))
          't)))
  )

;; TODO file? what's up?
;;
;; If we express the energy in terms of t,Q,P we have the Hamiltonian
;;
;;        H(t,Q,P) = P*Qdot - L(t, Q, Qdot(t, Q, P))
;;
;; To do this we need to invert P(t, Q, Qdot) to get Qdot(t, Q, P). This is easy
;; when L is a quadratic form in Qdot:
;;
;;        L(t, Q, Qdot) = 1/2*Qdot*M*Qdot + B*Qdot - V
;;
;; Fortunately this is the case in almost all of Newtonian mechanics, otherwise
;; the P(t,Q,Qdot) function would be much more difficult to invert to obtain
;; Qdot(t,Q,P).

;; Assume that F is quadratic in its arguments
;;  F(u) = 1/2 A u u + b u + c
;;  then v = A u + b, so u = A^(-1) (v - b)

(defn dual-zero [z]
  (if (s/structure? z) (-> z g/transpose v/zero-like) 0))

(defn- Legendre-transform-fn
  "A better definition of Legendre transform that works for structured coordinates
  that have substructure"
  [F]
  (let [w-of-v (D F)]
    (fn [w]
      (let [z (dual-zero w)
            M ((D w-of-v) z)
            b (w-of-v z)
            v (g/solve-linear-left M (- w b))]
        (- (* w v) (F v))))))

(comment
  ;; TODO: the one we HAVE above is actually a crappy one. replace with this
  ;; one, with compatible zero, and move THIS one even to the tests... then
  ;; convert the ugly one below.
  (define (Legendre-transform-procedure F)
    (let ((w-of-v (D F)))
      (define (G w)
        (let ((z (compatible-zero w)))
          (let ((M ((D w-of-v) z))
                (b (w-of-v z)))
	          ;; DM=0 for this code to be correct.
            (let ((v (solve-linear-left M (- w b))))
              (- (* w v) (F v))))))
      G)))

;; This ugly version tests for correctness of the result.

(comment
  (define (Legendre-transform-procedure F)
    (let ((untested? true)
	        (w-of-v (D F)))
      (define (putative-G w)
        (let ((z (compatible-zero w)))
          (let ((M ((D w-of-v) z))
                (b (w-of-v z)))
	          (if (and untested? (zero? (simplify (determinant M))))
	            (error "Legendre Transform Failure: determinant=0"
		                 F w))
            (let ((v (solve-linear-left M (- w b))))
	            (- (* w v) (F v))))))
      (define (G w)
        (if untested?
	        (let ((thing (typical-object w)))
	          (if (not (equal?
		                  (simplify
		                   ((compose w-of-v (D putative-G))
			                  thing))
		                  (simplify thing)))
		          (error "Legendre Transform Failure: not quadratic"
		                 F w)
		          (set! untested? #f))
	          'tested))
        (putative-G w))
      G)))

(def Legendre-transform
  (make-operator Legendre-transform-fn 'Legendre-transform))

;; Notice that Lagrangians and Hamiltonians are symmetrical with
;; respect to the Legendre transform.

(defn ^:private Lagrangian->Hamiltonian-fn
  [Lagrangian]
  (fn [[t q p]]  ;; H-state
    (let [L #(Lagrangian (up t q %))]
      ((Legendre-transform L) p))))

(def Lagrangian->Hamiltonian
  (make-operator Lagrangian->Hamiltonian-fn
                 'Lagrangian->Hamiltonian))

(comment
  (define ((Hamiltonian->Lagrangian-procedure the-Hamiltonian) L-state)
    (let ((t (time L-state))
	        (q (coordinate L-state))
	        (qdot (velocity L-state)))
      (define (H p)
        (the-Hamiltonian (->H-state t q p)))
      ((Legendre-transform-procedure H) qdot)))

  (define Hamiltonian->Lagrangian
    (make-operator Hamiltonian->Lagrangian-procedure
                   'Hamiltonian->Lagrangian)))

(defn H-central-polar
  [m V]
  (fn [[_ [r _] [p_r p_phi]]]
    (+ (/ (+ (g/square p_r)
             (g/square (/ p_phi r)))
          (* 2 m))
       (V r))))

(defn Hamiltonian
  "Return SICM-style function signature for a Hamiltonian with n
  degrees of freedom (or 1 if n is not given). Useful for constructing
  Hamiltonian literal functions."
  [& n]
  (if (nil? n)
    '(-> (UP Real (UP* Real) (DOWN* Real)) Real)
    `(~'-> (~'UP ~'Real (~'UP* ~'Real ~@n) (~'DOWN* ~'Real ~@n)) ~'Real)))

(defn Poisson-bracket
  [f g]
  (fn [x]
    (let [fx (f x)
          gx (g x)]
      (if (or (s/structure? fx) (s/structure? gx))
        (s/mapr (fn [af]
                  (s/mapr (fn [ag]
                            ((Poisson-bracket
                              (comp (apply s/component af) f)
                              (comp (apply s/component ag) g))
                             x))
                          (s/structure->access-chains gx)))
                (s/structure->access-chains fx))
        ((- (* ((partial 1) f) ((partial 2) g))
            (* ((partial 2) f) ((partial 1) g)))
         x)))))

;; TODO rename to Lie-derivative, since it's namespaced.
(defn- Hamiltonian-Lie-derivative
  "p. 428

  We define the Lie derivative of F, as a derivative-like operator, relative to
  the given Hamiltonian-like function, H. Generalization and redefinition in
  calculus/Lie.scm
  "
  [H]
  (make-operator
   (fn [F] (Poisson-bracket F H))
   `(~'Lie-derivative ~H)))

(defn flow-derivative
  "the flow derivative generalizes the Lie derivative to allow for time dependent
  H and F --- computes the 'time' derivative of F along the flow specified by H"
  [H]
  (make-operator
   (fn [F]
     (+ ((partial 0) F)
	      (Poisson-bracket F H)))
   `(~'flow-derivative ~H)))

(comment

  #|
;;; for Lie derivatives, a TEST TODO

  ;; (define F (literal-function 'F (Hamiltonian 2)))
  ;; (define G (literal-function 'G (Hamiltonian 2)))

  ;; (define L_F (Lie-derivative F))
  ;; (define L_G (Lie-derivative G))

  ;; (pe (((+ (commutator L_F L_G)
	;;          (Lie-derivative (Poisson-bracket F G)))
  ;;       H)
  ;;      (up 't (up 'x 'y) (down 'px 'py))))
  ;; 0
  ;; |#
  )

(defmethod g/Lie-derivative [::v/function] [f]
  (Hamiltonian-Lie-derivative f))

;; TODO what the hell, this is from Lie-transform.scm.
;;
;; TODO get the rest of the tests from that namespace, separate this out.

(defn Lie-transform
  "p. 428, the Lie transform is just the time-advance operator using the Lie
  derivative (see Hamiltonian.scm)."
  [H t]
  (make-operator
   (g/exp (* t (g/Lie-derivative H)))
   `(~'Lie-transform ~H ~t)))

(define flow-transform
  "The generalization of Lie-transform to include time dependence."
  [H delta-t]
  (make-operator
   (g/exp (* delta-t (flow-derivative H)))
   `(~'flow-transform ~H ~delta-t)))

;; TODO lie-transform tests:

;; #|
;; ;;; The general solution for a trajectory is:
;; ;;;
;; ;;;  q(t,q0,p0) = A(q0,p0) cos (sqrt(k/m)*t + phi(q0,p0))
;; ;;;
;; ;;;  where A(q0,p0) = sqrt(2/k)*sqrt(p0^2/(2*m) + (k/2)*q0^2)
;; ;;;                 = sqrt((2/k)*E0)
;; ;;;
;; ;;;  and   phi(q0,p0) = - atan((1/sqrt(k*m))*(p0/q0))
;; ;;;
;; ;;; Thus, with initial conditions q0, p0
;; ;;;   we should get q(t) = q0*cos(sqrt(k/m)*t)+p0*sin(sqrt(k/m)*t)
;; ;;;
;; ;;; We can expand this as a Lie series:

;; (define ((H-harmonic m k) state)
;;   (let ((q (coordinate state))
;; 	      (p (momentum state)))
;;     (+ (/ (square p) (* 2 m))
;;        (* 1/2 k (square q)))))

;; ;;; This works, but it takes forever! -- hung in deriv, not in simplify!

;; (series:for-each print-expression
;;                  (((Lie-transform (H-harmonic 'm 'k) 'dt)
;;                    state->q)
;;                   (->H-state 0 'x_0 'p_0))
;;                  6)
;; x_0
;; (/ (* dt p_0) m)
;; (/ (* -1/2 (expt dt 2) k x_0) m)
;; (/ (* -1/6 (expt dt 3) k p_0) (expt m 2))
;; (/ (* 1/24 (expt dt 4) (expt k 2) x_0) (expt m 2))
;; (/ (* 1/120 (expt dt 5) (expt k 2) p_0) (expt m 3))
;;                                         ;Value: ...

;; (series:for-each print-expression
;;                  (((Lie-transform (H-harmonic 'm 'k) 'dt)
;;                    momentum)
;;                   (->H-state 0 'x_0 'p_0))
;;                  6)
;; p_0
;; (* -1 dt k x_0)
;; (/ (* -1/2 (expt dt 2) k p_0) m)
;; (/ (* 1/6 (expt dt 3) (expt k 2) x_0) m)
;; (/ (* 1/24 (expt dt 4) (expt k 2) p_0) (expt m 2))
;; (/ (* -1/120 (expt dt 5) (expt k 3) x_0) (expt m 2))
;;                                         ;Value: ...

;; (series:for-each print-expression
;;                  (((Lie-transform (H-harmonic 'm 'k) 'dt)
;;                    (H-harmonic 'm 'k))
;;                   (->H-state 0 'x_0 'p_0))
;;                  6)
;; (/ (+ (* 1/2 k m (expt x_0 2)) (* 1/2 (expt p_0 2))) m)
;; 0
;; 0
;; 0
;; 0
;; 0
;;                                         ;Value: ...
;; |#
;; 
;; #|
;; (define ((H-central-polar m V) state)
;;   (let ((q (coordinate state))
;;         (p (momentum state)))
;;     (let ((r ((component 0) q))
;;           (phi ((component 1) q))
;;           (pr ((component 0) p))
;;           (pphi ((component 1) p)))
;;       (+ (/ (+ (square pr)
;; 	             (square (/ pphi r)))
;; 	          (* 2 m))
;;          (V r)))))

;; (series:for-each print-expression
;;                  (((Lie-transform
;;                     (H-central-polar 'm (literal-function 'U))
;;                     'dt)
;;                    state->q)
;;                   (->H-state 0
;; 	                           (coordinate-tuple 'r_0 'phi_0)
;; 	                           (momentum-tuple 'p_r_0 'p_phi_0)))
;;                  4)
;; (up r_0 phi_0)
;; (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
;; (up
;;  (+ (/ (* -1/2 (expt dt 2) ((D U) r_0)) m)
;;     (/ (* 1/2 (expt dt 2) (expt p_phi_0 2)) (* (expt m 2) (expt r_0 3))))
;;  (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
;; (up
;;  (+
;;   (/ (* -1/6 (expt dt 3) p_r_0 (((expt D 2) U) r_0)) (expt m 2))
;;   (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
;;  (+ (/ (* 1/3 (expt dt 3) p_phi_0 ((D U) r_0)) (* (expt m 2) (expt r_0 3)))
;;     (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))
;;     (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))))
;;                                         ;Value: ...
;; |#
;; 
;; #|
;; (define ((L-central-polar m V) local)
;;   (let ((q (coordinate local))
;;         (qdot (velocity local)))
;;     (let ((r (ref q 0))
;;           (phi (ref q 1))
;;           (rdot (ref qdot 0))
;;           (phidot (ref qdot 1)))
;;       (- (* 1/2 m
;;             (+ (square rdot)
;;                (square (* r phidot))) )
;;          (V r)))))


;; ;;; I left this one that uses the Lagrangian because it appears to be
;; ;;; used for timings
;; (show-time
;;  (lambda ()
;;          (series:print
;;           (((Lie-transform
;;              (Lagrangian->Hamiltonian
;; 	            (L-central-polar 'm (lambda (r) (- (/ 'GM r)))))
;;              'dt)
;;             state->q)
;;            (->H-state 0
;; 		                  (coordinate-tuple 'r_0 'phi_0)
;; 		                  (momentum-tuple 'p_r_0 'p_phi_0)))
;;           4)))
;; #|
;; ;;; 13 March 2012: I changed the system so that the original
;; ;;; normalization is available, without causing the original gcd bug.
;; ;;; This is done by adding an additional stage of simplification.
;; ;;; This new stage is enabled by "(divide-numbers-through-simplify
;; ;;; true/false)" The control is in simplify/rules.scm.  The default is
;; ;;; now true, yielding the old representation.
;; (up r_0 phi_0)
;; (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
;; (up
;;  (+ (/ (* -1/2 GM (expt dt 2)) (* m (expt r_0 2)))
;;     (/ (* 1/2 (* (expt dt 2) (expt p_phi_0 2))) (* (expt m 2) (expt r_0 3))))
;;  (/ (* -1 (expt dt 2) p_r_0 p_phi_0) (* (expt m 2) (expt r_0 3))))
;; (up
;;  (+ (/ (* 1/3 (* GM (expt dt 3) p_r_0)) (* (expt m 2) (expt r_0 3)))
;;     (/ (* -1/2 (expt dt 3) p_r_0 (expt p_phi_0 2)) (* (expt m 3) (expt r_0 4))))
;;  (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
;;     (/ (* 1/3 (* GM (expt dt 3) p_phi_0)) (* (expt m 2) (expt r_0 5)))
;;     (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
;;                                         ;process time: 1570 (1570 RUN + 0 GC); real time: 1573#| ... |#


;; ;;; 30 Jan 2011: I changed the normalization of rational functions to
;; ;;; favor integer coefficients.  This was to eliminate a bug in the
;; ;;; construction of polynomial gcds.
;; ;;; This is the new result.  It is algebraically equivalent to the old
;; ;;; result.
;; (up r_0 phi_0)
;; (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
;; (up
;;  (+ (/ (* -1 GM (expt dt 2)) (* 2 m (expt r_0 2)))
;;     (/ (* (expt dt 2) (expt p_phi_0 2)) (* 2 (expt m 2) (expt r_0 3))))
;;  (/ (* -1 (expt dt 2) p_r_0 p_phi_0) (* (expt m 2) (expt r_0 3))))
;; (up
;;  (+ (/ (* GM (expt dt 3) p_r_0) (* 3 (expt m 2) (expt r_0 3)))
;;     (/ (* -1 (expt dt 3) (expt p_phi_0 2) p_r_0) (* 2 (expt m 3) (expt r_0 4))))
;;  (+ (/ (* (expt dt 3) (expt p_r_0 2) p_phi_0) (* (expt m 3) (expt r_0 4)))
;;     (/ (* GM (expt dt 3) p_phi_0) (* 3 (expt m 2) (expt r_0 5)))
;;     (/ (* -1 (expt dt 3) (expt p_phi_0 3)) (* 3 (expt m 3) (expt r_0 6)))))
;; ;;; Binah 30 Jan 2011
;;                                         ;process time: 1600 (1600 RUN + 0 GC); real time: 1607#| ... |#
;; |#
;; #|
;; (up r_0 phi_0)
;; (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
;; (up
;;  (+ (/ (* -1/2 GM (expt dt 2)) (* m (expt r_0 2)))
;;     (/ (* 1/2 (expt dt 2) (expt p_phi_0 2)) (* (expt m 2) (expt r_0 3))))
;;  (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
;; (up
;;  (+
;;   (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
;;   (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
;;  (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
;;     (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
;;     (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
;; |#
;; ;;; Binah: 9 December 2009
;; ;;;  With simple-derivative-internal memoized
;; ;;;   process time: 2830 (2830 RUN + 0 GC); real time: 2846
;; ;;;  Without memoization
;; ;;;   process time: 1360 (1360 RUN + 0 GC); real time: 1377
;; ;;;  But memoization makes some stuff feasible (see calculus/tensor.scm).
;; ;;;
;; ;;; Earlier
;; ;;; MAHARAL
;; ;;;         process time: 3940 (3710 RUN + 230 GC); real time: 3956
;; ;;; HOD
;; ;;;         process time: 14590 (13610 RUN + 980 GC); real time: 14588
;; ;;; PLANET003 600MHz PIII
;; ;;;         process time: 19610 (17560 RUN + 2050 GC); real time: 19610
;; ;;; HEIFETZ xeon 400MHz 512K
;; ;;;         process time: 27380 (24250 RUN + 3130 GC); real time: 27385
;; ;;; GEVURAH 300 MHz
;; ;;;         process time: 36070 (33800 RUN + 2270 GC); real time: 36072
;; ;;; MAHARAL
;; ;;;         process time: 56390 (50970 RUN + 5420 GC); real time: 56386
;; ;;; ACTION1 200MHz Pentium Pro
;; ;;;         process time: 55260 (49570 RUN + 5690 GC); real time: 55257
;; ;;; PPA     200MHz Pentium Pro
;; ;;;         process time: 58840 (56500 RUN + 2340 GC); real time: 59165
;; ;;; ZOHAR   33MHz 486
;; ;;;         process time: 463610 (443630 RUN + 19980 GC); real time: 485593
;; |#


;; TODO its from sections.scm. AND that code has some shit that I absolutely
;; want to visualize.

(comment
  ;;; Now to do the Poincare section.
;;;  Map explorer:
;;;   Left button starts a trajectory.
;;;   Middle button continues a trajectory.
;;;   Right button interrogates coordinates.

  ;; (define (explore-map window poincare-map #!optional mode-or-n)
  ;;   (let* ((default-n 1000)
	;;          (collector
	;;           (cond ((default-object? mode-or-n)
	;; 	               (default-collector (default-monitor window)
	;; 			                              poincare-map
	;; 			                              default-n))
	;; 	              ((number? mode-or-n)
	;; 	               (default-collector (default-monitor window)
	;; 			                              poincare-map
	;; 			                              mode-or-n))
	;; 	              (else poincare-map))))
  ;;     (define (button-loop ox oy)
  ;;       (pointer-coordinates window
	;;                            (lambda (x y button)
  ;;                                    (case button
  ;;                                      ((0)
  ;;                                       (display "Started: ")
  ;;                                       (write-line (list x y))
  ;;                                       (collector x y button-loop map-failed))
  ;;                                      ((1)
  ;;                                       (if (eq? ox 'ignore)
  ;;                                         (button-loop 'ignore oy)
  ;;                                         (begin (display "Continued: ")
  ;;                                                (write-line (list ox oy))
  ;;                                                (collector ox oy button-loop map-failed))))
  ;;                                      ((2)
  ;;                                       (display "Hit: ")
  ;;                                       (write-line (list x y))
  ;;                                       (button-loop ox oy))))))
  ;;     (define (map-failed)
  ;;       (display "Illegal point \n")
  ;;       (button-loop 'ignore 'ignore))
  ;;     (newline)
  ;;     (display "Left button starts a trajectory.")
  ;;     (newline)
  ;;     (display "Middle button continues a trajectory.")
  ;;     (newline)
  ;;     (display "Right button interrogates coordinates.")
  ;;     (newline)
  ;;     (button-loop 'ignore 'ignore)))
  ;;
  ;; (define ((default-collector monitor pmap n) x y done fail)
  ;;   (let lp ((n n) (x x) (y y))
  ;;        (monitor x y)
  ;;        (if (fix:> n 0)
  ;;          (pmap x y
  ;;                (lambda (nx ny)
  ;;                        (lp (fix:- n 1) nx ny))
  ;;                fail)
  ;;          (done x y))))

  ;; (define ((default-monitor win) x y)
  ;;   (plot-point win x y))

  ;; (define (pointer-coordinates window continue)
  ;;   (beep)
  ;;   (get-pointer-coordinates window continue))

  ;; #| ;;; Test for standard map
  ;; (define win (frame 0.0 2pi 0.0 2pi))
  ;; (explore-map win (standard-map 1.0))
  ;; (explore-map win (standard-map 1.0) 5000)
  ;; (graphics-clear win)
  ;; (graphics-close win)
  ;; |#

  ;; #|
;;; This is used to zero in on crossings in autonomous systems,
;;;  such as Henon-Heiles.

  ;; TODO this was commented out but can we keep it?
  (define (refine-crossing sec-eps advance state)
    (let lp ((state state))
         (let ((x (g:ref state 1 0))
               (xd (g:ref state 2 0)))
           (let ((zstate (advance state (- (/ x xd)))))
	           (if (< (abs (g:ref zstate 1 0))
	                  sec-eps)
	             zstate
	             (lp zstate))))))

  (define (display-map window poincare-map x y n)
    (plot-point window x y)
    (if (fix:> n 0)
      (poincare-map
       x y
       (lambda (nx ny)
	             (display-map window poincare-map nx ny (fix:- n 1)))
       (lambda ()
	             (newline)
	             (display "Illegal point: ")
	             (write (list x y))))))
  )

;; TODO go to sections.scm.

(defn standard-map
  ;; from sections.
  [K]
  (fn [theta I return _]
    (let [nI (+ I (* K (sin theta)))]
      (return ((v/principal-value v/twopi) (+ theta nI))
              ((v/principal-value v/twopi) nI)))))

(comment
  ;; TODO for comparison:
  (define ((standard-map K) x y continue fail)
    (let ((yp (flo:pv (flo:+ y (flo:* K (flo:sin x))))))
      (continue (flo:pv (flo:+ x yp)) yp)))

  (define ((standard-map-inverse K) x y continue fail)
    (let ((xp (flo:pv (flo:- x y))))
      (continue xp (flo:pv (flo:- y (flo:* K (flo:sin xp)))))))
  )

;;; This is the 0-2pi principal value:

(comment
  (define (flo:pv x)
    (flo:- x (flo:* 2pi (flo:floor (flo:/ x 2pi))))))

(defn iterated-map
  "f is a function of (x y continue fail), which calls continue with
  the values of x' y' that follow x y in the mapping. Returns a map of
  the same shape that iterates the iterated map n times before
  invoking the continuation, or invokes the fail continuation if the
  inner map fails."
  [f n]
  (let [lulz (constantly nil)]
    (fn [x y continue fail]
      (when (< n 0) (u/illegal "Cannot invert map"))
      (loop [x x
             y y
             i n]
        (if (= i 0)
          (continue x y)
          (let [step (f x y vector lulz)]
            (if step
              (recur (step 0) (step 1) (dec i))
              (fail))))))))

;; TODO move to point-transformation or something, from point-transformation.scm

;; Makes a canonical point transformation from a
;;  time-invariant coordinate transformation T(q)

(defn F->CH
  "A transformation of configuration coordinates F to a procedure implementing a
  transformation of phase-space coordinates (p. 320)"
  [F]
  (fn [[t _ p :as H-state]]
    (up t
        (F H-state)
        (g/solve-linear-right
         p
         (((partial 1) F) H-state)))))

(def F->CT F->CH)

;; This is used in conjunction with a symplectic test for the C to establish
;; that a time-dependent transformation is canonical.

;; To compute the K (addition to the Hamiltonian) from a time-dependent
;; coordinate transformation F.

;; TODO replace p with (momentum H-state once we import lagrangian)
(defn F->K [F]
  (fn [[_ _ p :as H-state]]
    (- (* (g/solve-linear-right
           p
           (((partial 1) F) H-state))
          (((partial 0) F) H-state)))))

(defn H-central
  [m V]
  (fn [[_ q p]]
    (+  (/ (g/square p) (* 2 m))
        (V (g/abs q)))))

(comment
  ;; TODO test for H-central, for some point transformation code
  (is (= (+ (V r)
            (/ (* 1/2 (expt p_r 2)) m)
            (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))
         ((compose (H-central 'm (literal-function 'V))
                   (F->CT p->r))
          (->H-state 't
                     (coordinate-tuple 'r 'phi)
                     (momentum-tuple 'p_r 'p_phi))))))

;; page numbers here are references to the PDF; probably
;; do not correspond to 1ed.

;; TODO NOW we are cribbing from canonical.scm

(defn canonical?
  "p.324"
  [C H Hprime]
  (- (f/compose (Hamiltonian->state-derivative H) C)
     (* (D C) (Hamiltonian->state-derivative Hprime))))

(defn compositional-canonical?
  "p.324"
  [C H]
  (canonical? C H (f/compose H C)))

(defn J-func [DHs]
  {:pre [(compatible-H-state? DHs)]}
  (up 0 (nth DHs 2) (- (nth DHs 1))))

(defn T-func [s]
  (up 1
      (v/zero-like (coordinates s))
      (v/zero-like (momenta s))))

(defn canonical-H? [C H]
  (- (f/compose (D-phase-space H) C)
     (* (D C)
	      (D-phase-space (f/compose H C)))))

;; TODO use this vs chapter 5 test code
(defn canonical-K? [C K]
  (- (f/compose T-func C)
     (* (D C)
	      (+ T-func (D-phase-space K)))))

(defn linear-function->multiplier [F argument]
  ((D F) argument))

(defn Phi [A]
  (fn [v]
    (* A v)))

(defn Phi* [A]
  (fn [w]
    (* w A)))

(defn time-independent-canonical?
  "p.326"
  [C]
  (fn [s]
    ((- J-func
        (f/compose (Phi ((D C) s))
                   J-func
                   (Phi* ((D C) s))))
     (s/compatible-shape s))))

;; TODO tests or check ch5?
;; #|
;; (print-expression
;;  ((time-independent-canonical? (F->CT p->r))
;;   (up 't
;;       (coordinate-tuple 'r 'phi)
;;       (momentum-tuple 'p_r 'p_phi))))
;; (up 0 (up 0 0) (down 0 0))


;; ;;; but not all transforms are

;; (define (a-non-canonical-transform Istate)
;;   (let ((t (time Istate))
;;         (theta (coordinate Istate))
;; 	      (p (momentum Istate)))
;;     (let ((x (* p (sin theta)))
;; 	        (p_x (* p (cos theta))))
;;       (up t x p_x))))

;; (print-expression
;;  ((time-independent-canonical? a-non-canonical-transform)
;;   (up 't 'theta 'p)))
;; (up 0 (+ (* -1 p x8102) x8102) (+ (* p x8101) (* -1 x8101)))
;; |#

;; from time-varying.scm
(defn qp-canonical?
  "Tests that K yields a canonical transformation if the C is symplectic. (The
  qp-canonical? code is really a symplectic test without factoring out the
  Hamiltonian.)"
  [C H]
  (fn [s]
    (- (J-func ((D H) (C s)))
       (* ((D C) s)
	        (J-func
	         ((D (compose H C)) s))))))

;; (define ((canonical-K? C K) s)
;;   (let ((s* (compatible-shape s)))
;;     (- (T-func s*)
;;        (+ (* ((D C) s) (J-func ((D K) s)))
;; 	        (((partial 0) C) s)))))


;; (define ((canonical-K? C K) s)
;;   (let ((DCs ((D C) s))
;; 	      (s* (compatible-shape s)))
;;     (- (T-func s*)
;;        (* DCs ((Hamiltonian->state-derivative K) s)))))
;; |#
;; 
;; #|
;; (define ((rotating n) state)
;;   (let ((t (time state))
;; 	      (q (coordinate state)))
;;     (let ((x (ref q 0))
;; 	        (y (ref q 1))
;; 	        (z (ref q 2)))
;;       (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
;; 			                  (- (* (cos (* n t)) y) (* (sin (* n t)) x))
;; 			                  z))))

;; (define (C-rotating n) (F->CT (rotating n)))

;; (define ((K n) s)
;;   (let ((q (coordinate s))
;; 	      (p (momentum s)))
;;     (let ((x (ref q 0)) (y (ref q 1))
;; 	        (px (ref p 0)) (py (ref p 1)))
;;       (* n (- (* x py) (* y px))))))

;; (define a-state
;;   (up 't
;;       (coordinate-tuple 'x 'y 'z)
;;       (momentum-tuple 'p_x 'p_y 'p_z)))


;; (pe ((canonical-K? (C-rotating 'n) (K 'n)) a-state))
;; (up 0 (up 0 0 0) (down 0 0 0))

;; ;;; or getting K directly from F
;; (pe ((canonical-K? (C-rotating 'n) (F->K (rotating 'n))) a-state))
;; (up 0 (up 0 0 0) (down 0 0 0))

;; (pe ((- (F->K (rotating 'n))
;; 	      (K 'n))
;;      a-state))
;; 0

;; ;;; not all K's work

;; (define ((bad-K n) s)
;;   (- ((K n) s)))

;; (pe ((canonical-K? (C-rotating 'n) (bad-K 'n)) a-state))
;; (up
;;  0
;;  (up (+ (* 2 n x (sin (* n t))) (* -2 n y (cos (* n t))))
;;      (+ (* 2 n x (cos (* n t))) (* 2 n y (sin (* n t))))
;;      0)
;;  (down (+ (* 2 n p_x (sin (* n t))) (* -2 n p_y (cos (* n t))))
;;        (+ (* 2 n p_x (cos (* n t))) (* 2 n p_y (sin (* n t))))
;;        0))

;; back to previous file:
(defn polar-canonical
  "p.327"
  [alpha]
  (fn [[t theta I]]
    (let [x (* (g/sqrt (/ (* 2 I) alpha)) (sin theta))
          p_x (* (g/sqrt (* 2 alpha I)) (cos theta))]
      (up t x p_x))))

(comment
  (define ((polar-canonical-inverse alpha) s)
    (let ((t (time s))
	        (x (coordinate s))
	        (p (momentum s)))
      (let ((I (/ (+ (* alpha (square x))
		                 (/ (square p) alpha))
		              2)))
        (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
			                     (/ p (sqrt (* 2 I alpha))))))
	        (up t theta I))))))

;; TODO move to tests:
;; #|
;; (pe
;;  ((compose (polar-canonical-inverse 'alpha)
;; 	         (polar-canonical 'alpha))
;;   (up 't 'x 'p)))
;; (up t x p)

;; (print-expression
;;  ((time-independent-canonical? (polar-canonical 'alpha))
;;   (up 't 'a 'I)))
;; (up 0 0 0)
;; |#
;; 
;; #|
;; (define (Cmix H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;;         (coordinate-tuple (ref q 0) (- (ref p 1)))
;;         (momentum-tuple   (ref p 0) (ref q 1)))))

;; (define a-state
;;   (up 't
;;       (coordinate-tuple 'x 'y)
;;       (momentum-tuple 'p_x 'p_y)))

;; (print-expression
;;  ((time-independent-canonical? Cmix)
;;   a-state))
;; (up 0 (up 0 0) (down 0 0))

;; (define (Cmix2 H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;;         (flip-outer-index p)
;;         (- (flip-outer-index q)))))

;; (print-expression
;;  ((time-independent-canonical? Cmix2)
;;   a-state))
;; (up 0 (up 0 0) (down 0 0))
;; |#

(comment
  (define ((two-particle-center-of-mass m0 m1) H-state)
    (let ((q (coordinate H-state)))
      (let ((x0 (ref q 0))
	          (x1 (ref q 1)))
        (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
			                    (- x1 x0)))))

  (define ((two-particle-center-of-mass-canonical m0 m1) state)
    (let ((x (coordinate state))
	        (p (momentum state)))
      (let ((x0 (ref x 0))
	          (x1 (ref x 1))
	          (p0 (ref p 0))
	          (p1 (ref p 1)))
        (up (time state)
            (coordinate-tuple
             (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
             (- x1 x0))
            (momentum-tuple
             (+ p0 p1)
             (/ (- (* m0 p1) (* m1 p0))
                (+ m0 m1))))))))

;; TODO tests
;; #|
;; (define b-state
;;   (up 't
;;       (coordinate-tuple
;;        (coordinate-tuple 'x_1 'y_1)
;;        (coordinate-tuple 'x_2 'y_2))
;;       (momentum-tuple
;;        (momentum-tuple 'p_x_1 'p_y_1)
;;        (momentum-tuple 'p_x_2 'p_y_2))))

;; (pe (- ((F->CT (two-particle-center-of-mass 'm0 'm1)) b-state)
;;        ((two-particle-center-of-mass-canonical 'm0 'm1) b-state)))
;; (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))

;; (print-expression
;;  ((time-independent-canonical?
;;    (two-particle-center-of-mass-canonical 'm1 'm2))
;;   b-state))
;; (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;; |#

(comment
  (define ((multiplicative-transpose s) A)
    (linear-function->multiplier (transpose-function A) s))

  (define ((transpose-function A) p) (* p A)))

;; TODO remaining tests from canonical.scm

;; #|
;; (define (T v)
;;   (* (down (up 'a 'c) (up 'b 'd)) v))

;; (pe (T (up 'x 'y)))
;; (up (+ (* a x) (* b y)) (+ (* c x) (* d y)))

;; (pe (* (* (down 'p_x 'p_y) ((D T) (up 'x 'y))) (up 'v_x 'v_y)))
;; (+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))


;; (pe (* (down 'p_x 'p_y) (* ((D T) (up 'x 'y)) (up 'v_x 'v_y))))
;; (+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))

;; (pe (* (* ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y)))
;; 	        (down 'p_x 'p_y))
;;        (up 'v_x 'v_y)))
;; (+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))

;; ;;; But strangely enough...
;; (pe (* (* (down 'p_x 'p_y)
;; 	        ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y))))
;;        (up 'v_x 'v_y)))
;; (+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))
;; |#
;; 
;; #|
;; (define ((time-independent-canonical? C) s)
;;   (let ((s* (compatible-shape s)))
;;     (let ((J (linear-function->multiplier J-func s*)))
;;       (- J
;; 	       (* ((D C) s)
;; 	          (* J
;; 	             ((multiplicative-transpose s*) ((D C) s))))))))

;; (print-expression
;;  ((time-independent-canonical? (F->CT p->r))
;;   (up 't
;;       (coordinate-tuple 'r 'phi)
;;       (momentum-tuple 'p_r 'p_phi))))
;; (up 0 (up 0 0) (down 0 0))


;; ;;; but not all transforms are

;; (define (a-non-canonical-transform Istate)
;;   (let ((t (time Istate))
;;         (theta (coordinate Istate))
;; 	      (p (momentum Istate)))
;;     (let ((x (* p (sin theta)))
;; 	        (p_x (* p (cos theta))))
;;       (up t x p_x))))

;; (print-expression
;;  ((time-independent-canonical? a-non-canonical-transform)
;;   (up 't 'theta 'p)))
;; (up (up 0 0 0) (up 0 0 (+ -1 p)) (up 0 (+ 1 (* -1 p)) 0))

;; (print-expression
;;  ((time-independent-canonical? (polar-canonical 'alpha))
;;   (up 't 'a 'I)))
;; (up (up 0 0 0) (up 0 0 0) (up 0 0 0))
;; |#
;; 
;; #|
;; (define (Cmix H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;;         (coordinate-tuple (ref q 0) (- (ref p 1)))
;;         (momentum-tuple   (ref p 0) (ref q 1)))))

;; (define a-state
;;   (up 't
;;       (coordinate-tuple 'x 'y)
;;       (momentum-tuple 'p_x 'p_y)))

;; (print-expression
;;  ((time-independent-canonical? Cmix)
;;   a-state))
;; (up 0 (up 0 0) (down 0 0))

;; (define (Cmix2 H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;;         (flip-outer-index p)
;;         (- (flip-outer-index q)))))

;; (print-expression
;;  ((time-independent-canonical? Cmix2)
;;   a-state))
;; (up 0 (up 0 0) (down 0 0))
;; |#
;; 
;; #|
;; (define ((C m0 m1) state)
;;   (let ((x (coordinate state))
;; 	      (p (momentum state)))
;;     (let ((x0 (ref x 0))
;; 	        (x1 (ref x 1))
;; 	        (p0 (ref p 0))
;; 	        (p1 (ref p 1)))
;;       (up
;;        (time state)
;;        (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;; 			                   (- x1 x0))
;;        (momentum-tuple (+ p0 p1)
;; 		                   (/ (- (* m0 p1) (* m1 p0))
;; 			                    (+ m0 m1)))))))

;; (define b-state
;;   (up 't
;;       (coordinate-tuple
;;        (coordinate-tuple 'x_1 'y_1)
;;        (coordinate-tuple 'x_2 'y_2))
;;       (momentum-tuple
;;        (momentum-tuple 'p_x_1 'p_y_1)
;;        (momentum-tuple 'p_x_2 'p_y_2))))

;; (print-expression
;;  ((time-independent-canonical? (C 'm1 'm2)) b-state))
;; (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))

;; |#

;; TODO now we are onto symplectic.scm

(comment
  (define (symplectic-two-form zeta1 zeta2)
    (- (* (momenta zeta2) (coordinates zeta1))
       (* (momenta zeta1) (coordinates zeta2)))))

;; Without matrices

(comment
  (define ((canonical-transform? C) s)
    (let ((J ((D J-func) (compatible-shape s)))
          (DCs ((D C) s)))
      (let ((DCsT (transpose DCs s)))
        (- J (* DCs J DCsT))))))

;; TODO tests:

;; #|
;; (print-expression
;;  ((canonical-transform? (F->CT p->r))
;;   (up 't
;;       (up 'r 'phi)
;;       (down 'p_r 'p_phi))))
;; (up (up 0 (up 0 0) (down 0 0))
;;     (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;;     (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))


;; (print-expression
;;  ((canonical-transform? (polar-canonical 'alpha))
;;   (up 't 'a 'I)))
;; (up (up 0 0 0) (up 0 0 0) (up 0 0 0))


;; ;;; but not all transforms are

;; (define (a-non-canonical-transform Istate)
;;   (let ((t (time Istate))
;;         (theta (coordinate Istate))
;; 	      (p (momentum Istate)))
;;     (let ((x (* p (sin theta)))
;; 	        (p_x (* p (cos theta))))
;;       (up t x p_x))))

;; (print-expression
;;  ((canonical-transform? a-non-canonical-transform)
;;   (up 't 'theta 'p)))
;; (up (up 0 0 0) (up 0 0 (+ -1 p)) (up 0 (+ 1 (* -1 p)) 0))
;; |#
;; 
;; #|
;; (define (Cmix H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;; 	      (up (ref q 0) (- (ref p 1)))
;; 	      (down (ref p 0) (ref q 1)))))

;; (define a-state
;;   (up 't (up 'x 'y) (down 'p_x 'p_y)))

;; (print-expression
;;  ((canonical-transform? Cmix) a-state))
;; (up (up 0 (up 0 0) (down 0 0))
;;     (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;;     (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))


;; (define (Cmix2 H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;; 	      (flip-outer-index p)
;; 	      (- (flip-outer-index q)))))

;; (print-expression
;;  ((canonical-transform? Cmix2)
;;   a-state))
;; (up (up 0 (up 0 0) (down 0 0))
;;     (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;;     (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))
;; |#
;; 
;; #|
;; (define ((C m0 m1) state)
;;   (let ((x (coordinate state))
;; 	      (p (momentum state)))
;;     (let ((x0 (ref x 0))
;; 	        (x1 (ref x 1))
;; 	        (p0 (ref p 0))
;; 	        (p1 (ref p 1)))
;;       (up (time state)
;; 	        (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;; 	            (- x1 x0))
;; 	        (down (+ p0 p1)
;; 		            (/ (- (* m0 p1) (* m1 p0))
;; 		               (+ m0 m1)))))))

;; (define b-state
;;   (up 't
;;       (up (up 'x_1 'y_1)
;; 	        (up 'x_2 'y_2))
;;       (down (down 'p_x_1 'p_y_1)
;; 	          (down 'p_x_2 'p_y_2))))

;; (print-expression
;;  ((canonical-transform? (C 'm1 'm2)) b-state))
;; (up
;;  (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;;  (up
;;   (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;;       (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
;;   (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;;       (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
;;  (down
;;   (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;;         (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
;;   (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;;         (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))))
;; |#


(comment
  (define (J-matrix n)                    ;degrees of freedom
    (let ((2n+1 (fix:+ (fix:* 2 n) 1)))
      (m:generate 2n+1 2n+1
                  (lambda (a b)
	                        (cond ((fix:= a 0) 0)
                                ((fix:= b 0) 0)
                                ((fix:= (fix:+ a n) b) 1)
	                              ((fix:= (fix:+ b n) a) -1)
	                              (else 0)))))))

;; Symplectic test in terms of matrices

(comment
  (define ((symplectic? C) s)
    (let ((J (J-matrix (degrees-of-freedom s)))
          (DC ((D-as-matrix C) s)))
      (- J (* DC J (transpose DC))))))

;; TODO tests:

;; #|
;; (print-expression
;;  ((symplectic? (F->CT p->r))
;;   (up 't
;;       (up 'r 'phi)
;;       (down 'p_r 'p_phi))))
;; (matrix-by-rows (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0))


;; ;;; but not all transforms are

;; (define (a-non-canonical-transform Istate)
;;   (let ((t (time Istate))
;;         (theta (coordinate Istate))
;; 	      (p (momentum Istate)))
;;     (let ((x (* p (sin theta)))
;; 	        (p_x (* p (cos theta))))
;;       (up t x p_x))))

;; (print-expression
;;  ((symplectic? a-non-canonical-transform)
;;   (up 't 'theta 'p)))
;; (matrix-by-rows (list 0 0 0)
;; 		            (list 0 0 (+ 1 (* -1 p)))
;; 		            (list 0 (+ -1 p) 0))

;; (print-expression
;;  ((symplectic? (polar-canonical 'alpha))
;;   (up 't 'a 'I)))
;; (matrix-by-rows (list 0 0 0)
;; 		            (list 0 0 0)
;; 		            (list 0 0 0))

;; (define (Cmix H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;; 	      (up (ref q 0) (- (ref p 1)))
;; 	      (down   (ref p 0) (ref q 1)))))

;; (define a-state
;;   (up 't (up 'x 'y) (down 'p_x 'p_y)))

;; (print-expression ((symplectic? Cmix) a-state))
;; (matrix-by-rows (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0))
;; |#
;; 
;; #|
;; (define (Cmix2 H-state)
;;   (let ((t (time H-state))
;; 	      (q (coordinate H-state))
;; 	      (p (momentum H-state)))
;;     (up t
;; 	      (flip-outer-index p)
;; 	      (- (flip-outer-index q)))))

;; (print-expression
;;  ((canonical-transform? Cmix2)
;;   a-state))
;; (matrix-by-rows (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0)
;;                 (list 0 0 0 0 0))


;; (define ((C m0 m1) state)
;;   (let ((x (coordinate state))
;; 	      (p (momentum state)))
;;     (let ((x0 (ref x 0))
;; 	        (x1 (ref x 1))
;; 	        (p0 (ref p 0))
;; 	        (p1 (ref p 1)))
;;       (up (time state)
;; 	        (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;; 	            (- x1 x0))
;; 	        (down (+ p0 p1)
;; 		            (/ (- (* m0 p1) (* m1 p0))
;; 		               (+ m0 m1)))))))

;; (define b-state
;;   (up 't
;;       (up (up 'x_1 'y_1)
;; 	        (up 'x_2 'y_2))
;;       (down (down 'p_x_1 'p_y_1)
;; 	          (down 'p_x_2 'p_y_2))))

;; (print-expression
;;  ((canonical-transform? (C 'm1 'm2)) b-state))
;; (matrix-by-rows (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0)
;;                 (list 0 0 0 0 0 0 0 0 0))
;; |#

(defn symplectic-unit
  "p. 334 (used, but not defined there)"
  [n]
  (let [twoN (* 2 n)]
    (matrix/generate twoN twoN
                     (fn [a b]
                       (cond (= (+ a n) b) 1
                             (= (+ b n) a) -1
                             :else 0)))))

(comment
  ;; TODO add assertion back
  (define (symplectic-matrix? M)
    (let ((2n (m:dimension M)))
      (if (not (even? 2n))
	      (error "Wrong type -- SYMPLECTIC-MATRIX?" M))
      (let ((J (symplectic-unit (quotient 2n 2))))
        (- J (* M J (transpose M)))))))

(defn symplectic-matrix?
  "p. 334"
  [M]
  (let [twoN (matrix/dimension M)
        J (symplectic-unit (quot twoN 2))]
    (- J (* M J (matrix/transpose M)))))



(defn qp-submatrix [m]
  (matrix/without m 0 0))

(defn symplectic-transform?
  "p. 334"
  [C]
  (fn [s]
    (symplectic-matrix?
     (qp-submatrix
      ((D-as-matrix C) s)))))

;; TODO remaining tests

;; #|
;; ;;; For example, point transforms are canonical

;; (print-expression
;;  ((symplectic-transform? (F->CT p->r))
;;   (up 't
;;       (up 'r 'theta)
;;       (down 'p_r 'p_theta))))
;; (matrix-by-rows (list 0 0 0 0)
;; 		            (list 0 0 0 0)
;; 		            (list 0 0 0 0)
;; 		            (list 0 0 0 0))
;; |#
;; 
;; #|
;; (print-expression
;;  ((symplectic-transform? a-non-canonical-transform)
;;   (up 't 'theta 'p)))
;; (matrix-by-rows (list 0 (+ 1 (* -1 p))) (list (+ -1 p) 0))
;; |#

;; #|
;; ;;; One particularly useful canonical transform is the
;; ;;;  Poincare transform, which is good for simplifying
;; ;;;  oscillators.

;; (define ((polar-canonical alpha) Istate)
;;   (let ((t (time Istate))
;;         (theta (coordinate Istate))
;;         (I (momentum Istate)))
;;     (let ((x (* (sqrt (/ (* 2 I) alpha)) (sin theta)))
;; 	        (p_x (* (sqrt (* 2 alpha I)) (cos theta))))
;;       (up t x p_x))))

;; (define ((polar-canonical-inverse alpha) s)
;;   (let ((t (time s))
;; 	      (x (coordinate s))
;; 	      (p (momentum s)))
;;     (let ((I (/ (+ (* alpha (square x))
;; 		               (/ (square p) alpha))
;; 		            2)))
;;       (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
;; 			                   (/ p (sqrt (* 2 I alpha))))))
;; 	      (up t theta I)))))



;; (pe
;;  ((compose (polar-canonical-inverse 'alpha)
;; 	         (polar-canonical 'alpha))
;;   (up 't 'x 'p)))
;; (up t x p)

;; |#

;; #|
;; ;;; It is clearly canonical.

;; (print-expression
;;  ((symplectic-transform? (polar-canonical 'alpha))
;;   (up 't 'a 'I)))
;; (matrix-by-rows (list 0 0) (list 0 0))
;; |#
