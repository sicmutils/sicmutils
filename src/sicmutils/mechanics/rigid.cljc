#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.rigid
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [sin cos + - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.mechanics.rotation :as r]
            [sicmutils.quaternion :as q]
            [sicmutils.structure :as s :refer [up]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; ## ch2: generalized coordinates to angular velocities.

;; TODO move to matrix namespace
(defn- antisymmetric? [A]
  (v/zero?
   (g/simplify
    (+ (g/transpose A) A))))

(defn antisymmetric->column-matrix
  "Given an antisymmetric matrix-structure of dimension 3,
  return the column vector of its positive components."
  [a]
  {:pre [(antisymmetric? a)]}
  (matrix/column (get-in a [2 1])
                 (get-in a [0 2])
                 (get-in a [1 0])))

(defn three-vector-components->antisymmetric [[x y z]]
  (matrix/by-rows
   [0 (- z) y]
   [z 0 (- x)]
   [(- y) x 0]))



#_
(defn M-of-q->omega-of-t
  [M-of-q]
  (fn [q]
    (let [M-on-path (f/compose M-of-q q)]
      (fn [t]
        (let [omega-cross (fn [t]
                            (* ((D M-on-path) t)
                               (matrix/transpose (M-on-path t))))]
          (antisymmetric->column-matrix (omega-cross t)))))))

;; TODO test this new one!!

 ;;; Suggested by Jack Wisdom on 30 Sept 2019.
(defn M-of-q->omega-of-t [M-of-q]
  (fn [q]
    (let [M-of-t (f/compose M-of-q q)]
      (fn [t]
        (antisymmetric->column-matrix
         (* ((D M-of-t) t)
            (matrix/transpose (M-of-t t))))))))

(defn M-of-q->omega-body-of-t [M-of-q]
  (fn [q]
    (fn [t]
      (* (matrix/transpose (M-of-q (q t)))
         (((M-of-q->omega-of-t M-of-q) q) t)))))

(defn M->omega [M-of-q]
  (L/Gamma-bar
   (M-of-q->omega-of-t M-of-q)))

(defn M->omega-body [M-of-q]
  (L/Gamma-bar
   (M-of-q->omega-body-of-t M-of-q)))

;; Assuming omega-body is on principal axes, and A, B, C are the principal
;; moments. Angular velocity to kinetic energy and angular momenta

(defn T-body [A B C]
  (fn [[w0 w1 w2]]
    (* (/ 1 2)
       (+ (* A (g/square w0))
          (* B (g/square w1))
          (* C (g/square w2))))))

(defn L-body [A B C]
  (fn [[w0 w1 w2]]
    (s/down (* A w0)
            (* B w1)
            (* C w2))))

(defn L-space [M]
  (fn [A B C]
    (fn [omega-body]
      (* ((L-body A B C) omega-body)
         (g/transpose M)))))

;; Euler Angles

(defn Euler->M [[theta phi psi]]
  (* (r/rotate-z-matrix phi)
     (* (r/rotate-x-matrix theta)
        (r/rotate-z-matrix psi))))

(defn Euler->omega [angles-path]
  (fn [t]
    (letfn [(M-on-path [t]
              (Euler->M (angles-path t)))
            (w-cross [t]
              (* ((D M-on-path) t)
                 (g/transpose (M-on-path t))))]
      (antisymmetric->column-matrix
       (w-cross t)))))

(defn Euler->omega-body [angles-path]
  (fn [t]
    (* (g/transpose (Euler->M (angles-path t)))
       ((Euler->omega angles-path) t))))

;; (show-expression
;;  ((Euler->omega-body
;;    (up (literal-function 'theta)
;;       (literal-function 'phi)
;;       (literal-function 'psi)))
;;   't))
;; (matrix-by-rows
;;  (list (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
;;          (* ((D theta) t) (cos (psi t)))))
;;  (list (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
;;          (* -1 ((D theta) t) (sin (psi t)))))
;;  (list (+ (* (cos (theta t)) ((D phi) t))
;;          ((D psi) t))))
;; |#

;; #|
;; (show-expression
;;  (((M-of-q->omega-body-of-t Euler->M)
;;    (up (literal-function 'theta)
;;        (literal-function 'phi)
;;        (literal-function 'psi)))
;;   't))
;; (matrix-by-rows
;;  (list (+ (* (sin (theta t)) (sin (psi t)) ((D phi) t))
;;          (* ((D theta) t) (cos (psi t)))))
;;  (list (+ (* (sin (theta t)) (cos (psi t)) ((D phi) t))
;;          (* -1 ((D theta) t) (sin (psi t)))))
;;  (list (+ (* (cos (theta t)) ((D phi) t))
;;          ((D psi) t))))

;; (show-expression
;;  ((M->omega-body Euler->M)
;;   (up 't
;;       (up 'theta 'phi 'psi)
;;       (up 'thetadot 'phidot 'psidot))))
;; (matrix-by-rows
;;  (list (+ (* phidot (sin psi) (sin theta)) (* thetadot (cos psi))))
;;  (list (+ (* phidot (cos psi) (sin theta)) (* -1 thetadot (sin psi))))
;;  (list (+ (* phidot (cos theta)) psidot)))


;; Assuming Euler angles rotate principal axes from reference orientation.

;; Although this just appears to summarize (M->omega-body Euler->M) it is
;; actually essential to prevent intermediate expression explosion.

(defn Euler-state->omega-body
  [[_ [θ _ ψ] [θdot φdot ψdot]]]
  (let [ω-a (+ (* (sin ψ) (sin θ) φdot)
               (* (cos ψ) θdot))
        ω-b (+ (* (cos ψ) (sin θ) φdot)
               (* -1 (sin ψ) θdot))
        ω-c (+ (* (cos θ) φdot)
               ψdot)]
    (up ω-a ω-b ω-c)))

(defn T-body-Euler [A B C]
  (fn [local]
    ((T-body A B C)
     (Euler-state->omega-body local))))

(def T-rigid-body T-body-Euler)

(defn L-body-Euler [A B C]
  (fn [local]
    ((L-body A B C)
     (Euler-state->omega-body local))))

;; TODO this is new; the old one returned a column vector, here we have a down.

(def Euler-state->L-body L-body-Euler)

(defn L-space-Euler [A B C]
  (fn [local]
    (let [angles (L/coordinate local)]
      (* ((L-body-Euler A B C) local)
         (g/transpose (Euler->M angles))))))

(def Euler-state->L-space L-space-Euler)

;; TODO old way flips order and has no transpose. Good?
#_
(defn Euler-state->L-space [A B C]
  (fn [[_ angles _ :as local]]
    (* (r/Euler->M angles)
       ((Euler-state->L-body A B C) local))))


;; (define an-Euler-state
;;   (up 't
;;       (up 'theta 'phi 'psi)
;;       (up 'thetadot 'phidot 'psidot)))

;; (show-expression
;;  (ref
;;   (((partial 2) (T-body-Euler 'A 'B 'C))
;;    an-Euler-state)
;;   1))
;; (+ (* A phidot (expt (sin psi) 2) (expt (sin theta) 2))
;;    (* B phidot (expt (cos psi) 2) (expt (sin theta) 2))
;;    (* A thetadot (cos psi) (sin psi) (sin theta))
;;    (* -1 B thetadot (cos psi) (sin psi) (sin theta))
;;    (* C phidot (expt (cos theta) 2))
;;    (* C psidot (cos theta)))

;; (print-expression
;;  (- (ref ((L-space-Euler 'A 'B 'C) an-Euler-state) 2)        ;$L_z$
;;     (ref (((partial 2) (T-body-Euler 'A 'B 'C)) an-Euler-state) 1)  ;$p_\phi$
;;     ))
;; 0

;; (print-expression
;;  (determinant
;;   (((compose (partial 2) (partial 2))
;;     (T-body-Euler 'A 'B 'C))
;;    an-Euler-state)))
;; (* A B C (expt (sin theta) 2))


(defn relative-error [value reference-value]
  (if (zero? reference-value)
    (u/illegal "Zero reference value -- RELATIVE-ERROR")
    (/ (- value reference-value)
       reference-value)))

(defn rigid-sysder [A B C]
  (L/Lagrangian->state-derivative (T-rigid-body A B C)))


;; (define ((monitor-errors win A B C L0 E0) state)
;;   (let ((t (time state))
;;        (L ((L-space-Euler A B C) state))
;;        (E ((T-body-Euler A B C) state)))
;;     (plot-point win t (relative-error (ref L 0) (ref L0 0)))
;;     (plot-point win t (relative-error (ref L 1) (ref L0 1)))
;;     (plot-point win t (relative-error (ref L 2) (ref L0 2)))
;;     (plot-point win t (relative-error E E0))))


;; ;;; rkqc4
;; ;;;(set! *ode-integration-method* 'qcrk4)
;; ;;;(define win (frame 0. 100. -1.e-12 1.e-12))

;; ;;; bulirsch-stoer
;; (set! *ode-integration-method* 'bulirsch-stoer)
;; (define win (frame 0. 100. -2.e-13 2.e-13))

;; (let ((A 1.) (B (sqrt 2.)) (C 2.)
;;       (state0 (up 0.0
;;                  (up 1. 0. 0.)
;;                  (up 0.1 0.1 0.1))))
;;   (let ((L0 ((L-space-Euler A B C) state0))
;;        (E0 ((T-body-Euler A B C) state0)))
;;     ((evolve rigid-sysder A B C)
;;      state0
;;      (monitor-errors win A B C L0 E0)
;;      0.1
;;      100.0
;;      1.0e-12)))
;; #|
;; (up 99.99999999999864
;;     (up .6319896958334494 1.3610271540875034 17.437900484737938)
;;     (up -.12343716197181527 .09016109524808046 .07567921658605782))
;; |#

;; (graphics-clear win)
;; (graphics-close win)
;; |#
;; 
;; #|
;; (show-expression
;;  ((T-body-Euler 'A 'A 'C)
;;   (up 't
;;       (up 'theta 'phi 'psi)
;;       (up 'thetadot 'phidot 'psidot))))
;; (+ (* 1/2 A (expt phidot 2) (expt (sin theta) 2))
;;    (* 1/2 C (expt phidot 2) (expt (cos theta) 2))
;;    (* C phidot psidot (cos theta))
;;    (* 1/2 A (expt thetadot 2))
;;    (* 1/2 C (expt psidot 2)))

;; ;;; Transformation of A(v):
;; ;;;  M^T A(Mv) M = A(v) for arbitrary v orthogonal M

;; (print-expression
;;  (let ((Euler (up 'theta 'phi 'psi))
;;       (v (up 'x 'y 'z)))
;;    (let ((M (Euler->M Euler)))
;;      (- (* (3vector-components->antisymmetric (* M v))
;;           M)
;;        (* M
;;           (3vector-components->antisymmetric v))))))
;; (matrix-by-rows (list 0 0 0) (list 0 0 0) (list 0 0 0))
;; |#

;; #|
;; ;;; Configuration equations for Euler's equations with Euler angles

;; (print-expression
;;  (let ((Euler (up (literal-function 'theta)
;;                  (literal-function 'phi)
;;                  (literal-function 'psi))))
;;    (antisymmetric->column-matrix
;;     (* (transpose ((Euler->M Euler) 't))
;;       ((D (Euler->M Euler)) 't)))))
;; (matrix-by-rows
;;  (list
;;   (+ (* ((D phi) t) (sin (psi t)) (sin (theta t)))
;;      (* ((D theta) t) (cos (psi t)))))
;;  (list
;;   (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
;;      (* -1 (sin (psi t)) ((D theta) t))))
;;  (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))
;; |#
;; 
;; #|
;; (define ((L-axisymmetric-top A C gMR) local)
;;   (let ((q (coordinate local))
;;         (qdot (velocity local)))
;;     (let ((theta (ref q 0))
;;           (thetadot (ref qdot 0))
;;           (phidot (ref qdot 1))
;;           (psidot (ref qdot 2)))
;;       (+ (* 1/2 A
;;             (+ (square thetadot)
;;                (square (* phidot (sin theta)))))
;;          (* 1/2 C
;;             (square (+ psidot (* phidot (cos theta)))))
;;          (* -1 gMR (cos theta))))))


;; (define ((V_eff p A C gMR) theta)
;;   (+ (/ (square p) (* 2 C))
;;      (* (/ (square p) (* 2 A))
;;        (square (tan (/ theta 2))))
;;      (* gMR (cos theta))))


;; ;;; Critical value of bifurcation when D^2 V_eff (0) = 0

;; (print-expression
;;  (((square derivative) (V_eff 'p_c 'A 'C 'gMR)) 0))
;; (+ (* -1 gMR) (/ (* 1/4 (expt p_c 2)) A))

;; ;;; critical angular speed in RPM is:
;; (* (/ 60 2pi) (/ 7.734804457773965e-3 6.6e-5))
;;                                         ;Value: 1119.1203302763215
;; |#



;; ## Quaternion representation

(defn quaternion-state->omega-body [[_ q qdot]]
  (let [m**2 (g/dot-product q q)
        omega**a
        (/ (* 2 (g/dot-product q (* q/I qdot))) m**2)

        omega**b
        (/ (* 2 (g/dot-product q (* q/J qdot))) m**2)

        omega**c
        (/ (* 2 (g/dot-product q (* q/K qdot))) m**2)]
    (up omega**a omega**b omega**c)))

(defn quaternion-state->omega-space [[_ q qdot]]
  (let [q:a (matrix/by-rows
             (list  0 +1  0  0)
             (list -1  0  0  0)
             (list  0  0  0 +1)
             (list  0  0 -1  0))
        q:b (matrix/by-rows
             (list  0  0 +1  0)
             (list  0  0  0 -1)
             (list -1  0  0  0)
             (list  0 +1  0  0))
        q:c (matrix/by-rows
             (list  0  0  0 +1)
             (list  0  0 +1  0)
             (list  0 -1  0  0)
             (list -1  0  0  0))
        Q     (matrix/up->column-matrix q)
        QdotT (matrix/transpose
               (matrix/up->column-matrix qdot))
        m**2 (get-in (* (matrix/transpose Q) Q) [0 0])
        omega**x (/ (get-in (* -2 QdotT q:a Q) [0 0]) m**2)
        omega**y (/ (get-in (* -2 QdotT q:b Q) [0 0]) m**2)
        omega**z (/ (get-in (* -2 QdotT q:c Q) [0 0]) m**2)]
    (up omega**x omega**y omega**z)))

(defn qw-state->L-body [A B C]
  (fn [qw-state]
    ((L-body A B C) (get qw-state 2))))

(comment
  ;; TODO gah, can't activate this damned thing until we get quat->rot in.
  (defn qw-state->L-space A B C
    (fn [qw-state]
      (let [q (L/coordinates qw-state)
            Lbody ((qw-state->L-body A B C) qw-state)
            M (quaternion->rotation-matrix (q/make q))]
        (* Lbody (transpose M))))))

(defn T-quaternion-state [A B C]
  (fn [[_ q qdot]]
    (let [Q (matrix/up->column-matrix q)
          Qdot (matrix/up->column-matrix qdot)
          m**2 (get-in (* (matrix/transpose Q) Q) [0 0])
          x (/ (* q/I Qdot) m**2)
          y (/ (* q/J Qdot) m**2)
          z (/ (* q/K Qdot) m**2)
          M (* Q (matrix/transpose Q))]
      (* 2 (+ (* A (get-in (* (matrix/transpose x) M x) [0 0]))
              (* B (get-in (* (matrix/transpose y) M y) [0 0]))
              (* C (get-in (* (matrix/transpose z) M z) [0 0])))))))

;; (define (qw-sysder A B C)
;;   (let ((B-C/A (/ (- B C) A))
;;         (C-A/B (/ (- C A) B))
;;         (A-B/C (/ (- A B) C)))
;;     (define (the-deriv qw-state)
;;       (let ((t (time qw-state))
;;             (q (coordinates qw-state))
;;             (omega-body (ref qw-state 2)))
;;         (let ((omega**a (ref omega-body 0))
;;               (omega**b (ref omega-body 1))
;;               (omega**c (ref omega-body 2)))
;;           (let ((tdot 1)
;;                 (qdot      ;driven quaternion
;;                  (* -1/2
;;                     (+ (* omega**a q:i)
;;                        (* omega**b q:j)
;;                        (* omega**c q:k))
;;                     q))
;;                 (omegadot  ;Euler's equations
;;                  (up (* B-C/A omega**b omega**c)
;;                      (* C-A/B omega**c omega**a)
;;                      (* A-B/C omega**a omega**b))))
;;             (up tdot qdot omegadot)))))
;;     the-deriv))

;; (define ((monitor-errors win A B C L0 E0) qw-state)
;;   (let ((t (time qw-state))
;;         (L ((qw-state->L-space A B C) qw-state))
;;         (E ((T-body A B C) (ref qw-state 2))))
;;     (plot-point win t (relative-error (ref L 0) (ref L0 0)))
;;     (plot-point win t (relative-error (ref L 1) (ref L0 1)))
;;     (plot-point win t (relative-error (ref L 2) (ref L0 2)))
;;     (plot-point win t (relative-error E E0))
;;     qw-state))

;; (define win (frame 0. 100. -1.e-13 1.e-13))

;; (let* ((A 1.) (B (sqrt 2.)) (C 2.)   ; moments of inertia
;;        (Euler-state (up 0.0           ; initial state
;;                         (up 1. 0. 0.)
;;                         (up 0.1 0.1 0.1)))
;;        (M (Euler->M (coordinates Euler-state)))
;;        (q (quaternion->vector (rotation-matrix->quaternion M)))
;;        (qw-state0
;;         (up (time Euler-state)
;;             q
;;             (Euler-state->omega-body Euler-state))))
;;   (let ((L0 ((qw-state->L-space A B C) qw-state0))
;;         (E0 ((T-body A B C) (ref qw-state0 2))))
;;     (graphics-clear win)
;;     ((evolve qw-sysder A B C)
;;      qw-state0
;;      (monitor-errors win A B C L0 E0)
;;      0.1                  ; step between plotted points
;;      100.0                ; final time
;;      1.0e-12)))
