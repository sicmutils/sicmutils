#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.abstract.function :as f :include-macros true]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.matrix :as m]
            [sicmutils.mechanics.hamilton :as H]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.operator :as o]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [component up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest new-tests
  (let [s  (up 't (up 'x 'y) (down 'p_x 'p_y))
        m  (m/by-cols ['t 'x 'y 'p_x 'p_y])
        s2 (H/literal-Hamiltonian-state 2)]
    (is (= s
           (-> (H/H-state->matrix s)
               (H/matrix->H-state s2)))
        "round tripping from H-state to matrix and back")

    (is (= m
           (-> (H/matrix->H-state m s2)
               (H/H-state->matrix)))
        "round tripping from matrix to H-state and back"))

  (testing "H-rectangular"
    (is (= '(up 0
                (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m)
                    (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                      (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
           (simplify
            (f/with-literal-functions [x y p_x p_y [V [0 0] 0]]
              (((H/Hamilton-equations
                 (H/H-rectangular 'm V))
                (L/coordinate-tuple x y)
                (L/momentum-tuple p_x p_y))
               't)))))))

(deftest poisson
  (let [a-state (H/->H-state 't
                             (L/coordinate-tuple 'x 'y 'z)
                             (L/momentum-tuple 'p_x 'p_y 'p_z))]
    (is (= '(up (down 1 0 0)
                (down 0 1 0)
                (down 0 0 1))
           (simplify
            ((H/Poisson-bracket
              (up (comp (component 0) L/coordinate)
                  (comp (component 1) L/coordinate)
                  (comp (component 2) L/coordinate))
              (down (comp (component 0) H/momentum)
                    (comp (component 1) H/momentum)
                    (comp (component 2) H/momentum)))
             a-state))))
    (f/with-literal-functions
      [[FF (up 0 (up 1 2) (down 3 4)) 5]
       [GG (up 0 (up 1 2) (down 3 4)) 5]
       [HH (up 0 (up 1 2) (down 3 4)) 5]]
      (is (= '(FF (up t (up x y) (down pa pb)))
             (simplify (FF (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(down
               (((partial 0) FF) (up t (up x y) (down pa pb)))
               (down
                (((partial 1 0) FF) (up t (up x y) (down pa pb)))
                (((partial 1 1) FF) (up t (up x y) (down pa pb))))
               (up
                (((partial 2 0) FF) (up t (up x y) (down pa pb)))
                (((partial 2 1) FF) (up t (up x y) (down pa pb)))))
             (simplify ((D FF) (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(+
               (*
                -1
                (((partial 2 0) FF) (up t (up x y) (down p_x p_y)))
                (((partial 1 0) GG) (up t (up x y) (down p_x p_y))))
               (*
                -1
                (((partial 2 1) FF) (up t (up x y) (down p_x p_y)))
                (((partial 1 1) GG) (up t (up x y) (down p_x p_y))))
               (*
                (((partial 1 0) FF) (up t (up x y) (down p_x p_y)))
                (((partial 2 0) GG) (up t (up x y) (down p_x p_y))))
               (*
                (((partial 1 1) FF) (up t (up x y) (down p_x p_y)))
                (((partial 2 1) GG) (up t (up x y) (down p_x p_y)))))
             (simplify
              ((* (D FF)
                  (H/Poisson-bracket identity identity)
                  (D GG))
               (up 't (up 'x 'y) (down 'p_x 'p_y))))))
      (testing "Jacobi identity"
        (is (= 0 (simplify ((+ (H/Poisson-bracket FF (H/Poisson-bracket GG HH))
                               (H/Poisson-bracket GG (H/Poisson-bracket HH FF))
                               (H/Poisson-bracket HH (H/Poisson-bracket FF GG)))
                            (up 't (up 'x 'y) (down 'p_x 'p_y))))))))))

(deftest Lie-derivative-tests
  (let [F (f/literal-function 'F (H/Hamiltonian 2))
        G (f/literal-function 'G (H/Hamiltonian 2))
        H (f/literal-function 'H (H/Hamiltonian 2))
        L_F (H/Lie-derivative F)
        L_G (H/Lie-derivative G)]
    (is  (zero?
          (simplify
           (((+ (o/commutator L_F L_G)
                (H/Lie-derivative (H/Poisson-bracket F G)))
             H)
            (up 't (up 'x 'y) (down 'px 'py))))))))

(deftest Lie-transform-tests
  ;; The general solution for a trajectory is:
  ;;
  ;;  q(t,q0,p0) = A(q0,p0) cos (sqrt(k/m)*t + phi(q0,p0))
  ;;
  ;;  where A(q0,p0) = sqrt(2/k)*sqrt(p0^2/(2*m) + (k/2)*q0^2)
  ;;                 = sqrt((2/k)*E0)
  ;;
  ;;  and   phi(q0,p0) = - atan((1/sqrt(k*m))*(p0/q0))
  ;;
  ;; Thus, with initial conditions q0, p0
  ;;   we should get q(t) = q0*cos(sqrt(k/m)*t)+p0*sin(sqrt(k/m)*t)
  ;;
  ;; We can expand this as a Lie series:

  ;; (define ((H-harmonic m k) state)
  ;;   (let ((q (coordinate state))
  ;;        (p (momentum state)))
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
  ;;               (square (/ pphi r)))
  ;;            (* 2 m))
  ;;          (V r)))))

  ;; (series:for-each print-expression
  ;;                  (((Lie-transform
  ;;                     (H-central-polar 'm (literal-function 'U))
  ;;                     'dt)
  ;;                    state->q)
  ;;                   (->H-state 0
  ;;                             (coordinate-tuple 'r_0 'phi_0)
  ;;                             (momentum-tuple 'p_r_0 'p_phi_0)))
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
  ;;              (L-central-polar 'm (lambda (r) (- (/ 'GM r)))))
  ;;              'dt)
  ;;             state->q)
  ;;            (->H-state 0
  ;;                      (coordinate-tuple 'r_0 'phi_0)
  ;;                      (momentum-tuple 'p_r_0 'p_phi_0)))
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
  )

(deftest poincare-map-tests
  (comment
    ;; Now to do the Poincare section.
    ;;  Map explorer:
    ;;   Left button starts a trajectory.
    ;;   Middle button continues a trajectory.
    ;;   Right button interrogates coordinates.

    ;; (define (explore-map window poincare-map #!optional mode-or-n)
    ;;   (let* ((default-n 1000)
    ;;          (collector
    ;;           (cond ((default-object? mode-or-n)
    ;;                 (default-collector (default-monitor window)
    ;;                                    poincare-map
    ;;                                    default-n))
    ;;                ((number? mode-or-n)
    ;;                 (default-collector (default-monitor window)
    ;;                                    poincare-map
    ;;                                    mode-or-n))
    ;;                (else poincare-map))))
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
    ;; (define win (frame 0.0 v/twopi 0.0 v/twopi))
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
  )

(deftest more-tests
  #_
  (is (= (+ (V r)
            (/ (* 1/2 (expt p_r 2)) m)
            (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))
         ((compose (H-central 'm (literal-function 'V))
                   (F->CT p->r))
          (->H-state 't
                     (coordinate-tuple 'r 'phi)
                     (momentum-tuple 'p_r 'p_phi)))))

  (testing "time independent canonical"
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
    ;;        (p (momentum Istate)))
    ;;     (let ((x (* p (sin theta)))
    ;;          (p_x (* p (cos theta))))
    ;;       (up t x p_x))))

    ;; (print-expression
    ;;  ((time-independent-canonical? a-non-canonical-transform)
    ;;   (up 't 'theta 'p)))
    ;; (up 0 (+ (* -1 p x8102) x8102) (+ (* p x8101) (* -1 x8101)))
    ;; |#
    )

  (testing "qp-canonical?"
    ;; (define ((canonical-K? C K) s)
    ;;   (let ((s* (compatible-shape s)))
    ;;     (- (T-func s*)
    ;;        (+ (* ((D C) s) (J-func ((D K) s)))
    ;;          (((partial 0) C) s)))))


    ;; (define ((canonical-K? C K) s)
    ;;   (let ((DCs ((D C) s))
    ;;        (s* (compatible-shape s)))
    ;;     (- (T-func s*)
    ;;        (* DCs ((Hamiltonian->state-derivative K) s)))))
    ;; |#
    ;;
    ;; #|
    ;; (define ((rotating n) state)
    ;;   (let ((t (time state))
    ;;        (q (coordinate state)))
    ;;     (let ((x (ref q 0))
    ;;          (y (ref q 1))
    ;;          (z (ref q 2)))
    ;;       (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
    ;;                        (- (* (cos (* n t)) y) (* (sin (* n t)) x))
    ;;                        z))))

    ;; (define (C-rotating n) (F->CT (rotating n)))

    ;; (define ((K n) s)
    ;;   (let ((q (coordinate s))
    ;;        (p (momentum s)))
    ;;     (let ((x (ref q 0)) (y (ref q 1))
    ;;          (px (ref p 0)) (py (ref p 1)))
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
    ;;        (K 'n))
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

    )

  (testing "polar-canonical-tests"
    ;; TODO move to tests:
    ;; #|
    ;; (pe
    ;;  ((compose (polar-canonical-inverse 'alpha)
    ;;           (polar-canonical 'alpha))
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
    ;;        (q (coordinate H-state))
    ;;        (p (momentum H-state)))
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
    ;;        (q (coordinate H-state))
    ;;        (p (momentum H-state)))
    ;;     (up t
    ;;         (flip-outer-index p)
    ;;         (- (flip-outer-index q)))))

    ;; (print-expression
    ;;  ((time-independent-canonical? Cmix2)
    ;;   a-state))
    ;; (up 0 (up 0 0) (down 0 0))
    ;; |#
    )

  )

(deftest section-3-1-1
  ;; To move further into Hamiltonian mechanics, we will need
  ;; literal functions mapping structures to structures.

  (f/with-literal-functions [x y p_x p_y [V [1 2] 3]]
    (is (= '(V x y) (simplify (V 'x 'y))))
    (is (= '(up 0 (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m) (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t))) (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
           (simplify
            (((H/Hamilton-equations
               (H/H-rectangular
                'm V))
              (up x y)
              (down p_x p_y))
             't))))
    (is (= '(/ (* (/ 1 4) (expt y 2)) c)
           (simplify
            ((H/Legendre-transform (fn [x] (* 'c x x))) 'y))))

    (is (= '(* (/ 1 4) (expt p 2))
           (v/freeze
            (simplify ((H/Legendre-transform g/square) 'p)))))

    (is (= '(+ (* (/ 1 2) m (expt v_x 2))
               (* (/ 1 2) m (expt v_y 2))
               (* -1 (V x y)))
           (v/freeze
            (simplify
             ((L/L-rectangular 'm V) (up 't (up 'x 'y) (up 'v_x 'v_y)))))))

    (is (= '(/ (+ (* m (V x y))
                  (* (/ 1 2) (expt p_x 2))
                  (* (/ 1 2) (expt p_y 2)))
               m)
           (simplify ((H/Lagrangian->Hamiltonian
                       (L/L-rectangular 'm V))
                      (up 't (up 'x 'y) (down 'p_x 'p_y))))))))

(deftest gjs-tests
  (is (= '(up 0
              (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m)
                  (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
              (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                    (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))

         (f/with-literal-functions [x y p_x p_y [V [0 1] 2]]
           (simplify (((H/Hamilton-equations
                        (H/H-rectangular
                         'm V))
                       (L/coordinate-tuple x y)
                       (L/momentum-tuple p_x p_y))
                      't)))))

  (is (= '(/ (+ (* m (expt r 2) (V r))
                (* (/ 1 2) (expt p_r 2) (expt r 2))
                (* (/ 1 2) (expt p_phi 2)))
             (* m (expt r 2)))
         (simplify
          ((H/Lagrangian->Hamiltonian
            (L/L-central-polar 'm (f/literal-function 'V)))
           (H/->H-state 't
                        (L/coordinate-tuple 'r 'phi)
                        (L/momentum-tuple 'p_r 'p_phi))))))
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t))) m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t))) (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t)) (* m (expt (r t) 3) ((D V) (r t))) (* -1 (expt (p_phi t) 2))) (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (f/with-literal-functions [r phi p_r p_phi V]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-central-polar 'm V)))
              (L/coordinate-tuple r phi)
              (L/momentum-tuple p_r p_phi))
             't)))))
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t))) m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t))) (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t)) (* GM (expt m 2) (r t)) (* -1 (expt (p_phi t) 2))) (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (f/with-literal-functions [r phi p_r p_phi]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-central-polar 'm
                                   (fn [r] (- (/ (* 'GM 'm) r))))))
              (L/coordinate-tuple r phi)
              (L/momentum-tuple p_r p_phi))
             't)))))
  (let [F (f/literal-function 'F (H/Hamiltonian 2))
        G (f/literal-function 'G (H/Hamiltonian 2))
        H (f/literal-function 'G (H/Hamiltonian 2))
        L_F (g/Lie-derivative F)
        L_G (g/Lie-derivative G)]
    (is (= 0 (simplify (((+ (o/commutator L_F L_G)
                            (g/Lie-derivative (H/Poisson-bracket F G)))
                         H)
                        (up 't (up 'x 'y) (down 'px 'py))))))))

(deftest symplectic
  (testing "unit"
    (is (= (m/by-rows [0 0 0 0 0 0 1 0 0 0 0 0]
                      [0 0 0 0 0 0 0 1 0 0 0 0]
                      [0 0 0 0 0 0 0 0 1 0 0 0]
                      [0 0 0 0 0 0 0 0 0 1 0 0]
                      [0 0 0 0 0 0 0 0 0 0 1 0]
                      [0 0 0 0 0 0 0 0 0 0 0 1]
                      [-1 0 0 0 0 0 0 0 0 0 0 0]
                      [0 -1 0 0 0 0 0 0 0 0 0 0]
                      [0 0 -1 0 0 0 0 0 0 0 0 0]
                      [0 0 0 -1 0 0 0 0 0 0 0 0]
                      [0 0 0 0 -1 0 0 0 0 0 0 0]
                      [0 0 0 0 0 -1 0 0 0 0 0 0])
           (H/symplectic-unit 6)))))

(deftest iterated-map-test
  (let [fail (constantly false)
        M (fn [x y cont fail] (if (> x 10) (fail) (cont (inc x) (dec y))))]
    (is (= '(6 95) ((H/iterated-map M 5) 1 100 list fail)))
    (is (= '(10 91) ((H/iterated-map M 9) 1 100 list fail)))
    (is (= false ((H/iterated-map M 20) 1 100 list fail )))))


;; Next tests come from hamiltonian.scm

;; #|
;; TODO this will be in lagrange
;; (define ((L-rectangular m V) local)
;;   (let ((q (coordinate local))
;;         (qdot (velocity local)))
;;     (- (* 1/2 m (square qdot))
;;        (V (ref q 0) (ref q 1)))))

;; (show-expression
;;  ((Lagrangian->Hamiltonian
;;    (L-rectangular 'm
;; 		              (literal-function 'V
;; 				                            (-> (X Real Real) Real))))
;;   (->H-state 't
;;              (coordinate-tuple 'x 'y)
;;              (momentum-tuple 'p_x 'p_y))))
;; (+ (V x y)
;;    (/ (* 1/2 (expt p_x 2)) m)
;;    (/ (* 1/2 (expt p_y 2)) m))
;; |#

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

;; (show-expression
;;  ((Lagrangian->Hamiltonian
;;    (L-central-polar 'm (literal-function 'V)))
;;   (->H-state 't
;;              (coordinate-tuple 'r 'phi)
;;              (momentum-tuple 'p_r 'p_phi))))
;; (+ (V r)
;;    (/ (* 1/2 (expt p_r 2)) m)
;;    (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))

;; (show-expression
;;  (((Hamilton-equations
;;     (Lagrangian->Hamiltonian
;;      (L-central-polar 'm (literal-function 'V))))
;;    (coordinate-tuple (literal-function 'r)
;;                      (literal-function 'phi))
;;    (momentum-tuple (literal-function 'p_r)
;;                    (literal-function 'p_phi)))
;;   't))
;; (up
;;  0
;;  (up (+ ((D r) t) (/ (* -1 (p_r t)) m))
;;      (+ ((D phi) t) (/ (* -1 (p_phi t)) (* m (expt (r t) 2)))))
;;  (down
;;   (+ ((D p_r) t)
;;      ((D V) (r t))
;;      (/ (* -1 (expt (p_phi t) 2)) (* m (expt (r t) 3))))
;;   ((D p_phi) t)))
;;
;; ;;; If we substitute a Coulomb potential in for V we get the equations
;; ;;;  for satellite motion around a spherical primary.

;; (show-expression
;;  (((Hamilton-equations
;;     (Lagrangian->Hamiltonian
;;      (L-central-polar 'm
;; 		                  (lambda (r)
;; 			                        (- (/ (* 'GM 'm) r))))))
;;    (coordinate-tuple (literal-function 'r)
;; 		                 (literal-function 'phi))
;;    (momentum-tuple (literal-function 'p_r)
;; 		               (literal-function 'p_phi)))
;;   't))
;; (up 0
;;     (up (+ ((D r) t) (/ (* -1 (p_r t)) m))
;; 	      (+ ((D phi) t) (/ (* -1 (p_phi t)) (* m (expt (r t) 2)))))
;;     (down
;;      (+ ((D p_r) t)
;; 	      (/ (* GM m) (expt (r t) 2))
;; 	      (/ (* -1 (expt (p_phi t) 2)) (* m (expt (r t) 3))))
;;      ((D p_phi) t)))

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
;; |#

;; #|
;; (define ((L-harmonic m k) local)
;;   (let ((q (coordinate local))
;;         (v (velocity local)))
;;     (- (* 1/2 m (square v))
;;        (* 1/2 k (square q)))))

;; (show-expression
;;  (((Hamilton-equations
;;     (Lagrangian->Hamiltonian (L-harmonic 'm 'k)))
;;    (coordinate-tuple (literal-function 'x_1)
;; 		                 (literal-function 'x_2))
;;    (momentum-tuple (literal-function 'p_1)
;; 		               (literal-function 'p_2)))
;;   't))
;; (up 0
;;     (up (+ ((D x_1) t) (/ (* -1 (p_1 t)) m_1))
;; 	      (+ ((D x_2) t) (/ (* -1 (p_2 t)) m_2)))
;;     (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
;; 	        (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))
;;
;; ;;; Continuing with our coupled harmonic oscillators
;; ;;;  we obtain the Hamiltonian:

;; (define ((L-coupled-harmonic m k) state)
;;   (let ((q (coordinate state))
;; 	      (qdot (velocity state)))
;;     (- (* 1/2 qdot m qdot)
;;        (* 1/2 q k q))))

;; (show-expression
;;  ((Lagrangian->Hamiltonian
;;    (L-coupled-harmonic (down (down 'm_1 0)
;; 			                       (down 0 'm_2))
;; 		                   (down (down 'k_1 'c)
;; 			                       (down 'c 'k_2))))
;;   (->H-state 't
;; 	           (coordinate-tuple 'x_1 'x_2)
;; 	           (momentum-tuple 'p_1 'p_2))))
;; (+ (* c x_1 x_2)
;;    (* 1/2 k_1 (expt x_1 2))
;;    (* 1/2 k_2 (expt x_2 2))
;;    (/ (* 1/2 (expt p_2 2)) m_2)
;;    (/ (* 1/2 (expt p_1 2)) m_1))

;; (show-expression
;;  (((Hamilton-equations
;;     (Lagrangian->Hamiltonian
;;      (L-coupled-harmonic (down (down 'm_1 0)
;; 			                         (down 0 'm_2))
;; 			                   (down (down 'k_1 'c)
;; 			                         (down 'c 'k_2)))))
;;    (coordinate-tuple (literal-function 'x_1)
;; 		                 (literal-function 'x_2))
;;    (momentum-tuple (literal-function 'p_1)
;; 		               (literal-function 'p_2)))
;;   't))
;; (up
;;  0
;;  (up (+ ((D x_1) t) (/ (* -1 (p_1 t)) m_1))
;;      (+ ((D x_2) t) (/ (* -1 (p_2 t)) m_2)))
;;  (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
;;        (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))

;; |#
;;
;; #|
;; ;;; Continuation of demonstration of bundled coordinates.

;; (define ((L-two-particle m1 m2) local)
;;   (let ((x (coordinate local))
;; 	      (v (velocity local))
;; 	      (V (literal-function 'V (-> (X (^ Real 2) (^ Real 2)) Real))))
;;     (let ((x1 (ref x 0)) (x2 (ref x 1))
;;           (v1 (ref v 0)) (v2 (ref v 1)))
;;       (- (+ (* 1/2 m1 (square v1))
;; 	          (* 1/2 m2 (square v2)))
;; 	       (V x1 x2)))))

;; (show-expression
;;  (((Hamilton-equations
;;     (Lagrangian->Hamiltonian
;;      (L-two-particle 'm_1 'm_2)))
;;    (coordinate-tuple (coordinate-tuple (literal-function 'x_1)
;; 				                               (literal-function 'y_1))
;; 		                 (coordinate-tuple (literal-function 'x_2)
;; 				                               (literal-function 'y_2)))
;;    (momentum-tuple (momentum-tuple (literal-function 'p_x_1)
;; 				                           (literal-function 'p_y_1))
;; 		               (momentum-tuple (literal-function 'p_x_2)
;; 				                           (literal-function 'p_y_2))))
;;   't))
;; (up 0
;;     (up (up (+ ((D x_1) t) (/ (* -1 (px_1 t)) m_1))
;; 	          (+ ((D y_1) t) (/ (* -1 (py_1 t)) m_1)))
;; 	      (up (+ ((D x_2) t) (/ (* -1 (px_2 t)) m_2))
;; 	          (+ ((D y_2) t) (/ (* -1 (py_2 t)) m_2))))
;;     (down (down
;; 	         (+ ((D px_1) t)
;; 	            (((partial 0 0) V)
;; 	             (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
;; 	         (+ ((D py_1) t)
;; 	            (((partial 0 1) V)
;; 	             (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
;; 	        (down
;; 	         (+ ((D px_2) t)
;; 	            (((partial 1 0) V)
;; 	             (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
;; 	         (+ ((D py_2) t)
;; 	            (((partial 1 1) V)
;; 	             (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))))
;; |#
;;
;; #|
;; ;;; From 3.3 -- Phase-space reduction


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


;; (show-expression
;;  ((Lagrangian->Hamiltonian (L-axisymmetric-top 'A 'C 'gMR))
;;   (->H-state 't
;;              (vector 'theta 'phi 'psi)
;;              (vector 'p_theta 'p_phi 'p_psi))))
;; (+ (* gMR (cos theta))
;;    (/ (* 1/2 (expt p_psi 2)) C)
;;    (/ (* 1/2 (expt p_psi 2) (expt (cos theta) 2)) (* A (expt (sin theta) 2)))
;;    (/ (* 1/2 (expt p_theta 2)) A)
;;    (/ (* -1 p_phi p_psi (cos theta)) (* A (expt (sin theta) 2)))
;;    (/ (* 1/2 (expt p_phi 2)) (* A (expt (sin theta) 2))))
;; |#
;;
;; #|
;; ;;; ********** This got ugly *************

;; (show-expression
;;  (((Hamilton-equations
;;     (Lagrangian->Hamiltonian
;;      (L-axisymmetric-top 'A 'C 'gMR)))
;;    (coordinate-tuple (literal-function 'theta)
;; 		                 (literal-function 'phi)
;; 		                 (literal-function 'psi))
;;    (momentum-tuple (literal-function 'p_theta)
;; 		               (literal-function 'p_phi)
;; 		               (literal-function 'p_psi)))
;;   't))
;; (up
;;  0
;;  (up
;;   (+ ((D theta) t) (/ (* -1 (p_theta t)) A))
;;   (+ ((D phi) t)
;;      (/ (* (cos (theta t)) (p_psi t)) (* A (expt (sin (theta t)) 2)))
;;      (/ (* -1 (p_phi t)) (* A (expt (sin (theta t)) 2))))
;;   (+
;;    ((D psi) t)
;;    (/ (* -1 (p_psi t)) C)
;;    (/ (* -1 (expt (cos (theta t)) 2) (p_psi t)) (* A (expt (sin (theta t)) 2)))
;;    (/ (* (p_phi t) (cos (theta t))) (* A (expt (sin (theta t)) 2)))))
;;  (down
;;   (+
;;    (/ (* -1 gMR (expt (cos (theta t)) 4)) (expt (sin (theta t)) 3))
;;    ((D p_theta) t)
;;    (/ (* 2 gMR (expt (cos (theta t)) 2)) (expt (sin (theta t)) 3))
;;    (/ (* (p_phi t) (expt (cos (theta t)) 2) (p_psi t))
;;       (* A (expt (sin (theta t)) 3)))
;;    (/ (* -1 (cos (theta t)) (expt (p_psi t) 2)) (* A (expt (sin (theta t)) 3)))
;;    (/ (* -1 (expt (p_phi t) 2) (cos (theta t))) (* A (expt (sin (theta t)) 3)))
;;    (/ (* -1 gMR) (expt (sin (theta t)) 3))
;;    (/ (* (p_phi t) (p_psi t)) (* A (expt (sin (theta t)) 3))))
;;   ((D p_phi) t)
;;   ((D p_psi) t)))
;; |#
;;
;; ;;; The Poisson Bracket is a differential operator on H-functions:
;; #|
;; ;;; This can give WRONG ANSWERS on structure-valued functions...
;; (define (Poisson-bracket f g)
;;   (- (* ((partial 1) f) ((partial 2) g))
;;      (* ((partial 2) f) ((partial 1) g))))

;; (define (Poisson-bracket f g)
;;   (cond ((and (structure? f) (structure? g))
;; 	       (s:map/r (lambda (fi)
;; 		                      (s:map/r (lambda (gj)
;; 			                                     (Poisson-bracket fi gj))
;; 			                             g))
;; 		              f)
;;          #;(error "Poisson bracket of two structures" f g)
;;          )
;;         ((structure? f)
;;          (s:generate (s:length f) (s:same f)
;;                      (lambda (i)
;;                              (Poisson-bracket (ref f i) g))))
;;         ((structure? g)
;;          (s:generate (s:length g) (s:same g)
;;                      (lambda (i)
;;                              (Poisson-bracket f (ref g i)))))
;;         (else
;;          (- (* ((partial 1) f) ((partial 2) g))
;;             (* ((partial 2) f) ((partial 1) g))))))
;; |#


;; TODO and then the next block come from after Poisson-bracket def:

;; #|
;; (pe ((Poisson-bracket
;;       (up (compose (component 0) coordinate)
;;           (compose (component 1) coordinate)
;;           (compose (component 2) coordinate))
;;       (down (compose (component 0) momentum)
;;             (compose (component 1) momentum)
;;             (compose (component 2) momentum)))
;;      a-state))
;; (up (down 1 0 0) (down 0 1 0) (down 0 0 1))
;; |#

;; #|
;; (define FF
;;   (literal-function 'F
;;                     (-> (UP Real
;;                             (UP Real Real)
;;                             (DOWN Real Real))
;;                         Real)))

;; (define GG
;;   (literal-function 'G
;;                     (-> (UP Real
;;                             (UP Real Real)
;;                             (DOWN Real Real))
;;                         Real)))

;; (pe ((* (D FF)
;;         (Poisson-bracket identity identity)
;;         (D GG))
;;      (up 't (up 'x 'y) (down 'px 'py))))
;; (+ (* -1
;;       (((partial 1 0) G) (up t (up x y) (down px py)))
;;       (((partial 2 0) F) (up t (up x y) (down px py))))
;;    (* -1
;;       (((partial 1 1) G) (up t (up x y) (down px py)))
;;       (((partial 2 1) F) (up t (up x y) (down px py))))
;;    (* (((partial 2 0) G) (up t (up x y) (down px py)))
;;       (((partial 1 0) F) (up t (up x y) (down px py))))
;;    (* (((partial 2 1) G) (up t (up x y) (down px py)))
;;       (((partial 1 1) F) (up t (up x y) (down px py)))))
;; |#
;;
;; #|
;; (define F (literal-function 'F (Hamiltonian 2)))
;; (define G (literal-function 'G (Hamiltonian 2)))
;; (define H (literal-function 'H (Hamiltonian 2)))

;; ;;; Jacobi identity
;; (pe ((+ (Poisson-bracket F (Poisson-bracket G H))
;;         (Poisson-bracket G (Poisson-bracket H F))
;;         (Poisson-bracket H (Poisson-bracket F G)))
;;      (up 't (up 'x 'y) (down 'px 'py))))
;; 0
;; |#

;; #|
;; (define Sx (compose (component 0) coordinate))
;; (define Sy (compose (component 1) coordinate))
;; (define Sz (compose (component 2) coordinate))

;; (define Spx (compose (component 0) momentum))
;; (define Spy (compose (component 1) momentum))
;; (define Spz (compose (component 2) momentum))

;; ;;; for example L = [Lx, Ly, Lz]
;; ;;; where Li are components of angular momentum

;; (define Lx (- (* Sy Spz) (* Spy Sz)))
;; (define Ly (- (* Sz Spx) (* Spz Sx)))
;; (define Lz (- (* Sx Spy) (* Spx Sy)))
;; (define L (down Lx Ly Lz))

;; (define 3-state
;;   (->H-state 't
;;              (coordinate-tuple 'x 'y 'z)
;;              (momentum-tuple 'p_x 'p_y 'p_z)))

;; (pe ((Poisson-bracket Lx L) 3-state))
;; (down 0 (+ (* -1 p_x y) (* p_y x)) (+ (* -1 p_x z) (* p_z x)))

;; ;;; Poisson brackets are compositional with canonical transformations
;; ;;; (see point-transformations.scm for F->CT and time-varying.scm for
;; ;;; C-rotating, repeated here.

;; (define ((rotating n) state)
;;   (let ((t (time state))
;;         (q (coordinate state)))
;;     (let ((x (ref q 0))
;;           (y (ref q 1))
;;           (z (ref q 2)))
;;       (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
;;                         (- (* (cos (* n t)) y) (* (sin (* n t)) x))
;;                         z))))

;; (define (C-rotating n) (F->CT (rotating n)))

;; (pe ((- (compose (Poisson-bracket Lx Ly) (C-rotating 'n))
;;         (Poisson-bracket (compose Lx (C-rotating 'n))
;;                          (compose Ly (C-rotating 'n))) )
;;      3-state))
;; 0
;; |#
;;
;; #|
;; ;;; Poisson brackets in terms of J
;; ;;;  Guaranteed to work only for scalar valued functions

;; #|
;; ;;; From canonical.scm

;; (define (J-func DH)
;;   (->H-state 0
;;              (ref DH 2)
;;              (- (ref DH 1))))
;; |#

;; (define ((PB f g) s)
;;   (* ((D f) s) (J-func ((D g) s))))

;; (define a-state
;;   (->H-state 't
;;              (coordinate-tuple 'x 'y 'z)
;;              (momentum-tuple 'p_x 'p_y 'p_z)))

;; (pe ((- (Poisson-bracket Lx Ly) Lz) a-state))
;; 0
;; (pe ((- (PB Lx Ly) Lz) a-state))
;; 0

;; (define ((PB f g) s)
;;   (let ((J ((D J-func) ((D g) s))))
;;     (* ((D f) s) (* J ((D g) s)))))

;; (define ((PB f g) s)
;;   (let ((J (linear-function->multiplier J-func ((D g) s))))
;;     (* ((D f) s) (* J ((D g) s)))))

;; (define (H-harmonic m k)
;;   (Lagrangian->Hamiltonian (L-harmonic m k)))

;; (pe
;;  (- ((Poisson-bracket (H-harmonic 'm 'k)
;;                       ((component 0) coordinate))
;;      a-state)
;;     ((PB (H-harmonic 'm 'k)
;;          (compose (component 0) coordinate))
;;      a-state)))
;; 0

;; (pe
;;  (- ((Poisson-bracket (H-harmonic 'm 'k) coordinate)
;;      a-state)
;;     ((PB (H-harmonic 'm 'k) coordinate)
;;      a-state)))
;; (up 0 0 0)

;; (pe ((PB momentum (H-harmonic 'm 'k))
;;      a-state))
;; (down (* -1 k x) (* -1 k y) (* -1 k z))

;; (pe ((PB coordinate (H-harmonic 'm 'k))
;;      a-state))
;; (up (/ p_x m) (/ p_y m) (/ p_z m))
;; |#
