#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.hamilton
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.core :as core]
            [pattern.rule :as r
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D D-as-matrix partial]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [sin cos + - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.mechanics.lagrange :as l :refer [momentum-tuple]]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; Hamiltonian mechanics requires a phase space QxP, and a function H:RxQxP -->
;; R
;;
;; A system has a dynamic state, which has the time, the configuration, and the
;; momenta. Hamiltonian mechanics is formulated in terms of the dynamic state.

(defn ->H-state
  [t q p]
  (up t q p))

(defn H-state? [s]
  (and (s/up? s)
       (= (count s) 3)
       (let [[t q v] s]
         (and (v/numerical? t)
              (or (and (v/numerical? q)
                       (v/numerical? v))
                  (and (s/up? q)
                       (s/down? v)
                       (= (s/dimension q)
                          (s/dimension v))))))))

;; TODO merge this and above somehow.
(defn compatible-H-state? [s]
  (and (s/down? s)
       (= (count s) 3)
       (let [[t q v] s]
         (and (v/numerical? t)
              (or (and (v/numerical? q)
                       (v/numerical? v))
                  (and (s/down? q)
                       (s/up? v)
                       (= (s/dimension q)
                          (s/dimension v))))))))

(defn momentum
  "Returns the momentum element of a local Hamiltonian state tuple (by convention,
  the third element)."
  [H-state]
  {:pre [(s/up? H-state) (> (count H-state) 2)]}
  (nth H-state 2))

(def ^{:doc "Alias for [[momentum]]."}
  state->p momentum)

(def ^{:doc "Alias for [[momentum]]."}
  momenta momentum)

(def ^{:doc "Alias for [[momentum]]."}
  P momentum)

(defn state->qp [[_ q p]]
  (up q p))

(defn literal-Hamiltonian-state [n-dof]
  (up (gensym 't)
      (s/literal-up (gensym 'x) n-dof)
      (s/literal-down (gensym 'p) n-dof)))

(defn Lstate->Hstate [L]
  (fn [Ls]
    (up (l/time Ls)
        (l/coordinate Ls)
        (((partial 2) L) Ls))))

(defn Hstate->Lstate [H]
  (fn [Hs]
    (up (l/time Hs)
        (l/coordinate Hs)
        (((partial 2) H) Hs))))

(defn H-state->matrix [s]
  (matrix/s->m
   (s/compatible-shape s) s 1))

(defn matrix->H-state [m s]
  (assert (= (matrix/num-cols m) 1))
  (assert (and (odd? (matrix/num-rows m))
               (> (matrix/num-rows m) 2)))
  (matrix/m->s
   (s/compatible-shape s) m 1))

(defn degrees-of-freedom [H-state]
  {:pre [(= (count H-state) 3)
         (= (s/dimension (l/coordinate H-state))
            (s/dimension (momentum H-state)))]}
  (s/dimension
   (l/coordinate H-state)))

(defn make-Hamiltonian [kinetic-energy potential-energy]
  (+ kinetic-energy potential-energy))

(defn qp->H-state-path [q p]
  (fn [t]
    (->H-state t (q t) (p t))))

(defn Hamiltonian->state-derivative [Hamiltonian]
  (fn [H-state]
    (->H-state 1
               (((partial 2) Hamiltonian) H-state)
               (- (((partial 1) Hamiltonian) H-state)))))

(def ^{:doc "Alias for [[Hamiltonian->state-derivative]], for compatibility with
  1st edition of SICM."}
  phase-space-derivative
  Hamiltonian->state-derivative)

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

;; If we express the energy in terms of t,Q,P we have the Hamiltonian. A
;; Hamiltonian is an example of an H-function: an H-function takes 2 vector
;; arguments and a scalar argument (t, Q, P). It produces a scalar result.

(defn H-rectangular
  "Returns a function of H-state..."
  [m V]
  (fn [[_ q p]]
    (+ (/ (g/square p) (* 2 m))
       (apply V q))))

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

;; From GJS: This ugly version tests for correctness of the result.

(defn Legendre-transform-procedure [F]
  (let [untested? (atom true)
        w-of-v    (D F)]
    (letfn [(putative-G [w]
              (let [z (s/compatible-zero w)
                    M ((D w-of-v) z)
                    b (w-of-v z)]
                (if (and @untested?
                         (v/zero?
                          (g/simplify
                           (matrix/determinant M))))
                  (throw
                   (ex-info "Legendre Transform Failure: determinant=0"
                            {:F F :w w}))
                  (let [v (g/solve-linear-left M (- w b))]
                    (- (* w v) (F v))))))]
      (fn G [w]
        (when @untested?
          (let [thing (s/typical-object w)]
            (if (v/= (g/simplify
                      (w-of-v ((D putative-G) thing)))
                     (g/simplify thing))
              (reset! untested? false)
              (throw
               (ex-info "Legendre Transform Failure: not quadratic"
                        {:F F :w w})))))
        (putative-G w)))))

(def Legendre-transform
  (o/make-operator Legendre-transform-procedure
                   'Legendre-transform))

;; Notice that Lagrangians and Hamiltonians are symmetrical with
;; respect to the Legendre transform.

(defn ^:private Lagrangian->Hamiltonian-fn
  [Lagrangian]
  (fn [[t q p]]  ;; H-state
    (let [L #(Lagrangian (up t q %))]
      ((Legendre-transform L) p))))

(def Lagrangian->Hamiltonian
  (o/make-operator Lagrangian->Hamiltonian-fn
                   'Lagrangian->Hamiltonian))

(defn Hamiltonian->Lagrangian-procedure [the-Hamiltonian]
  (fn [[t q qdot]]
    (letfn [(H [p]
              (the-Hamiltonian
               (->H-state t q p)))]
      ((Legendre-transform-procedure H) qdot))))

(def Hamiltonian->Lagrangian
  (o/make-operator Hamiltonian->Lagrangian-procedure
                   'Hamiltonian->Lagrangian))

(defn H-central-polar
  [m V]
  (fn [[_ [r _] [p_r p_phi]]]
    (+ (/ (+ (g/square p_r)
             (g/square (/ p_phi r)))
          (* 2 m))
       (V r))))

(defn Hamiltonian
  "Return SICM-style function signature for a Hamiltonian with n degrees of
  freedom (or 1 if n is not given).

  Useful for constructing Hamiltonian literal functions."
  ([] '(-> (UP Real (UP* Real) (DOWN* Real)) Real))
  ([n]
   (r/template
    (-> (UP Real (UP* Real ~n) (DOWN* Real ~n)) Real))))

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

(defn ^:no-doc Lie-derivative
  "p. 428

  We define the Lie derivative of F, as a derivative-like operator, relative to
  the given Hamiltonian-like function, H. Generalization and redefinition in
  calculus/Lie.scm
  "
  [H]
  (o/make-operator
   (fn [F] (Poisson-bracket F H))
   (list 'Lie-derivative H)))

(defn flow-derivative
  "the flow derivative generalizes the Lie derivative to allow for time dependent
  H and F --- computes the 'time' derivative of F along the flow specified by H"
  [H]
  (o/make-operator
   (fn [F]
     (+ ((partial 0) F)
        (Poisson-bracket F H)))
   (list 'flow-derivative H)))

(defmethod g/Lie-derivative [::v/function] [f]
  (Lie-derivative f))

(defn Lie-transform
  "p. 428, the Lie transform is just the time-advance operator using the Lie
  derivative (see Hamiltonian.scm)."
  [H t]
  (o/make-operator
   (g/exp (* t (g/Lie-derivative H)))
   `(~'Lie-transform ~H ~t)))

(defn flow-transform
  "The generalization of Lie-transform to include time dependence."
  [H delta-t]
  (o/make-operator
   (g/exp (* delta-t (flow-derivative H)))
   `(~'flow-transform ~H ~delta-t)))

(defn standard-map
  [K]
  (let [pv (v/principal-value v/twopi)]
    (fn [theta I return _fail]
      (let [nI (+ I (* K (sin theta)))]
        (return
         (pv (+ theta nI))
         (pv nI))))))


(comment
  ;; TODO for comparison:
  (define ((standard-map K) x y continue fail)
    (let ((yp (flo:pv (flo:+ y (flo:* K (flo:sin x))))))
      (continue (flo:pv (flo:+ x yp)) yp)))

  (define ((standard-map-inverse K) x y continue fail)
    (let ((xp (flo:pv (flo:- x y))))
      (continue xp (flo:pv (flo:- y (flo:* K (flo:sin xp)))))))
  )

;;; This is the 0-v/twopi principal value:

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

(defn F->K [F]
  (fn [H-state]
    (- (* (g/solve-linear-right
           (momentum H-state)
           (((partial 1) F) H-state))
          (((partial 0) F) H-state)))))

(defn H-central
  [m V]
  (fn [[_ q p]]
    (+  (/ (g/square p) (* 2 m))
        (V (g/abs q)))))


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
      (v/zero-like (l/coordinates s))
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
           ((D (f/compose H C)) s))))))



;; back to previous file:
(defn polar-canonical
  "p.327"
  [alpha]
  (fn [[t theta I]]
    (let [x (* (g/sqrt (/ (* 2 I) alpha)) (sin theta))
          p_x (* (g/sqrt (* 2 alpha I)) (cos theta))]
      (up t x p_x))))

(defn polar-canonical-inverse [alpha]
  (fn [[t x p]]
    (let [I (/ (+ (* alpha (g/square x))
                  (/ (g/square p) alpha))
               2)
          theta (g/atan (/ x (g/sqrt (/ (* 2 I) alpha)))
                        (/ p (g/sqrt (* 2 I alpha))))]
      (up t theta I))))



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
;;          (down 'p_x 'p_y))
;;        (up 'v_x 'v_y)))
;; (+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))

;; ;;; But strangely enough...
;; (pe (* (* (down 'p_x 'p_y)
;;          ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y))))
;;        (up 'v_x 'v_y)))
;; (+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))
;; |#
;;
;; #|
;; (define ((time-independent-canonical? C) s)
;;   (let ((s* (compatible-shape s)))
;;     (let ((J (linear-function->multiplier J-func s*)))
;;       (- J
;;         (* ((D C) s)
;;            (* J
;;               ((multiplicative-transpose s*) ((D C) s))))))))

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
;;
;; #|
;; (define ((C m0 m1) state)
;;   (let ((x (coordinate state))
;;        (p (momentum state)))
;;     (let ((x0 (ref x 0))
;;          (x1 (ref x 1))
;;          (p0 (ref p 0))
;;          (p1 (ref p 1)))
;;       (up
;;        (time state)
;;        (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;;                         (- x1 x0))
;;        (momentum-tuple (+ p0 p1)
;;                       (/ (- (* m0 p1) (* m1 p0))
;;                          (+ m0 m1)))))))

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
;;        (p (momentum Istate)))
;;     (let ((x (* p (sin theta)))
;;          (p_x (* p (cos theta))))
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
;;        (q (coordinate H-state))
;;        (p (momentum H-state)))
;;     (up t
;;        (up (ref q 0) (- (ref p 1)))
;;        (down (ref p 0) (ref q 1)))))

;; (define a-state
;;   (up 't (up 'x 'y) (down 'p_x 'p_y)))

;; (print-expression
;;  ((canonical-transform? Cmix) a-state))
;; (up (up 0 (up 0 0) (down 0 0))
;;     (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;;     (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))


;; (define (Cmix2 H-state)
;;   (let ((t (time H-state))
;;        (q (coordinate H-state))
;;        (p (momentum H-state)))
;;     (up t
;;        (flip-outer-index p)
;;        (- (flip-outer-index q)))))

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
;;        (p (momentum state)))
;;     (let ((x0 (ref x 0))
;;          (x1 (ref x 1))
;;          (p0 (ref p 0))
;;          (p1 (ref p 1)))
;;       (up (time state)
;;          (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;;              (- x1 x0))
;;          (down (+ p0 p1)
;;                (/ (- (* m0 p1) (* m1 p0))
;;                   (+ m0 m1)))))))

;; (define b-state
;;   (up 't
;;       (up (up 'x_1 'y_1)
;;          (up 'x_2 'y_2))
;;       (down (down 'p_x_1 'p_y_1)
;;            (down 'p_x_2 'p_y_2))))

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
    (let ((twon+1 (fix:+ (fix:* 2 n) 1)))
      (m:generate twon+1 twon+1
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
;;        (p (momentum Istate)))
;;     (let ((x (* p (sin theta)))
;;          (p_x (* p (cos theta))))
;;       (up t x p_x))))

;; (print-expression
;;  ((symplectic? a-non-canonical-transform)
;;   (up 't 'theta 'p)))
;; (matrix-by-rows (list 0 0 0)
;;                (list 0 0 (+ 1 (* -1 p)))
;;                (list 0 (+ -1 p) 0))

;; (print-expression
;;  ((symplectic? (polar-canonical 'alpha))
;;   (up 't 'a 'I)))
;; (matrix-by-rows (list 0 0 0)
;;                (list 0 0 0)
;;                (list 0 0 0))

;; (define (Cmix H-state)
;;   (let ((t (time H-state))
;;        (q (coordinate H-state))
;;        (p (momentum H-state)))
;;     (up t
;;        (up (ref q 0) (- (ref p 1)))
;;        (down   (ref p 0) (ref q 1)))))

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
;;        (q (coordinate H-state))
;;        (p (momentum H-state)))
;;     (up t
;;        (flip-outer-index p)
;;        (- (flip-outer-index q)))))

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
;;        (p (momentum state)))
;;     (let ((x0 (ref x 0))
;;          (x1 (ref x 1))
;;          (p0 (ref p 0))
;;          (p1 (ref p 1)))
;;       (up (time state)
;;          (up (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;;              (- x1 x0))
;;          (down (+ p0 p1)
;;                (/ (- (* m0 p1) (* m1 p0))
;;                   (+ m0 m1)))))))

;; (define b-state
;;   (up 't
;;       (up (up 'x_1 'y_1)
;;          (up 'x_2 'y_2))
;;       (down (down 'p_x_1 'p_y_1)
;;            (down 'p_x_2 'p_y_2))))

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
    (let ((two-n (m:dimension M)))
      (if (not (even? two-n))
        (error "Wrong type -- SYMPLECTIC-MATRIX?" M))
      (let ((J (symplectic-unit (quotient two-n 2))))
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
;;                (list 0 0 0 0)
;;                (list 0 0 0 0)
;;                (list 0 0 0 0))
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
;;          (p_x (* (sqrt (* 2 alpha I)) (cos theta))))
;;       (up t x p_x))))

;; (define ((polar-canonical-inverse alpha) s)
;;   (let ((t (time s))
;;        (x (coordinate s))
;;        (p (momentum s)))
;;     (let ((I (/ (+ (* alpha (square x))
;;                   (/ (square p) alpha))
;;                2)))
;;       (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
;;                         (/ p (sqrt (* 2 I alpha))))))
;;        (up t theta I)))))



;; (pe
;;  ((compose (polar-canonical-inverse 'alpha)
;;           (polar-canonical 'alpha))
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
