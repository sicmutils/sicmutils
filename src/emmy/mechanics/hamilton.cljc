#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.hamilton
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.core :as core]
            [pattern.rule :as r]
            [emmy.calculus.derivative :refer [D D-as-matrix partial]]
            [emmy.function :as f]
            [emmy.generic :as g :refer [sin cos + - * /]]
            [emmy.matrix :as matrix]
            [emmy.mechanics.lagrange :as l]
            [emmy.operator :as o]
            [emmy.structure :as s :refer [up]]
            [emmy.value :as v]))

;; Hamiltonian mechanics requires a phase space QxP, and a function H:RxQxP -->
;; R
;;
;; A system has a dynamic state, which has the time, the configuration, and the
;; momenta. Hamiltonian mechanics is formulated in terms of the dynamic state.

(defn Hamiltonian
  "Returns function signature for a Hamiltonian with n degrees of freedom (or an
  unrestricted number if n is not given).

  Useful for constructing Hamiltonian literal functions."
  [n]
  (r/template
   (-> (UP Real (UP* Real ~n) (DOWN* Real ~n)) Real)))

(defn ->H-state
  "Given a time `t`, coordinate tuple (or scalar) `q` and momentum tuple (or
  scalar) `p`, returns a 'Hamiltonian state tuple', ie, the state expected by a
  Hamiltonian."
  [t q p]
  (up t q p))

(defn H-state?
  "Returns true if the supplied state is

  - of type [[emmy.structure/up]]

  - contains three elements of `time`, `coordinate` and `momentum` of either of
    the following type shapes:

  ```
  (up <number> <number> <number>)
  (up <number> (up <number>*) (down <number>*))
  ```

  If structural, the dimension of the coordinate and momentum tuples must match."
  [s]
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

(defn compatible-H-state?
  "Returns true if `s` is compatible for contraction with a proper H-state, false
  otherwise."
  [s]
  (H-state?
   (s/transpose s)))

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

(defn state->qp
  "Given a hamiltonian state, returns a [[emmy.structure/up]] containing the
  coordinate and momentum components. "
  [s]
  {:pre [(H-state? s)]}
  (up (l/coordinate s)
      (momentum s)))

(defn qp->H-state-path [q p]
  (fn [t]
    (->H-state t (q t) (p t))))

(defn literal-Hamiltonian-state [n-dof]
  (->H-state
   (gensym 't)
   (s/literal-up (gensym 'x) n-dof)
   (s/literal-down (gensym 'p) n-dof)))

(defn L-state->H-state [L]
  (fn [Ls]
    (->H-state
     (l/time Ls)
     (l/coordinate Ls)
     (((partial 2) L) Ls))))

(defn H-state->L-state [H]
  (fn [Hs]
    (l/->L-state
     (l/time Hs)
     (l/coordinate Hs)
     (((partial 2) H) Hs))))

(defn H-state->matrix [s]
  (matrix/s->m
   (s/compatible-shape s) s 1))

(defn matrix->H-state [m s]
  {:pre [(= (matrix/num-cols m) 1)
         (odd? (matrix/num-rows m))
         (> (matrix/num-rows m) 2)]}
  (matrix/m->s
   (s/compatible-shape s) m 1))

(defn make-Hamiltonian [kinetic-energy potential-energy]
  (+ kinetic-energy potential-energy))

(defn Hamiltonian->state-derivative [H]
  (fn [H-state]
    (->H-state 1
               (((partial 2) H) H-state)
               (- (((partial 1) H) H-state)))))

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

(defn H-rectangular [m V]
  (fn [[_ q p]]
    (make-Hamiltonian
     (/ (g/square p) (* 2 m))
     (apply V q))))

(defn H-central [m V]
  (fn [[_ q p]]
    (make-Hamiltonian
     (/ (g/square p) (* 2 m))
     (V (g/abs q)))))

(defn H-central-polar [m V]
  (fn [[_ [r _] [p_r p_phi]]]
    (make-Hamiltonian
     (/ (+ (g/square p_r)
           (g/square (/ p_phi r)))
        (* 2 m))
     (V r))))

(defn H-harmonic [m k]
  (fn [[_ q p]]
    (+ (/ (g/square p) (* 2 m))
       (* (/ 1 2) k (g/square q)))))

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

(def ^{:dynamic true
       :doc "If true, the state passed to the fn returned
       by [[Legendre-transform]] is checked for correctness. If `false` errors
       may occur. See the code body for more detail.

       Defaults to `false`."}
  *validate-Legendre-transform?*
  false)

(defn ^:no-doc Legendre-transform-procedure
  "Note from GJS: This ugly version tests for correctness of the result."
  [F]
  (let [w-of-v  (D F)
        Dw-of-v (D w-of-v)]
    (letfn [(putative-G [w]
              (let [z (s/compatible-zero w)
                    M (Dw-of-v z)
                    b (w-of-v z)]
                (if (and *validate-Legendre-transform?*
                         (v/zero?
                          (g/simplify
                           (g/determinant M))))
                  (throw
                   (ex-info "Legendre Transform Failure: determinant = 0"
                            {:F F :w w}))
                  (let [v (g/solve-linear-left M (- w b))]
                    (- (* w v) (F v))))))]
      (let [Dpg (D putative-G)]
        (fn G [w]
          (if (and *validate-Legendre-transform?*
                   (let [thing (s/typical-object w)]
                     (not (v/= thing
                               (g/simplify
                                (w-of-v (Dpg thing)))))))
            (throw
             (ex-info "Legendre Transform Failure: not quadratic"
                      {:F F :w w}))
            (putative-G w)))))))

(def Legendre-transform
  (o/make-operator Legendre-transform-procedure
                   'Legendre-transform))

;; Notice that Lagrangians and Hamiltonians are symmetrical with
;; respect to the Legendre transform.

(defn ^:no-doc Lagrangian->Hamiltonian-procedure
  [Lagrangian]
  (fn [[t q p]]  ;; H-state
    (let [L #(Lagrangian (up t q %))]
      ((Legendre-transform L) p))))

(def Lagrangian->Hamiltonian
  (o/make-operator Lagrangian->Hamiltonian-procedure
                   'Lagrangian->Hamiltonian))

(defn ^:no-doc Hamiltonian->Lagrangian-procedure [Hamiltonian]
  (fn [[t q qdot]]
    (letfn [(H [p]
              (Hamiltonian
               (->H-state t q p)))]
      ((Legendre-transform-procedure H) qdot))))

(def Hamiltonian->Lagrangian
  (o/make-operator Hamiltonian->Lagrangian-procedure
                   'Hamiltonian->Lagrangian))

(defn Poisson-bracket [f g]
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
  (-> (fn [F]
        (+ ((partial 0) F)
           (Poisson-bracket F H)))
      (o/make-operator
       (list 'flow-derivative H))))

(defmethod g/Lie-derivative [::v/function] [f]
  (Lie-derivative f))

(defn Lie-transform
  "p. 428, the Lie transform is just the time-advance operator using the Lie
  derivative (see Hamiltonian.scm)."
  [H t]
  (-> (g/exp (* t (g/Lie-derivative H)))
      (o/make-operator
       `(~'Lie-transform ~H ~t))))

(defn flow-transform
  "The generalization of Lie-transform to include time dependence."
  [H delta-t]
  (-> (g/exp (* delta-t (flow-derivative H)))
      (o/make-operator
       `(~'flow-transform ~H ~delta-t))))

(defn standard-map [K]
  (let [pv (v/principal-value v/twopi)]
    (fn [theta I cont _fail]
      (let [nI (pv (core/+ I (core/* K (Math/sin theta))))]
        (cont
         (pv (core/+ theta nI))
         nI)))))

(defn standard-map-inverse [K]
  (let [pv (v/principal-value v/twopi)]
    (fn [theta I cont _fail]
      (let [ntheta (pv (core/- theta I))]
        (cont ntheta
              (pv (core/- I (core/*
                             K (Math/sin ntheta)))))))))

(defn iterated-map
  "f is a function of (x y continue fail), which calls continue with the values of
  x' y' that follow x y in the mapping.

  Returns a map of the same shape that iterates the iterated map n times before
  invoking the continuation, or invokes the fail continuation if the inner map
  fails."
  [f n]
  {:pre [(not (neg? n))]}
  (let [lulz (constantly nil)]
    (fn [x y continue fail]
      (loop [x x
             y y
             i n]
        (if (= i 0)
          (continue x y)
          (if-let [[x' y'] (f x y vector lulz)]
            (recur x' y' (dec i))
            (fail)))))))

;; ## Point Transformations
;;
;; Makes a canonical point transformation from a time-invariant coordinate
;; transformation T(q)

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

(def ^{:doc "Alias for [[F->CH]]."}
  F->CT F->CH)

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

;; ## Canonical Transformations

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
  (up 0 (nth DHs 2) (- (nth DHs 1))))

(defn T-func [s]
  (up 1
      (v/zero-like (l/coordinates s))
      (v/zero-like (momenta s))))

(defn canonical-H? [C H]
  (- (f/compose (D-phase-space H) C)
     (* (D C)
        (D-phase-space (f/compose H C)))))

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

;; Time-Varying code
;;
;; Originally from time-varying.scm.

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

;; One particularly useful canonical transform is the Poincare transform, which
;; is good for simplifying oscillators.

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

(defn two-particle-center-of-mass [m0 m1]
  (fn [[_ [x0 x1]]]
    (l/coordinate-tuple
     (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
     (- x1 x0))))

(defn two-particle-center-of-mass-canonical [m0 m1]
  (fn [[t [x0 x1] [p0 p1]]]
    (up t
        (l/coordinate-tuple
         (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
         (- x1 x0))
        (l/momentum-tuple
         (+ p0 p1)
         (/ (- (* m0 p1) (* m1 p0))
            (+ m0 m1))))))

(defn transpose-function [A]
  (fn [p]
    (* p A)))

(defn multiplicative-transpose [s]
  (fn [A]
    (linear-function->multiplier
     (transpose-function A) s)))

;; Symplectic
;;
;; Originally from symplectic.scm.

(defn symplectic-two-form [zeta1 zeta2]
  (- (* (momenta zeta2) (l/coordinates zeta1))
     (* (momenta zeta1) (l/coordinates zeta2))))

;; Without matrices

(defn canonical-transform? [C]
  (fn [s]
    (let [J ((D J-func) (s/compatible-shape s))
          DCs ((D C) s)
          DCsT (matrix/s:transpose DCs s)]
      (- J (* DCs J DCsT)))))

(defn J-matrix
  "n == degrees of freedom"
  [n]
  (let [twon+1 (inc (* 2 n))]
    (matrix/generate twon+1 twon+1
                     (fn [a b]
                       (cond (zero? a) 0
                             (zero? b) 0
                             (= (core/+ a n) b)  1
                             (= (core/+ b n) a) -1
                             :else 0)))))

(defn symplectic?
  "Symplectic test in terms of matrices"
  [C]
  (fn [s]
    (let [J (J-matrix (l/state->n-dof s))
          DC ((D-as-matrix C) s)]
      (- J (* DC J (g/transpose DC))))))

(defn symplectic-unit
  "p. 334 (used, but not defined there)"
  [n]
  (let [twoN (* 2 n)]
    (matrix/generate twoN twoN
                     (fn [a b]
                       (cond (= (+ a n) b) 1
                             (= (+ b n) a) -1
                             :else 0)))))

(defn symplectic-matrix?
  "p. 334"
  [M]
  (let [two-n (matrix/dimension M)]
    (when-not (even? two-n)
      (throw
       (ex-info "Wrong type -- symplectic-matrix?"
                {:M M})))
    (let [J (symplectic-unit (quot two-n 2))]
      (- J (* M J (g/transpose M))))))

(defn qp-submatrix [m]
  (matrix/without m 0 0))

(defn symplectic-transform?
  "p. 334"
  [C]
  (fn [s]
    (symplectic-matrix?
     (qp-submatrix
      ((D-as-matrix C) s)))))
