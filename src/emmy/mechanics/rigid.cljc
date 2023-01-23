#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.rigid
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.calculus.derivative :refer [D]]
            [emmy.function :as f]
            [emmy.generic :as g :refer [sin cos + - * /]]
            [emmy.matrix :as matrix]
            [emmy.mechanics.lagrange :as L]
            [emmy.mechanics.rotation :as r]
            [emmy.quaternion :as q]
            [emmy.structure :as s :refer [up]]))

;; ## Chapter 2: generalized coordinates to angular velocities

(defn three-vector-components->antisymmetric [[x y z]]
  (matrix/by-rows
   [0 (- z) y]
   [z 0 (- x)]
   [(- y) x 0]))

(defn antisymmetric->column-matrix
  "Given an antisymmetric matrix `a` of dimension 3, returns a column vector of
  its positive components."
  [a]
  {:pre [(matrix/antisymmetric? a)]}
  (matrix/column
   (get-in a [2 1])
   (get-in a [0 2])
   (get-in a [1 0])))

;; Note from GJS: Suggested by Jack Wisdom on 30 Sept 2019.

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

;; ## Euler Angles

(defn Euler->omega [angles-path]
  (fn [t]
    (letfn [(M-on-path [t]
              (r/Euler->M (angles-path t)))
            (w-cross [t]
              (* ((D M-on-path) t)
                 (g/transpose (M-on-path t))))]
      (antisymmetric->column-matrix
       (w-cross t)))))

(defn Euler->omega-body [angles-path]
  (fn [t]
    (* (g/transpose (r/Euler->M (angles-path t)))
       ((Euler->omega angles-path) t))))

;; Assuming Euler angles rotate principal axes from reference orientation.
;;


(defn Euler-state->omega-body
  "Although this implementation appears to summarize `(M->omega-body r/Euler->M)`,
  it is actually essential to prevent intermediate expression explosion."
  [[_ [theta _ psi] [thetadot phidot psidot]]]
  (let [omega-a (+ (* (sin psi) (sin theta) phidot)
                   (* (cos psi) thetadot))
        omega-b (+ (* (cos psi) (sin theta) phidot)
                   (* -1 (sin psi) thetadot))
        omega-c (+ (* (cos theta) phidot)
                   psidot)]
    (up omega-a omega-b omega-c)))

(defn T-body-Euler [A B C]
  (fn [local]
    ((T-body A B C)
     (Euler-state->omega-body local))))

(def ^{:doc "Alias for [[T-body-Euler]]."}
  T-rigid-body T-body-Euler)

(defn L-body-Euler [A B C]
  (fn [local]
    ((L-body A B C)
     (Euler-state->omega-body local))))

(def ^{:doc "Alias for [[L-body-Euler]]."}
  Euler-state->L-body L-body-Euler)

(defn L-space-Euler [A B C]
  (fn [local]
    (let [angles (L/coordinate local)]
      (* ((L-body-Euler A B C) local)
         (g/transpose (r/Euler->M angles))))))

(def ^{:doc "Alias for [[L-space-Euler]]."}
  Euler-state->L-space L-space-Euler)

(defn rigid-sysder [A B C]
  (L/Lagrangian->state-derivative
   (T-rigid-body A B C)))

;; ## Quaternion representation

(defn quaternion-state->omega-body [[_ q qdot]]
  (let [two-q-norm (/ (* 2 q)
                      (g/dot-product q q))
        omega**a (g/dot-product two-q-norm (* q/I-matrix qdot))
        omega**b (g/dot-product two-q-norm (* q/J-matrix qdot))
        omega**c (g/dot-product two-q-norm (* q/K-matrix qdot))]
    (up omega**a omega**b omega**c)))

;; I'm not sure what these are in Quaternion land, so I'll leave them here as
;; private for now.

(let [q:a (matrix/by-rows
           [0 1 0 0]
           [-1 0 0 0]
           [0 0 0 1]
           [0 0 -1 0])
      q:b (matrix/by-rows
           [0 0 1 0]
           [0 0 0 -1]
           [-1 0 0 0]
           [0 1 0 0])
      q:c (matrix/by-rows
           [0 0 0 1]
           [0 0 1 0]
           [0 -1 0 0]
           [-1 0 0 0])]
  (defn quaternion-state->omega-space [[_ q qdot]]
    (let [Q     (matrix/up->column-matrix q)
          QdotT (matrix/row* qdot)
          two-m**2-inv (/ -2 (g/dot-product q q))
          omega**x (* two-m**2-inv (get-in (* QdotT q:a Q) [0 0]))
          omega**y (* two-m**2-inv (get-in (* QdotT q:b Q) [0 0]))
          omega**z (* two-m**2-inv (get-in (* QdotT q:c Q) [0 0]))]
      (up omega**x omega**y omega**z))))

(defn qw-state->L-body [A B C]
  (fn [[_ _ omega]]
    ((L-body A B C) omega)))

(defn qw-state->L-space [A B C]
  (let [state->body (qw-state->L-body A B C)]
    (fn [[_ q :as qw-state]]
      (let [Lbody (state->body qw-state)
            M     (q/->rotation-matrix (q/make q))]
        (* Lbody (g/transpose M))))))

(defn T-quaternion-state [A B C]
  (fn [[_ q qdot]]
    (let [Q        (matrix/up->column-matrix q)
          Qdot     (matrix/up->column-matrix qdot)
          m**2-inv (g/invert (get-in (* (matrix/transpose Q) Q) [0 0]))
          x        (* m**2-inv q/I-matrix Qdot)
          y        (* m**2-inv q/J-matrix Qdot)
          z        (* m**2-inv q/K-matrix Qdot)
          M        (* Q (matrix/transpose Q))]
      (* 2 (+ (* A (get-in (* (matrix/transpose x) M x) [0 0]))
              (* B (get-in (* (matrix/transpose y) M y) [0 0]))
              (* C (get-in (* (matrix/transpose z) M z) [0 0])))))))
