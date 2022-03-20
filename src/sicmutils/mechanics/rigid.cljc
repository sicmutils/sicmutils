#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.rigid
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [sin cos + * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.mechanics.rotation :as r]
            [sicmutils.structure :refer [up]]))

(defn antisymmetric->column-matrix
  "Given an antisymmetric matrix-structure of dimension 3,
  return the column vector of its positive components."
  [a]
  ;; XXX: we should assert anstisymmetricity here
  (matrix/column (get-in a [2 1])
                 (get-in a [0 2])
                 (get-in a [1 0])))

(defn M-of-q->omega-of-t
  [M-of-q]
  (fn [q]
    (let [M-on-path (f/compose M-of-q q)]
      (fn [t]
        (let [omega-cross (fn [t]
                            (* ((D M-on-path) t)
                               (matrix/transpose (M-on-path t))))]
          (antisymmetric->column-matrix (omega-cross t)))))))

(defn M-of-q->omega-body-of-t
  [M-of-q]
  (fn [q]
    (fn [t]
      (* (matrix/transpose (M-of-q (q t)))
         (((M-of-q->omega-of-t M-of-q) q) t)))))

(defn M->omega
  [M-of-q]
  (-> M-of-q M-of-q->omega-of-t L/Gamma-bar))

(defn M->omega-body
  [M-of-q]
  (-> M-of-q M-of-q->omega-body-of-t L/Gamma-bar))

(defn Euler-state->omega-body
  [[_ [θ _ ψ] [θdot φdot ψdot]]]
  (let [ω-a (+ (* (sin ψ) (sin θ) φdot) (* (cos ψ) θdot))
        ω-b (+ (* (cos ψ) (sin θ) φdot) (* -1 (sin ψ) θdot))
        ω-c (+ (* (cos θ) φdot) ψdot)]
    (up ω-a ω-b ω-c)))

(defn T-rigid-body
  [A B C]
  (fn [local]
    (let [[ω0 ω1 ω2] (Euler-state->omega-body local)]
      (* (/ 1 2)
         (+ (* A (g/square ω0))
            (* B (g/square ω1))
            (* C (g/square ω2)))))))

(defn rigid-sysder
  [A B C]
  (L/Lagrangian->state-derivative (T-rigid-body A B C)))

(defn Euler-state->L-body
  [A B C]
  (fn [local]
    (let [[ω0 ω1 ω2] (Euler-state->omega-body local)]
      (matrix/column (* A ω0)
                     (* B ω1)
                     (* C ω2)))))

(defn Euler-state->L-space
  [A B C]
  (fn [[_ angles _ :as local]]
    (* (r/Euler->M angles)
       ((Euler-state->L-body A B C) local))))
