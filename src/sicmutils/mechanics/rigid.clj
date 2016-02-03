;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.mechanics.rigid
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [sicmutils
             [generic :refer :all]
             [structure :refer :all]
             [function :refer :all]]
            [sicmutils.calculus.derivative :refer :all]
            [sicmutils.mechanics
             [lagrange :refer :all]
             [rotation :refer :all]]))

(defn antisymmetric->column-matrix
  "Given an antisymmetric matrix-structure of dimension 3,
  return the column vector of its positive components."
  [a]
  ;; XXX: we should assert anstisymmetricity here
  (up (get-in a [1 2])
      (get-in a [2 0])
      (get-in a [0 1])))

(defn M-of-q->omega-of-t
  [M-of-q]
  (fn [q]
    (let [M-on-path (compose M-of-q q)]
      (fn [t]
        (let [omega-cross (fn [t]
                            (* ((D M-on-path) t)
                               (m:transpose (M-on-path t))))]
          (antisymmetric->column-matrix (omega-cross t)))))))

(defn M-of-q->omega-body-of-t
  [M-of-q]
  (fn [q]
    (fn [t]
      (* (m:transpose (M-of-q (q t)))
         (((M-of-q->omega-of-t M-of-q) q) t)))))

(defn M->omega
  [M-of-q]
  (-> M-of-q M-of-q->omega-of-t Γ-bar))

(defn M->omega-body
  [M-of-q]
  (-> M-of-q M-of-q->omega-body-of-t Γ-bar))

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
      (* 1/2
         (+ (* A (square ω0))
            (* B (square ω1))
            (* C (square ω2)))))))

(defn rigid-sysder
  [A B C]
  (Lagrangian->state-derivative (T-rigid-body A B C)))

(defn Euler-state->L-body
  [A B C]
  (fn [local]
    (let [[ω0 ω1 ω2] (Euler-state->omega-body local)]
      (up (* A ω0)
          (* B ω1)
          (* C ω2)))))

(defn Euler-state->L-space
  [A B C]
  (fn [[_ angles _ :as local]]
    (* (Euler->M angles)
       ((Euler-state->L-body A B C) local))))
