;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns net.littleredcomputer.math.mechanics.hamilton
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [net.littleredcomputer.math
             [generic :refer :all]
             [structure :refer :all]
             [value :refer :all]
             [function :refer :all]]
            [net.littleredcomputer.math.calculus.derivative :refer :all]))

(defn phase-space-derivative
  [Hamiltonian]
  (fn [H-state]
    (up 1
        (((pd 2) Hamiltonian) H-state)
        (- (((pd 1) Hamiltonian) H-state)))))

(defn qp->H-state-path
  [q p]
  (fn [t]
    (up t (q t) (p t))))

(defn Hamilton-equations
  [Hamiltonian]
  (fn [q p]
    (let [H-state-path (qp->H-state-path q p)]
      (- (D H-state-path)
         (compose (phase-space-derivative Hamiltonian)
                  H-state-path)))))

(defn H-rectangular
  [m V]
  (fn [[_ [q0 q1] p]]  ;; H-state
    (+ (/ (square p) (* 2 m))
       (V q0 q1))))

(defn dual-zero [z]
  (if (structure? z) (-> z transpose zero-like) 0))

(defn Legendre-transform
  [F]
  (let [w-of-v (D F)]
    (fn [w]
      (let [z (dual-zero w)
            M ((D w-of-v) z)
            b (w-of-v z)
            v (/ (- w b) M)]
        (- (* w v) (F v))))))

(defn Lagrangian->Hamiltonian
  [Lagrangian]
  (fn [[t q p]]  ;; H-state
    (let [L #(Lagrangian (up t q %))]
      ((Legendre-transform L) p))))
