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

(ns math.mechanics.hamilton
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :refer :all]
            [math.calculus.derivative :refer :all]
            [math.structure :refer :all]
            [math.function :refer :all]))

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
