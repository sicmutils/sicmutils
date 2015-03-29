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

(ns math.mechanics.rigid
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :refer :all]
            [math.structure :refer :all]
            [math.calculus.derivative :refer :all]
            [math.mechanics.lagrange :refer :all]
            [math.mechanics.rotation :refer :all]
            [math.function :refer :all]))

(defn antisymmetric->column-matrix
  "Given an antisymmetric matrix-structure of dimension 3,
  return the column vector of its positive components."
  [a]
  ;; XXX: we should assert anstisymmetricity here
  (up (get-in a [2 1])
      (get-in a [0 2])
      (get-in a [1 0])))

(defn M-of-q->omega-of-t
  [M-of-q]
  (fn [q]
    (let [M-on-path (compose M-of-q q)]
      (fn [t]
        (let [omega-cross (fn [t]
                            (* ((D M-on-path) t)
                               (transpose (M-on-path t))))]
          (antisymmetric->column-matrix (omega-cross t)))))))

(defn M-of-q->omega-body-of-t
  [M-of-q]
  (fn [q]
    (fn [t]
      (* (transpose (M-of-q (q t)))
         (((M-of-q->omega-of-t M-of-q) q) t)))))

(defn M->omega
  [M-of-q]
  (-> M-of-q M-of-q->omega-of-t Γ-bar))

(defn M->omega-body
  [M-of-q]
  (-> M-of-q M-of-q->omega-body-of-t Γ-bar))
