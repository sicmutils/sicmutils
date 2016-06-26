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

(ns sicmutils.mechanics.hamilton
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [sicmutils
             [generic :refer :all]
             [structure :refer :all]
             [operator :refer [make-operator]]
             [value :refer :all]
             [function :refer :all]]
            [sicmutils.calculus.derivative :refer :all]))

(defn momentum-tuple
  [& ps]
  (apply down ps))

(defn momentum
  "See coordinate: this returns the momentum element of a
  Hammilton state tuple (by convention, the element at index 2)."
  [local]
  (nth local 2))

(defn ->H-state
  [t q p]
  (up t q p))

(defn Hamiltonian->state-derivative
  [Hamiltonian]
  (fn [H-state]
    (->H-state 1
               (((∂ 2) Hamiltonian) H-state)
               (- (((∂ 1) Hamiltonian) H-state)))))

(def phase-space-derivative Hamiltonian->state-derivative)

(defn qp->H-state-path
  [q p]
  (fn [t]
    (->H-state t (q t) (p t))))

(defn Hamilton-equations
  [Hamiltonian]
  (fn [q p]
    (let [H-state-path (qp->H-state-path q p)]
      (- (D H-state-path)
         (compose (phase-space-derivative Hamiltonian)
                  H-state-path)))))

(defn H-rectangular
  [m V]
  (fn [[_ q p]]  ;; H-state
    (+ (/ (square p) (* 2 m))
       (apply V q))))

(defn dual-zero [z]
  (if (structure? z) (-> z transpose zero-like) 0))

(defn ^:private Legendre-transform-fn
  [F]
  (let [w-of-v (D F)]
    (fn [w]
      (let [z (dual-zero w)
            M ((D w-of-v) z)
            b (w-of-v z)
            v (/ (- w b) M)]
        (- (* w v) (F v))))))

(def Legendre-transform (make-operator Legendre-transform-fn "Legendre-transform"))

(defn ^:private Lagrangian->Hamiltonian-fn
  [Lagrangian]
  (fn [[t q p]]  ;; H-state
    (let [L #(Lagrangian (up t q %))]
      ((Legendre-transform L) p))))

(def Lagrangian->Hamiltonian (make-operator Lagrangian->Hamiltonian-fn "Lagrangian->Hamiltonian"))

(defn Poisson-bracket
  [f g]
  (fn [x]
    (let [fx (f x)
          gx (g x)]
      (if (or (structure? fx) (structure? gx))
        (mapr (fn [af]
                (mapr (fn [ag]
                        ((Poisson-bracket
                          (comp (apply component af) f)
                          (comp (apply component ag) g))
                         x))
                      (structure->access-chains gx)))
              (structure->access-chains fx))
        ((- (* ((∂ 1) f) ((∂ 2) g))
            (* ((∂ 2) f) ((∂ 1) g)))
         x)))))

(defn standard-map
  [K]
  (fn [theta I return failure]
    (let [nI (+ I (* K (sin theta)))]
      (return ((principal-value twopi) (+ theta nI))
              ((principal-value twopi) nI)))))

(defn iterated-map
  [f n]
  (let [lulz (constantly nil)]
    (fn [x y continue fail]
     (when (< n 0) (throw (IllegalArgumentException. "Cannot invert map")))
     (loop [x x
            y y
            i n]
       (if (= i 0)
         (continue x y)
         (let [step (f x y vector lulz)]
           (if step
             (recur (step 0) (step 1) (dec i))
             (fail))))))))
