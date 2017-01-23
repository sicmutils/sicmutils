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
             [value :as v] ;; XXX
             [generic :refer :all]
             [structure :refer :all]
             [operator :refer [make-operator]]
             [value :refer :all]
             [matrix :as matrix]
             [function :refer :all]]
            [sicmutils.calculus.derivative :refer :all]))

(defn momentum-tuple
  [& ps]
  (apply down ps))

(defn momentum
  "See coordinate: this returns the momentum element of a
  Hammilton state tuple (by convention, the element at index 2)."
  [H-state]
  (nth H-state 2))

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
      (println (str "Poisson-bracket\n  f " f "\n  g " g "\n  x " x "\n  fx " fx "\n  gx " gx))
      (let [P (if (or (structure? fx) (structure? gx))
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
                 x))]
        (println (str "  yields: " P))
        P))))

(defn standard-map
  [K]
  (fn [theta I return failure]
    (let [nI (+ I (* K (sin theta)))]
      (return ((principal-value twopi) (+ theta nI))
              ((principal-value twopi) nI)))))

(defn iterated-map
  "f is a function of (x y continue fail), which calls continue with
  the values of x' y' that follow x y in the mapping. Returns a map of
  the same shape that iterates the iterated map n times before
  invoking the continuation, or invokes the fail continuation if the
  inner map fails."
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

(defn F->CT
  "A transformation of configuration coordinates F to a procedure
  implementing a transformation of phase-space coordinates (p. 320)"
  [F]
  (fn [[t q p :as H-state]]
    (up t
        (F H-state)
        (* p (invert (((∂ 1) F) H-state))))))

(defn H-central
  [m V]
  (fn [[t x p :as H-state]]
    (+  (/ (square p) (* 2 m))
        (V (sqrt (square x))))))

;; page numbers here are references to the PDF; probably
;; do not correspond to 1ed.

(defn canonical?
  "p.324"
  [C H Hprime]
  (- (compose (phase-space-derivative H) C)
     (* (D C) (phase-space-derivative Hprime))))

(defn compositional-canonical?
  "p.324"
  [C H]
  (canonical? C H (compose H C)))

(defn time-independent-canonical?
  "p.326"
  [C]
  (let [J-func #(up 0 (nth % 2) (- (nth % 1)))
        Phi (fn [A] #(* A %))
        Phi* (fn [A] #(* % A))]
    (fn [s]
      ((- J-func
          (compose (Phi ((D C) s))
                   J-func
                   (Phi* ((D C) s))))
       (compatible-shape s)))))

(defn polar-canonical
  "p.327"
  [alpha]
  (fn [[t theta I]]
    (let [x (* (sqrt (/ (* 2 I) alpha)) (sin theta))
          p_x (* (sqrt (* 2 alpha I)) (cos theta))]
      (up t x p_x))))

(defn symplectic-unit
  "p. 334 (used, but not defined there)"
  [n]
  (let [twoN (* 2 n)]
    (matrix/generate twoN twoN (fn [a b]
                                 (cond (= (+ a n) b) 1
                                       (= (+ b n) a) -1
                                       :else 0)))))

(defn symplectic-matrix?
  "p. 334"
  [M]
  (let [twoN (matrix/dimension M)
        J (symplectic-unit (quot twoN 2))]
    (- J (* M J (matrix/transpose M)))))

(defn symplectic-transform?
  "p. 334"
  [C]
  (fn [s]
    (symplectic-matrix?
     (matrix/without
      (matrix/s->m (compatible-shape s)
                   ((D C) s)
                   s) 0 0))))

(defn Lie-derivative
  "p. 428"
  [H]
  (make-operator
   #(Poisson-bracket % H)
   `(~'Lie-derivative ~H)))

(defn Lie-transform
  "p. 428"
  [H t]
  (make-operator
   (exp (* t (Lie-derivative H)))
   `(~'Lie-transform ~H ~t)))

(defn H-central-polar
  [m V]
  (fn [[_ [r phi] [pr pphi]]]
    (+ (/ (+ (square pr)
             (square (/ pphi r)))
          (* 2 m))
       (V r))))
