;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [sin cos + - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :refer [make-operator]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

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
               (((partial 2) Hamiltonian) H-state)
               (- (((partial 1) Hamiltonian) H-state)))))

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
         (f/compose (phase-space-derivative Hamiltonian)
                    H-state-path)))))

(defn H-rectangular
  [m V]
  (fn [[_ q p]]  ;; H-state
    (+ (/ (g/square p) (* 2 m))
       (apply V q))))

(defn dual-zero [z]
  (if (s/structure? z) (-> z g/transpose v/zero-like) 0))

(defn ^:private Legendre-transform-fn
  [F]
  (let [w-of-v (D F)]
    (fn [w]
      (let [z (dual-zero w)
            M ((D w-of-v) z)
            b (w-of-v z)
            v (/ (- w b) M)]
        (- (* w v) (F v))))))

(def Legendre-transform
  (make-operator Legendre-transform-fn 'Legendre-transform))

(defn ^:private Lagrangian->Hamiltonian-fn
  [Lagrangian]
  (fn [[t q p]]  ;; H-state
    (let [L #(Lagrangian (up t q %))]
      ((Legendre-transform L) p))))

(def Lagrangian->Hamiltonian
  (make-operator Lagrangian->Hamiltonian-fn 'Lagrangian->Hamiltonian))

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

(defn standard-map
  [K]
  (fn [theta I return _]
    (let [nI (+ I (* K (sin theta)))]
      (return ((v/principal-value v/twopi) (+ theta nI))
              ((v/principal-value v/twopi) nI)))))

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

(defn F->CT
  "A transformation of configuration coordinates F to a procedure
  implementing a transformation of phase-space coordinates (p. 320)"
  [F]
  (fn [[t _ p :as H-state]]
    (up t
        (F H-state)
        (* p (g/invert (((partial 1) F) H-state))))))

(defn H-central
  [m V]
  (fn [[_ q p]]
    (+  (/ (g/square p) (* 2 m))
        (V (g/abs q)))))

;; page numbers here are references to the PDF; probably
;; do not correspond to 1ed.

(defn canonical?
  "p.324"
  [C H Hprime]
  (- (f/compose (phase-space-derivative H) C)
     (* (D C) (phase-space-derivative Hprime))))

(defn compositional-canonical?
  "p.324"
  [C H]
  (canonical? C H (f/compose H C)))

(defn time-independent-canonical?
  "p.326"
  [C]
  (let [J-func #(up 0 (nth % 2) (- (nth % 1)))
        Phi (fn [A] #(* A %))
        Phi* (fn [A] #(* % A))]
    (fn [s]
      ((- J-func
          (f/compose (Phi ((D C) s))
                     J-func
                     (Phi* ((D C) s))))
       (s/compatible-shape s)))))

(defn polar-canonical
  "p.327"
  [alpha]
  (fn [[t theta I]]
    (let [x (* (g/sqrt (/ (* 2 I) alpha)) (sin theta))
          p_x (* (g/sqrt (* 2 alpha I)) (cos theta))]
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

(defn qp-submatrix [m]
  (matrix/without m 0 0))

(defn symplectic-transform?
  "p. 334"
  [C]
  (fn [s]
    (symplectic-matrix?
     (qp-submatrix
      (matrix/s->m (s/compatible-shape s)
                   ((D C) s)
                   s)))))

(defn ^:private Hamiltonian-Lie-derivative
  "p. 428"
  [H]
  (make-operator
   #(Poisson-bracket % H)
   `(~'Lie-derivative ~H)))

(defmethod g/Lie-derivative [::v/function] [f] (Hamiltonian-Lie-derivative f))

(defn Lie-transform
  "p. 428"
  [H t]
  (make-operator
   (g/exp (* t (g/Lie-derivative H)))
   `(~'Lie-transform ~H ~t)))

(defn H-central-polar
  [m V]
  (fn [[_ [r _] [p_r p_phi]]]
    (+ (/ (+ (g/square p_r)
             (g/square (/ p_phi r)))
          (* 2 m))
       (V r))))

(defn Hamiltonian
  "Return SICM-style function signature for a Hamiltonian with n
  degrees of freedom (or 1 if n is not given). Useful for constructing
  Hamiltonian literal functions."
  [& n]
  (if (nil? n)
    '(-> (UP Real (UP* Real) (DOWN* Real)) Real)
    `(~'-> (~'UP ~'Real (~'UP* ~'Real ~@n) (~'DOWN* ~'Real ~@n)) ~'Real)))
