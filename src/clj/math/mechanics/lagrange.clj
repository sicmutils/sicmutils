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

(ns math.mechanics.lagrange
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :refer :all]
            [math.calculus.derivative :refer :all]
            [math.structure :refer :all]
            [math.numerical.integrate :refer :all]))

(defn velocity [local] (nth local 2))

(defn L-free-particle [mass]
  (fn [[_ _ v]]
    (* 1/2 mass (square v))))

(defn L-harmonic [m k]
  (fn [[_ q v]]
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

(defn L-uniform-acceleration [m g]
  (fn [[_ [_ y] v]]
    (- (* 1/2 m (square v)) (* m g y))))

(defn L-central-rectangular [m U]
  (fn [[_ q v]]
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(defn L-central-polar [m U]
  (fn [[_ [r] [rdot φdot]]]
    (- (* 1/2 m
          (+ (square rdot)
             (square (* r φdot))))
       (U r))))

(defn δ
  "The variation operator (p. 28)."
  [η]
  (fn [f]
    ;; Define g(ε) as in Eq. 1.22; then δ_η f[q] = Dg(0)
    (fn [q]
      (let [g (fn [ε]
                (f (+ q (* ε η))))]

        ((D g) 0)))))

(def ->local up)

(defn F->C [F]
  (fn [[t _ v :as local]]
    (->local t
             (F local)
             (+ (((pd 0) F) local)
                (* (((pd 1) F) local) v)))))

(defn p->r [[_ [r φ]]]
  (up (* r (cos φ)) (* r (sin φ))))

;; XXX: GJS allows for a gamma procedure that contains higher
;; derivatives

(defn Γ
  [q]
  (let [Dq (D q)]
    (fn [t]
      (up t (q t) (Dq t)))))

(defn Lagrangian-action
  [L q t1 t2]
  (integrate (comp L (Γ q)) t1 t2))

(defn linear-interpolants
  [x0 x1 n]
  (let [n+1 (inc n)
        dx (/ (- x1 x0) n+1)]
    (for [i (range 1 n+1)]
      (+ x0 (* i dx)))))

(defn Lagrange-interpolation-function
  [ys xs]
  (let [n (count ys)]
    (assert (= (count xs) n))
    (fn [x]
      (reduce + 0
              (for [i (range n)]
                (/ (reduce * 1
                           (for [j (range n)]
                             (if (= j i)
                               (nth ys i)
                               (- x (nth xs j)))))
                   (let [xi (nth xs i)]
                     (reduce * 1
                             (for [j (range n)]
                               (cond (< j i) (- (nth xs j) xi)
                                     (= j i) (if (odd? i) -1 1)
                                     :else (- xi (nth xs j))))))))))))

(defn Lagrangian->acceleration
  [L]
  (let [P ((pd 2) L)
        F ((pd 1) L)]
    (/ (- F
          (+ ((pd 0) P)
             (* ((pd 1) P) velocity)))
       ((pd 2) P))))

(defn Lagrangian->state-derivative
  [L]
  (let [acceleration (Lagrangian->acceleration L)]
    (fn [[_ _ v :as state]]
      (up 1 v (acceleration state)))))


(defn Lagrange-equations
  [Lagrangian]
    (fn [q]
      (- (D (comp ((pd 2) Lagrangian) (Γ q)))
         (comp ((pd 1) Lagrangian) (Γ q)))))
