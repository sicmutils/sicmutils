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

(ns net.littleredcomputer.math.mechanics.lagrange
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [net.littleredcomputer.math
             [generic :refer :all]
             [structure :refer :all]
             [function :refer :all]]
            [net.littleredcomputer.math.numerical.integrate :refer :all]
            [net.littleredcomputer.math.calculus.derivative :refer :all]))

(defn coordinate
  "A convenience function on local tuples. A local tuple describes
  the state of a system at a particular time: [t, q, D q, D^2 q]
  representing time, position, velocity (and optionally acceleration
  etc.) Returns the q element, which is expected to be a mapping
  from time to a structure of coordinates"
  [local]
  (nth local 1))

(defn velocity
  "See coordinate: this returns the velocity element of a local
  tuple (by convention, the 2nd element)."
  [local]
  (nth local 2))

(defn L-free-particle
  "The lagrangian of a free particle of mass m. The Lagrangian
  returned is a function of the local tuple. Since the particle
  is free, there is no potential energy, so the Lagrangian is
  just the kinetic energy."
  [mass]
  (fn [[_ _ v]]
    (* 1/2 mass (square v))))

(defn L-harmonic
  "The Lagrangian of a simple harmonic oscillator (mass-spring
  system). m is the mass and k is the spring constant used in
  Hooke's law. The resulting Lagrangian is a function of the
  local tuple of the system."
  [m k]
  (fn [[_ q v]]
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

(defn L-uniform-acceleration
  "The Lagrangian of an object experiencing uniform acceleration
  in the negative y direction, i.e. the acceleration due to gravity"
  [m g]
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

(defn Γ
  ([q]
   (let [Dq (D q)]
     (with-meta
       (fn [t]
         (up t (q t) (Dq t)))
       {:arity 1})))
  ([q n]
   (let [Dqs (->> q (iterate D) (take (- n 1)))]
     (fn [t]
       (->> Dqs (map #(% t)) (cons t) (apply up))))))

(defn Lagrangian-action
  [L q t1 t2]
  (integrate (compose L (Γ q)) t1 t2 {:compile true}))

(defn Lagrange-equations
  [Lagrangian]
  (fn [q]
    (- (D (compose ((pd 2) Lagrangian) (Γ q)))
       (compose ((pd 1) Lagrangian) (Γ q)))))

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
    (with-meta
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
                                      :else (- xi (nth xs j))))))))))
      {:arity 1})))

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

(defn qv->state-path
  [q v]
  #(up % (q %) (v %)))

(defn Lagrange-equations-first-order
  [L]
  (fn [q v]
    (let [state-path (qv->state-path q v)]
      (- (D state-path)
         (compose (Lagrangian->state-derivative L)
               state-path)))))

(defn Lagrangian->energy
  [L]
  (let [P ((pd 2) L)]
    (- (* P velocity) L)))

(defn osculating-path
  [state0]
  (let [[t0 q0] state0
        k (count state0)]
    (fn [t]
      (let [dt (- t t0)]
        (loop [n 2 sum q0 dt-n:n! dt]
          (if (= n k)
            sum
            (recur (inc n)
              (+ sum (* (nth state0 n) dt-n:n!))
              (/ (* dt-n:n! dt) n))))))))

(defn Γ-bar
  [f]
  (fn [local]
    ((f (osculating-path local)) (first local))))

(defn Dt
  [F]
  (let [G-bar (fn [q]
                (D (compose F (Γ q))))]
    (Γ-bar G-bar)))

(defn Euler-Lagrange-operator
  [L]
  (- (Dt ((pd 2) L)) ((pd 1) L)))

(defn L-rectangular
  "Lagrangian for a point mass on with the potential energy V(x, y)"
  [m V]
  (fn [[_ [q0 q1] qdot]]  ;; local
    (- (* 1/2 m (square qdot))
       (V q0 q1))))
