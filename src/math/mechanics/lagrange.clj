(ns math.mechanics.lagrange
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :refer :all]
            [math.structure :refer :all]
            [math.numerical.integrate :refer :all]))

(defn velocity [local] (nth local 2))

(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* 1/2 mass (square v)))))

;; XXX: GJS allows for a gamma procedure that contains higher
;; derivatives

(defn Γ
  [q]
  (let [dq (D q)]
    (fn [t]
      (up t (q t) (dq t)))))

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


(defn Lagrange-equations
  [Lagrangian]
    (fn [q]
      (- (D (comp ((pd 2) Lagrangian) (Γ q)))
         (comp ((pd 1) Lagrangian) (Γ q)))))
