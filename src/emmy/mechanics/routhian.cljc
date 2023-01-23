#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.routhian
  (:refer-clojure :exclude [+ - * / partial])
  (:require [emmy.calculus.derivative :refer [D partial]]
            [emmy.generic :as g :refer [+ - *]]
            [emmy.matrix :as m]
            [emmy.mechanics.hamilton :as h]
            [emmy.mechanics.lagrange :as l]
            [emmy.structure :refer [up]]))

;; ## Routhian equations of motion

;; From the 2nd edition of SICM, p.233.

;; Assume a Lagrangian of the form L(t; x, y; vx, vy), where x and y may have
;; substructure.
;;
;; We perform a Legendre transform on vy to get the Routhian.
;;
;; The equations of motion are Hamilton's equations for the py, y and Lagrange's
;; equations for vx, x.

(defn Lagrangian->Routhian [Lagrangian]
  (fn [[t q [vx py]]]
    (letfn [(L [vy]
              (Lagrangian (up t q (up vx vy))))]
      ((h/Legendre-transform-procedure L) py))))

(defn Routh-equations [Routhian]
  (fn [x y py]
    (fn [t]
      (letfn [(L [[tau q v]]
                (Routhian
                 (up tau (up q (y tau)) (up v (py tau)))))
              (H [[tau q p]]
                (Routhian
                 (up tau (up (x tau) q) (up ((D x) tau) p))))]
        (up
         (((l/Lagrange-equations L) x) t)
         (((h/Hamilton-equations H) y py) t))))))

(defn Routhian->acceleration
  ([R]
   (fn [[_ _ [vx] :as s]]
     (let [minus-P ((partial 2 0) R)
           minus-F (((partial 1 0) R) s)
           vy      (((partial 2 1) R) s)
           pyd     ((* -1 ((partial 1 1) R)) s)]
       (* (m/s:inverse vx
                       (((partial 2 0) minus-P) s)
                       vx)
          (- minus-F
             (+ (((partial 0) minus-P) s)
                (* (((partial 1 0) minus-P) s) vx)
                (* (((partial 1 1) minus-P) s) vy)
                (* (((partial 2 1) minus-P) s) pyd)))))))
  ([R dissipation-fn]
   (fn [[t q [vx] :as s]]
     (let [minus-P ((partial 2 0) R)
           minus-F (((partial 1 0) R) s)
           vy      (((partial 2 1) R) s)
           L-state (up t q (up vx vy))
           minus-F0 (((partial 2 0) dissipation-fn) L-state)
           minus-F1 (((partial 2 1) dissipation-fn) L-state)
           pyd (- ((* -1 ((partial 1 1) R)) s)
                  minus-F1)]
       (* (m/s:inverse vx
                       (((partial 2 0) minus-P) s)
                       vx)
          (+ (- minus-F
                (+ (((partial 0) minus-P) s)
                   (* (((partial 1 0) minus-P) s) vx)
                   (* (((partial 1 1) minus-P) s) vy)
                   (* (((partial 2 1) minus-P) s) pyd)))
             minus-F0))))))


(defn Routhian->state-derivative
  ([R]
   (fn [[_ _ [vx] :as s]]
     (let [minus-P ((partial 2 0) R)
           minus-F (((partial 1 0) R) s)
           vy      (((partial 2 1) R) s)
           pyd     (- (((partial 1 1) R) s))]
       (up 1
           (up vx vy)
           (up
            (* (m/s:inverse vx (((partial 2 0) minus-P) s) vx)
               (- minus-F
                  (+ (((partial 0) minus-P) s)
                     (* (((partial 1 0) minus-P) s) vx)
                     (* (((partial 1 1) minus-P) s) vy)
                     (* (((partial 2 1) minus-P) s) pyd)
                     )))
            pyd)))))
  ([R dissipation-fn]
   (fn [[t q [vx] :as s]]
     (let [minus-P ((partial 2 0) R)
           minus-F (((partial 1 0) R) s)
           vy (((partial 2 1) R) s)
           L-state (up t q (up vx vy))
           minus-F0 (((partial 2 0) dissipation-fn) L-state)
           minus-F1 (((partial 2 1) dissipation-fn) L-state)
           pyd (- ((* -1 ((partial 1 1) R)) s) minus-F1)]
       (up 1
           (up vx vy)
           (up
            (* (m/s:inverse vx (((partial 2 0) minus-P) s) vx)
               (+ (- minus-F
                     (+ (((partial 0) minus-P) s)
                        (* (((partial 1 0) minus-P) s) vx)
                        (* (((partial 1 1) minus-P) s) vy)
                        (* (((partial 2 1) minus-P) s) pyd)
                        ))
                  minus-F0))
            pyd))))))

(defn Lagrangian-state->Routhian-state [L]
  (fn [[t q [vx] :as s]]
    (let [py (-> (((partial 2) L) s)
                 (nth 1))]
      (up t q (up vx py)))))

(defn Routhian-state->Lagrangian-state [R]
  (fn [[t q [vx] :as s]]
    (let [vy (-> (((partial 2) R) s)
                 (nth 1))]
      (up t q (up vx vy)))))
