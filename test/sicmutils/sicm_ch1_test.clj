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

(ns sicmutils.sicm-ch1-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :refer :all]
             [structure :refer :all]
             [numbers]
             [numsymb]
             [simplify]
             [function :refer :all]
             [operator :refer :all]
             [value :as v]]
            [sicmutils.numerical
             [ode :refer :all]
             [integrate :refer :all]
             [minimize :refer :all]]
            [sicmutils.calculus.derivative :refer :all]
            [sicmutils.mechanics
             [lagrange :refer :all]
             [rotation :refer :all]]))

(def ^:private near (v/within 1e-6))

(deftest ^:long section-1.4
  (with-literal-functions [x y z]
    (let [q (up x y z)
          test-path (fn [t]
                      (up (+ (* 4 t) 7)
                          (+ (* 3 t) 5)
                          (+ (* 2 t) 1)))
          make-η (fn [ν t1 t2]
                   (fn [t]
                     (* (- t t1) (- t t2) (ν t))))
          varied-free-particle-action (fn [mass q ν t1 t2]
                                        (fn [ε]
                                          (let [η (make-η ν t1 t2)]
                                            (Lagrangian-action (L-free-particle mass)
                                                               (+ q (* ε η)) t1 t2))))]
      ;; p. 18
      (is (= '(up (x t)
                  (y t)
                  (z t))
             (simplify (q 't))))
      (is (= '(up ((D x) t)
                  ((D y) t)
                  ((D z) t))
             (simplify ((D q) 't))))
      (is (= '(up (((expt D 2) x) t)
                  (((expt D 2) y) t)
                  (((expt D 2) z) t))
             (simplify (((expt D 2) q) 't))))
      (is (= '(up (((expt D 2) x) t)
                  (((expt D 2) y) t)
                  (((expt D 2) z) t))
             (simplify ((D (D q)) 't))))
      (is (= '(up t
                  (up (x t) (y t) (z t))
                  (up ((D x) t) ((D y) t) ((D z) t)))
             (simplify ((Γ q) 't))))
      (is (= '(/
               (+
                (* (expt ((D x) t) 2) m)
                (* (expt ((D y) t) 2) m)
                (* (expt ((D z) t) 2) m))
               2)
             (simplify ((compose (L-free-particle 'm) (Γ q)) 't))))
      ;; at this point in the text we should be able to show-expression
      ;; in TeX form XXX.
      ;; p. 20
      (is (= 435.0 (Lagrangian-action (L-free-particle 3.0) test-path 0.0 10.0)))
      (let [m (minimize (varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) -2.0 1.0)]
        (is (near 0.0 (first m)))
        (is (near 435 (second m))))
      (is (near 436.2912143 ((varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) 0.001)))
      ;; This is fairly time consuming since every evaluation of a candidate point in the
      ;; multidimensional minimization of find-path involves computing a numeric integration
      ;; to find the Lagrangian of the path induced by the point. But it works.
      (let [values (atom [])
            minimal-path (find-path (L-harmonic 1.0 1.0) 0. 1. (/ Math/PI 2) 0. 3
                                    :observe (fn [pt _] (swap! values conj pt)))
            good? (partial (v/within 2e-4) 0)
            errors (for [x (range 0.0 (/ Math/PI 2) 0.02)]
                     (abs (- (Math/cos x) (minimal-path x))))]
        ;; the minimization is supposed to discover the cosine function in the interval [0..pi/2].
        ;; Check that it has done so over a variety of points to within 2e-4.
        ;; (prn values)
        (is ((v/within 1e-4) 1 (minimal-path 0)))
        (is ((v/within 1e-5) 0 (minimal-path (/ Math/PI 2))))
        (is (every? good? errors))))))

(deftest section-1.5
  (with-literal-functions [x f g q η φ]
    (let [F (fn [q] (fn [t] (f (q t))))
          G (fn [q] (fn [t] (g (q t))))
          δ_η (δ η)
          φ (fn [f] (fn [q] (fn [t] (φ ((f q) t)))))
          test-path (fn [t] (up (+ 'a0 (* 'a t))
                                (+ 'b0 (* 'b t))
                                (+ 'c0 (* 'c t))))
          proposed-solution (fn [t] (* 'a (cos (+ (* 'ω t) 'φ))))]
      ;; p. 29
      (is (= '(η t) (simplify (((δ_η identity) q) 't))))
      (is (= '(* ((D f) (q t)) (η t)) (simplify (((δ_η F) q) 't))))
      (is (= '(* ((D g) (q t)) (η t)) (simplify (((δ_η G) q) 't))))
      (is (= '(+ (* ((D f) (q t)) (η t) (g (q t))) (* (η t) (f (q t)) ((D g) (q t))))
             (simplify (((δ_η (* F G)) q) 't))))
      (is (= '(+ (* ((D f) (q t)) (η t) (g (q t))) (* (η t) (f (q t)) ((D g) (q t))))
             (simplify (((δ_η (* F G)) q) 't))))
      (is (= '(* ((D φ) (f (q t))) ((D f) (q t)) (η t)) (simplify (((δ_η (φ F)) q) 't))))
      ;; p. 35
      (is (= (down 0 0 0) (((Lagrange-equations (L-free-particle 'm)) test-path) 't)))
      (is (= '(* (((expt D 2) x) t) m)
             (simplify (((Lagrange-equations (L-free-particle 'm)) x) 't))))
      (is (= '(+ (* -1 (cos (+ (* t ω) φ)) a m (expt ω 2)) (* (cos (+ (* t ω) φ)) a k))
             (simplify (((Lagrange-equations (L-harmonic 'm 'k)) proposed-solution) 't)))))))

(defn- T-pend
  [m l _ ys]
  (fn [local]
    (let [[t θ θdot] local
          vys (D ys)]
      (* 1/2 m
         (+ (square (* l θdot))
            (square (vys t))
            (* 2 l (vys t) θdot (sin θ)))))))

(defn- V-pend
  [m l g ys]
  (fn [local]
    (let [[t θ _] local]
      (* m g (- (ys t) (* l (cos θ)))))))

(def ^:private L-pend (- T-pend V-pend))

(deftest section-1.6
  (with-literal-functions [x y r θ φ U y_s]
    (let [L-alternate-central-polar (fn [m U]
                                      (compose (L-central-rectangular m U)
                                               (F->C p->r)))]
      (is (= '(down (* (((expt D 2) x) t) m)
                    (+ (* (((expt D 2) y) t) m) (* g m)))
             (simplify (((Lagrange-equations (L-uniform-acceleration 'm 'g))
                         (up x y)) 't))))
      (is (= '(down (/ (+ (* (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) x) t) m)
                          (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) y) t) m)
                          (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
             (simplify (((Lagrange-equations (L-central-rectangular 'm U))
                         (up x y))
                        't))))
      (is (= '(down
               (+ (* -1 (r t) (expt ((D φ) t) 2) m)
                  (* (((expt D 2) r) t) m) ((D U) (r t)))
               (+ (* (expt (r t) 2) (((expt D 2) φ) t) m)
                  (* 2 (r t) ((D r) t) ((D φ) t) m)))
             (simplify (((Lagrange-equations (L-central-polar 'm U))
                         (up r φ))
                        't))))
      (is (= '(up
               (+ (* -1 (sin φ) r φdot) (* (cos φ) rdot))
               (+ (* (cos φ) r φdot) (* (sin φ) rdot)))
             (simplify (velocity ((F->C p->r)
                                  (->local 't (up 'r 'φ) (up 'rdot 'φdot)))))))
      (is (= '(/ (+ (* m (expt r 2) (expt φdot 2))
                    (* m (expt rdot 2)) (* -2N (U r)))
                 2)
             (simplify ((L-alternate-central-polar 'm U)
                        (->local 't (up 'r 'φ) (up 'rdot 'φdot))))))
      (is (= '(down (+ (* -1 (r t) (expt ((D φ) t) 2) m)
                       (* (((expt D 2) r) t) m)
                       ((D U) (r t)))
                    (+ (* (expt (r t) 2) (((expt D 2) φ) t) m)
                       (* 2 (r t) ((D r) t) ((D φ) t) m)))
             (simplify (((Lagrange-equations (L-alternate-central-polar 'm U))
                         (up r φ))
                        't))))
      (is (= '(+ (* (((expt D 2) θ) t) (expt l 2) m)
                 (* (((expt D 2) y_s) t) (sin (θ t)) l m)
                 (* (sin (θ t)) g l m))
             (simplify (((Lagrange-equations (L-pend 'm 'l 'g y_s)) θ) 't))))
      ;; p. 61
      (let [Lf (fn [m g]
                 (fn [[_ [_ y] v]]
                   (- (* 1/2 m (square v)) (* m g y))))
            dp-coordinates (fn [l y_s]
                             (fn [[t θ]]
                               (let [x (* l (sin θ))
                                     y (- (y_s t) (* l (cos θ)))]
                                 (up x y))))
            L-pend2 (fn [m l g y_s]
                      (compose (Lf m g)
                               (F->C (dp-coordinates l y_s))))]
        (is (= '(/ (+ (* 2 (sin θ) ((D y_s) t) l m θdot)
                      (* (expt l 2) m (expt θdot 2))
                      (* 2 (cos θ) g l m)
                      (* (expt ((D y_s) t) 2) m)
                      (* -2 (y_s t) g m))
                   2)
               (simplify ((L-pend2 'm 'l 'g y_s) (->local 't 'θ 'θdot)))))))))

(deftest ^:long section-1.7-1
  (with-literal-functions [x y v_x v_y]
    (let [harmonic-state-derivative (fn [m k]
                                      (Lagrangian->state-derivative (L-harmonic m k)))]
      (is (= '(up 1
                  (up v_x v_y)
                  (up (/ (* -1 k x) m) (/ (* -1 k y) m)))
             (simplify ((harmonic-state-derivative 'm 'k)
                        (up 't (up 'x 'y) (up 'v_x 'v_y))))))
      ;; p. 71
      (is (= '(up
               0
               (up (+ ((D x) t) (* -1 (v_x t))) (+ ((D y) t) (* -1 (v_y t))))
               (up
                (/ (+ (* ((D v_x) t) m) (* (x t) k)) m)
                (/ (+ (* ((D v_y) t) m) (* (y t) k)) m)))
             (simplify (((Lagrange-equations-first-order (L-harmonic 'm 'k))
                         (up x y)
                         (up v_x v_y))
                        't))))
      (is (= (up 1 (up 3.0 4.0) (up -0.5 -1.0))
             ((harmonic-state-derivative 2. 1.) (up 0 (up 1. 2.) (up 3. 4.)))))
      (is (= '(1 3.0 4.0 -0.5 -1.0)
             (flatten ((harmonic-state-derivative 2. 1.)
                       (up 0 (up 1. 2.) (up 3. 4.))))))
      ;; p. 72
      (dotimes [_ 1]  ;; this is just here in case we want to watch in the profiler
        (let [answer ((state-advancer harmonic-state-derivative 2. 1.)
                      (up 0. (up 1. 2.) (up 3. 4.))
                      10.
                      1e-12
                      :compile true)
              expected (up 10.
                           (up 3.71279166 5.42062082)
                           (up 1.61480309 1.81891037))
              delta (->> answer (- expected) flatten (map abs) (reduce max))]
          (is (< delta 1e-8)))))))

(deftest section-1.7-2
  (let [periodic-drive (fn [amplitude frequency phase]
                         (fn [t]
                           (* amplitude (cos (+ (* frequency t) phase)))))
        L-periodically-driven-pendulum (fn [m l g a ω]
                                         (let [ys (periodic-drive a ω 0)]
                                           (L-pend m l g ys)))
        pend-state-derivative (fn [m l g a ω]
                                (Lagrangian->state-derivative
                                 (L-periodically-driven-pendulum m l g a ω)))]
    (is (= '(+ (* -1 (cos (* t ω)) (sin (θ t)) a l m (expt ω 2))
               (* (sin (θ t)) g l m)
               (* (((expt D 2) θ) t) (expt l 2) m))
           (simplify (((Lagrange-equations
                        (L-periodically-driven-pendulum 'm 'l 'g 'a 'ω))
                       (literal-function 'θ))
                      't))))
    ;; NB. fraction simplification not happening here
    (is (= '(up 1
                θdot
                (/ (+ (* (sin θ) (cos (* t ω)) a (expt ω 2)) (* -1 (sin θ) g))
                   l))
           (simplify ((pend-state-derivative 'm 'l 'g 'a 'ω)
                      (up 't 'θ 'θdot)))))
    (let [answer ((evolve pend-state-derivative
                          1.0
                          1.0
                          9.8
                          0.1
                          (* 2.0 (sqrt 9.8)))
                  (up 0.0
                      1.
                      0.)
                  (constantly nil)
                  0.01
                  1.0
                  1.0e-13
                  :compile true)
          expected (up 1.0 -1.030115687 -1.40985359)
          delta (->> answer (- expected) flatten (map abs) (reduce max))]
      (is (< delta 1e-8)))))

(deftest section-1.8
  (with-literal-functions [U V]
    (let [spherical-state (up 't
                              (up 'r 'θ 'φ)
                              (up 'rdot 'θdot 'φdot))
          T3-spherical (fn [m]
                         (fn [[_ [r θ _] [rdot θdot φdot]]]
                           (* 1/2 m (+ (square rdot)
                                       (square (* r θdot))
                                       (square (* r (sin θ) φdot))))))
          L3-central (fn [m Vr]
                       (let [Vs (fn [[_ [r]]] (Vr r))]
                         (- (T3-spherical m) Vs)))
          ang-mom-z (fn [m]
                      (fn  [[_ q v]]
                        (nth (cross-product q (* m v)) 2)))]
      ;; p. 81
      (is (= '(down (+ (* (expt (sin θ) 2) m r (expt φdot 2))
                       (* m r (expt θdot 2))
                       (* -1 ((D V) r)))
                    (* (cos θ) (sin θ) m (expt r 2) (expt φdot 2))
                    0)
             (simplify (((partial 1) (L3-central 'm V)) spherical-state))))
      (is (= '(down (* m rdot)
                    (* m (expt r 2) θdot)
                    (* (expt (sin θ) 2) m (expt r 2) φdot))
             (simplify (((partial 2) (L3-central 'm V)) spherical-state))))
      ;; p. 82
      (is (= '(* (expt (sin θ) 2) m (expt r 2) φdot)
             (simplify ((compose (ang-mom-z 'm) (F->C s->r)) spherical-state))))
      ;; p. 84
      (is (= '(/ (+ (* (expt (sin θ) 2) m (expt r 2) (expt φdot 2))
                    (* m (expt r 2) (expt θdot 2))
                    (* m (expt rdot 2))
                    (* 2 (V r)))
                 2)
             (simplify ((Lagrangian->energy (L3-central 'm V)) spherical-state))))
      (let [L (L-central-rectangular 'm U)
            F-tilde (fn [angle-x angle-y angle-z]
                      (compose (Rx angle-x)
                               (Ry angle-y)
                               (Rz angle-z)
                               coordinate))
            Noether-integral (* ((partial 2) L) ((D F-tilde) 0 0 0))
            state (up 't (up 'x 'y 'z) (up 'vx 'vy 'vz))]
        (is (= '(down (* m vx) (* m vy) (* m vz)) (simplify (((partial 2) L) state))))
        (is (= '(up x y z) (simplify ((F-tilde 0 0 0) state))))
        (is (= '(down (up 0 (* -1 z) y)
                      (up z 0 (* -1 x))
                      (up (* -1 y) x 0))
               (simplify (((D F-tilde) 0 0 0) state))))
        (is (= '(down (+ (* -1 m vy z) (* m vz y))
                      (+ (* m vx z) (* -1 m vz x))
                      (+ (* -1 m vx y) (* m vy x)))
               (simplify (Noether-integral state))))))))

(deftest section-1.9
  (let [F->C (fn [F]
               (let [f-bar #(->> % Γ (compose F) Γ)]
                 (Γ-bar f-bar)))]
    (is (= '(up t
                (up (* (cos θ) r)
                    (* (sin θ) r))
                (up (+ (* -1 (sin θ) r θdot) (* (cos θ) rdot))
                    (+ (* (cos θ) r θdot) (* (sin θ) rdot))))
           (simplify ((F->C p->r)
                      (->local 't (up 'r 'θ) (up 'rdot 'θdot)))))))
  (is (= '(+ (* a m) (* k x))
         (simplify ((Euler-Lagrange-operator (L-harmonic 'm 'k))
                    (->local 't 'x 'v 'a)))))
  (with-literal-functions [x]
    (is (= '(+ (* (((expt D 2) x) t) m) (* (x t) k))
           (simplify ((compose
                       (Euler-Lagrange-operator (L-harmonic 'm 'k))
                       (Γ x 4)) 't))))))
