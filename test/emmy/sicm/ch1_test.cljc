#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sicm.ch1-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [emmy.env :as e
             :refer [+ - * / D partial
                     compose up down abs sin cos square expt
                     Lagrange-equations
                     F->C p->r s->r ->local
                     coordinate velocity
                     Rx Ry Rz]]
            [emmy.examples.driven-pendulum :as driven]
            [emmy.examples.pendulum :as pendulum]
            [emmy.mechanics.lagrange :as L]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v :refer [within]]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp e/freeze e/simplify))

(def ^:private near (within 1e-6))

(deftest ^:long section-1-4
  (e/with-literal-functions [x y z]
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
                                            (e/Lagrangian-action (L/L-free-particle mass)
                                                                 (+ q (* ε η)) t1 t2))))]
      ;; integral
      (is (near 2.0 (e/definite-integral sin 0 Math/PI)))

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
             (simplify ((e/Gamma q) 't))))

      (is (= '(+ (* (/ 1 2) m (expt ((D x) t) 2))
                 (* (/ 1 2) m (expt ((D y) t) 2))
                 (* (/ 1 2) m (expt ((D z) t) 2)))
             (v/freeze
              (simplify ((compose (L/L-free-particle 'm) (e/Gamma q)) 't)))))

      ;; p. 20
      (is (= 435.0 (e/Lagrangian-action (L/L-free-particle 3.0) test-path 0.0 10.0)))

      (let [m (e/minimize
               (varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0)
               -2.0 1.0)]
        (is (near 0.0 (:result m)))
        (is (near 435 (:value m))))

      (is (near 436.2912143 ((varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) 0.001)))

      ;; This is fairly time consuming since every evaluation of a candidate
      ;; point in the multidimensional minimization of find-path involves
      ;; computing a numeric integration to find the Lagrangian of the path
      ;; induced by the point. But it works.
      (let [values (atom [])
            minimal-path (e/find-path
                          (L/L-harmonic 1.0 1.0) 0. 1. (/ Math/PI 2) 0. 3
                          :observe (fn [_ pt _] (swap! values conj pt)))
            good? (partial (within 2e-4) 0)
            errors (for [x (range 0.0 (/ Math/PI 2) 0.02)]
                     (abs (- (Math/cos x) (minimal-path x))))]

        ;; the minimization is supposed to discover the cosine function in the
        ;; interval [0..pi/2]. Check that it has done so over a variety of
        ;; points to within 2e-4.
        (is ((within 1e-4) 1 (minimal-path 0)))
        (is ((within 1e-5) 0 (minimal-path (/ Math/PI 2))))
        (is (every? good? errors))))))

(defn ^:private δ
  "The variation operator (p. 28)."
  [η]
  (fn [f]
    ;; Define g(ε) as in Eq. 1.22; then δ_η f[q] = Dg(0)
    (fn [q]
      (let [g (fn [ε]
                (f (+ q (* ε η))))]
        ((D g) 0)))))

(deftest section-1-5
  (e/with-literal-functions [x f g q η φ]
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
      (is (= '(* (η t) ((D f) (q t))) (simplify (((δ_η F) q) 't))))
      (is (= '(* (η t) ((D g) (q t))) (simplify (((δ_η G) q) 't))))
      (is (= '(+ (* (η t) ((D f) (q t)) (g (q t))) (* (η t) ((D g) (q t)) (f (q t))))
             (simplify (((δ_η (* F G)) q) 't))))
      (is (= '(+ (* (η t) ((D f) (q t)) (g (q t))) (* (η t) ((D g) (q t)) (f (q t))))
             (simplify (((δ_η (* F G)) q) 't))))
      (is (= '(* (η t) ((D f) (q t)) ((D φ) (f (q t)))) (simplify (((δ_η (φ F)) q) 't))))

      ;; p. 35
      (is (= (down 0 0 0) (((Lagrange-equations (L/L-free-particle 'm)) test-path) 't)))
      (is (= '(* m (((expt D 2) x) t))
             (simplify (((Lagrange-equations (L/L-free-particle 'm)) x) 't))))
      (is (= '(+ (* -1 a m (expt ω 2) (cos (+ (* t ω) φ))) (* a k (cos (+ (* t ω) φ))))
             (simplify (((Lagrange-equations (L/L-harmonic 'm 'k)) proposed-solution) 't)))))))


(deftest section-1-6
  (e/with-literal-functions [x y r θ φ U y_s]
    (let [L-alternate-central-polar (fn [m U]
                                      (compose (L/L-central-rectangular m U)
                                               (F->C p->r)))]
      (is (= '(down (* m (((expt D 2) x) t))
                    (+ (* g m) (* m (((expt D 2) y) t))))
             (simplify (((Lagrange-equations (L/L-uniform-acceleration 'm 'g))
                         (up x y)) 't))))

      (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                          (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* m (((expt D 2) y) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                          (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
             (simplify (((Lagrange-equations (L/L-central-rectangular 'm U))
                         (up x y))
                        't))))

      (is (= '(down (+ (* -1 m (r t) (expt ((D φ) t) 2))
                       (* m (((expt D 2) r) t))
                       ((D U) (r t)))
                    (+ (* m (expt (r t) 2) (((expt D 2) φ) t))
                       (* 2 m (r t) ((D φ) t) ((D r) t))))
             (simplify (((Lagrange-equations (L/L-central-polar 'm U))
                         (up r φ))
                        't))))

      (is (= '(up (+ (* -1 r φdot (sin φ)) (* rdot (cos φ)))
                  (+ (* r φdot (cos φ)) (* rdot (sin φ))))
             (simplify (velocity ((F->C p->r)
                                  (->local 't (up 'r 'φ) (up 'rdot 'φdot)))))))

      (is (= '(+
               (* (/ 1 2) m (expt r 2) (expt φdot 2))
               (* (/ 1 2) m (expt rdot 2))
               (* -1 (U r)))
             (v/freeze
              (simplify ((L-alternate-central-polar 'm U)
                         (->local 't (up 'r 'φ) (up 'rdot 'φdot)))))))

      (is (= '(down (+ (* -1 m (r t) (expt ((D φ) t) 2))
                       (* m (((expt D 2) r) t))
                       ((D U) (r t)))
                    (+ (* m (expt (r t) 2) (((expt D 2) φ) t))
                       (* 2 m (r t) ((D φ) t) ((D r) t))))
             (simplify (((Lagrange-equations (L-alternate-central-polar 'm U))
                         (up r φ))
                        't))))

      (is (=  '(+ (* g l m (sin (θ t)))
                  (* (expt l 2) m (((expt D 2) θ) t))
                  (* l m (sin (θ t)) (((expt D 2) y_s) t)))
              (simplify
               (((Lagrange-equations
                  (pendulum/L 'm 'l 'g (up (fn [_] 0) y_s))) θ) 't))))

      ;; p. 61
      (let [Lf (fn [m g]
                 (fn [[_ [_ y] v]]
                   (- (* #sicm/ratio 1/2 m (square v)) (* m g y))))
            dp-coordinates (fn [l y_s]
                             (fn [[t θ]]
                               (let [x (* l (sin θ))
                                     y (- (y_s t) (* l (cos θ)))]
                                 (up x y))))
            L-pend2 (fn [m l g y_s]
                      (compose (Lf m g)
                               (F->C (dp-coordinates l y_s))))]

        (is (= '(+ (* (/ 1 2) (expt l 2) m (expt θdot 2))
                   (* l m θdot (sin θ) ((D y_s) t))
                   (* g l m (cos θ))
                   (* -1 g m (y_s t))
                   (* (/ 1 2) m (expt ((D y_s) t) 2)))
               (v/freeze
                (simplify ((L-pend2 'm 'l 'g y_s) (->local 't 'θ 'θdot))))))))))

(deftest ^:long section-1-7-1
  (e/with-literal-functions [x y v_x v_y]
    (let [harmonic-state-derivative (fn [m k]
                                      (e/Lagrangian->state-derivative (L/L-harmonic m k)))]
      (is (= '(up 1
                  (up v_x v_y)
                  (up (/ (* -1 k x) m) (/ (* -1 k y) m)))
             (simplify ((harmonic-state-derivative 'm 'k)
                        (up 't (up 'x 'y) (up 'v_x 'v_y))))))
      ;; p. 71
      (is (= '(up 0
                  (up (+ ((D x) t) (* -1 (v_x t)))
                      (+ ((D y) t) (* -1 (v_y t))))
                  (up (/ (+ (* k (x t)) (* m ((D v_x) t))) m)
                      (/ (+ (* k (y t)) (* m ((D v_y) t))) m)))
             (simplify (((e/Lagrange-equations-first-order (L/L-harmonic 'm 'k))
                         (up x y)
                         (up v_x v_y))
                        't))))
      (is (= (up 1 (up 3.0 4.0) (up #sicm/ratio -1/2 -1.0))
             ((harmonic-state-derivative 2. 1.) (up 0 (up 1. 2.) (up 3. 4.)))))

      (is (= '(1 3.0 4.0 (/ -1 2) -1.0)
             (v/freeze
              (flatten ((harmonic-state-derivative 2. 1.)
                        (up 0 (up 1. 2.) (up 3. 4.)))))))
      ;; p. 72
      (dotimes [_ 1]  ;; this is just here in case we want to watch in the profiler
        (let [answer ((e/state-advancer harmonic-state-derivative 2. 1.)
                      (up 0. (up 1. 2.) (up 3. 4.))
                      10.
                      {:epsilon 1e-12
                       :compile? true})
              expected (up 10.
                           (up 3.71279166 5.42062082)
                           (up 1.61480309 1.81891037))
              delta (->> answer (- expected) flatten (map abs) (reduce max))]
          (is (< delta 1e-8)))))))

(deftest section-1-7-2
  (let [pend-state-derivative (fn [m l g a ω]
                                (e/Lagrangian->state-derivative
                                 (driven/L m l g a ω)))]
    (is (= '(+ (* -1 a l m (expt ω 2) (sin (θ t)) (cos (* t ω)))
               (* g l m (sin (θ t)))
               (* (expt l 2) m (((expt D 2) θ) t)))
           (simplify (((Lagrange-equations
                        (driven/L 'm 'l 'g 'a 'ω))
                       (e/literal-function 'θ))
                      't))))
    ;; NB. fraction simplification not happening here
    (is (= '(up 1
                θdot
                (/ (+ (* a (expt ω 2) (cos (* t ω)) (sin θ)) (* -1 g (sin θ))) l))
           (simplify
            ((pend-state-derivative 'm 'l 'g 'a 'ω)
             (up 't 'θ 'θdot)))))

    (let [opts      {:compile? true :epsilon 1.0e-13}
          evolve-fn (e/evolve pend-state-derivative
                              1.0
                              1.0
                              9.8
                              0.1
                              (* 2.0 (e/sqrt 9.8)))
          answer (evolve-fn (up 0.0 1. 0.) 0.01 1.0 opts)
          expected (up 1.0 -1.030115687 -1.40985359)
          delta (->> answer (- expected) flatten (map abs) (reduce max))]
      (is (< delta 1e-8)))))

(deftest section-1-8
  (e/with-literal-functions [U V]
    (let [spherical-state (up 't
                              (up 'r 'θ 'φ)
                              (up 'rdot 'θdot 'φdot))
          T3-spherical (fn [m]
                         (fn [[_ [r θ _] [rdot θdot φdot]]]
                           (* (/ 1 2) m (+ (square rdot)
                                           (square (* r θdot))
                                           (square (* r (sin θ) φdot))))))
          L3-central (fn [m Vr]
                       (let [Vs (fn [[_ [r]]] (Vr r))]
                         (- (T3-spherical m) Vs)))
          ang-mom-z (fn [m]
                      (fn  [[_ q v]]
                        (nth (e/cross-product q (* m v)) 2)))]
      ;; p. 81
      (is (= '(down (+ (* m r (expt φdot 2) (expt (sin θ) 2))
                       (* m r (expt θdot 2))
                       (* -1 ((D V) r)))
                    (* m (expt r 2) (expt φdot 2) (sin θ) (cos θ))
                    0)
             (simplify
              (((partial 1) (L3-central 'm V)) spherical-state))))

      (is (= '(down (* m rdot)
                    (* m (expt r 2) θdot)
                    (* m (expt r 2) φdot (expt (sin θ) 2)))
             (simplify (((partial 2) (L3-central 'm V)) spherical-state))))

      ;; p. 82
      (is (= '(* m (expt r 2) φdot (expt (sin θ) 2))
             (simplify ((compose (ang-mom-z 'm) (F->C s->r)) spherical-state))))
      ;; p. 84
      (is (= '(+ (* (/ 1 2) m (expt r 2) (expt φdot 2) (expt (sin θ) 2))
                 (* (/ 1 2) m (expt r 2) (expt θdot 2))
                 (* (/ 1 2) m (expt rdot 2))
                 (V r))
             (v/freeze
              (simplify ((e/Lagrangian->energy (L3-central 'm V)) spherical-state)))))
      (let [L (L/L-central-rectangular 'm U)
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

(deftest section-1-9
  (let [F->C (fn [F]
               (let [f-bar #(->> % e/Gamma (compose F) e/Gamma)]
                 (e/Gamma-bar f-bar)))]
    (is (= '(up t
                (up (* r (cos θ)) (* r (sin θ)))
                (up (+ (* -1 r θdot (sin θ))
                       (* rdot (cos θ)))
                    (+ (* r θdot (cos θ))
                       (* rdot (sin θ)))))
           (simplify ((F->C p->r)
                      (->local 't (up 'r 'θ) (up 'rdot 'θdot)))))))
  (is (= '(+ (* a m) (* k x))
         (simplify ((e/Euler-Lagrange-operator (L/L-harmonic 'm 'k))
                    (->local 't 'x 'v 'a)))))
  (e/with-literal-functions [x]
    (is (= '(+ (* k (x t)) (* m (((expt D 2) x) t)))
           (simplify ((compose
                       (e/Euler-Lagrange-operator (L/L-harmonic 'm 'k))
                       (e/Gamma x 4)) 't))))))
