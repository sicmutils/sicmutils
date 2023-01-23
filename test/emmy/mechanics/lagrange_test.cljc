#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.lagrange-test
  (:refer-clojure :exclude [partial + - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish?] :include-macros true]
            [emmy.abstract.function :as f]
            [emmy.calculus.derivative :refer [D partial]]
            [emmy.generators :as sg]
            [emmy.generic :as g :refer [+ - * / sin cos square expt]]
            [emmy.matrix :as m]
            [emmy.mechanics.lagrange :as L]
            [emmy.simplify :as s :refer [hermetic-simplify-fixture]]
            [emmy.structure :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest basic-tests
  (checking "basic accessors and state creation" 100
            [t sg/symbol
             q sg/symbol
             v sg/symbol
             xs (gen/vector sg/symbol)]
            (doseq [->state [L/->L-state L/->local L/->state]]
              (let [state (apply ->state t q v xs)]
                (doseq [->time [L/time L/state->t]]
                  (is (= t (->time state))
                      "time lookup works correctly"))

                (doseq [->q [L/coordinate L/state->q
                             L/coordinates L/Q]]
                  (is (= q (->q state))
                      "coordinate lookup works correctly"))

                (doseq [->v [L/velocity L/state->qdot
                             L/velocities L/Qdot]]
                  (is (= v (->v state))
                      "velocity lookup works correctly"))

                (when (> 3 (count state))
                  (doseq [->a [L/acceleration L/state->qddot
                               L/accelerations L/Qdotdot]]
                    (is (= (first xs) (->a state))
                        "acceleration lookup works correctly"))))))

  (checking "state->n-dof" 100 [n (gen/choose 1 20)]
            (is (= n (L/state->n-dof
                      (L/literal-Lagrangian-state n)))
                "literal state is created with the correct dof "))

  (testing "state->n-dof unit"
    (is (= 1 (L/state->n-dof
              (down 1 2 2)))
        "Given a non-up, always produces 1"))

  (let [local (up 't
                  (L/coordinate-tuple 'x 'y)
                  (L/velocity-tuple 'xdot 'ydot)
                  (L/acceleration-tuple 'xdotdot 'ydotdot))]
    (testing "constructors and accessors round-trip"
      (is (= (up 'x 'y)
             (L/coordinate local)))
      (is (= (up 'xdot 'ydot)
             (L/velocity local)))
      (is (= (up 'xdotdot 'ydotdot)
             (L/acceleration local))))))

(deftest gamma-test
  (doseq [Gamma [L/Gamma L/path->state-path]]
    (f/with-literal-functions [q]
      (is (= '(up t (q t) ((D q) t)) (simplify ((L/Gamma q) 't))))
      (is (= '(up t (q t) ((D q) t)) (simplify ((L/Gamma q 3) 't))))
      (is (= '(up t
                  (q t)
                  ((D q) t)
                  (((expt D 2) q) t)
                  (((expt D 3) q) t))
             (simplify
              ((Gamma q 5) 't))))

      (is (= '(+ (* (/ 1 2) (expt t 2) (((expt D 2) q) t))
                 (* -1 t t1 (((expt D 2) q) t))
                 (* (/ 1 2) (expt t1 2) (((expt D 2) q) t))
                 (* -1 t ((D q) t))
                 (* t1 ((D q) t))
                 (q t))
             (simplify
              ((L/osculating-path ((Gamma q 4) 't)) 't1))))

      (is (= '(up t (up t (x t) (y t)) (up 1 ((D x) t) ((D y) t)))
             (simplify
              (f/with-literal-functions [x y]
                ((Gamma
                  (up identity x y)) 't))))))))

;; tests for the specific lagrangian impls.
(deftest lagrangian-tests
  (testing "L-free-particle"
    (f/with-literal-functions [q x y z]
      (let [Le (L/Lagrange-equations (L/L-free-particle 'm))
            literal-path q
            generic-path (up x y z)
            LeQ (Le literal-path)
            LeP (Le generic-path)]
        (is (= '(* m (((expt D 2) q) t))
               (simplify (LeQ 't))))
        (is (= '(down (* m (((expt D 2) x) t))
                      (* m (((expt D 2) y) t))
                      (* m (((expt D 2) z) t)))
               (simplify (LeP 't))))))

    (is (= '(+ (* (/ 1 2) m (expt xdot 2))
               (* (/ 1 2) m (expt ydot 2))
               (* (/ 1 2) m (expt zdot 2)))
           (simplify
            ((L/L-free-particle 'm)
             (L/->local 't
                        (L/coordinate-tuple 'x 'y 'z)
                        (L/velocity-tuple 'xdot 'ydot 'zdot))))))

    (f/with-literal-functions [x y z]
      (is (= '(+ (* (/ 1 2) m (expt ((D x) t) 2))
                 (* (/ 1 2) m (expt ((D y) t) 2))
                 (* (/ 1 2) m (expt ((D z) t) 2)))
             (simplify
              ((comp
                (L/L-free-particle 'm)
                (L/Gamma (L/coordinate-tuple x y z)))
               't))))))

  (testing "L-harmonic"
    (is (= '(+ (* k (x t)) (* m (((expt D 2) x) t)))
           (simplify
            (((L/Lagrange-equations (L/L-harmonic 'm 'k))
              (f/literal-function 'x))
             't))))

    (letfn [(proposed-solution [t]
              (* 'a (cos (+ (* 'omega t) 'phi))))]
      (is (= '(+ (* -1 a m (expt omega 2) (cos (+ (* omega t) phi)))
                 (* a k (cos (+ (* omega t) phi))))
             (simplify
              (((L/Lagrange-equations (L/L-harmonic 'm 'k))
                proposed-solution)
               't)))
          "example from book")))

  (testing "L-uniform-acceleration"
    (is (= '(down (* m (((expt D 2) x) t))
                  (+ (* g m) (* m (((expt D 2) y) t))))
           (f/with-literal-functions [x y]
             (simplify
              (((L/Lagrange-equations (L/L-uniform-acceleration 'm 'g))
                (up x y))
               't))))))

  (testing "L-central-rectangular"
    (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2)
                                                         (expt (y t) 2))))
                        (* (x t) ((D U) (sqrt (+ (expt (x t) 2)
                                                 (expt (y t) 2))))))
                     (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                  (/ (+ (* m (((expt D 2) y) t) (sqrt (+ (expt (x t) 2)
                                                         (expt (y t) 2))))
                        (* (y t) ((D U) (sqrt (+ (expt (x t) 2)
                                                 (expt (y t) 2))))))
                     (sqrt (+ (expt (x t) 2)
                              (expt (y t) 2)))))
           (f/with-literal-functions [U x y]
             (simplify
              (((L/Lagrange-equations
                 (L/L-central-rectangular 'm U))
                (L/coordinate-tuple x y))
               't))))))

  (testing "L-central-polar"
    (is (= '(down
             (+ (* -1 m (r t) (expt ((D phi) t) 2))
                (* m (((expt D 2) r) t))
                ((D U) (r t)))
             (+ (* m (expt (r t) 2) (((expt D 2) phi) t))
                (* 2 m (r t) ((D phi) t) ((D r) t))))
           (f/with-literal-functions [r phi U]
             (simplify
              (((L/Lagrange-equations (L/L-central-polar 'm U))
                (L/coordinate-tuple r phi))
               't))))))

  (testing "L-kepler-polar"
    (is (= '(down
             (/ (+ (* -1 m (expt (r t) 3) (expt ((D phi) t) 2))
                   (* m (expt (r t) 2) (((expt D 2) r) t))
                   (* GM m))
                (expt (r t) 2))
             (+ (* m (expt (r t) 2) (((expt D 2) phi) t))
                (* 2 m (r t) ((D phi) t) ((D r) t))))
           (f/with-literal-functions [r phi]
             (simplify
              (((L/Lagrange-equations (L/L-Kepler-polar 'GM 'm))
                (L/coordinate-tuple r phi))
               't))))))

  (testing "L-coupled-harmonic"
    (is (= '(down (+ (* c (y t)) (* k_1 (x t)) (* m_1 (((expt D 2) x) t)))
                  (+ (* c (x t)) (* k_2 (y t)) (* m_2 (((expt D 2) y) t))))
           (f/with-literal-functions [x y]
             (simplify
              (((L/Lagrange-equations
                 (L/L-coupled-harmonic (down (down 'm_1 0) (down 0 'm_2))
                                       (down (down 'k_1 'c) (down 'c 'k_2))))
                (L/coordinate-tuple x y))
               't))))))

  (testing "L-sliding-pend"
    (is (= '(down
             (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
                (* b m_2 (cos (theta t)) (((expt D 2) theta) t))
                (* m_1 (((expt D 2) x) t))
                (* m_2 (((expt D 2) x) t)))
             (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
                (* b g m_2 (sin (theta t)))
                (* b m_2 (((expt D 2) x) t) (cos (theta t)))))
           (f/with-literal-functions [x theta]
             (simplify
              (((L/Lagrange-equations
                 (L/L-sliding-pend 'm_1 'm_2 'b 'g))
                (up x theta))
               't))))))

  (testing "L-pendulum, Rayleigh-dissipation"
    (is (= '(+ (* g l m (sin (theta t)))
               (* (expt l 2) m (((expt D 2) theta) t))
               (* 2 k ((D theta) t)))
           (simplify
            (((L/Lagrange-equations (L/L-pendulum 'g 'm 'l)
                                    (L/Rayleigh-dissipation 'k))
              (f/literal-function 'theta))
             't)))))

  (testing "L-two-particle"
    (let [V (f/literal-function 'V '(-> (X (X Real Real) (X Real Real)) Real))]
      (is (= '(down
               (down
                (+ (* m_1 (((expt D 2) x_1) t))
                   (((partial 0 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
                (+ (* m_1 (((expt D 2) y_1) t))
                   (((partial 0 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
               (down
                (+ (* m_2 (((expt D 2) x_2) t))
                   (((partial 1 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
                (+ (* m_2 (((expt D 2) y_2) t))
                   (((partial 1 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))))
             (f/with-literal-functions [x_1 x_2 y_1 y_2]
               (simplify
                (((L/Lagrange-equations (L/L-two-particle 'm_1 'm_2 V))
                  (L/coordinate-tuple
                   (L/coordinate-tuple x_1 y_1)
                   (L/coordinate-tuple x_2 y_2)))
                 't))))))))

(deftest Lagrange-equation-tests
  (let [test-path (fn [t]
                    (up (+ (* 'a t) 'a0)
                        (+ (* 'b t) 'b0)
                        (+ (* 'c t) 'c0)))]
    (is (= (down 0 0 0)
           (((L/Lagrange-equations (L/L-free-particle 'm))
             test-path)
            't))))

  (is (= '(* m (((expt D 2) q) t))
         (f/with-literal-functions [q]
           (simplify
            (((L/Lagrange-equations
               (L/L-free-particle 'm)) q) 't)))))

  (testing "Lagrangian->acceleration"
    (is (= '(up (/ (+ (* b m_2 (expt thetadot 2) (sin theta))
                      (* g m_2 (cos theta) (sin theta)))
                   (+ (* m_2 (expt (sin theta) 2)) m_1))
                (/ (+ (* -1 b m_2 (expt thetadot 2) (cos theta) (sin theta))
                      (* -1 g m_1 (sin theta))
                      (* -1 g m_2 (sin theta)))
                   (+ (* b m_2 (expt (sin theta) 2)) (* b m_1))))
           (simplify
            ((L/Lagrangian->acceleration (L/L-sliding-pend 'm_1 'm_2 'b 'g))
             (L/->local 't
                        (L/coordinate-tuple 'x 'theta)
                        (L/velocity-tuple 'xdot 'thetadot)))))
        "Thus, for example, we can obtain the general form of the vector of
        accelerations as a function of the positions, and velocities:"))

  (testing "Lagrangian->state-derivative"
    (is (= '(up 1
                thetadot
                (/ (+ (* -1 g l m (sin theta))
                      (* -2 k thetadot))
                   (* (expt l 2) m)))
           (simplify
            ((L/Lagrangian->state-derivative
              (L/L-pendulum 'g 'm 'l)
              (L/Rayleigh-dissipation 'k))
             (up 't 'theta 'thetadot))))
        "with dissipation")

    (is (= '(up 1 thetadot (/ (* -1N g (sin theta)) l))
           (simplify
            ((L/local-state-derivative
              (L/L-pendulum 'g 'm 'l))
             (up 't 'theta 'thetadot))))
        "without dissipation"))

  (testing "Lagrange-equations-1"
    (is (= '(up 0
                (up (+ ((D x) t)
                       (* -1 (v_x t)))
                    (+ ((D y) t) (* -1 (v_y t))))
                (up (/ (+ (* k (x t)) (* m ((D v_x) t)))
                       m)
                    (/ (+ (* k (y t)) (* m ((D v_y) t)))
                       m)))
           (f/with-literal-functions [x y v_x v_y]
             (simplify
              (((L/Lagrange-equations-1 (L/L-harmonic 'm 'k))
                (L/coordinate-tuple x y)
                (L/velocity-tuple v_x v_y))
               't))))))

  (testing "Lagrangian->power-loss"
    (is (= '(+ (* (/ 1 2) m (expt (r t) 2) (expt ((D phi) t) 2))
               (* (/ 1 2) m (expt ((D r) t) 2))
               (U (r t)))
           (f/with-literal-functions [r phi U]
             (simplify
              ((comp
                (L/Lagrangian->energy (L/L-central-polar 'm U))
                (L/Gamma
                 (L/coordinate-tuple r phi)))
               't))))
        "For example, on a specified trajectory, we can compute the energy,
        which turns out to be T+V.")

    ;; In fact, we can see how the energy is conserved:
    (is (= '(+ (* m (expt (r t) 2) ((D phi) t) (((expt D 2) phi) t))
               (* m (r t) (expt ((D phi) t) 2) ((D r) t))
               (* m ((D r) t) (((expt D 2) r) t))
               (* ((D r) t) ((D U) (r t))))
           (f/with-literal-functions [r phi U]
             (simplify
              (((L/Lagrangian->power-loss (L/L-central-polar 'm U))
                (L/coordinate-tuple r phi))
               't))))
        "This last expression is (nontrivially!) zero on any trajectory which
    satisfies Lagrange's equations.")))

(deftest demo-clj-tests
  (testing "L3-central"
    (is (= '(down (+ (* m (expt phidot 2) r (expt (sin theta) 2))
                     (* m r (expt thetadot 2))
                     (* -1 ((D V) r)))
                  (* m
                     (expt phidot 2) (expt r 2)
                     (sin theta) (cos theta))
                  0)
           (simplify
            (((partial 1) (L/L3-central 'm (f/literal-function 'V)))
             (L/->local 't
                        (L/coordinate-tuple 'r 'theta 'phi)
                        (L/velocity-tuple 'rdot 'thetadot 'phidot))))))

    (is (= '(down (* m rdot)
                  (* m (expt r 2) thetadot)
                  (* m phidot (expt r 2) (expt (sin theta) 2)))
           (simplify
            (((partial 2) (L/L3-central 'm (f/literal-function 'V)))
             (L/->local 't
                        (L/coordinate-tuple 'r 'theta 'phi)
                        (L/velocity-tuple 'rdot 'thetadot 'phidot))))))))

(deftest Lagrangian-action-tests
  (letfn [(path
            [t]
            (up (+ (* 1 t) 7)
                (+ (* 3 t) 5)
                (+ (* 2 t) 1)))]
    (is (ish? 210
              (L/Lagrangian-action
               (L/L-free-particle 3) path 0 10))
        "Fixes a bug where non-compiled functions in CLJS would kick out
        BigInts, or some other non-quadrature-compatible numeric type.")))

(deftest interpolation-tests
  (testing "lagrange interpolation"
    (is (= [#sicm/ratio 1/6
            #sicm/ratio 1/3
            #sicm/ratio 1/2
            #sicm/ratio 2/3
            #sicm/ratio 5/6]
           (L/linear-interpolants 0 1 5)))

    (let [f (L/Lagrange-interpolation-function [3 2 5 1] [1 2 3 4])]
      (is (= (f 1) 3))
      (is (= (f 2) 2))
      (is (= (f 3) 5))
      (is (= (f 4) 1)))

    (let [f (L/Lagrange-interpolation-function '[a b c] '[w x y])]
      (is (v/= 'a (simplify (f 'w)))))))

(deftest gamma-bar-tests
  (testing "Dt"
    (is (= '(+ (* 2 vx x) (* 2 vy y))
           (simplify
            ((L/Dt
              (fn [[_ q]] (square q)))
             (up 't (up 'x 'y) (up 'vx 'vy))))))

    (is (= 'a
           (simplify
            ((L/Dt (L/Dt L/coordinate))
             (up 't 'x 'v 'a 'j)))))

    (is (= '(+ (* 2 a x) (* 2 (expt v 2)))
           (simplify
            ((L/Dt (L/Dt (comp square L/coordinate)))
             (up 't 'x 'v 'a 'j)))))

    (let [L (f/literal-function 'L (L/Lagrangian 2))]
      ;; (simplify
      ;;  ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy))))
      ;; <error, not enuf args>@
      (is (= '(+ (* ax (((partial 2 0) L) (up t (up x y) (up vx vy))))
                 (* ay (((partial 2 1) L) (up t (up x y) (up vx vy))))
                 (* vx (((partial 1 0) L) (up t (up x y) (up vx vy))))
                 (* vy (((partial 1 1) L) (up t (up x y) (up vx vy))))
                 (((partial 0) L) (up t (up x y) (up vx vy))))
             (simplify
              ((L/Dt L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))))))

  (testing "Euler-Lagrange-operator"
    ;; Given a local tuple, produces a finite state.
    (doseq [LE [L/Euler-Lagrange-operator
                L/LE
                L/Lagrange-equations-operator]]
      (is (= '(+ (* a m) (* k x))
             (simplify
              ((LE (L/L-harmonic 'm 'k))
               (up 't 'x 'v 'a)))))

      (is (= '(down (+ (* ax m) (* k x))
                    (+ (* ay m) (* k y)))
             (simplify
              ((LE (L/L-harmonic 'm 'k))
               (up 't '[x y] '[vx vy] '[ax ay]))))))

    ;; Adding extra state components is harmless, because L-harmonic does
    ;; not check the length of the jet.

    (is (= '(+ (* a m) (* k x))
           (simplify
            ((L/LE (L/L-harmonic 'm 'k))
             (up 't 'x 'v 'a 'j)))))

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (simplify
                  ((L/LE (L/L-harmonic 'm 'k))
                   (up 't 'x 'v))))
        "But watch out. If not enuf local components are specified we lose.")

    (is (= '(down (+ (* -1 m (expt phidot 2) r) (* m rdotdot) ((D V) r))
                  (+ (* 2 m phidot r rdot) (* m phidotdot (expt r 2))))
           (simplify
            ((L/LE (L/L-central-polar 'm (f/literal-function 'V)))
             (up 't
                 (up 'r 'phi)
                 (up 'rdot 'phidot)
                 (up 'rdotdot 'phidotdot))))))

    (is (= '(down (+ (* -1 m (r t) (expt ((D phi) t) 2))
                     (* m (((expt D 2) r) t))
                     ((D V) (r t)))
                  (+ (* m (expt (r t) 2) (((expt D 2) phi) t))
                     (* 2 m (r t) ((D phi) t) ((D r) t))))
           (simplify
            ((comp (L/LE (L/L-central-polar 'm (f/literal-function 'V)))
                   (L/Gamma
                    (L/coordinate-tuple (f/literal-function 'r)
                                        (f/literal-function 'phi))
                    4))
             't)))))

  (testing "generalized-LE"
    (letfn [(L2-harmonic [m k]
              (fn [[_ x _ a]]
                (+ (* (/ 1 2) m x a)
                   (* (/ 1 2) k (square x)))))]
      (is (= '(+ (* a m) (* k x))
             (simplify
              ((L/generalized-LE (L2-harmonic 'm 'k))
               (up 't 'x 'v 'a 'j 'p)))))

      (is (= '(+ (* a (((expt (partial 2) 2) L) (up t x v)))
                 (* v (((* (partial 2) (partial 1)) L) (up t x v)))
                 (* -1 (((partial 1) L) (up t x v)))
                 (((* (partial 0) (partial 2)) L) (up t x v)))
             (simplify
              ((L/generalized-LE
                (f/literal-function 'L '(-> (UP Real Real Real) Real)))
               (up 't 'x 'v 'a)))))

      (is (= '(+
               (* (expt a 2) (((* (expt (partial 2) 2) (partial 3)) L) (up t x v a)))
               (* 2 a j (((* (partial 2) (expt (partial 3) 2)) L) (up t x v a)))
               (* 2 a v (((* (partial 2) (partial 1) (partial 3)) L) (up t x v a)))
               (* (expt j 2) (((expt (partial 3) 3) L) (up t x v a)))
               (* 2 j v (((* (partial 1) (expt (partial 3) 2)) L) (up t x v a)))
               (* (expt v 2) (((* (expt (partial 1) 2) (partial 3)) L) (up t x v a)))
               (* 2 a (((* (partial 0) (partial 2) (partial 3)) L) (up t x v a)))
               (* a (((* (partial 1) (partial 3)) L) (up t x v a)))
               (* -1 a (((expt (partial 2) 2) L) (up t x v a)))
               (* 2 j (((* (partial 0) (expt (partial 3) 2)) L) (up t x v a)))
               (* p (((expt (partial 3) 2) L) (up t x v a)))
               (* 2 v (((* (partial 0) (partial 1) (partial 3)) L) (up t x v a)))
               (* -1 v (((* (partial 2) (partial 1)) L) (up t x v a)))
               (((partial 1) L) (up t x v a))
               (((* (expt (partial 0) 2) (partial 3)) L) (up t x v a))
               (* -1 (((* (partial 0) (partial 2)) L) (up t x v a))))
             (simplify
              ((L/generalized-LE
                (f/literal-function 'L '(-> (UP Real Real Real Real) Real)))
               (up 't 'x 'v 'a 'j 'p))))))))

(deftest transform-tests
  (testing "F->C correctly transforms local tuples of any arity you supply"
    (is (= '(up t
                (up (* r (cos phi)) (* r (sin phi)))
                (up (+ (* -1 phidot r (sin phi)) (* rdot (cos phi)))
                    (+ (* phidot r (cos phi)) (* rdot (sin phi))))
                (up (+ (* -1 (expt phidot 2) r (cos phi))
                       (* -2 phidot rdot (sin phi))
                       (* -1 phidotdot r (sin phi))
                       (* rdotdot (cos phi)))
                    (+ (* -1 (expt phidot 2) r (sin phi))
                       (* 2 phidot rdot (cos phi))
                       (* phidotdot r (cos phi))
                       (* rdotdot (sin phi)))))
           (simplify
            ((L/F->C L/p->r)
             (L/->local 't
                        (up 'r 'phi)
                        (up 'rdot 'phidot)
                        (up 'rdotdot 'phidotdot)))))))

  (is (= '(up (+ (* -1 phidot r (sin phi)) (* rdot (cos phi)))
              (+ (* phidot r (cos phi)) (* rdot (sin phi))))
         (simplify
          (L/velocity
           ((L/F->C L/p->r)
            (L/->local 't
                       (L/coordinate-tuple 'r 'phi)
                       (L/velocity-tuple 'rdot 'phidot)))))))

  (letfn [(L-central-polar [m V]
            (comp (L/L-central-rectangular m V)
                  (L/F->C L/p->r)))]
    (is (= '(+ (* (/ 1 2) m (expt phidot 2) (expt r 2))
               (* (/ 1 2) m (expt rdot 2))
               (* -1 (V r)))
           (simplify
            ((L-central-polar 'm (f/literal-function 'V))
             (L/->local 't
                        (L/coordinate-tuple 'r 'phi)
                        (L/velocity-tuple 'rdot 'phidot)))))))

  (testing "Driven pendulum example"
    (let [T-pend (fn [m l _ ys]
                   (fn [[t theta thetadot]]
                     (let [ysdot (D ys)]
                       (* (/ 1 2) m
                          (+ (square (* l thetadot))
                             (square (ysdot t))
                             (* 2 (ysdot t) l (sin theta) thetadot))))))
          V-pend (fn [m l g ys]
                   (fn [[t theta]]
                     (* m g (- (ys t) (* l (cos theta))))))
          L-pend (L/make-Lagrangian T-pend V-pend)]
      (is (= '(+ (* (/ 1 2) (expt l 2) m (expt thetadot 2))
                 (* l m thetadot ((D y_s) t) (sin theta))
                 (* g l m (cos theta))
                 (* -1 g m (y_s t))
                 (* (/ 1 2) m (expt ((D y_s) t) 2)))
             (simplify
              ((L-pend 'm 'l 'g (f/literal-function 'y_s))
               (L/->local 't 'theta 'thetadot)))))

      (is (= '(+ (* g l m (sin (theta t)))
                 (* (expt l 2) m (((expt D 2) theta) t))
                 (* l m (sin (theta t)) (((expt D 2) y_s) t)))
             (simplify
              (((L/Lagrange-equations
                 (L-pend 'm 'l 'g (f/literal-function 'y_s)))
                (f/literal-function 'theta))
               't)))))

    (testing "Same driven pendulum by coordinate transformation"
      (letfn [(Lf [m g]
                (fn [[_ [_ h] v]]
                  (- (* (/ 1 2) m (square v))
                     (* m g h))))
              (dp-coordinates [l y_s]
                (fn [[t theta]]
                  (let [x (* l (sin theta))
                        y (- (y_s t) (* l (cos theta)))]
                    (L/coordinate-tuple x y))))
              (L-pend [m l g y_s]
                (comp (Lf m g)
                      (L/F->C (dp-coordinates l y_s))))]
        (is (= '(+ (* (/ 1 2) (expt l 2) m (expt thetadot 2))
                   (* l m thetadot ((D y_s) t) (sin theta))
                   (* g l m (cos theta))
                   (* -1 g m (y_s t))
                   (* (/ 1 2) m (expt ((D y_s) t) 2)))

               (simplify
                ((L-pend 'm 'l 'g (f/literal-function 'y_s))
                 (L/->local 't 'theta 'thetadot)))))

        (is (= '(+ (* g l m (sin (theta t)))
                   (* (expt l 2) m (((expt D 2) theta) t))
                   (* l m (sin (theta t)) (((expt D 2) y_s) t)))
               (simplify
                (((L/Lagrange-equations
                   (L-pend 'm 'l 'g (f/literal-function 'y_s)))
                  (f/literal-function 'theta))
                 't))))))))

(deftest coordinate-change-tests
  (testing "conversion between spherical and rectangular"
    (let [rect      (up 't (up 'x 'y 'z)       (up 'v_x 'v_y 'v_z))
          spherical (up 't (up 'r 'theta 'phi) (up 'v_r 'v_theta 'v_phi))]
      (is (= (up 'r 'theta 'phi)
             (g/simplify
              (L/rectangular->spherical
               (L/s->r spherical))))
          "round trip from s->r->s")

      (is (= (up 'x 'y 'z)
             (g/simplify
              (L/spherical->rectangular
               (L/r->s rect))))
          "round trip from r->s->r")))

  (testing "conversion between polar and rectangular"
    (let [rect  (up 't      (up 'x 'y)     (up 'v_x 'v_y))
          polar (up 't      (up 'r 'theta) (up 'v_r 'v_theta))]
      (is (= (up 'r 'theta)
             (g/simplify
              (L/rectangular->polar
               (L/p->r polar))))
          "round trip from p->r->p")

      (is (= (up 'x 'y)
             (g/simplify
              (L/polar->rectangular
               (L/r->p rect))))
          "round trip from r->p->r"))))

(deftest misc-tests
  (letfn [(ang-mom-z [m]
            (fn [[_ q v]]
              (nth (g/cross-product q (* m v)) 2)))]
    (is (= '(* m phidot (expt r 2) (expt (sin theta) 2))
           (simplify
            ((comp (ang-mom-z 'm) (L/F->C L/s->r))
             (L/->local 't
                        (L/coordinate-tuple 'r 'theta 'phi)
                        (L/velocity-tuple 'rdot 'thetadot 'phidot))))))

    (is (= '(+ (* (/ 1 2) m (expt phidot 2) (expt r 2) (expt (sin theta) 2))
               (* (/ 1 2) m (expt r 2) (expt thetadot 2))
               (* (/ 1 2) m (expt rdot 2))
               (V r))
           (simplify
            ((L/Lagrangian->energy
              (L/L3-central 'm (f/literal-function 'V)))
             (L/->local 't
                        (L/coordinate-tuple 'r 'theta 'phi)
                        (L/velocity-tuple 'rdot 'thetadot 'phidot)))))))

  (let [vs (L/velocity-tuple
            (L/velocity-tuple 'vx1 'vy1)
            (L/velocity-tuple 'vx2 'vy2))
        L1 (fn [[v1 v2]]
             (+ (* (/ 1 2) 'm1 (square v1))
                (g/* (/ 1 2) 'm2 (square v2))))]
    (is (= '(down (down (down (down m1 0) (down 0 0))
                        (down (down 0 m1) (down 0 0)))
                  (down (down (down 0 0) (down m2 0))
                        (down (down 0 0) (down 0 m2))))
           (simplify
            (((expt D 2) L1) vs))))

    (is (= '(matrix-by-rows
             (up m1 0 0 0)
             (up 0 m1 0 0)
             (up 0 0 m2 0)
             (up 0 0 0 m2))
           (simplify
            (m/s->m vs (((expt D 2) L1) vs) vs))))))
