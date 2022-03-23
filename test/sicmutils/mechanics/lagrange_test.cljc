#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.lagrange-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?] #?@(:cljs [:include-macros true])]
            [sicmutils.abstract.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

;; TODO test tuple creation, ->L-state etc and the various selectors with a
;; generative test.

(deftest basic-tests
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

;; TODO test literal-lagrangian-state

(deftest gamma-test
  ;; TODO test path->state-path alias.
  (f/with-literal-functions [q]
    (is (= '(up t (q t) ((D q) t)) (simplify ((L/Gamma q) 't))))
    (is (= '(up t (q t) ((D q) t)) (simplify ((L/Gamma q 3) 't))))
    (is (= '(up t
                (q t)
                ((D q) t)
                (((expt D 2) q) t)
                (((expt D 3) q) t))
           (simplify ((L/Gamma q 5) 't))))

    (is (= '(+ (* (/ 1 2) (expt t 2) (((expt D 2) q) t))
               (* -1 t t1 (((expt D 2) q) t))
               (* (/ 1 2) (expt t1 2) (((expt D 2) q) t))
               (* -1 t ((D q) t))
               (* t1 ((D q) t))
               (q t))
           (v/freeze
            (simplify ((L/osculating-path ((L/Gamma q 4) 't)) 't1)))))

    (is (= '(up t (up t (x t) (y t)) (up 1 ((D x) t) ((D y) t)))
           (simplify
            (f/with-literal-functions [x y]
              ((L/Gamma (up identity x y)) 't)))))))

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

  (testing "L-rectangular"
    ;; add some tests
    )

  (testing "L-harmonic"
    (is (= '(+ (* k (x t)) (* m (((expt D 2) x) t)))
           (simplify
            (((L/Lagrange-equations (L/L-harmonic 'm 'k))
              (f/literal-function 'x))
             't))))

    (letfn [(proposed-solution [t]
              (g/* 'a (g/cos (g/+ (g/* 'omega t) 'phi))))]
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
    (is (= '(down (/ (+ (* m
                           (((expt D 2) x) t)
                           (sqrt (+ (expt (x t) 2)
                                    (expt (y t) 2))))
                        (* (x t) ((D U) (sqrt (+ (expt (x t) 2)
                                                 (expt (y t) 2))))))
                     (sqrt (+ (expt (x t) 2)
                              (expt (y t) 2))))
                  (/ (+ (* m (sqrt (+ (expt (x t) 2)
                                      (expt (y t) 2)))
                           (((expt D 2) y) t))
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

  (testing "L-coupled-harmonic"
    ;; (show-expression
    ;;   (((Lagrange-equations
    ;;      (L-coupled-harmonic (down (down 'm_1 0) (down 0 'm_2))
    ;;                         (down (down 'k_1 'c) (down 'c 'k_2))))
    ;;     (coordinate-tuple (f/literal-function 'x)
    ;;                      (f/literal-function 'y)))
    ;;    't))
    ;; (down (+ (* c (y t)) (* k_1 (x t)) (* m_1 (((expt D 2) x) t)))
    ;;       (+ (* c (x t)) (* k_2 (y t)) (* m_2 (((expt D 2) y) t))))
    )

  (testing "L-sliding-pend"
    ;; (show-expression
    ;;  (((Lagrange-equations (L-sliding-pend 'm_1 'm_2 'b 'g))
    ;;    (coordinate-tuple (f/literal-function 'x)
    ;;                     (f/literal-function 'theta)))
    ;;   't))
    ;; (down
    ;;  (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
    ;;     (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
    ;;     (* m_1 (((expt D 2) x) t))
    ;;     (* m_2 (((expt D 2) x) t)))
    ;;  (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
    ;;     (* b g m_2 (sin (theta t)))
    ;;     (* b m_2 (((expt D 2) x) t) (cos (theta t)))))
    )

  (testing "L-sliding-pend*"
    ;; (show-expression
    ;;  (((Lagrange-equations
    ;;     (L-sliding-pend* 'm_1 'm_2 'b 'g))
    ;;    (up (f/literal-function 'x)
    ;;        (f/literal-function 'theta)))
    ;;   't))
    ;; (down
    ;;  (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
    ;;     (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
    ;;     (* m_1 (((expt D 2) x) t))
    ;;     (* m_2 (((expt D 2) x) t)))
    ;;  (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
    ;;     (* b g m_2 (sin (theta t)))
    ;;     (* b m_2 (cos (theta t)) (((expt D 2) x) t))))
    )

  (testing "L-pendulum, Rayleigh-dissipation"
    ;; (show-expression
    ;;  (((Lagrange-equations (L-pendulum 'g 'm 'l)
    ;;                       (Rayleigh-dissipation 'k))
    ;;    (f/literal-function 'theta))
    ;;   't))
    ;; (+ (* 2 k ((D theta) t))
    ;;    (* g l m (sin (theta t)))
    ;;    (* (expt l 2) m (((expt D 2) theta) t)))
    )

  (testing "L-two-particle"
    ;; (show-expression
    ;;  (((Lagrange-equations (L-two-particle 'm_1 'm_2))
    ;;    (coordinate-tuple
    ;;     (coordinate-tuple (f/literal-function 'x_1) (f/literal-function 'y_1))
    ;;     (coordinate-tuple (f/literal-function 'x_2) (f/literal-function 'y_2))))
    ;;   't))
    ;; (down
    ;;  (down
    ;;   (+ (* m_1 (((expt D 2) x_1) t))
    ;;      (((partial 0 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
    ;;   (+ (* m_1 (((expt D 2) y_1) t))
    ;;      (((partial 0 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
    ;;  (down
    ;;   (+ (* m_2 (((expt D 2) x_2) t))
    ;;      (((partial 1 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
    ;;   (+ (* m_2 (((expt D 2) y_2) t))
    ;;      (((partial 1 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))))
    )
  )

(deftest Lagrange-equation-tests
  (let [test-path (fn [t]
                    (up (g/+ (g/* 'a t) 'a0)
                        (g/+ (g/* 'b t) 'b0)
                        (g/+ (g/* 'c t) 'c0)))]
    (is (= (down 0 0 0)
           (((L/Lagrange-equations (L/L-free-particle 'm))
             test-path)
            't))))

  (is (= '(* m (((expt D 2) q) t))
         (f/with-literal-functions [q]
           (simplify
            (((L/Lagrange-equations
               (L/L-free-particle 'm)) q) 't)))))

  ;; TODO test with dissipation function

  (testing "Lagrangian->acceleration"
    ;; Thus, for example, we can obtain the general form of the vector of
    ;; accelerations as a function of the positions, and velocities:

    ;; (show-expression
    ;;  ((Lagrangian->acceleration (L-sliding-pend 'm_1 'm_2 'b 'g))
    ;;   (->local 't
    ;;           (coordinate-tuple 'x 'theta)
    ;;           (velocity-tuple 'xdot 'thetadot))))
    ;; (up
    ;;  (+
    ;;   (/ (* b m_2 (expt thetadot 2) (sin theta))
    ;;      (+ (* m_2 (expt (sin theta) 2)) m_1))
    ;;   (/ (* g m_2 (sin theta) (cos theta))
    ;;      (+ (* m_2 (expt (sin theta) 2)) m_1)))
    ;;  (+
    ;;   (/ (* -1 m_2 (expt thetadot 2) (sin theta) (cos theta))
    ;;      (+ (* m_2 (expt (sin theta) 2)) m_1))
    ;;   (/ (* -1 g m_1 (sin theta))
    ;;      (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))
    ;;   (/ (* -1 g m_2 (sin theta))
    ;;      (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))))
    )

  (testing "Lagrangian->state-derivative"
    ;; (print-expression
    ;;  ((Lagrangian->state-derivative (L-pendulum 'g 'm 'l)
    ;;                                 (Rayleigh-dissipation 'k))
    ;;   (up 't 'theta 'thetadot)))
    ;; (up 1
    ;;     thetadot
    ;;     (+ (/ (* -1 g (sin theta)) l)
    ;;        (/ (* -2 k thetadot) (* (expt l 2) m))))

    )

  (testing "Lagrange-equations-1"
    ;; (show-expression
    ;;  (((Lagrange-equations-1 (L-harmonic 'm 'k))
    ;;    (coordinate-tuple (af/literal-function 'x)
    ;;                      (af/literal-function 'y))
    ;;    (velocity-tuple (af/literal-function 'v_x)
    ;;                    (af/literal-function 'v_y)))
    ;;   't))
    ;; (up 0
    ;;     (up (+ ((D x) t) (* -1 (v_x t))) (+ ((D y) t) (* -1 (v_y t))))
    ;;     (up (+ (/ (* k (x t)) m) ((D v_x) t)) (+ (/ (* k (y t)) m) ((D v_y) t))))
    )

  (testing "Lagrangian->power-loss"
    ;; For example, on a specified trajectory, we can compute the energy, which
    ;; turns out to be T+V.

    ;; (show-expression
    ;;  ((compose
    ;;    (Lagrangian->energy (L-central-polar 'm (af/literal-function 'U)))
    ;;    (Gamma
    ;;     (coordinate-tuple (af/literal-function 'r) (af/literal-function 'phi))))
    ;;   't))
    ;; (+ (* (/ 1 2) m (expt (r t) 2) (expt ((D phi) t) 2))
    ;;    (* (/ 1 2) m (expt ((D r) t) 2))
    ;;    (U (r t)))


    ;; In fact, we can see how the energy is conserved:

    ;; (show-expression
    ;;  (((Lagrangian->power-loss (L-central-polar 'm (af/literal-function 'U)))
    ;;    (coordinate-tuple (af/literal-function 'r) (af/literal-function 'phi)))
    ;;   't))
    ;; (+ (* m (((expt D 2) phi) t) ((D phi) t) (expt (r t) 2))
    ;;    (* m (expt ((D phi) t) 2) (r t) ((D r) t))
    ;;    (* m (((expt D 2) r) t) ((D r) t))
    ;;    (* ((D U) (r t)) ((D r) t)))

    ;; This last expression is (nontrivially!) zero on any trajectory
    ;; which satisfies Lagrange's equations.
    )
  )

(deftest demo-clj-tests
  (testing "L3-central"
    ;; (show-expression
    ;;  (((partial 1) (L3-central 'm (af/literal-function 'V)))
    ;;   (->local 't
    ;;            (coordinate-tuple 'r 'theta 'phi)
    ;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
    ;; (down
    ;;  (+ (* m r (expt phidot 2) (expt (sin theta) 2))
    ;;     (* m r (expt thetadot 2))
    ;;     (* -1 ((D V) r)))
    ;;  (* m (expt r 2) (expt phidot 2) (cos theta) (sin theta))
    ;;  0)

    ;; (show-expression
    ;;  (((partial 2) (L3-central 'm (af/literal-function 'V)))
    ;;   (->local 't
    ;;            (coordinate-tuple 'r 'theta 'phi)
    ;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
    ;; (down (* m rdot)
    ;;       (* m (expt r 2) thetadot)
    ;;       (* m (expt r 2) phidot (expt (sin theta) 2)))
    ))

(deftest Lagrangian-action-tests
  (letfn [(path
            [t]
            (up (g/+ (g/* 1 t) 7)
                (g/+ (g/* 3 t) 5)
                (g/+ (g/* 2 t) 1)))]
    (is (ish? 210
              (L/Lagrangian-action
               (L/L-free-particle 3) path 0 10))
        "Fixes a bug where non-compiled functions in CLJS would kick out
        BigInts, or some other non-quadrature-compatible numeric type."))

  ;; in ch1 tests, but add more.

  ;; (define (test-path t)
  ;;   (coordinate-tuple (+ (* 4 t) 7)
  ;;                    (+ (* 3 t) 5)
  ;;                    (+ (* 2 t) 1)))

  ;; (Lagrangian-action (L-free-particle 3) test-path 0 10)
  ;;                                         ;Value: 435.

  ;; (define ((variation nu t1 t2 h) t)
  ;;   (* h (- t t1) (- t t2) (nu t)))

  ;; (define ((varied-free-particle-action mass path nu t1 t2) h)
  ;;   (let ((dpath (variation nu t1 t2 h)))
  ;;     (Lagrangian-action (L-free-particle mass)
  ;;                        (+ path dpath)
  ;;                        t1
  ;;                        t2)))

  ;; ((varied-free-particle-action 3.0 test-path
  ;;                               (coordinate-tuple sin cos square)
  ;;                               0.0 10.0)
  ;;  0.001)
  ;;                                         ;Value: 436.29121428571443

  ;; (minimize
  ;;  (varied-free-particle-action 3.0 test-path
  ;;                               (coordinate-tuple sin cos square)
  ;;                               0.0 10.0)
  ;;  -2.0 1.0)
  ;;                                         ;Value: (-5.828670879282072e-16 435.00000000000085 5)


  )

(deftest gamma-bar-tests
  (testing "Dt"
    ;; (print-expression
    ;;  ((Dt
    ;;    (lambda (state)
    ;;            (let ((t (time state))
    ;;                 (q (coordinate state)))
    ;;              (square q))))
    ;;   (up 't (up 'x 'y) (up 'vx 'vy))))
    ;; (+ (* 2 vx x) (* 2 vy y))


    ;; (print-expression
    ;;  ((Dt (Dt (lambda (state) (coordinate state))))
    ;;   (up 't 'x 'v 'a 'j)))
    ;; a

    ;; (print-expression
    ;;  ((Dt (Dt (lambda (state)
    ;;                  (square (coordinate state)))))
    ;;   (up 't 'x 'v 'a 'j)))
    ;; (+ (* 2 a x) (* 2 (expt v 2)))

    ;; (define L (af/literal-function 'L (Lagrangian 2)))

    ;; (print-expression
    ;;  ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy))))
    ;; <error, not enuf args>

    ;; (print-expression
    ;;  ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))
    ;; (+ (* ax (((partial 2 0) L) (up t (up x y) (up vx vy))))
    ;;    (* ay (((partial 2 1) L) (up t (up x y) (up vx vy))))
    ;;    (* vx (((partial 1 0) L) (up t (up x y) (up vx vy))))
    ;;    (* vy (((partial 1 1) L) (up t (up x y) (up vx vy))))
    ;;    (((partial 0) L) (up t (up x y) (up vx vy))))
    )

  (testing "Euler-Lagrange-operator"
    ;; Given a local tuple, produces a finite state.

    ;; (print-expression
    ;;  ((LE (L-harmonic 'm 'k))
    ;;   (up 't 'x 'v 'a)))
    ;; (+ (* a m) (* k x))

    ;; (print-expression
    ;;  ((LE (L-harmonic 'm 'k))
    ;;   (up 't #(x y) #(vx vy) #(ax ay))))
    ;; (down (+ (* ax m) (* k x))
    ;;       (+ (* ay m) (* k y)))


    ;; (print-expression
    ;;  ((LE L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))
    ;; (down
    ;;  (+ (* ax (((partial 2 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    ;;     (* ay (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    ;;     (* vx (((partial 1 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    ;;     (* vy (((partial 1 1) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    ;;     (* -1 (((partial 1 0) L) (up t (up x y) (up vx vy))))
    ;;     (((partial 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
    ;;  (+ (* ax (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    ;;     (* ay (((partial 2 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    ;;     (* vx (((partial 1 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    ;;     (* vy (((partial 1 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
    ;;     (* -1 (((partial 1 1) L) (up t (up x y) (up vx vy))))
    ;;     (((partial 0) ((partial 2 1) L)) (up t (up x y) (up vx vy)))))


    ;; ;;; Adding extra state components is harmless, because L-harmonic does
    ;; ;;; not check the length of the jet.

    ;; (print-expression
    ;;  ((LE (L-harmonic 'm 'k))
    ;;   (up 't 'x 'v 'a 'j)))
    ;; (+ (* a m) (* k x))

    ;; ;;; But watch out.  If not enuf local componenents
    ;; ;;;  are specified we lose.

    ;; (print-expression
    ;;  ((LE (L-harmonic 'm 'k))
    ;;   (up 't 'x 'v)))
    ;; Cannot extract velocity from #((*diff* ... ...) x)
    ;; ;;; error
    ;;
    ;; (print-expression
    ;;  ((LE (L-central-polar 'm (af/literal-function 'V)))
    ;;   (up 't
    ;;       (up 'r 'phi)
    ;;       (up 'rdot 'phidot)
    ;;       (up 'rdotdot 'phidotdot))))
    ;; (down (+ (* -1 m (expt phidot 2) r) (* m rdotdot) ((D V) r))
    ;;       (+ (* 2 m phidot r rdot) (* m phidotdot (expt r 2))))

    ;; (print-expression
    ;;  ((compose (LE (L-central-polar 'm (af/literal-function 'V)))
    ;;           (Gamma
    ;;            (coordinate-tuple (af/literal-function 'r)
    ;;                              (af/literal-function 'phi))
    ;;            4))
    ;;   't))
    ;; (down
    ;;  (+ (* -1 m (expt ((D phi) t) 2) (r t))
    ;;     (* m (((expt D 2) r) t))
    ;;     ((D V) (r t)))
    ;;  (+ (* 2 m ((D r) t) ((D phi) t) (r t))
    ;;     (* m (((expt D 2) phi) t) (expt (r t) 2))))
    )

  (testing "generalized-LE"
    ;; (define ((L2harmonic m k) state)
    ;;   (let ((x (coordinate state))
    ;;        (a (acceleration state)))
    ;;     (+ (* (/ 1 2) m x a) (* (/ 1 2) k (square x)))))

    ;; (print-expression
    ;;  ((generalized-LE (L2harmonic 'm 'k))
    ;;   (up 't 'x 'v 'a 'j 'p)))
    ;; (+ (* a m) (* k x))


    ;; (pe ((generalized-LE
    ;;       (af/literal-function 'L (-> (UP Real Real Real) Real)))
    ;;      (up 't 'x 'v 'a)))
    ;; (+ (* a (((partial 2) ((partial 2) L)) (up t x v)))
    ;;    (* v (((partial 1) ((partial 2) L)) (up t x v)))
    ;;    (((partial 0) ((partial 2) L)) (up t x v))
    ;;    (* -1 (((partial 1) L) (up t x v))))


    ;; (pe ((generalized-LE
    ;;       (af/literal-function 'L (-> (UP Real Real Real Real) Real)))
    ;;      (up 't 'x 'v 'a 'j 'p)))
    ;; (+ (* (expt a 2) (((partial 2) ((partial 2) ((partial 3) L))) (up t x v a)))
    ;;    (* 2 a j (((partial 2) ((partial 3) ((partial 3) L))) (up t x v a)))
    ;;    (* 2 a v (((partial 1) ((partial 2) ((partial 3) L))) (up t x v a)))
    ;;    (* (expt j 2) (((partial 3) ((partial 3) ((partial 3) L))) (up t x v a)))
    ;;    (* 2 j v (((partial 1) ((partial 3) ((partial 3) L))) (up t x v a)))
    ;;    (* (expt v 2) (((partial 1) ((partial 1) ((partial 3) L))) (up t x v a)))
    ;;    (* 2 a (((partial 0) ((partial 2) ((partial 3) L))) (up t x v a)))
    ;;    (* a (((partial 1) ((partial 3) L)) (up t x v a)))
    ;;    (* -1 a (((partial 2) ((partial 2) L)) (up t x v a)))
    ;;    (* 2 j (((partial 0) ((partial 3) ((partial 3) L))) (up t x v a)))
    ;;    (* p (((partial 3) ((partial 3) L)) (up t x v a)))
    ;;    (* 2 v (((partial 0) ((partial 1) ((partial 3) L))) (up t x v a)))
    ;;    (* -1 v (((partial 1) ((partial 2) L)) (up t x v a)))
    ;;    (((partial 0) ((partial 0) ((partial 3) L))) (up t x v a))
    ;;    (* -1 (((partial 0) ((partial 2) L)) (up t x v a)))
    ;;    (((partial 1) L) (up t x v a)))
    ))

;; TODO some of this should live in the driven pendulum namespace I think?

(deftest transform-tests
  ;; (show-expression
  ;;  (velocity
  ;;   ((F->C p->r)
  ;;    (->local 't
  ;;            (coordinate-tuple 'r 'phi)
  ;;            (velocity-tuple 'rdot 'phidot)))))
  ;; (up (+ (* -1 r phidot (sin phi)) (* rdot (cos phi)))
  ;;     (+ (* r phidot (cos phi)) (* rdot (sin phi))))


  ;; (define (L-central-polar m V)
  ;;   (compose (L-central-rectangular m V)
  ;;           (F->C p->r)))

  ;; (show-expression
  ;;  ((L-central-polar 'm (af/literal-function 'V))
  ;;   (->local 't (coordinate-tuple 'r 'phi)
  ;;            (velocity-tuple 'rdot 'phidot))))
  ;; (+ (* (/ 1 2) m (expt phidot 2) (expt r 2))
  ;;    (* (/ 1 2) m (expt rdot 2))
  ;;    (* -1 (V r)))

  ;; ### Driven pendulum example

  ;; (define ((T-pend m l g ys) local)
  ;;   (let ((t (time local))
  ;;         (theta (coordinate local))
  ;;         (thetadot (velocity local)))
  ;;     (let ((ysdot (D ys)))
  ;;       (* (/ 1 2) m
  ;;          (+ (square (* l thetadot))
  ;;             (square (ysdot t))
  ;;             (* 2 (ysdot t) l (sin theta) thetadot))))))

  ;; (define ((V-pend m l g ys) local)
  ;;   (let ((t (time local))
  ;;         (theta (coordinate local)))
  ;;     (* m g (- (ys t) (* l (cos theta))))))

  ;; (define L-pend (- T-pend V-pend))

  ;; (show-expression
  ;;  ((L-pend 'm 'l 'g (af/literal-function 'y_s))
  ;;   (->local 't 'theta 'thetadot)))
  ;; (+ (* (/ 1 2) (expt l 2) m (expt thetadot 2))
  ;;    (* l m thetadot ((D y_s) t) (sin theta))
  ;;    (* g l m (cos theta))
  ;;    (* -1 g m (y_s t))
  ;;    (* (/ 1 2) m (expt ((D y_s) t) 2)))

  ;; (show-expression
  ;;  (((Lagrange-equations
  ;;     (L-pend 'm 'l 'g (af/literal-function 'y_s)))
  ;;    (af/literal-function 'theta))
  ;;   't))
  ;; (+ (* g l m (sin (theta t)))
  ;;    (* (expt l 2) m (((expt D 2) theta) t))
  ;;    (* l m (((expt D 2) y_s) t) (sin (theta t))))

;;; Same driven pendulum by coordinate transformation

  ;; (define ((Lf m g) local)
  ;;   (let ((q (coordinate local))
  ;;         (v (velocity local)))
  ;;     (let ((h (ref q 1)))
  ;;       (- (* (/ 1 2) m (square v)) (* m g h)))))

  ;; (define ((dp-coordinates l y_s) local)
  ;;   (let ((t (time local))
  ;;        (theta (coordinate local)))
  ;;     (let ((x (* l (sin theta)))
  ;;          (y (- (y_s t) (* l (cos theta)))))
  ;;       (coordinate-tuple x y))))

  ;; (define (L-pend m l g y_s)
  ;;   (compose (Lf m g)
  ;;            (F->C (dp-coordinates l y_s))))

  ;; (show-expression
  ;;  ((L-pend 'm 'l 'g (af/literal-function 'y_s))
  ;;   (->local 't 'theta 'thetadot)))
  ;; (+ (* (/ 1 2) (expt l 2) m (expt thetadot 2))
  ;;    (* l m thetadot (sin theta) ((D y_s) t))
  ;;    (* g l m (cos theta))
  ;;    (* -1 g m (y_s t))
  ;;    (* (/ 1 2) m (expt ((D y_s) t) 2)))

  ;; (show-expression
  ;;  (((Lagrange-equations
  ;;     (L-pend 'm 'l 'g (af/literal-function 'y_s)))
  ;;    (af/literal-function 'theta))
  ;;   't))
  ;; (+ (* g l m (sin (theta t)))
  ;;    (* (expt l 2) m (((expt D 2) theta) t))
  ;;    (* l m (((expt D 2) y_s) t) (sin (theta t))))
  )

;; categorize?
(deftest last-tests
  ;; (define (L3-central m Vr)
  ;;   (define (Vs local)
  ;;     (let ((r (ref (coordinate local) 0)))
  ;;       (Vr r)))
  ;;   (- (T3-spherical m) Vs))

  ;; (define ((ang-mom-z m) local)
  ;;   (let ((q (coordinate local))
  ;;         (v (velocity local)))
  ;;     (ref (cross-product q (* m v)) 2)))

  ;; (show-expression
  ;;  ((compose (ang-mom-z 'm) (F->C s->r))
  ;;   (->local 't
  ;;            (coordinate-tuple 'r 'theta 'phi)
  ;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
  ;; (* m (expt r 2) phidot (expt (sin theta) 2))

  ;; (show-expression
  ;;  ((Lagrangian->energy
  ;;    (L3-central 'm (af/literal-function 'V)))
  ;;   (->local 't
  ;;            (coordinate-tuple 'r 'theta 'phi)
  ;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
  ;; (+ (* (/ 1 2) m (expt r 2) (expt phidot 2) (expt (sin theta) 2))
  ;;    (* (/ 1 2) m (expt r 2) (expt thetadot 2))
  ;;    (* (/ 1 2) m (expt rdot 2))
  ;;    (V r))

  )

(deftest F->C-tests
  (testing "F->C correctly transforms local tuples of any arity you supply"
    (is (= '(up t
                (up (* r (cos phi))
                    (* r (sin phi)))
                (up (+ (* -1 r phidot (sin phi))
                       (* rdot (cos phi)))
                    (+ (* r phidot (cos phi))
                       (* rdot (sin phi))))
                (up (+ (* -1 r (expt phidot 2) (cos phi))
                       (* -1 r phidotdot (sin phi))
                       (* -2 rdot phidot (sin phi))
                       (* rdotdot (cos phi)))
                    (+ (* -1 r (expt phidot 2) (sin phi))
                       (* r phidotdot (cos phi))
                       (* 2 rdot phidot (cos phi))
                       (* rdotdot (sin phi)))))
           (simplify
            ((L/F->C L/p->r)
             (L/->local 't
                        (up 'r 'phi)
                        (up 'rdot 'phidot)
                        (up 'rdotdot 'phidotdot))))))))

(deftest interpolation
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

(deftest misc-tests
  (let [vs (L/velocity-tuple
            (L/velocity-tuple 'vx1 'vy1)
            (L/velocity-tuple 'vx2 'vy2))
        L1 (fn [[v1 v2]]
             (g/+ (g/* (/ 1 2) 'm1 (g/square v1))
                  (g/* (/ 1 2) 'm2 (g/square v2))))]
    (is (= '(down (down (down (down m1 0) (down 0 0))
                        (down (down 0 m1) (down 0 0)))
                  (down (down (down 0 0) (down m2 0))
                        (down (down 0 0) (down 0 m2))))
           (simplify
            (((g/expt D 2) L1) vs))))

    (is (= '(matrix-by-rows
             (up m1 0 0 0)
             (up 0 m1 0 0)
             (up 0 0 m2 0)
             (up 0 0 0 m2))
           (simplify
            (m/s->m vs (((g/expt D 2) L1) vs) vs))))))
