#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch9-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.calculus.curvature-test :refer [S2-Christoffel]]
            [emmy.env :as e :refer [+ - * /
                                    square sin expt zero?
                                    compose D partial
                                    up
                                    point
                                    R2-rect R3-rect
                                    define-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(define-coordinates [theta phi] e/S2-spherical)
(define-coordinates [t x y z] e/spacetime-rect)

(def S2-basis (e/coordinate-system->basis S2-spherical))
(def S2C (S2-Christoffel S2-basis theta))
(def sphere-Cartan (e/Christoffel->Cartan S2C))

(defn g-sphere [R]
  (fn [u v]
    (* (e/square R)
       (+ (* (dtheta u) (dtheta v))
          (* (e/compose (square sin) theta)
             (dphi u)
             (dphi v))))))

(defn metric->Lagrangian [metric coordsys]
  (fn L [[_ q qd]]
    (let [v (e/components->vector-field (fn [_] qd) coordsys)]
      ((* (/ 1 2) (metric v v)) ((point coordsys) q)))))

(defn Lagrange-explicit [L]
  (let [P ((partial 2) L)
        F ((partial 1) L)]
    (/ (- F (+ ((partial 0) P)
               (* ((partial 1) P) e/velocity)))
       ((partial 2) P))))

(deftest ch9-tests
  (testing "computing Christoffel, page 136"
    (is (= '(down
             (down (down 0 0)
                   (down 0 (* (expt R 2) (sin theta0) (cos theta0))))
             (down (down 0 (* (expt R 2) (sin theta0) (cos theta0)))
                   (down (* -1 (expt R 2) (sin theta0) (cos theta0))
                         0)))
           (simplify
            ((e/Christoffel->symbols
              (e/metric->Christoffel-1 (g-sphere 'R) S2-basis))
             ((point S2-spherical) (up 'theta0 'phi0)))))
        "Computing Christoffel coefficients, page 136"))

  (is (= '(down
           (down (up 0 0)
                 (up 0 (/ (cos theta0) (sin theta0))))
           (down (up 0 (/ (cos theta0) (sin theta0)))
                 (up (* -1 (sin theta0) (cos theta0)) 0)))
         (simplify
          ((e/Christoffel->symbols
            (e/metric->Christoffel-2 (g-sphere 'R) S2-basis))
           ((point S2-spherical) (up 'theta0 'phi0)))))
      "page 137")

  (is (= '(down (down (up 0 0 0) (up 0 0 0) (up 0 0 0))
                (down (up 0 0 0) (up 0 0 0) (up 0 0 0))
                (down (up 0 0 0) (up 0 0 0) (up 0 0 0)))
         (simplify
          (let [metric (e/literal-metric 'g R3-rect)
                q (e/typical-coords R3-rect)
                L2 (metric->Lagrangian metric R3-rect)]
            (+ (* (/ 1 2)
                  (((expt (partial 2) 2) (Lagrange-explicit L2))
                   (up 't q (e/corresponding-velocities q))))
               ((e/Christoffel->symbols
                 (e/metric->Christoffel-2 metric
                                          (e/coordinate-system->basis R3-rect)))
                ((point R3-rect) q))))))
      "page 138"))

(def L2
  (metric->Lagrangian (e/literal-metric 'm R2-rect)
                      R2-rect))
(defn L1 [state]
  (e/sqrt (* 2 (L2 state))))

(deftest arc-length-tests
  (testing "page 140"
    (is (= '(+ (* (m_00 (up x y)) (m_11 (up x y)))
               (* -1 (expt (m_01 (up x y)) 2)))
           (simplify
            (e/determinant
             (((partial 2) ((partial 2) L2))
              (up 't (up 'x 'y) (up 'vx 'vy))))))
        "mass matrix of L_2 is nonsingular")

    (is (zero?
         (simplify
          (e/determinant
           (((partial 2) ((partial 2) L1))
            (up 't (up 'x 'y) (up 'vx 'vy))))))
        "mass matrix of L_1 has determinant 0")

    (testing "page 141"
      (letfn [(L1 [state]
                (e/sqrt (square (e/velocity state))))]
        (is (= '(down
                 (/ (+ (* -1 ((D x) t) ((D y) t) (((expt D 2) y) t))
                       (* (((expt D 2) x) t) (expt ((D y) t) 2)))
                    (+ (* (expt ((D x) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2))))
                       (* (expt ((D y) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2))))))
                 (/ (+ (* (expt ((D x) t) 2) (((expt D 2) y) t))
                       (* -1 ((D x) t) (((expt D 2) x) t) ((D y) t)))
                    (+ (* (expt ((D x) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2))))
                       (* (expt ((D y) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2)))))))
               (simplify
                (((e/Lagrange-equations L1)
                  (up (e/literal-function 'x) (e/literal-function 'y)))
                 't)))
            "NOTE that the simplifier here and in the current version of
             scmutils can't figure out that the denominators are to the 3/2
             power.")))

    (e/with-literal-functions [x y f]
      (let [E1 (e/Euler-Lagrange-operator L1)]
        (is (= '(down 0 0)
               (simplify
                ((- (compose E1
                             (e/Gamma (up (compose x f)
                                          (compose y f))
                                      4))
                    (* (compose E1
                                (e/Gamma (up x y) 4)
                                f)
                       (D f)))
                 't)))
            "page 142"))

      (let [q (up x y)]
        (is (= '(down
                 (+ (* (m_00 (up (x (f t)) (y (f t)))) ((D x) (f t)) (((expt D 2) f) t))
                    (* (m_01 (up (x (f t)) (y (f t)))) ((D y) (f t)) (((expt D 2) f) t)))
                 (+ (* ((D x) (f t)) (m_01 (up (x (f t)) (y (f t)))) (((expt D 2) f) t))
                    (* ((D y) (f t)) (m_11 (up (x (f t)) (y (f t)))) (((expt D 2) f) t))))
               (simplify
                ((- (compose (e/Euler-Lagrange-operator L2)
                             (e/Gamma (compose q f) 4))
                    (* (compose (e/Euler-Lagrange-operator L2)
                                (e/Gamma q 4)
                                f)
                       (expt (D f) 2)))
                 't)))
            "page 143")))))

(def spacetime-rect-basis
  (e/coordinate-system->basis spacetime-rect))

(defn Newton-metric [_ _ c V]
  (let [a (+ 1 (* (/ 2 (square c))
                  (compose V (up x y z))))]
    (fn g [v1 v2]
      (+ (* -1 (square c) a (dt v1) (dt v2))
         (* (dx v1) (dx v2))
         (* (dy v1) (dy v2))
         (* (dz v1) (dz v2))))))

(defn Newton-connection [M G c V]
  (e/Christoffel->Cartan
   (e/metric->Christoffel-2 (Newton-metric M G c V)
                            spacetime-rect-basis)))

(def nabla
  (e/covariant-derivative
   (Newton-connection
    'M 'G 'c
    (e/literal-function 'V '(-> (UP Real Real Real) Real)))))

(deftest general-relativity-tests
  (is (= '(/ (+ (* (expt c 2) (((expt (partial 0) 2) V) (up y z t)))
                (* (expt c 2) (((expt (partial 1) 2) V) (up y z t)))
                (* (expt c 2) (((expt (partial 2) 2) V) (up y z t)))
                (* -1 (expt (((partial 0) V) (up y z t)) 2))
                (* 2 (V (up y z t)) (((expt (partial 0) 2) V) (up y z t)))
                (* 2 (V (up y z t)) (((expt (partial 1) 2) V) (up y z t)))
                (* 2 (V (up y z t)) (((expt (partial 2) 2) V) (up y z t)))
                (* -1 (expt (((partial 1) V) (up y z t)) 2))
                (* -1 (expt (((partial 2) V) (up y z t)) 2)))
             (+ (expt c 2) (* 2N (V (up y z t)))))
         (simplify
          (((e/Ricci nabla spacetime-rect-basis)
            d:dt d:dt)
           ((point spacetime-rect) (up 'x 'y 'z 't)))))
      "page 146")

  (testing "stress-energy tensor, page 147"
    (let [Tdust (fn [rho]
                  (fn T [w1 w2]
                    (* rho (w1 d:dt) (w2 d:dt))))
          V (e/literal-function 'V '(-> (UP Real Real Real) Real))
          g (Newton-metric 'M 'G 'c V)
          T_ij ((e/drop2 g spacetime-rect-basis) (Tdust 'rho))
          T ((e/trace2down g spacetime-rect-basis) T_ij)]
      (is (= '(+ (* (/ 1 2) (expt c 4) rho)
                 (* 2 (expt c 2) rho (V (up x y z)))
                 (* 2 rho (expt (V (up x y z)) 2)))
             (simplify
              ((- (T_ij d:dt d:dt) (* (/ 1 2) T (g d:dt d:dt)))
               ((point spacetime-rect) (up 't 'x 'y 'z)))))))))
