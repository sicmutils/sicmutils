;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns sicmutils.fdg.ch9-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e :refer [+ - * / square sin expt
                                         zero?
                                         compose D d freeze partial
                                         up down
                                         point chart
                                         R2-rect R3-rect S2-spherical
                                         let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.curvature-test :refer [S2-Christoffel]]
            [sicmutils.operator :as o]
            [sicmutils.value :as v]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(let-coordinates [[theta phi] S2-spherical]
  (def S2-basis (e/coordinate-system->basis S2-spherical))
  (def S2C (S2-Christoffel S2-basis theta))
  (def sphere-Cartan (e/Christoffel->Cartan S2C))

  (defn g-sphere [R]
    (fn [u v]
      (* (e/square R)
         (+ (* (dtheta u) (dtheta v))
            (* (e/compose (square sin) theta)
               (dphi u)
               (dphi v)))))))

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
  ;; `lower` and `raise` live in metric.cljc. TODO note that raise in the book
  ;; does not return a legit procedure->vector-field. Probably will break the
  ;; scheme.


  ;; TODO all setup here is missing, or assumed to come from the previous
  ;; section again.
  (testing "computing Christoffel, page 136"
    (is (= '(down
             (down (down 0 0)
                   (down 0 (* (expt R 2) (cos theta0) (sin theta0))))
             (down
              (down 0 (* (expt R 2) (cos theta0) (sin theta0)))
              (down (* -1 (expt R 2) (cos theta0) (sin theta0))
                    0)))
           (simplify
            ((e/Christoffel->symbols
              (e/metric->Christoffel-1 (g-sphere 'R) S2-basis))
             ((point S2-spherical) (up 'theta0 'phi0)))))
        "Computing Christoffel coefficients, page 136"))

  (is (= '(down (down (up 0 0)
                      (up 0 (/ (cos theta0) (sin theta0))))
                (down (up 0 (/ (cos theta0) (sin theta0)))
                      (up (* -1 (cos theta0) (sin theta0)) 0)))
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
      "page 138")

  (testing "Kinetic energy or Arc Length"
    )
  )


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
        (is (= '(down (+ (* (m_00 (up (x (f t)) (y (f t)))) ((D x) (f t)) (((expt D 2) f) t))
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

;; TODO the system did not say to install spacetime coordinates!

(let-coordinates [[x y z t] e/spacetime-rect]
  ;; TODO this was missing!
  (def spacetime-rect-basis
    (e/coordinate-system->basis spacetime-rect))

  (defn Newton-metric [M G c V]
    (let [a
          (+ 1 (* (/ 2 (square c))
                  (compose V (up x y z))))]
      (fn g [v1 v2]
        (+ (* -1 (square c) a (dt v1) (dt v2))
           (* (dx v1) (dx v2))
           (* (dy v1) (dy v2))
           (* (dz v1) (dz v2))))))

  (defn Newton-connection [M G c V]
    (e/Christoffel->Cartan
     (e/metric->Christoffel-2 (Newton-metric M G c V)
                              spacetime-rect-basis))))
(def nabla
  (e/covariant-derivative
   (Newton-connection
    'M 'G 'c
    (e/literal-function 'V '(-> (UP Real Real Real) Real)))))

(deftest general-relativity-tests
  ;; NOTE that this is a bug... the variable order should be x y z t. In the
  ;; book it is t x y z.

  ;; TODO this should just use `space-time-rect-basis` from above.
  #_(let-coordinates [[x y z t] e/spacetime-rect]
      (simplify
       (((e/Ricci nabla spacetime-rect-basis)
         d:dt d:dt)
        ((point spacetime-rect) (up 'x 'y 'z 't)))))

  (testing "OKAY, here we go, this is a clue! This one takes 158 seconds. Why is
  this slow? It only takes 135 seconds on my old machine. At least in this case
  we get the correct answer."
    (time
     (let-coordinates [[x y z t] e/spacetime-rect]
       (simplify
        (time (((e/Ricci nabla spacetime-rect-basis)
                d:dt d:dx)
               ((point spacetime-rect) (up 'x 'y 'z 't)))))))))
