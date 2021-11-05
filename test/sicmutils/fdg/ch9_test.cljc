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
                                         D d freeze partial
                                         up down
                                         point chart
                                         R3-rect S2-spherical
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
