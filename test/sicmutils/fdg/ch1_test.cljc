;;
;; Copyright © 2017 Colin Smith.
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
;;

(ns sicmutils.fdg.ch1-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [sicmutils.env :as e :refer [+ - * /
                                         D compose
                                         up down
                                         sin cos square
                                         R1-rect R2-rect
                                         chart point
                                         let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(defn Lfree
  [mass]
  (fn [[t q v]]
    (* (/ 1 2) mass (square v))))

(defn sphere->R3
  [R]
  (fn [[t [theta phi] v]]
    (up (* R (sin theta) (cos phi))                         ; x
        (* R (sin theta) (sin phi))                         ; y
        (* R (cos theta)))))                                ; z

(defn Lsphere
  [m R]
  (compose (Lfree m) (e/F->C (sphere->R3 R))))

(defn L2
  [mass metric]
  (fn [place velocity]
    (* (/ 1 2) mass ((metric velocity velocity) place))))

(defn Lc [mass metric coordsys]
  (fn [[_ x v]]
    (let [e (e/coordinate-system->vector-basis coordsys)]
      ((L2 mass metric) ((point coordsys) x) (* e v)))))

(deftest chapter-one-tests
  (is (= '(+ (* (/ 1 2) (expt R 2) m (expt phidot 2) (expt (sin theta) 2))
             (* (/ 1 2) (expt R 2) m (expt thetadot 2)))
         (simplify
          ((Lsphere 'm 'R)
           (up 't (up 'theta 'phi) (up 'thetadot 'phidot)))))
      "p4")

  (let [the-metric (e/literal-metric 'g R2-rect)
        L (Lc 'm the-metric R2-rect)]
    (is (= '(+ (* (/ 1 2) m (expt vx 2) (g_00 (up x y)))
               (* m vx vy (g_01 (up x y)))
               (* (/ 1 2) m (expt vy 2) (g_11 (up x y))))
           (simplify
            (L (up 't (up 'x 'y) (up 'vx 'vy)))))
        "p7: Compare this result with equation (1.3)")

    (let [gamma (e/literal-manifold-map 'q R1-rect R2-rect)
          coordinate-path (compose (chart R2-rect)
                                   gamma
                                   (point R1-rect))]
      (is (= (up '(q↑0 t) '(q↑1 t))
             ((chart R2-rect) (gamma ((point R1-rect) 't))))
          "page 8")

      (is (= (up '(q↑0 t) '(q↑1 t))
             (coordinate-path 't)))

      ;; Now we can compute the residuals of the Euler-Lagrange equations, but
      ;; we get a large messy expression that we will not show.
      (let-coordinates [t R1-rect]
        (let [Lagrange-residuals (((e/Lagrange-equations L) coordinate-path) 't)
              R2-basis (e/coordinate-system->basis R2-rect)
              Cartan
              (e/Christoffel->Cartan
               (e/metric->Christoffel-2 the-metric R2-basis))
              geodesic-equation-residuals (((((e/covariant-derivative Cartan gamma) d:dt)
                                             ((e/differential gamma) d:dt))
                                            (chart R2-rect))
                                           ((point R1-rect) 't))
              metric-components (e/metric->components the-metric R2-basis)]
          (is (= '(down 0 0)
                 (simplify
                  (- Lagrange-residuals
                     (* (* 'm (metric-components (gamma ((point R1-rect) 't))))
                        geodesic-equation-residuals))))
              "p10: This establishes that for a 2-dimensional space the
               Euler-Lagrange equations are equivalent to the geodesic
               equations."))))))
