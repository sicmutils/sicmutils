#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch1-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [emmy.env :as e :refer [- * /
                                    compose
                                    up
                                    sin cos square
                                    R2-rect
                                    chart point
                                    define-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(defn Lfree
  [mass]
  (fn [[_ _ v]]
    (* (/ 1 2) mass (square v))))

(defn sphere->R3
  [R]
  (fn [[_ [theta phi]]]
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


(define-coordinates t e/R1-rect)

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
             equations.")))))
