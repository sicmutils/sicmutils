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
;;

(ns sicmutils.fdg.einstein-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.indexed :as ci]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.env :as e :refer [+ - * / expt sin
                                         literal-function
                                         with-literal-functions
                                         spacetime-rect spacetime-sphere
                                         compose square point up
                                         let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

;; Einstein Field Equations

(defn Einstein [coordinate-system metric-tensor]
  (let [basis      (e/coordinate-system->basis coordinate-system)
        connection (e/Christoffel->Cartan
                    (e/metric->Christoffel-2 metric-tensor basis))
        nabla      (e/covariant-derivative connection)
        Ricci-tensor (e/Ricci nabla basis)
        Ricci-scalar ((e/trace2down metric-tensor basis) Ricci-tensor)]
    (-> (fn Einstein-tensor [v1 v2]
          (- (Ricci-tensor v1 v2)
             (* (/ 1 2)
                Ricci-scalar
                (metric-tensor v1 v2))))
        (ci/with-argument-types
          [::vf/vector-field
           ::vf/vector-field]))))

(defn Einstein-field-equation [coordinate-system K]
  (fn [metric-tensor Lambda stress-energy-tensor]
    (let [Einstein-tensor (Einstein coordinate-system metric-tensor)]
      (let [EFE-residuals (- (+ Einstein-tensor (* Lambda metric-tensor))
                             (* K stress-energy-tensor))]
        (ci/with-argument-types
          EFE-residuals
          [::vf/vector-field
           ::vf/vector-field])))))

;; (define K (/ (* 8 :pi :G) (expt :c 4)))
;; (& 2.076115391974129e-43 (/ (expt second 2) (* kilogram meter)))

;; Some metrics

(let-coordinates [[t x y z] spacetime-rect]
  (defn Minkowski-metric [c]
    (letfn [(g [v1 v2]
              (+ (* -1 (square c) (dt v1) (dt v2))
                 (* (dx v1) (dx v2))
                 (* (dy v1) (dy v2))
                 (* (dz v1) (dz v2))))]
      (ci/with-argument-types
        g
        [::vf/vector-field
         ::vf/vector-field]))))

(let-coordinates [[t r theta phi] spacetime-sphere]
  (defn Schwarzschild-metric [c G M]
    (let [a (- 1 (* (/ 2 (square c))
                    (/ (* G M) r)))
          g (fn [v1 v2]
              (+ (*  -1 (square c) a (dt v1) (dt v2))
                 (* (/ 1 a) (dr v1) (dr v2))
                 (* (square r)
                    (+ (* (dtheta v1) (dtheta v2))
                       (* (square (sin theta))
                          (dphi v1) (dphi v2))))))]
      (ci/with-argument-types
        g
        [::vf/vector-field
         ::vf/vector-field])))

  ;; Friedmann-Lemaître-Robertson-Walker metric

  (defn FLRW-metric [c k R]
    (let [a (/ (square (compose R t))
               (- 1 (* k (square r))))
          b (square (* (compose R t) r))
          g (fn [v1 v2]
              (+ (*  -1 (square c) (dt v1) (dt v2))
                 (* a (dr v1) (dr v2))
                 (* b
                    (+ (* (dtheta v1) (dtheta v2))
                       (* (square (sin theta))
                          (dphi v1) (dphi v2))))))]
      (ci/with-argument-types
        g
        [::vf/vector-field
         ::vf/vector-field])))

  ;; ## Stress-Energy tensors
  ;;
  ;; Note that these are (2,0) tensors.

  (defn Tperfect-fluid [rho p c metric]
    (let [basis (e/coordinate-system->basis spacetime-sphere)
          es (e/basis->vector-basis basis)
          inverse-metric (e/metric:invert metric basis)
          T (fn [w1 w2]
              (+ (* (+ (compose rho t)
                       (/ (compose p t)
                          (square c)))
                    (w1 d:dt)
                    (w2 d:dt))
                 (* (compose p t)
                    (inverse-metric w1 w2))))]
      (ci/with-argument-types
        T
        [::ff/oneform-field
         ::ff/oneform-field])))

  (comment
    (deftest einstein-field-equations-benchmark
      (testing "NOTE: These are a bit slow to include in the default test suite.
      But they are important and they work!"
        (with-literal-functions [R rho p]
          (let [basis  (e/coordinate-system->basis spacetime-sphere)
                g      (FLRW-metric 'c 'k R)
                T_ij   ((e/drop2 g basis) (Tperfect-fluid rho p 'c g))
                [d:dt d:dr] (e/coordinate-system->vector-basis spacetime-sphere)
                K (/ (* 8 'pi 'G) (expt 'c 4))]

            (testing "first challenge (89s)"
              (is (= '(/ (+ (* -8 G pi (expt (R t) 2) (rho t))
                            (* -1 Lambda (expt c 2) (expt (R t) 2))
                            (* 3 (expt c 2) k)
                            (* 3 (expt ((D R) t) 2)))
                         (expt (R t) 2))
                     (simplify
                      ((((Einstein-field-equation spacetime-sphere K)
                         g 'Lambda T_ij)
                        d:dt d:dt)
                       ((point spacetime-sphere) (up 't 'r 'theta 'phi)))))))

            (testing "second challenge (89s)"
              (is (= '(/ (+ (* -1 Lambda (expt c 4) (expt (R t) 2))
                            (* 8 G pi (expt (R t) 2) (p t))
                            (* (expt c 4) k)
                            (* 2 (expt c 2) (R t) (((expt D 2) R) t))
                            (* (expt c 2) (expt ((D R) t) 2)))
                         (+ (* (expt c 4) k (expt r 2)) (* -1 (expt c 4))))
                     (simplify
                      ((((Einstein-field-equation spacetime-sphere
                                                  (/ (* 8 'pi 'G) (expt 'c 4)))
                         g 'Lambda T_ij)
                        d:dr d:dr)
                       ((point spacetime-sphere) (up 't 'r 'theta 'phi)))))))))

        (testing "Conservation of energy-momentum (13s)"
          (with-literal-functions [R p rho]
            (let [metric (FLRW-metric 'c 'k R)
                  basis (e/coordinate-system->basis spacetime-sphere)
                  nabla (e/covariant-derivative
                         (e/Christoffel->Cartan
                          (e/metric->Christoffel-2 metric basis)))
                  es (e/basis->vector-basis basis)]
              (is (= '[(/ (+ (* -1 (expt c 2) (R t) ((D rho) t))
                             (* -3 (expt c 2) (rho t) ((D R) t))
                             (* -3 ((D R) t) (p t))) (R t))
                       0 0 0]
                     (map (fn [i]
                            (simplify
                             ((e/contract
                               (fn [ej wj]
                                 (* (metric ej (nth es i))
                                    (e/contract
                                     (fn [ei wi]
                                       (((nabla ei)
                                         (Tperfect-fluid rho p 'c metric))
                                        wj
                                        wi))
                                     basis)))
                               basis)
                              ((point spacetime-sphere) (up 't 'r 'theta 'phi)))))
                          (range 4))))))))))

  (deftest einstein-field-equations-fast
    (testing "final challenge, actually fast enough to enable"
      (with-literal-functions [R p rho]
        (let [metric (FLRW-metric 'c 'k R)
              basis (e/coordinate-system->basis spacetime-sphere)
              nabla (e/covariant-derivative
                     (e/Christoffel->Cartan
                      (e/metric->Christoffel-2 metric basis)))
              ws    (e/basis->oneform-basis basis)]
          (is (= ['(/ (+ (* (expt c 2) ((D rho) t) (R t))
                         (* 3 (expt c 2) ((D R) t) (rho t))
                         (* 3 ((D R) t) (p t)))
                      (* (expt c 2) (R t)))
                  0 0 0]
                 (map (fn [i]
                        (simplify
                         ((e/contract
                           (fn [ei wi]
                             (((nabla ei)
                               (Tperfect-fluid rho p 'c metric))
                              (nth ws i)
                              wi))
                           basis)
                          ((point spacetime-sphere) (up 't 'r 'theta 'phi)))))
                      (range 4)))))))))
