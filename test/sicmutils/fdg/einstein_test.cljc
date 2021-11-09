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
            [sicmutils.calculus.coordinate :as coord]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.indexed :as ci]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.polynomial.gcd :as pg]
            [sicmutils.env :as e :refer [+ - * / expt sin let-coordinates
                                         literal-function
                                         with-literal-functions
                                         spacetime-rect spacetime-sphere
                                         compose square point up]
             #?@(:cljs [:include-macros true])]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
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

  (deftest einstein-field-equations
    (comment
      (with-literal-functions [R rho p]
        (let [basis  (e/coordinate-system->basis spacetime-sphere)
              g      (FLRW-metric 'c 'k R)
              T_ij   ((e/drop2 g basis) (Tperfect-fluid rho p 'c g))
              [d:dt d:dr] (e/coordinate-system->vector-basis spacetime-sphere)
              K (/ (* 8 'pi 'G) (expt 'c 4))]

          (testing "first challenge"
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

          (testing "second challenge"
            (is (= '(/ (+ (* -1 Lambda (expt c 4) (expt (R t) 2))
                          (* 8 G pi (expt (R t) 2) (p t))
                          (* (expt c 4) k)
                          (* (expt c 2) (expt ((D R) t) 2))
                          (* 2 (expt c 2) (R t) (((expt D 2) R) t)))
                       (+ (* (expt c 4) k (expt r 2)) (* -1 (expt c 4))))
                   (simplify
                    ((((Einstein-field-equation spacetime-sphere
                                                (/ (* 8 'pi 'G) (expt 'c 4)))
                       g 'Lambda T_ij)
                      d:dr d:dr)
                     ((point spacetime-sphere) (up 't 'r 'theta 'phi))))))))))

    ;; TRUE!
    (comment
      (testing "Conservation of energy-momentum"
        (with-literal-functions [R p rho]
          (let [metric (FLRW-metric 'c 'k R)
                basis (e/coordinate-system->basis spacetime-sphere)
                nabla (e/covariant-derivative
                       (e/Christoffel->Cartan
                        (e/metric->Christoffel-2 metric basis)))
                es (e/basis->vector-basis basis)]
            (is (= '[(/ (+ (* -3 (expt c 2) ((D R) t) (rho t))
                           (* -1 (expt c 2) (R t) ((D rho) t))
                           (* -3 ((D R) t) (p t)))
                        (R t))
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
                        (range 4))))))))

    ;; TRUE!
    (comment
      (with-literal-functions [R p rho]
        (let [metric (FLRW-metric 'c 'k R)
              basis (e/coordinate-system->basis spacetime-sphere)
              nabla (e/covariant-derivative
                     (e/Christoffel->Cartan
                      (e/metric->Christoffel-2 metric basis)))
              ws    (e/basis->oneform-basis basis)]
          (is (= ['(/ (+ (* 3 (expt c 2) ((D R) t) (rho t))
                         (* (expt c 2) (R t) ((D rho) t))
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


(comment

  ;; error repro, faster!

  (p :with-setup
     (with-literal-functions [R p rho]
       (let [metric (FLRW-metric 'c 'k R)
             basis (e/coordinate-system->basis spacetime-sphere)
             nabla (e/covariant-derivative
                    (e/Christoffel->Cartan
                     (e/metric->Christoffel-2 metric basis)))
             [dx0] (e/basis->oneform-basis basis)]
         ;; okay... so the metric itself seems okay, wtf???
         (try
           (p :simplifying
              (simplify
               (p :inside-call ((e/contract
                                 (fn [ei wi]
                                   (prn ei wi)
                                   (((nabla ei)
                                     (Tperfect-fluid rho p 'c metric))
                                    dx0
                                    wi))
                                 basis)
                                ((point spacetime-sphere) (up 't 'r 'theta 'phi))))))
           (catch Exception e (def donkey e))))))

  (let-coordinates [[t r theta phi] spacetime-sphere]
    (defn FLRW-metric-stripped [c k R]
      (let [a (/ (square (compose R t))
                 (- 1 (* k (square r))))
            b (square (* (compose R t) r))
            g (fn [v1 v2]
                (fn [_] 'x)
                #_(+ (*  -1 (square c) (dt v1) (dt v2))
                     (* a (dr v1) (dr v2))
                     (* b
                        (+ (* (dtheta v1) (dtheta v2))
                           (* (square (sin theta))
                              (dphi v1) (dphi v2))))))]
        (ci/with-argument-types
          g
          [::vf/vector-field
           ::vf/vector-field])))

    (defn Tperfect-fluid-stripped [rho p c metric]
      (let [#_#_basis (e/coordinate-system->basis spacetime-sphere)
            #_#_es (e/basis->vector-basis basis)
            #_#_inverse-metric (e/metric:invert metric basis)
            T ]
        (ci/with-argument-types
          T
          [::ff/oneform-field
           ::ff/oneform-field])))

    ;; even simplier:
    (with-literal-functions [R]
      (let [metric (FLRW-metric 'c 'k R)
            basis (e/coordinate-system->basis spacetime-sphere)
            nabla (e/covariant-derivative
                   (e/Christoffel->Cartan
                    (e/metric->Christoffel-2 metric basis)))
            [d:dx0 d:dx1 d:dx2 d:dx3] (e/basis->vector-basis basis)
            [dx0 dx1 dx2 dx3] (e/basis->oneform-basis basis)

            ei d:dx1
            wi dx1]
        ;; okay... so the metric itself seems okay, wtf???
        (try
          (clojure.pprint/pprint
           (second (e/expression-of
                    ((((nabla ei)
                       (fn [w1 w2] (fn [_] 1)))
                      dx0
                      wi)
                     ((point spacetime-sphere) (up 't 'r 'theta 'phi))))))
          (catch Exception e (def donkey e)))))))


(comment
  ;; simplier still... AND YES, we have a repro!!!!! If you use the double
  ;; let-coordinates bindings, then we get the major problem. If they are
  ;; still active, basically, you are good to go. But if they are no longer
  ;; active...
  (let-coordinates [[t r theta phi] spacetime-sphere]
    (let [FLRW-metric (fn [c k R]
                        (let [a (/ (square (compose R t))
                                   (- 1 (* k (square r))))
                              b (square (* (compose R t) r))
                              g (fn [v1 v2]
                                  #_(* a (dr v1) (dr v2))
                                  (+
                                   (*  -1 (square c) (dt v1) (dt v2))
                                   (* a (dr v1) (dr v2))
                                   (* b
                                      (+ (* (dtheta v1) (dtheta v2))
                                         (* (square (sin theta))
                                            (dphi v1) (dphi v2))))))]
                          (ci/with-argument-types
                            g
                            [::vf/vector-field
                             ::vf/vector-field])))]
      (let-coordinates [[t r theta phi] spacetime-sphere]
        (with-literal-functions [R]
          (let [metric (FLRW-metric 'c 'k R)
                basis (e/coordinate-system->basis spacetime-sphere)
                [d:dx0 d:dx1 d:dx2 d:dx3] (e/basis->vector-basis basis)
                [dx0 dx1 dx2 dx3] (e/basis->oneform-basis basis)
                pt ((point spacetime-sphere) (up 't 'r 'theta 'phi))]
            ;; okay... so the metric itself seems okay, wtf???
            (clojure.pprint/pprint
             (e/expression-of
              ((metric d:dx1 d:dx1) pt))))))))


  ;; So this is a BIG problem - if they are actually getting CREATED as
  ;; different things... we want it to be the case that dr == dr2 if you use
  ;; `let-coordinates` twice. No good otherwise.
  ;;
  ;; Here is where I am at now.
  (let-coordinates [[t r theta phi] e/spacetime-sphere]
    (let [FLRW-metric (fn [c k R]
                        (let [a (/ (square (compose R t))
                                   (- 1 (* k (square r))))
                              b (square (* (compose R t) r))
                              g (fn [v1 v2]
                                  (+
                                   (*  -1 (square c) (dt v1) (dt v2))
                                   (* a (dr v1) (dr v2))
                                   (* b
                                      (+ (* (dtheta v1) (dtheta v2))
                                         (* (square (sin theta))
                                            (dphi v1) (dphi v2))))))]
                          (ci/with-argument-types
                            g
                            [::vf/vector-field
                             ::vf/vector-field])))]
      (let-coordinates [[t2 r2 theta2 phi2] e/spacetime-sphere]
        (with-literal-functions [R]
          (let [metric (FLRW-metric 'c 'k R)
                basis (e/coordinate-system->basis e/spacetime-sphere)
                pt ((point e/spacetime-sphere) (up 't 'r 'theta 'phi))]
            ;; okay... so the metric itself seems okay, wtf???
            (clojure.pprint/pprint
             (v/freeze
              ((metric d:dr d:dr) pt))))))))


  ;; that all works. Now, the hardre
  )

(comment
  ;; BUT AGAIN for some fucking reason!!! the following works, but what I was
  ;; doing BEFORE did not!
  (let-coordinates [[t r theta phi] spacetime-sphere]
    (with-literal-functions [R]
      (let [FLRW-metric (fn [c k R]
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
            metric (FLRW-metric 'c 'k R)
            basis (e/coordinate-system->basis e/spacetime-sphere)
            nabla (e/covariant-derivative
                   (e/Christoffel->Cartan
                    (e/metric->Christoffel-2 metric basis)))
            [d:dx0 d:dx1 d:dx2 d:dx3] (e/basis->vector-basis basis)
            [dx0 dx1 dx2 dx3] (e/basis->oneform-basis basis)
            ei d:dx1
            wi dx1]
        ;; okay... so the metric itself seems okay, wtf???
        (try
          (clojure.pprint/pprint
           (simplify
            ((((nabla ei)
               (fn [w1 w2] (fn [_] 1)))
              dx0
              wi)
             ((point spacetime-sphere) (up 't 'r 'theta 'phi)))))
          (catch Exception e (def donkey e))))))

  )

;; Okay, I figured it out. The whole bullshit is related to the
;; `let-coordinates` thing... When it rebound the coordinate system it forced a
;; new entry in the cache for the point. And I guess when I try to go backward
;; that is no good!!
;;
;; SO for GJS the goal is to figure out why everything


#_(= (+ (* -8 G pi (rho t))
        (* -1 (expt c 2) Lambda)
        (/ (* 3 k (expt c 2)) (expt (R t) 2))
        (/ (* 3 (expt ((D R) t) 2)) (expt (R t) 2)))

     )
