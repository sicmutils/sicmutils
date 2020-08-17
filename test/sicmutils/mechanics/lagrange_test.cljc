;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.mechanics.lagrange-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing use-fixtures]])
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]
            [sicmutils.matrix :as m]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]
            [sicmutils.mechanics.lagrange :as L]))

(use-fixtures :once hermetic-simplify-fixture)

(def ^:private near? (v/within 1e-8))

(deftest interpolation
  (testing "lagrange interpolation"
    #?(:clj
       (is (= [1/6 1/3 1/2 2/3 5/6] (L/linear-interpolants 0 1 5)))
       :cljs
       (is (every?
            (map near?
                 [(/ 1 6) (/ 1 3) (/ 1 2) (/ 2 3) (/ 5 6)]
                 (L/linear-interpolants 0 1 5)))))
    (let [f (L/Lagrange-interpolation-function [3 2 5 1] [1 2 3 4])]
      (is (= (f 1) 3))
      (is (= (f 2) 2))
      (is (= (f 3) 5))
      (is (= (f 4) 1)))
    (let [f (L/Lagrange-interpolation-function '[a b c] '[w x y])]
      (is (= 'a (g/simplify (f 'w)))))))

(deftest misc
  (let [vs (L/velocity-tuple
            (L/velocity-tuple 'vx1 'vy1)
            (L/velocity-tuple 'vx2 'vy2))
        L1 (fn [[v1 v2]]
             (g/+ (g/* (/ 1 2) 'm1 (g/square v1))
                  (g/* (/ 1 2) 'm2 (g/square v2))))]
    (is (= '(down (down (down (down m1 0) (down 0 0)) (down (down 0 m1) (down 0 0)))
                  (down (down (down 0 0) (down m2 0)) (down (down 0 0) (down 0 m2))))
           (g/simplify (((g/expt D 2) L1) vs))))
    (is (= '(matrix-by-rows [m1 0 0 0]
                            [0 m1 0 0]
                            [0 0 m2 0]
                            [0 0 0 m2])
           (g/simplify (m/s->m vs (((g/expt D 2) L1) vs) vs))))))

(deftest gamma-test
  (f/with-literal-functions [q]
    (is (= '(up t (q t) ((D q) t)) (g/simplify ((L/Gamma q) 't))))
    (is (= '(up t (q t) ((D q) t)) (g/simplify ((L/Gamma q 3) 't))))
    (is (= '(up t
                (q t)
                ((D q) t)
                (((expt D 2) q) t)
                (((expt D 3) q) t))
           (g/simplify ((L/Gamma q 5) 't))))
    (is (= '(+ (* #?(:clj 1/2 :cljs 0.5) (expt t 2) (((expt D 2) q) t))
               (* -1 t t1 (((expt D 2) q) t))
               (* #?(:clj 1/2 :cljs 0.5) (expt t1 2) (((expt D 2) q) t))
               (* -1 t ((D q) t))
               (* t1 ((D q) t))
               (q t))
           (g/simplify ((L/osculating-path ((L/Γ q 4) 't)) 't1))))
    (is (= '(up t (up t (x t) (y t)) (up 1 ((D x) t) ((D y) t)))
           (g/simplify
            (f/with-literal-functions [x y]
              ((L/Gamma (up identity x y)) 't)))))))

(deftest lagrange-equations
  (testing "moved-from-simplify"
    (let [xy (up (f/literal-function 'x) (f/literal-function 'y))
          LE (((L/Lagrange-equations
                (L/L-central-rectangular 'm (f/literal-function 'U))) xy) 't)]
      (is (= '(up x y) (g/simplify xy)))
      (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                          (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* m (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) y) t))
                          (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
             (g/simplify LE)))))

  (testing "basics"
    (f/with-literal-functions [q x y z]
      (let [Le (L/Lagrange-equations (L/L-free-particle 'm))
            literal-path q
            generic-path (up x y z)
            LeQ (Le literal-path)
            LeP (Le generic-path)]
        (is (= '(* m (((expt D 2) q) t))
               (g/simplify (LeQ 't))))
        (is (= '(down (* m (((expt D 2) x) t))
                      (* m (((expt D 2) y) t))
                      (* m (((expt D 2) z) t)))
               (g/simplify (LeP 't)))))))

  (testing "derivations"
    (f/with-literal-functions [q x y U r φ]
      (let [test-path (fn [t]
                        (up (g/+ (g/* 'a t) 'a0)
                            (g/+ (g/* 'b t) 'b0)
                            (g/+ (g/* 'c t) 'c0)))]
        (is (= (down 0 0 0)
               (((L/Lagrange-equations (L/L-free-particle 'm))
                 test-path)
                't))))
      (is (= '(* m (((expt D 2) q) t))
             (g/simplify (((L/Lagrange-equations (L/L-free-particle 'm)) q) 't))))

      (let [proposed-solution (fn [t] (g/* 'a (g/cos (g/+ (g/* 'ω t) 'φ))))
            lagrange-eqns (g/simplify (((L/Lagrange-equations (L/L-harmonic 'm 'k))
                                        proposed-solution)
                                       't))]
        (is (= '(+ (* -1 a m (expt ω 2) (cos (+ (* t ω) φ))) (* a k (cos (+ (* t ω) φ))))
               lagrange-eqns)))

      (is (= '(down (* m (((expt D 2) x) t))
                    (+ (* g m) (* m (((expt D 2) y) t))))
             (g/simplify (((L/Lagrange-equations (L/L-uniform-acceleration 'm 'g))
                           (up x y))
                          't))))

      (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* m (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) y) t)) (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
             (g/simplify (((L/Lagrange-equations (L/L-central-rectangular 'm U))
                           (up x y))
                          't))))

      (is (= '(down (+ (* -1 m (expt ((D φ) t) 2) (r t)) (* m (((expt D 2) r) t)) ((D U) (r t)))
                    (+ (* 2 m ((D φ) t) (r t) ((D r) t)) (* m (expt (r t) 2) (((expt D 2) φ) t))))
             (g/simplify (((L/Lagrange-equations (L/L-central-polar 'm U))
                           (up r φ))
                          't))))

      (is (= '(up (+ (* -1 r φdot (sin φ)) (* rdot (cos φ)))
                  (+ (* r φdot (cos φ)) (* rdot (sin φ))))
             (g/simplify
              (L/velocity
               ((L/F->C L/p->r)
                (L/->local 't (up 'r 'φ) (up 'rdot 'φdot))))))))))
