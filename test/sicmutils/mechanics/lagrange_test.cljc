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

(ns sicmutils.mechanics.lagrange-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
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

(deftest gamma-test
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

(deftest misc
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
             (L/acceleration local)))))

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
           (v/freeze
            (simplify
             (m/s->m vs (((g/expt D 2) L1) vs) vs)))))))

(deftest lagrange-equations
  (testing "moved-from-simplify"
    (let [xy (up (f/literal-function 'x) (f/literal-function 'y))
          LE (((L/Lagrange-equations
                (L/L-central-rectangular 'm (f/literal-function 'U))) xy) 't)]
      (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                          (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* m (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) y) t))
                          (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
             (v/freeze
              (simplify LE))))))

  (testing "basics"
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
               (simplify (LeP 't)))))))

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
             (simplify (((L/Lagrange-equations (L/L-free-particle 'm)) q) 't))))

      (let [proposed-solution (fn [t] (g/* 'a (g/cos (g/+ (g/* 'ω t) 'φ))))
            lagrange-eqns (simplify (((L/Lagrange-equations (L/L-harmonic 'm 'k))
                                      proposed-solution)
                                     't))]
        (is (= '(+ (* -1 a m (expt ω 2) (cos (+ (* t ω) φ))) (* a k (cos (+ (* t ω) φ))))
               lagrange-eqns)))

      (is (= '(down (* m (((expt D 2) x) t))
                    (+ (* g m) (* m (((expt D 2) y) t))))
             (simplify (((L/Lagrange-equations (L/L-uniform-acceleration 'm 'g))
                         (up x y))
                        't))))

      (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                          (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* m (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) y) t))
                          (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2)
                                (expt (y t) 2)))))
             (simplify (((L/Lagrange-equations (L/L-central-rectangular 'm U))
                         (up x y))
                        't))))

      (is (= '(down (+ (* -1 m (r t) (expt ((D φ) t) 2))
                       (* m (((expt D 2) r) t))
                       ((D U) (r t)))
                    (+ (* m (expt (r t) 2) (((expt D 2) φ) t))
                       (* 2 m (r t) ((D φ) t) ((D r) t))))
             (simplify (((L/Lagrange-equations (L/L-central-polar 'm U))
                         (up r φ))
                        't))))

      (testing "F->C correctly transforms local tuples of any arity you supply"
        (is (= '(up t
                    (up (* r (cos φ))
                        (* r (sin φ)))
                    (up (+ (* -1 r φdot (sin φ))
                           (* rdot (cos φ)))
                        (+ (* r φdot (cos φ))
                           (* rdot (sin φ))))
                    (up (+ (* -1 r (expt φdot 2) (cos φ))
                           (* -1 r φdotdot (sin φ))
                           (* -2 rdot φdot (sin φ))
                           (* rdotdot (cos φ)))
                        (+ (* -1 r (expt φdot 2) (sin φ))
                           (* r φdotdot (cos φ))
                           (* 2 rdot φdot (cos φ))
                           (* rdotdot (sin φ)))))
               (simplify
                ((L/F->C L/p->r)
                 (L/->local 't
                            (up 'r 'φ)
                            (up 'rdot 'φdot)
                            (up 'rdotdot 'φdotdot))))))))))
