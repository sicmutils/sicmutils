;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all :exclude [function?]]
            [sicmutils
             [generic :refer :all]
             [function :refer :all]
             [numbers]
             [simplify :refer [hermetic-simplify-fixture]]
             [structure :refer :all]]
            [sicmutils.mechanics.lagrange :refer :all]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest interpolation
  (testing "lagrange interpolation"
    (is (= [1/6 1/3 1/2 2/3 5/6] (linear-interpolants 0 1 5)))
    (let [f (Lagrange-interpolation-function [3 2 5 1] [1 2 3 4])]
      (is (= (f 1) 3))
      (is (= (f 2) 2))
      (is (= (f 3) 5))
      (is (= (f 4) 1)))
    (let [f (Lagrange-interpolation-function '[a b c] '[w x y])]
      (is (= 'a (simplify (f 'w)))))
    ))

(deftest gamma-test
  (with-literal-functions [q]
    (is (= '(up t (q t) ((D q) t)) (simplify ((Γ q) 't))))
    (is (= '(up t (q t) ((D q) t)) (simplify ((Γ q 3) 't))))
    (is (= '(up t
                (q t)
                ((D q) t)
                (((expt D 2) q) t)
                (((expt D 3) q) t))
           (simplify ((Γ q 5) 't))))
    (is (= '(+ (* 1/2 (((expt D 2) q) t) (expt t 2))
               (* -1 (((expt D 2) q) t) t t1)
               (* 1/2 (((expt D 2) q) t) (expt t1 2))
               (* -1 ((D q) t) t)
               (* ((D q) t) t1) (q t))
           (simplify ((osculating-path ((Γ q 4) 't)) 't1))))
    (is (= '(up t (up t (x t) (y t)) (up 1 ((D x) t) ((D y) t)))
           (simplify
            (with-literal-functions [x y]
              ((Gamma (up identity x y)) 't)))))))

(deftest lagrange-equations
  (testing "basics"
    (let [Le (Lagrange-equations (L-free-particle 'm))
          literal-path (literal-function 'q)
          generic-path (up (literal-function 'x)
                           (literal-function 'y)
                           (literal-function 'z))
          LeQ (Le literal-path)
          LeP (Le generic-path)]
      (is (= '(* (((expt D 2) q) t) m)
             (simplify (LeQ 't))))
      (is (= '(down
               (* (((expt D 2) x) t) m)
               (* (((expt D 2) y) t) m)
               (* (((expt D 2) z) t) m))
             (simplify (LeP 't))))))
  (testing "derivations"
    (with-literal-functions [q x y U r φ]
      (let [test-path (fn [t]
                        (up (+ (* 'a t) 'a0)
                            (+ (* 'b t) 'b0)
                            (+ (* 'c t) 'c0)))]
        (is (= (down 0 0 0)
               (((Lagrange-equations (L-free-particle 'm))
                 test-path)
                't))))
      (is (= '(* (((expt D 2) q) t) m)
             (simplify (((Lagrange-equations (L-free-particle 'm)) q) 't))))
      (let [proposed-solution (fn [t]
                                (* 'a (cos (+ (* 'ω t) 'φ))))
            lagrange-eqns (simplify (((Lagrange-equations (L-harmonic 'm 'k))
                                      proposed-solution)
                                     't))]
        (is (= '(+ (* -1 (cos (+ (* t ω) φ)) a m (expt ω 2))
                   (* (cos (+ (* t ω) φ)) a k))
               lagrange-eqns)))
      (is (= '(down (* (((expt D 2) x) t) m)
                    (+ (* (((expt D 2) y) t) m) (* g m)))
             (simplify (((Lagrange-equations (L-uniform-acceleration 'm 'g))
                         (up x y))
                        't))))
      (is (= '(down (/ (+ (* (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))) m)
                          (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                    (/ (+ (* (((expt D 2) y) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))) m)
                          (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                       (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
             (simplify (((Lagrange-equations (L-central-rectangular 'm U))
                         (up x y))
                        't))))
      (is (= '(down (+ (* -1 (expt ((D φ) t) 2) (r t) m)
                       (* (((expt D 2) r) t) m)
                       ((D U) (r t)))
                    (+ (* 2 ((D φ) t) (r t) ((D r) t) m)
                       (* (expt (r t) 2) (((expt D 2) φ) t) m)))
             (simplify (((Lagrange-equations (L-central-polar 'm U))
                         (up r φ))
                        't))))

      (is (= '(up (+ (* -1 (sin φ) r φdot) (* (cos φ) rdot))
                  (+ (* (cos φ) r φdot) (* (sin φ) rdot)))
             (simplify (velocity ((F->C p->r)
                                  (->local 't (up 'r 'φ) (up 'rdot 'φdot))))))))
    ))
