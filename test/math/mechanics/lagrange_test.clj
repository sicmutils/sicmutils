;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.mechanics.lagrange-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.mechanics.lagrange :refer :all]
            [math.function :refer :all]
            [math.numbers]
            [math.simplify]
            [math.structure :refer :all]))

(deftest interpolation
  (testing "lagrange interpolation"
    (is (= [1/6 1/3 1/2 2/3 5/6] (linear-interpolants 0 1 5)))
    (let [f (Lagrange-interpolation-function [3 2 5 1] [1 2 3 4])]
      (is (= (f 1) 3))
      (is (= (f 2) 2))
      (is (= (f 3) 5))
      (is (= (f 4) 1)))
    ;; works, but simplification can't yet see that the answer is
    ;; just "a".
    ;; (let [f (Lagrange-interpolation-function '[a b c] '[w x y])]
    ;;   (is (= 'a (f 'w))))
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
    (is (= '(+ (* -1 ((D q) t) t)
               (* ((D q) t) t1)
               (* (((expt D 2) q) t) (/ (+ (expt t 2) (* -2 t t1) (expt t1 2)) 2))
               (q t))
           (simplify ((osculating-path ((Γ q 4) 't)) 't1))))))

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
    (let [test-path (fn [t]
                      (up (+ (* 'a t) 'a0)
                          (+ (* 'b t) 'b0)
                          (+ (* 'c t) 'c0)))]
      (is (= (down 0 0 0)
             (((Lagrange-equations (L-free-particle 'm))
               test-path)
              't))))
    ;; TODO: for the present, we just ensure the following expressions
    ;; compile & execute without checking their values. This is because
    ;; simplification isn't online yet, so the values are going to change
    ;; soon enough, and the current forms of the values are pretty big.
    (is (((Lagrange-equations (L-free-particle 'm))
          (literal-function 'q))
         't))
    (let [proposed-solution (fn [t]
                              (* 'a (cos (+ (* 'omega t) 'phi))))]
      (is (((Lagrange-equations (L-harmonic 'm 'k))
            proposed-solution)
           't)))
    (is (((Lagrange-equations (L-uniform-acceleration 'm 'g))
          (up (literal-function 'x) (literal-function 'y)))
         't))
    (is (((Lagrange-equations (L-central-rectangular 'm (literal-function 'U)))
          (up (literal-function 'x)
              (literal-function 'y)))
         't))
    (is (((Lagrange-equations (L-central-polar 'm (literal-function 'U)))
          (up (literal-function 'r)
              (literal-function 'phi)))
         't))

    (is (velocity ((F->C p->r)
                   (->local 't (up 'r 'phi) (up 'rdot 'phidot)))))
    ))
