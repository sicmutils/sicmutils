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

(ns math.sicm-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.numbers :refer :all]
            [math.numsymb]
            [math.simplify]
            [math.expression :refer :all]
            [math.numerical.integrate :refer :all]
            [math.numerical.minimize :refer :all]
            [math.function :refer :all]
            [math.operator :refer :all]
            [math.value :as v]
            [math.calculus.derivative :refer :all]
            [math.mechanics.lagrange :refer :all]
            [math.generic :as g]))

(def ^:private near (v/within 1e-6))

(defn pe [x] (-> x simplify print-expression))
(def q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(defn test-path
  [t]
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(defn make-η
  [ν t1 t2]
  (fn [t]
    (* (- t t1) (- t t2) (ν t))))

(defn varied-free-particle-action
  [mass q ν t1 t2]
  (fn [ε]
    (let [η (make-η ν t1 t2)]
      (Lagrangian-action (L-free-particle mass)
                         (+ q (* ε η)) t1 t2))))

(defn make-path
  [t0 q0 t1 q1 qs]
  (let [n (count qs)
        ts (linear-interpolants t0 t1 n)]
    (Lagrange-interpolation-function
      `[~q0 ~@qs ~q1]
      `[~t0 ~@ts ~t1])))

(deftest sicm
  (testing "Chapter 1"
    (is (= '(up (x t)
                (y t)
                (z t))
           (pe (q 't))))
    (is (= '(up ((D x) t)
                ((D y) t)
                ((D z) t))
           (pe ((D q) 't))))
    (is (= '(up (((expt D 2) x) t)
                (((expt D 2) y) t)
                (((expt D 2) z) t))
           (pe ((D (D q)) 't))))
    ;(is (= '(up (((expt D 2) x) t)
    ;            (((expt D 2) y) t)
    ;            (((expt D 2) z) t))
    ;       (pe (((expt D 2) q) 't))))
    (is (= '(up t
                (up (x t)
                    (y t)
                    (z t))
                (up ((D x) t)
                    ((D y) t)
                    ((D z) t)))
           (pe ((Γ q) 't))))
    ;; this is beginning to take shape... the simplifier is not
    ;; deployed yet; the text has (expt ((D x) t) 2) where we have
    ;; literal multiplication.
    (is (= '(+
              (* 1/2 (expt ((D x) t) 2) m)
              (* 1/2 (expt ((D y) t) 2) m)
              (* 1/2 (expt ((D z) t) 2) m))
           (pe ((comp (L-free-particle 'm) (Γ q)) 't))))
    ;; at this point in the text we should be able to show-expression
    ;; in TeX form XXX.
    (is (= 435.0 (Lagrangian-action (L-free-particle 3.0) test-path 0.0 10.0)))
    (is (= (up (sin 2.0) (cos 2.0) (square 2.0)) ((up sin cos square) 2.0)))
    ;; variation operator
    (let [F (fn [q] (fn [t] ((literal-function 'f) (q t))))
          G (fn [q] (fn [t] ((literal-function 'g) (q t))))
          δ_η (δ (literal-function 'η))
          q (literal-function 'q)
          φ (fn [f] (fn [q] (fn [t] ((literal-function 'φ) ((f q) t)))))]
      (is (= '(η t) (pe (((δ_η identity) q) 't))))
      (is (= '(* ((D f) (q t)) (η t)) (pe (((δ_η F) q) 't))))
      (is (= '(* ((D g) (q t)) (η t)) (pe (((δ_η G) q) 't))))
      (is (= '(+ (* ((D f) (q t)) (η t) (g (q t))) (* (η t) (f (q t)) ((D g) (q t)))) (pe (((δ_η (* F G)) q) 't))))
      (is (= '(+ (* ((D f) (q t)) (η t) (g (q t))) (* (η t) (f (q t)) ((D g) (q t)))) (pe (((δ_η (* F G)) q) 't))))
      (is (= '(* ((D φ) (f (q t))) ((D f) (q t)) (η t)) (pe (((δ_η (φ F)) q) 't)))))
    (let [η (make-η #(* % %) 0 1)
          ε 1/1000
          f (* η ε)
          η2 (make-η (up sin cos square) 0 1)]
      (is (= 0.0 (η 0.0)))
      (is (= 0.0 (η 1.0)))
      (is (= -1/16 (η 1/2)))
      (is (= -1/16000 (f 1/2)))
      (is (= (up 0 0 0) (η2 0.0)))
      (is (= (up 0 0 0) (η2 1.0)))
      (is (= (up (* -1/4 (sin 0.5)) (* -1/4 (cos 0.5)) (/ 1. -16.)) (η2 0.5)))
      (is (= (up 0 0 0) ((Γ η) 0)))
      (is (= (up 1 0 1) ((Γ η) 1)))
      (is (= '(+ (expt t 4) (* -1 (expt t 3))) (pe (η 't))))
      (is (= '(+ (* 4 (expt t 3)) (* -3 (expt t 2))) (pe ((D η) 't))))
      (is (= '(up t (+ (expt t 4) (* -1 (expt t 3))) (+ (* 4 (expt t 3)) (* -3 (expt t 2)))) (pe ((Γ η) 't))))
      (is (= '(up
                (+ (* (cos t) (expt t 2)) (* 2 (sin t) t) (* -1 (cos t) t) (* -1 (sin t)))
                (+ (* -1 (sin t) (expt t 2)) (* 2 (cos t) t) (* (sin t) t) (* -1 (cos t)))
                (+ (* 4 (expt t 3)) (* -3 (expt t 2))))
             (pe ((D η2) 't))))
      )

    (is (near 436.2912143 ((varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) 0.001)))
    ;; temporarily disabled because they take a long time
    (let [m (minimize (varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) -2.0 1.0)]
       (is (near 0.0 (first m)))
       (is (near 435 (second m))))
    ))
