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

(ns math.sicm-ch7-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.numsymb]
            [math.simplify :refer [pe]]
            [math.function :refer :all]
            [math.operator :refer :all]
            [math.value :as v]
            [math.calculus.derivative :refer :all]))

(def ^:private near (v/within 1.0e-6))

(deftest section-1
  (let [h (compose cube sin)
        g (* cube sin)]
    (is (= (h 2) (cube (sin 2))))
    (is (near (h 2) 0.7518269))
    (is (= (g 2) (* (cube 2) (sin 2))))
    (is (near (g 2) 7.2743794))
    (is (= '(expt (sin a) 3) (simplify (h 'a))))
    (is (= 0 (simplify ((- (+ (square sin) (square cos)) 1) 'a))))
    (is (= '(f x) (simplify ((literal-function 'f) 'x))))
    (is (= '(f (g x))
           (simplify ((compose (literal-function 'f) (literal-function 'g)) 'x))))
    ;; need tests for function domain/range notation here, when we have it
    ))

(deftest section-2
  (let [g (literal-function 'g)]
    ;; doesn't work yet; don't have literal functions of arity > 1
    ;; (is (= '(g x y) (simplify (g 'x 'y))))
    ))

(deftest section-3
  (let [derivative-of-sine (D sin)]
    (is (= '(cos x) (simplify (derivative-of-sine 'x)))))
  (is (= '(+ (((expt D 2) f) x) (* -1 (f x)))
         (simplify (((* (- D 1) (+ D 1)) (literal-function 'f)) 'x)))))

(deftest section-4
  (let [g (fn [x y] (up (square (+ x y)) (cube (- y x)) (exp (+ x y))))]
    (is (= '(down
             (up (+ (* 2 x) (* 2 y))
                 (+ (* -3 (expt x 2)) (* 6 x y) (* -3 (expt y 2)))
                 (exp (+ x y)))
             (up (+ (* 2 x) (* 2 y))
                 (+ (* 3 (expt x 2)) (* -6 x y) (* 3 (expt y 2)))
                 (exp (+ x y))))
           (simplify ((D g) 'x 'y))))))
