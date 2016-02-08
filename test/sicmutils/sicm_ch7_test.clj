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

(ns sicmutils.sicm-ch7-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :refer :all]
             [structure :refer :all]
             [numsymb]
             [numbers]
             [simplify :refer [pe]]
             [function :refer :all]
             [operator :refer :all]
             [value :as v]]
            [sicmutils.calculus.derivative :refer :all]))

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
           (simplify ((compose (literal-function 'f) (literal-function 'g)) 'x))))))

(deftest section-2
  (let [g (literal-function 'g)]
    (testing "literal functions"
      (is (= '(g x y) (simplify ((literal-function 'g [0 0] 0) 'x 'y)))))
    (testing "structured arguments"
      (let [s (up 't (up 'x 'y) (down 'p_x 'p_y))
            H (literal-function 'H (up 0 (up 0 0) (down 0 0)) 0)]
        (is (= '(H (up t (up x y) (down p_x p_y)))
               (simplify (H s))))
        (is (thrown? IllegalArgumentException (H (up 0 (up 1 2) (down 1 2 3)))))
        (is (thrown? IllegalArgumentException (H (up 0 (up 1) (down 1 2)))))
        (is (thrown? IllegalArgumentException (H (up (up 1 2) (up 1 2) (down 1 2)))))
        (is (= '(down (((∂ 0) H) (up t (up x y) (down p_x p_y)))
                      (down (((∂ 1 0) H) (up t (up x y) (down p_x p_y)))
                            (((∂ 1 1) H) (up t (up x y) (down p_x p_y))))
                      (up (((∂ 2 0) H) (up t (up x y) (down p_x p_y)))
                          (((∂ 2 1) H) (up t (up x y) (down p_x p_y)))))
               (-> s ((D H)) simplify)))))))

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
