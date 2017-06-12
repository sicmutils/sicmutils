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

(ns sicmutils.simplify-test
  (:require [clojure.test :refer :all]
            [sicmutils
             [simplify :refer [hermetic-simplify-fixture
                               simplify-expression
                               expression->string
                               trig-cleanup]]
             [structure :refer :all]
             [complex :as c]
             [generic :as g]
             [structure :as s]
             [matrix :as matrix]
             [numbers]
             [function :as f]
             [value :as v]]
            [sicmutils.mechanics.lagrange :refer :all]))

(use-fixtures :once hermetic-simplify-fixture)

(defn ^:private symbol-generator
  "Returns a function which generates a sequence of symbols
  staring with the initial prefix."
  [fmt]
  (let [i (atom -1)]
    #(->> (swap! i inc) (format fmt) symbol)))

(deftest generator
  (let [g (symbol-generator "k%d")
        a (for [_ (range 5)] (g))
        b (for [_ (range 5)] (g))
        h (symbol-generator "k%d")
        c (for [_ (range 5)] (h))]
    (is (= '(k0 k1 k2 k3 k4) a))
    (is (= '(k5 k6 k7 k8 k9) b))
    (is (= '(k0 k1 k2 k3 k4) c))))

(deftest simplify-expressions
  (is (= 6 (simplify-expression '(* 1 2 3))))
  (is (= 2/3 (simplify-expression '(/ 2 3)))))

(deftest trivial-simplifications
  (is (= 1 (g/simplify 1)))
  (is (= 1.0 (g/simplify 1.0)))
  (is (= 'foo (g/simplify 'foo)))
  (is (= 3 (g/simplify (g/+ 1 2))))
  (is (= 6 (g/simplify (g/+ 1 2 3))))
  (is (= nil (g/simplify nil)))
  (is (= '(* 2 x) (g/simplify (g/+ 'x 'x))))
  (is (= '(+ x 1) (g/simplify (g/+ 1 'x)))))

(deftest divide-numbers-through
  (is (= 'x (simplify-expression '(* 1 x))))
  (is (= '(* x y z) (simplify-expression '(* 1 x y z))))
  (is (= 2/3 (simplify-expression '(/ 2 3))))
  (is (= '(+ (* 1/2 x) (* 1/2 y)) (simplify-expression '(/ (+ x y) 2))))
  (is (= '(+ x y) (simplify-expression '(/ (* 2 (+ x y)) 2)))))

(deftest equations
  (let [xy (s/up (f/literal-function 'x) (f/literal-function 'y))
        xyt (xy 't)
        U (f/literal-function 'U)
        xyt2 (g/square xyt)
        Uxyt2 (U xyt2)]
    (is (= '(up x y) (g/simplify xy)))
    (is (= '(up (x t) (y t)) (g/simplify xyt)))
    (is (= '(+ (expt (x t) 2) (expt (y t) 2)) (g/simplify xyt2)))
    (is (= '(U (+ (expt (x t) 2) (expt (y t) 2))) (g/simplify Uxyt2)))
    (is (= 1 (g/simplify (g/+ (g/expt (g/sin 'x) 2) (g/expt (g/cos 'x) 2)))))
    ;; why doesn't the following work given that the rules are meant
    ;; to pull sines to the left?
    (is (= 1 (g/simplify (g/+ (g/expt (g/cos 'x) 2) (g/expt (g/sin 'x) 2)))))
    ))

(deftest structures
  (let [A (matrix/by-rows [1 2] [3 4])
        C (matrix/by-rows [1 2 3] [0 4 5] [1 0 6])]
    (testing "characteristic polynomial"
      (is (= '(+ (expt x 2) (* -5 x) -2) (g/simplify (matrix/characteristic-polynomial A 'x))))
      (is (= '(+ (expt y 3) (* -11 (expt y 2)) (* 31 y) -22) (g/simplify (matrix/characteristic-polynomial C 'y))))
      (is ((v/within 1e-12) 0.0 (g/simplify (matrix/characteristic-polynomial A (g/divide (g/- 5 (g/sqrt 33)) 2))))))))

(deftest native-clojure-things
  (is (= "foo" (g/simplify "foo")))
  (is (= '(2 3) (g/simplify '(2 3))))
  (let [a (g/simplify [2 3])]
    (is (= [2 3] a))
    (is (vector? a)))
  (is (= [] (g/simplify [])))
  (is (= '[x y] (g/simplify '[x y])))
  (let [a (g/simplify [(g/+ 'x 'x) (g/* 'y 'y)])]
    (is (= '[(* 2 x) (expt y 2)] a))
    (is (vector? a))))

(deftest sincos-oscillation
  (let [X '(- (expt (sin a) 2) (* (expt (cos b) 2) (expt (sin a) 2)))]
    (is (= '(* (expt (sin a) 2) (expt (sin b) 2)) (simplify-expression X)))))

(deftest lagrange-equations-test
  (let [xy (s/up (f/literal-function 'x) (f/literal-function 'y))
        LE (((Lagrange-equations (L-central-rectangular 'm (f/literal-function 'U))) xy) 't)]
    (is (= '(up x y) (g/simplify xy)))
    (is (= '(down (/ (+ (* m (((expt D 2) x) t) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                        (* (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                     (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                  (/ (+ (* m (sqrt (+ (expt (x t) 2) (expt (y t) 2))) (((expt D 2) y) t))
                        (* (y t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                     (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
           (g/simplify LE)))))

(deftest complex-units
  (is (= '(1 (complex 0.0 1.0) -1 (complex 0 -1) 1 (complex 0 1) -1 (complex 0 -1))
         (for [n (range 8)]
           (simplify-expression `(~'expt (~'complex 0.0 1.0) ~n))))))

(deftest arc-tan-cleanup
  (is (= '(atan 1 1)  (trig-cleanup '(atan 1 1))))
  (is (= '(atan -1 1)  (trig-cleanup '(atan -1 1))))
  (is (= '(atan 1 -1) (g/simplify (trig-cleanup '(atan 1 -1)))))
  (is (= '(atan -1 -1) (g/simplify (trig-cleanup '(atan -1 -1)))))
  (is (= '(atan y x) (g/simplify (trig-cleanup '(atan y x)))))
  (is (= '(atan 1) (g/simplify (trig-cleanup '(atan x x)))))
  (is (= '(atan 1 -1) (g/simplify (trig-cleanup '(atan x (* -1 x))))))
  (is (= '(atan -1) (g/simplify (trig-cleanup '(atan (* -1 x) x))))))

(deftest string-form-test
  (is (= "(up sin cos tan)" (expression->string (s/up g/sin g/cos g/tan))))
  (is (= "+" (expression->string g/+)))
  (is (= "1" (expression->string ((g/+ (g/square g/sin) (g/square g/cos)) 'x))))
  (is (= "(/ (+ (* -1 (expt (cos x) 4)) 1) (expt (cos x) 2))"
         (expression->string ((g/+ (g/square g/sin) (g/square g/tan)) 'x))))
  (is (= "nil" (expression->string nil)))
  (is (= "[nil 3 (+ x 2)]" (expression->string [nil 3 (g/+ 2 'x)])))
  (is (= "(complex 0.0 1.0)" (expression->string (c/complex 0 1)))))

(deftest more-trig
  (is (= '(* -1 (expt (sin x) 2)) (g/simplify (g/+ (g/expt (g/cos 'x) 2) -1))))
  (is (= '(tan x) (g/simplify (g/tan 'x))))
  (is (= '(/ (+ (sin x) (cos x)) (cos x)) (g/simplify (g/+ 1 (g/tan 'x)))))
  (is (= '(/ (+ (sin x) (cos x)) (cos x)) (g/simplify (g/+ (g/tan 'x) 1))))
  (is (= '(* -1 (expt (cos x) 2)) (g/simplify (g/+ (g/expt (g/sin 'x) 2) -1))))
  (is (= '(expt (cos x) 2) (g/simplify (g/- 1 (g/expt (g/sin 'x) 2)))))
  (is (= '(* -1 (expt (cos x) 2)) (g/simplify (g/+ (g/expt (g/sin 'x) 2) -1))))
  (testing "symbolic arguments"
    (is (= '(atan y x) (g/simplify (g/atan 'y 'x))))))
