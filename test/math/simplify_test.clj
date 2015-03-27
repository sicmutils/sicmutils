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

(ns math.simplify-test
  (require [clojure.test :refer :all]
           [math.simplify :refer :all]
           [math.structure :refer :all]
           [math.expression :as x]
           [math.generic :as g]
           [math.structure :as s]
           [math.numbers]
           [math.mechanics.lagrange :refer :all]
           [math.function :as f]
           [math.polynomial :as poly]
           [math.value :as v]))

(defn- pe [x]
  (-> x g/simplify x/print-expression))

(deftest generator
  (let [g (symbol-generator "k%d")
        a (for [_ (range 5)] (g))
        b (for [_ (range 5)] (g))
        h (symbol-generator "k%d")
        c (for [_ (range 5)] (h))]
    (is (= '(k0 k1 k2 k3 k4) a))
    (is (= '(k5 k6 k7 k8 k9) b))
    (is (= '(k0 k1 k2 k3 k4) c))))

(deftest analyzer-test
  (let [new-analyzer (fn [] (analyzer (symbol-generator "k%d")
                                      poly/expression->
                                      poly/->expression
                                      poly/operators-known))
        A (fn [x]
            (x/print-expression ((new-analyzer) x)))]
    (is (= '(+ x 1) (A '(+ 1 x))))
    (is (= '(+ x 1) (A '[+ 1 x])))
    (is (= 'x (A '(* 1/2 (+ x x)))))
    (is (= '(* (sin y) (cos (+ (expt (sin y) 4) (* 2 (sin y)) 1)) y)
           (A '(* y (sin y) (cos (+ 1 (sin y) (sin y) (expt (sin y) 4)))))))
    (is (= '(+ (* -1N (expt ((D phi) t) 2) (r t) m) (* (((expt D 2) r) t) m) ((D U) (r t)))
           (A '(- (* 1/2 m (+ (((expt D 2) r) t) (((expt D 2) r) t)))
                    (+ (* 1/2 m (+ (* ((D phi) t) ((D phi) t) (r t))
                                         (* ((D phi) t) ((D phi) t) (r t))))
                         (* -1 ((D U) (r t))))))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt cos 2) (expt sin 2)))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt sin 2) (expt cos 2)))))))

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
  (is (= '(* 2 x) (pe (g/+ 'x 'x))))
  (is (= '(+ x 1) (pe (g/+ 1 'x)))))

(deftest equations
  (let [xy (s/up (f/literal-function 'x) (f/literal-function 'y))
        xyt (xy 't)
        U (f/literal-function 'U)
        xyt2 (g/square xyt)
        Uxyt2 (U xyt2)
        ]
    (is (= '(up x y) (pe xy)))
    (is (= '(up (x t) (y t)) (pe xyt)))
    (is (= '(+ (expt (x t) 2) (expt (y t) 2)) (pe xyt2)))
    (is (= '(U (+ (expt (x t) 2) (expt (y t) 2))) (pe Uxyt2)))
    (is (= 1 (pe (g/+ (g/expt (g/sin 'x) 2) (g/expt (g/cos 'x) 2)))))
    ;; why doesn't the following work given that the rules are meant
    ;; to pull sines to the left?
    (is (= 1 (pe (g/+ (g/expt (g/cos 'x) 2) (g/expt (g/sin 'x) 2)))))
    ))

(deftest structures
  (let [A (up (up 1 2) (up 3 4))
        C (down (up 1 2 3) (up 0 4 5) (up 1 0 6))]
    (testing "characteristic polynomial"
      (is (= '(+ (expt x 2) (* -5 x) -2) (pe (characteristic-polynomial A 'x))))
      (is (= '(+ (expt y 3) (* -11 (expt y 2)) (* 31 y) -22) (pe (characteristic-polynomial C 'y))))
      (is ((v/within 1e-12) 0.0 (pe (characteristic-polynomial A (g/divide (g/- 5 (g/sqrt 33)) 2))))))))

(deftest lagrange-equations-test
  (let [xy (s/up (f/literal-function 'x) (f/literal-function 'y))
        LE (((Lagrange-equations (L-central-rectangular 'm (f/literal-function 'U))) xy) 't)]
    (is (= '(up x y) (pe xy)))
    (is (= '(down (+ (* 2 (x t)
                        ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2))))
                        (/ 1 (* 2 (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
                     (* (((expt D 2) x) t) m))
                  (+ (* 2 (y t)
                        ((D U) (sqrt (+ (expt (y t) 2) (expt (x t) 2))))
                        (/ 1 (* 2 (sqrt (+ (expt (y t) 2) (expt (x t) 2))))))
                     (* (((expt D 2) y) t) m)))
           (pe LE)))))
