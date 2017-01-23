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

(ns sicmutils.operator-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils
             [value :as v]
             [env :refer :all]
             [series :as series]
             [operator :refer :all]
             [simplify :refer [hermetic-simplify-fixture]]]))

(use-fixtures :once hermetic-simplify-fixture)

(def f (literal-function 'f))
(def g (literal-function 'g))
(def ff (literal-function 'ff [0 0] 0))
(def gg (literal-function 'gg [0 0] 0))

;; Test operations with Operators
(deftest Operator-tests
  (testing "that our known Operators work with basic arithmetic"
    (is (every? operator? [(+ D 1)(+ 2 D)(- D 3)(- 4 D)(* 5 D)(* D 6)]))
    (is (every? operator? [(+ (partial 0) 1)(+ 2 (partial 0))(- (partial 0) 3)(- 4 (partial 0))(* 5 (partial 0))(* (partial 0) 6)]))
    )
  (testing "that they compose with other Operators"
    (is (every? operator? [(* D D)(* D (partial 0))(*(partial 0) D)(* (partial 0)(partial 1))])))
  (testing "that their arithmetic operations compose correctly, as per SICM -  'Our Notation'"
    (is (= (simplify (((* (+ D 1)(- D 1)) f) 'x))
           '(+ (((expt D 2) f) x) (* -1 (f x))) )))
  (testing "that Operators compose correctly with functions"
    (is (= '(+ (* -1 (((expt D 2) f) x) (g x))
               (* -1 ((D f) x) ((D g) x))
               (* -1 ((D f) x) (g x))
               (* -1 (f x) ((D g) x))
               (((expt D 2) f) x)
               (((expt D 3) f) x))
           (simplify ((D ((* (- D g) (+ D 1)) f)) 'x)))))
  (testing "that basic arithmetic operations work on multivariate literal functions"
    (is (= (simplify (((+  D  D) ff) 'x 'y))
           '(down (* 2 (((∂ 0) ff) x y)) (* 2 (((∂ 1) ff) x y)))))
    (is (= (simplify (((-  D  D) ff) 'x 'y))
           '(down 0 0)))
    (is (= (((*  D  D) ff) 'x 'y)
           (down
            (down (((partial 0) ((partial 0) ff)) 'x 'y) (((partial 0) ((partial 1) ff)) 'x 'y))
            (down (((partial 1) ((partial 0) ff)) 'x 'y) (((partial 1) ((partial 1) ff)) 'x 'y)))))
    (is (= (((*  (partial 1)  (partial 0)) ff) 'x 'y)
           (((partial 1) ((partial 0) ff)) 'x 'y))))
  (testing "operator derivative shape"
    (is (= [:exactly 1] (:arity identity-operator)))
    (is (= [:exactly 1] (:arity D)))
    (is (= [:exactly 1] (:arity (* D identity-operator))))
    (is (= [:exactly 1] (:arity (* 'e D))))
    (is (= [:exactly 1] (:arity (* D 'e))))
    (is (= [:exactly 1] (v/arity sin)))
    (is (= [:exactly 1] (v/arity (identity-operator sin))))
    (is (= '(sin x) (simplify ((identity-operator sin) 'x))))
    (is (= '(cos x) (simplify (((* D identity-operator) sin) 'x))))
    (is (= '(cos x) (simplify (((* identity-operator D) sin) 'x)))))
  (testing "exponentiation"
    (is (= '((f t)
             (* ((D f) t) ε)
             (* 1/2 (((expt D 2) f) t) (expt ε 2))
             (* 1/6 (((expt D 3) f) t) (expt ε 3))
             (* 1/24 (((expt D 4) f) t) (expt ε 4))
             (* 1/120 (((expt D 5) f) t) (expt ε 5)))
           (simplify (take 6 (series/->seq
                              (((exp (* 'ε D)) (literal-function 'f)) 't))))))
    (is (= '(0
             ε
             0
             (* -1/6 (expt ε 3))
             0
             (* 1/120 (expt ε 5))
             0
             (* -1/5040 (expt ε 7))
             0
             (* 1/362880 (expt ε 9))
             0
             (* -1/39916800 (expt ε 11)))
           (simplify (take 12 (series/->seq
                               (((exp (* 'ε D)) sin) 0))))))
    (is (= '(1
             0
             (* -1/2 (expt ε 2))
             0
             (* 1/24 (expt ε 4))
             0
             (* -1/720 (expt ε 6))
             0
             (* 1/40320 (expt ε 8))
             0
             (* -1/3628800 (expt ε 10))
             0) (simplify (take 12 (series/->seq
                                    (((exp (* 'ε D)) cos) 0))))))
    (is (= '(1
             (* 1/2 ε)
             (* -1/8 (expt ε 2))
             (* 1/16 (expt ε 3))
             (* -5/128 (expt ε 4))
             (* 7/256 (expt ε 5)))
           (simplify (take 6 (series/->seq
                              (((exp (* 'ε D)) #(sqrt (+ % 1))) 0))))))
    (is (= '(+
             (* 1/5040 (expt n 7) (expt ε 7))
             (* -1/240 (expt n 6) (expt ε 7))
             (* 5/144 (expt n 5) (expt ε 7))
             (* -7/48 (expt n 4) (expt ε 7))
             (* 29/90 (expt n 3) (expt ε 7))
             (* -7/20 (expt n 2) (expt ε 7))
             (* 1/7 n (expt ε 7)))
           (simplify (nth (series/->seq
                           (((exp (* 'ε D)) #(expt (+ 1 %) 'n)) 0)) 7))))))

    ;;; more testing to come as we implement multivariate literal functions that rely on operations on structures....
