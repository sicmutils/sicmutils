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
            [sicmutils.env :refer :all]
            [sicmutils.operator :refer :all]
            ))

; make private?            
(def f ^:private (literal-function 'f))
(def g ^:private (literal-function 'g))
(def ff ^:private (literal-function 'ff [0 0] 0))
(def gg ^:private (literal-function 'gg [0 0] 0))


;; Test operations with Operators
(deftest Operator-tests
  (testing "that our known Operators work with basic arithmetic"
    (is (every? operator? [(+ D 1)(+ 2 D)(- D 3)(- 4 D)(* 5 D)(* D 6)]))
    (is (every? operator? [(+ (partial 0) 1)(+ 2 (partial 0))(- (partial 0) 3)(- 4 (partial 0))(* 5 (partial 0))(* (partial 0) 6)]))
    )
  (testing "that they compose with other Operators"
    (is (every? operator? [(* D D)(* D (partial 0))(*(partial 0) D)(* (partial 0)(partial 1))])))
        
  (testing "that multiplication of Operators is equivalent to application/composition"           
     (is (= (((* D D) ff) 'x 'y)
	    ((D (D ff)) 'x 'y)))
     (is (= (((* (partial 0)(partial 1)) ff) 'x 'y)
            (((partial 0) ((partial 1) ff)) 'x 'y))))
            
  (testing "that their arithmetic operations compose correctly, as per SICM -  'Our Notation'"
    (is (= (simplify (((* (+ D 1)(- D 1)) f) 'x))
           '(+ (((expt D 2) f) x) (* -1 (f x))) )))
  (testing "that Operators compose correctly with functions"
    (is (= (simplify ((D ((* (- D g)(+ D 1)) f)) 'x))
           '(+ (* -1 (((expt D 2) f) x) (g x))
               (* -1 (g x) ((D f) x))
               (* -1 ((D f) x) ((D g) x))
               (* -1 ((D g) x) (f x))
               (((expt D 2) f) x)
               (((expt D 3) f) x)))))

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
           
  (testing "negation of Operator"
    (is (= (simplify (((* (- (- D))) ff) 'x 'y))  (simplify (( D ff) 'x 'y)))))

  (testing "Addition of Operator and Structure (for multivariate literal-functions)"
    (is (= (((+ (expt D 2) D 1) ff) 1 2)
           (down
             (down
               (+ (((∂ 0) ((∂ 0) ff)) 1 2) (((∂ 0) ff) 1 2) (ff 1 2))
               (+ (((∂ 0) ((∂ 1) ff)) 1 2) (((∂ 0) ff) 1 2) (ff 1 2)))
             (down
               (+ (((∂ 1) ((∂ 0) ff)) 1 2) (((∂ 1) ff) 1 2) (ff 1 2))
               (+ (((∂ 1) ((∂ 1) ff)) 1 2) (((∂ 1) ff) 1 2) (ff 1 2))))))
    (is (= (let [grfn (fn [x y] (+ (* x x)(* 3 y)))]  ;; test against a real function
              (((+ (expt D 2) D 1) grfn) 1 2))
           (down (down 11 9) 10))))
    
  (testing "Subtraction of Operator and Structure (for multivariate literal-functions)"
    (is (= (simplify (((- (expt D 2) D) ff) 1 2))
           '(down
              (down
                (+ (((∂ 0) ((∂ 0) ff)) 1 2) (* -1 (((∂ 0) ff) 1 2)))
                (+ (((∂ 0) ((∂ 1) ff)) 1 2) (* -1 (((∂ 0) ff) 1 2))))
              (down
                (+ (((∂ 1) ((∂ 0) ff)) 1 2) (* -1 (((∂ 1) ff) 1 2)))
                (+ (((∂ 1) ((∂ 1) ff)) 1 2) (* -1 (((∂ 1) ff) 1 2)))))))
    (is (= (let [grfn (fn [x y] (+ (* x x)(* 3 y)))]  ;; test against a real function
              (((- (expt D 2) D) grfn) 1 2))
           (down (down 0 -2) -3))))
        
  (testing "Addition of Differential and Structure (for multivariate literal-functions)"
    (is (= (simplify (((* (+ D 3)(+ D 2)) ff) 1 2))
           (simplify (((+ (expt D 2) (* 5 D) 6) ff) 1 2)))
           ))
    
  (testing "Subtraction of Differential to Structure (for multivariate literal-functions)"
    (is (= (simplify (((* (+ D 1)(- D 1)) ff) 'x 'y))
           (simplify (((+ (expt D 2) -1) ff) 'x 'y)))
           )
    (is (= (simplify (((* (+ D 3)(- D 2)) ff) 1 2))
           (simplify (((+ (expt D 2) D -6) ff) 1 2)))
           )
    (is (= (let [grfn (fn [x y] (+ (* x x)(* 3 y)))]  ;; test against a real function
              (((* (+ D 3)(- D 2)) grfn) 1 2))
           (down (down -38 -40) (down -39 -39))))
           )
           )
           
           
