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

(ns sicmutils.numerical.compile-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.value :as v]
            [sicmutils.numerical.compile :refer :all]))

(def ^:private near (v/within 1e-6))

(deftest compile-univariate
  (let [f (fn [x] (+ 1 (square (sin x))))
        cf (compile-univariate-function f)]
    (is (near (f 0.5) (cf 0.5)))))

(deftest compile-state
  (let [f (fn [[[a b] [c d]]] (- (* a d) (* b c)))
        sf (fn [k] (fn [s] (* k (f s))))
        s (up (down 2 3) (down 4 5))
        t (up (down 3 4) (down -1 2))
        cf (compile-state-function sf [1] s)]
    (is (= -2 (f s)))
    (is (= 10 (f t)))
    (is (= -4 ((sf 2) s)))
    (is (= 20 ((sf 2) t)))
    (is (= -2 (cf [2 3 4 5 1])))
    (is (= -4 (cf [2 3 4 5 2])))
    (is (= 10 (cf (concat (flatten t) [1]))))
    (is (= 20 (cf [3 4 -1 2 2])))))

(defn ^:private make-generator
  [s]
  (let [i (atom 0)]
    (fn []
      (symbol (format "%s%d" s (swap! i inc))))))

(deftest subexp
  (is (= '[(* g1 (+ x z) g1) ([g1 (+ x y)])]
         (extract-common-subexpressions
          '(* (+ x y) (+ x z) (+ x y))
          (make-generator "g")
          vector)))
  (is (= '[(+ K1 (expt K1 2) K2 (sqrt K2)) ([K1 (sin x)] [K2 (cos x)])]
         (extract-common-subexpressions
          '(+ (sin x) (expt (sin x) 2) (cos x) (sqrt (cos x)))
          (make-generator "K")
          vector))))

(deftest subexp-compile
  (let [x '(+ (sin x) (expt (sin x) 2) (cos x) (sqrt (cos x)) (tan x))
        cse (common-subexpression-elimination x :symbol-generator (make-generator "g"))]
    (is (= '(clojure.core/let [g1 (sin x) g2 (cos x)]
              (+ g1 (expt g1 2) g2 (sqrt g2) (tan x))) cse))
    (is (= '(+ a b (sin x) (cos y))
           (common-subexpression-elimination '(+ a b (sin x) (cos y)))))))
