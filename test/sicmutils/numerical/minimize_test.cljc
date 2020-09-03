;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.numerical.minimize-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.value :as v]
            [same :refer [ish? with-comparator]]
            [sicmutils.numerical.minimize :as m]))

(def ^:private near (v/within 1e-5))

#?(:clj
   (deftest one-variable-minimize
     (testing "convergence"
         (let [M (fn [f a b] (first (m/minimize f a b)))]
           ;; http://www.wolframalpha.com/input/?i=x%5E2+%2B+exp%28-x%29
           (is (near 0.351734 (M #(+ (* % %) (Math/exp (- %))) 0.0 1.0)))
           ;; http://www.wolframalpha.com/input/?i=x%5E4%2B2x%5E2%2Bx%2B3
           (is (near -0.236733 (M (fn [x]
                                    (let [x2 (* x x)
                                          x4 (* x2 x2)]
                                      (+ x4 (* 2 x2) x 3))) -1.0 0)))
           ;; http://www.wolframalpha.com/input/?i=exp%28x%29%2B0.01%2Fx
           (is (near 0.0953446 (M #(+ (Math/exp %) (/ 0.01 %)) 0.001 1)))
           ;; http://www.wolframalpha.com/input/?i=exp%28x%29+-+2x+%2B+%28.01%2Fx%29+-+%280.000001%2Fx%2Fx%29
           (is (near 0.703205 (M #(+ (/ 0.01 %) (- (Math/exp %) (* 2 %) (/ 0.000001 % %))) 0.001 1)))
           (is (near 0. (M #(+ 5 (* % %)) -2 2)))))))

(deftest multi-variable-minimize
  (testing "convergence"
    (let [p (m/multidimensional-minimize
             (fn [[x y]]
               (let [x' (- x 3)
                     y' (+ y 4)]
                 (+ (* x' x') (* y' y'))))
             [0 0]
             :simplex-tolerance 1e-5)]
      (is (near 3 (first p)))
      (is (near -4 (second p))))
    ;; from scipy's test_optimize.py
    (let [v (m/multidimensional-minimize
             (fn [[x0 x1 x2]]
               (let [log_pdot [(+ x0 x1 x2)
                               (+ x0 x1)
                               (+ x0 x2)
                               x0
                               x0]
                     logZ (Math/log (reduce + (map #(Math/exp %) log_pdot)))]
                 (- logZ (+ x0 (* 0.3 x1) (* 0.5 x2)))))
             [0 0 0]
             :simplex-tolerance 1e-5
             :adaptive? false)]
      (with-comparator near
        (is (ish? [0.17285378 -0.524869316 0.487525860]
                  v))))))
