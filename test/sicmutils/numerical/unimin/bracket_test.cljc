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

(ns sicmutils.numerical.unimin.bracket-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.generic :as g]
            [sicmutils.numerical.unimin.bracket :as b]))

;; TODO test that the minimum of the polynomial interpolation has derivative 0!
;; Since we have functions to do both.

(deftest bracket-tests
  (testing "cubic-from-java"
    (let [min-f (fn [x]
                  (if (< x -2)
                    -2
                    (* (- x 1)
                       (+ x 2)
                       (+ x 3))))
          max-f (comp g/negate min-f)
          expected {:lo [-2 0]
                    :mid [-1 -4]
                    :hi [0.6180339887498949 -3.6180339887498945]
                    :fncalls 3
                    :iterations 0}]
      (is (ish? expected (b/bracket-min min-f {:xa -2 :xb -1})))
      (is (ish? expected (b/bracket-max max-f {:xa -2 :xb -1})))))

  (checking "bracket-{min,max} properly brackets a quadratic"
            100
            [lower  gen/large-integer
             upper  gen/large-integer
             offset gen/small-integer]
            (let [upper (if (= lower upper) (inc lower) upper)
                  min-f (fn [x] (g/square (- x offset)))]
              (testing "bracket-min"
                (let [{:keys [lo hi fncalls iterations]} (b/bracket-min min-f {:xa lower :xb upper})]
                  (is (<= (first lo) offset)
                      "bracket-min lower bound is <= argmin.")
                  (is (>= (first hi) offset)
                      "bracket-min upper bound is >= argmin.")))

              (let [max-f (comp g/negate min-f)
                    {:keys [lo hi]} (b/bracket-max max-f {:xa lower :xb upper})]
                (is (<= (first lo) offset)
                    "bracket-max lower bound is <= argmax.")
                (is (>= (first hi) offset)
                    "bracket-max upper bound is >= argmax."))))

  ;; from scipy:
  #_(-1.618034, -4.999999999999998, -10.472135974843995, 11.437694025156002, 3.1554436208840472e-30, 29.944272127181844, 5)

  #_((sicmutils.calculus.derivative/D
      (lagrange-interpolating-polynomial [-3 10] [-1 8] [1 6.1])) (parabolic-step [-3 10] [-1 8] [1 6.1])))

(deftest scmutils-bracket-test
  (testing "bracket"
    (is (ish? {:lo [-46 2116]
               :mid [-12 144]
               :hi [43 1849]
               :fncalls 11
               :converged? true
               :iterations 8}
              (b/bracket-min-from-scmutils g/square {:start -100 :step 1})))))
