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

(ns sicmutils.numerical.unimin-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numerical.unimin :as m]))

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
      (is (ish? expected (m/bracket-min min-f {:xa -2 :xb -1})))
      (is (ish? expected (m/bracket-max max-f {:xa -2 :xb -1})))))

  (checking "bracket-{min,max} properly brackets a quadratic"
            100
            [lower  gen/large-integer
             upper  gen/large-integer
             offset gen/small-integer]
            (let [upper (if (= lower upper) (inc lower) upper)
                  min-f (fn [x] (g/square (- x offset)))]
              (testing "bracket-min"
                (let [{:keys [lo hi fncalls iterations]} (m/bracket-min min-f {:xa lower :xb upper})]
                  (is (<= (first lo) offset)
                      "bracket-min lower bound is <= argmin.")
                  (is (>= (first hi) offset)
                      "bracket-min upper bound is >= argmin.")))

              (let [max-f (comp g/negate min-f)
                    {:keys [lo hi]} (m/bracket-max max-f {:xa lower :xb upper})]
                (is (<= (first lo) offset)
                    "bracket-max lower bound is <= argmax.")
                (is (>= (first hi) offset)
                    "bracket-max upper bound is >= argmax."))))




  ;; from scipy:

  #_(-1.618034, -4.999999999999998, -10.472135974843995, 11.437694025156002, 3.1554436208840472e-30, 29.944272127181844, 5)

  #_((sicmutils.calculus.derivative/D
      (lagrange-interpolating-polynomial [-3 10] [-1 8] [1 6.1])) (parabolic-step [-3 10] [-1 8] [1 6.1])))

(deftest golden-cut-tests
  (checking "golden-cut"
            100
            [l gen/small-integer, r gen/small-integer]
            (let [lo (min l r)
                  hi (max l r)
                  cut (m/golden-cut l r)]
              (is (<= lo cut hi) "golden-cut is always between (or
              equal to) the two numbers.")

              (when (not= l r)
                ;; `golden-cut` returns the golden-ratioed point closer to the
                ;; right side. So the ratio of this point relative to the
                ;; original segment has to be equal to the ratio between the
                ;; shorter and longer cuts.
                (ish? (/ (m/golden-cut l r)
                         (- r l))
                      (/ (m/golden-cut r l)
                         (m/golden-cut l r))))

              ;; m/golden-cut returns the point between the two inputs such that
              (ish? hi
                    (+ (m/golden-cut l r)
                       (m/golden-cut r l)))

              ;; Golden-cutting twice in one direction is equivalent to cutting
              ;; once in the reverse direction.
              (ish? (m/golden-cut l (m/golden-cut l r))
                    (m/golden-cut r l)))))

(deftest golden-section-tests
  (let [close? (v/within 1e-10)
        good-enuf? (fn [[_ fa] [_ fl] [_ fr] [_ fb] iterations]
                     (or (> iterations 100)
                         (close? (max fa fb)
                                 (min fl fr))))]
    (with-comparator (v/within 1e-6)
      (testing "minimize"
        (is (ish? {:result 2
                   :value 0
                   :iterations 26
                   :fncalls 30}
                  (-> (fn [x] (g/square (- x 2)))
                      (m/golden-section-min* 1 5 {:stop? good-enuf?}))))

        (is (ish? {:result 1.5
                   :value -0.8
                   :iterations 31
                   :fncalls 35}
                  (-> (fn [x] (- (g/square (- x 1.5)) 0.8))
                      (m/golden-section-min -15 15 {:stop? good-enuf?}))))))))
