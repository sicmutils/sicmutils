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

(ns sicmutils.numerical.unimin.golden-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish? zeroish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numerical.unimin.bracket :as b]
            [sicmutils.numerical.unimin.golden :as ug]))

(deftest golden-ratio-tests
  (testing "constants work as defined"
    (is (ish? ug/inv-phi  (/ 1 ug/phi)))
    (is (ish? ug/inv-phi2 (/ 1 (* ug/phi ug/phi))))
    (is (ish? ug/inv-phi2 (/ ug/inv-phi ug/phi)))))

(deftest golden-cut-tests
  (with-comparator (v/within 1e-8)
    (checking "golden-cut"
              100
              [l gen/small-integer, r gen/small-integer]
              (let [lo (min l r)
                    hi (max l r)
                    cut (ug/golden-cut l r)]
                (is (<= lo cut hi) "golden-cut is always between (or
              equal to) the two numbers."))

              (when (not= l r)
                (is (ish? (/ (- (ug/golden-cut l r) l)
                             (- r l))
                          (/ (- (ug/golden-cut r l) l)
                             (- (ug/golden-cut l r) l)))
                    "`golden-cut` returns the golden-ratioed point closer to the
                     right side. So the ratio of this point relative to the
                     original segment has to be equal to the ratio between the
                     shorter and longer cuts."))

              (is (zeroish?
                   (+ (- (ug/golden-cut l r) l)
                      (- (ug/golden-cut r l) r)))
                  "ug/golden-cut returns the point between the two inputs such
                  that they add up to the original interval.")

              (is (ish? (ug/golden-cut l (ug/golden-cut l r))
                        (ug/golden-cut r l))
                  "Golden-cutting twice in one direction is equivalent to
                   cutting once in the reverse direction."))

    (checking "extend-pt vs golden-cut"
              100
              [x gen/small-integer, away-from gen/small-integer]
              (let [x' (ug/extend-pt x away-from)]
                (is (ish? x (ug/golden-cut x' away-from))
                    "Extending x away from a point should place x in the
                    golden-ratioed spot between them.")))))

(deftest golden-section-tests
  (with-comparator (v/within 1e-3)
    (checking (str "golden-section finds a quadratic min with help from bracket.")
              100
              [lower gen/large-integer
               upper  gen/large-integer
               offset gen/small-integer]
              (let [f (fn [x] (g/square (- x offset)))
                    upper (if (= lower upper) (inc lower) upper)
                    {:keys [lo hi]} (b/bracket-min f {:xa lower :xb upper})
                    {:keys [result value converged? iterations fncalls] :as m}
                    (ug/golden-section-min f lo hi {:arg-tolerance 1e-10
                                                    :fn-tolerance 1e-10})]

                (when converged?
                  (when-not (ish? result offset)
                    (prn m))
                  (is (ish? result offset)))))

    (testing "with-helper"
      (let [f (fn [x] (* x x))
            {:keys [lo hi]} (b/bracket-min f {:xa -100 :xb -99})]
        (is (ish?
             {:result 0
              :value 0
              :converged? true
              :iterations 35
              :fncalls 37}
             (ug/golden-section-min f lo hi {:fn-tolerance 1e-10}))
            "Converges on 0, AND reuses the two function evaluations from the
            bracketing process.")))
    (testing "minimize"
      (is (ish? {:result 2
                 :value 0
                 :converged? true
                 :iterations 26
                 :fncalls 30}
                (-> (fn [x] (g/square (- x 2)))
                    (ug/golden-section-min 1 5 {:fn-tolerance 1e-10}))))

      (is (ish? {:result 1.5
                 :value -0.8
                 :converged? true
                 :iterations 29
                 :fncalls 33}
                (-> (fn [x] (- (g/square (- x 1.5)) 0.8))
                    (ug/golden-section-min -15 5 {:fn-tolerance 1e-10})))))))
