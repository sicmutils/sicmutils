#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.unimin.golden-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish? zeroish? with-comparator] :include-macros true]
            [emmy.generic :as g]
            [emmy.numerical.unimin.bracket :as b]
            [emmy.numerical.unimin.golden :as ug]
            [emmy.value :as v]))

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

(defn golden-checker
  "Takes a description string, function of an offset, a bracketer and min/max
  optimizer and runs a bunch of tests."
  [description f bracket-fn optimizer]
  (with-comparator (v/within 1e-5)
    (checking description
              100
              [lower gen/small-integer
               upper  gen/small-integer
               offset gen/small-integer]
              (let [f (f offset)
                    upper (if (= lower upper) (inc lower) upper)
                    {:keys [lo hi]} (bracket-fn f {:xa lower :xb upper})
                    {:keys [result iterations fncalls]}
                    (optimizer f lo hi
                               {:fn-tolerance 1e-10
                                :callback (fn [[xa] [xl] [xr] [xb] _]
                                            (is (< xa xl xr xb)
                                                "the l and r points
                                                            stay fully within
                                                            the bounds."))})]

                (is (ish? result offset) "The result converges to the supplied offset.")

                (is (= fncalls (+ 2 iterations))
                    "The bound search takes care of 2 fncalls, so we only need 2
                    additional (for the first interior points) in addition to 1
                    per iteration.")))))

(deftest golden-section-tests
  (golden-checker "golden-min finds a quadratic min with help from bracket-min."
                  (fn [offset] (fn [x] (g/square (- x offset))))
                  b/bracket-min
                  ug/golden-section-min)

  (golden-checker "golden-max finds a quadratic max with help from bracket-max."
                  (fn [offset] (fn [x] (- (g/square (- x offset)))))
                  b/bracket-max
                  ug/golden-section-max)

  (with-comparator (v/within 1e-5)
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
