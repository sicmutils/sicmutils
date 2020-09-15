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
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numerical.unimin.bracket :as b]
            [sicmutils.numerical.unimin.golden :as ug]))

;; TODO add the ability for the golden section stuff to share code.

(deftest golden-cut-tests
  (checking "golden-cut"
            100
            [l gen/small-integer, r gen/small-integer]
            (let [lo (min l r)
                  hi (max l r)
                  cut (ug/golden-cut l r)]
              (is (<= lo cut hi) "golden-cut is always between (or
              equal to) the two numbers.")

              (when (not= l r)
                ;; `golden-cut` returns the golden-ratioed point closer to the
                ;; right side. So the ratio of this point relative to the
                ;; original segment has to be equal to the ratio between the
                ;; shorter and longer cuts.
                (ish? (/ (ug/golden-cut l r)
                         (- r l))
                      (/ (ug/golden-cut r l)
                         (ug/golden-cut l r))))

              ;; ug/golden-cut returns the point between the two inputs such
              ;; that they add up to the original interval.
              (ish? hi
                    (+ (ug/golden-cut l r)
                       (ug/golden-cut r l)))

              ;; Golden-cutting twice in one direction is equivalent to cutting
              ;; once in the reverse direction.
              (ish? (ug/golden-cut l (ug/golden-cut l r))
                    (ug/golden-cut r l)))))

(deftest golden-section-tests
  (let [close? (v/within 1e-10)
        good-enuf? (fn [[_ fa] [_ fl] [_ fr] [_ fb] iterations]
                     (or (> iterations 100)
                         (close? (max fa fb)
                                 (min fl fr))))]
    (with-comparator (v/within 1e-6)
      (testing "with-helper"
        (let [f (fn [x] (* x x))
              {:keys [lo hi]} (b/bracket-min f {:xa -100
                                                :xb -99})]
          (is (ish?
               {:result 0
                :value 0
                :iterations 35
                :fncalls 39}
               (ug/golden-section-min f {:xa (first lo)
                                         :xb (first hi)
                                         :stop? good-enuf?})))))
      (testing "minimize"
        (is (ish? {:result 2
                   :value 0
                   :iterations 26
                   :fncalls 30}
                  (-> (fn [x] (g/square (- x 2)))
                      (ug/golden-section-min {:xa 1
                                              :xb 5
                                              :stop? good-enuf?}))))

        (is (ish? {:result 1.5
                   :value -0.8
                   :iterations 31
                   :fncalls 35}
                  (-> (fn [x] (- (g/square (- x 1.5)) 0.8))
                      (ug/golden-section-min {:xa -15
                                              :xb 15
                                              :stop? good-enuf?}))))))))
