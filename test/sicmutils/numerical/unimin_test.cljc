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
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
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
  (let [close?     (v/within 1e-10)
        good-enuf? (fn [[_ fa] [_ fminx] [_ fb] _]
                     (close? (max fa fb) fminx))]
    (with-comparator (v/within 1e-6)
      (testing "minimize"
        (is (ish? {:result 2
                   :value 0
                   :iterations 25}
                  (-> (fn [x] (g/square (- x 2)))
                      (m/golden-section-min 1 5 good-enuf?))))

        (is (ish? {:result 1.5
                   :value -0.8
                   :iterations 30}
                  (-> (fn [x] (- (g/square (- x 1.5)) 0.8))
                      (m/golden-section-min -15 15 good-enuf?))))))))
