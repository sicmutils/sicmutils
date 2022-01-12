;;
;; Copyright © 2022 Sam Ritchie.
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

(ns sicmutils.algebra.fold-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.numbers]
            [sicmutils.algebra.fold :as af]))

(deftest fold-tests
  (is (= 45 (af/generic-sum-fold
             (reduce af/generic-sum-fold (af/generic-sum-fold)
                     (range 10))))
      "example of using a fold in the raw.")

  (let [sum (af/fold->sum-fn af/generic-sum-fold)
        scan (af/fold->scan-fn af/generic-sum-fold)]
    (is (= 45 (sum (range 10)))
        "summing via fold->sum-fn.")

    (is (= [0 1 3 6 10 15 21 28 36 45]
           (scan (range 10)))
        "scanning via fold->scan-fn."))

  (letfn [(average
            ([] [0.0 0])
            ([[sum n]] (/ sum n))
            ([[sum n] x]
             [(+ sum x) (inc n)]))]
    (let [sum (af/fold->sum-fn average)
          scan (af/fold->scan-fn average)
          xs (range 1 10)]
      (is (= 4.5 (sum (range 10)))
          "the average fold does the right thing, with intermediate state built
          up.")))

  (testing "join and primitive tests"
    (let [fold (af/join af/min af/max (af/constant "face") af/generic-sum-fold)
          sum (af/fold->sum-fn fold)
          scan (af/fold->scan-fn fold)]
      (is (= [0 9 "face" 45]
             (sum (range 10)))
          "join runs folds in parallel.")

      (is (= [[0 0 "face" 0]
              [0 1 "face" 1]
              [0 2 "face" 3]
              [0 3 "face" 6]
              [0 4 "face" 10]]
             (scan (range 5)))
          "join runs folds in parallel."))))

(deftest compensated-summation-tests
  (let [xs  [1.0 1e-8 -1e-8]
        xs2 [1.0 1e100 1.0 -1e100]]
    (is (not= 1 ((af/fold->sum-fn af/generic-sum-fold) xs))
        "Without the summation trick, errors build up.")

    (is (= 1.0 ((af/fold->sum-fn af/kahan) xs))
        "kahan resolves this.")

    (is (= 0.0 ((af/fold->sum-fn af/kahan) xs2))
        "kahan breaks when new numbers are much bigger than the accumulated
        total so far!")

    (is (= 2.0 ((af/fold->sum-fn af/kbn) xs2))
        "kahan-babushka-neumaier fixes this.")

    (is (= ((af/fold->sum-fn af/kahan-babushka-klein) xs2)
           ((af/fold->sum-fn af/kbn) xs2))
        "kahan-babushka-neumaier matches the klein variant on this.")

    (let [kbk10 (af/kbk-n 10)
          sum (af/fold->sum-fn kbk10)]
      (is (= 2.0 (sum xs2))
          "the special, macro-generated 10th order klein algorithm works
          too :)"))))
