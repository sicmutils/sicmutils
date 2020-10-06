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

(ns sicmutils.util.aggregate-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.numbers]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

(deftest sum-tests
  (testing "Kahan summation"
    (is (= 1.0 (ua/sum [1.0 1e-8 -1e-8]))
        "Kahan's summation trick allows us to keep precision.")

    (is (not= 1 (reduce + 0.0 [1.0 1e-8 -1e-8]))
        "Without the summation trick, errors build up.")

    (let [xs [1.0 1e-8 -1e-8]]
      (is (= [1.0 1.00000001 1.0] (ua/scanning-sum xs)))

      (is (= (ua/scanning-sum xs)
             ((us/scan ua/kahan-sum :present first) xs))
          "scanning-sum acts just like an actual `scan` call."))))
