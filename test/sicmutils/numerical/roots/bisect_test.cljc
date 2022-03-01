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

(ns sicmutils.numerical.roots.bisect-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.numerical.roots.bisect :as bi]
            [sicmutils.value :as v]))

(deftest bisect-tests
  (doseq [method bi/all-methods]
    (is (= {:result 0
            :value 0
            :iterations 0
            :converged? true
            :fncalls 2}
           (bi/bisect g/square 0 1 {:method method}))
        (str method "returns 0 at bounds"))

    (is (= {:error "Root not bounded"
            :bounds {:lower 2, :f-lower 4, :upper 3, :f-upper 9}
            :iterations 0
            :converged? false
            :fncalls 2}
           (bi/bisect g/square 2 3 {:method method}))
        (str method " errors if no root's bounded")))

  (letfn [(kepler [ecc m opts]
            (bi/bisect
             (fn [e]
               (- e (* ecc (g/sin e)) m))
             0.0 v/twopi opts))]
    (is (ish? {:result 0.34227031649177475
               :value 0.0
               :fncalls 58
               :iterations 55
               :converged? true}
              (kepler 0.99 0.01 {:method :bisection}))
        "bisection method")

    (is (ish? {:result 0.34227031649177486
               :value 0.0
               :iterations 540
               :fncalls 543
               :converged? true}
              (kepler 0.99 0.01 {:method :secant}))
        "secant method")

    (is (ish? {:result 0.3422703164917748
               :value 0.0
               :iterations 20
               :fncalls 23
               :converged? true}
              (kepler 0.99 0.01 {:method :mixed :n-break 10}))
        "mixed method"))

  (testing "search-for-roots"
    (letfn [(poly [x] (* (- x 1) (- x 2) (- x 3)))]
      (let [dx 2]
        (is (ish? [1 2 3]
                  (bi/search-for-roots poly -10 10 dx))
            "search-for-roots finds all roots.")))))
