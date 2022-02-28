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
            [sicmutils.numerical.roots.bisect :as bi]
            [sicmutils.value :as v]))

(deftest bisect-1-tests
  (letfn [(kepler-1 [ecc m]
            (let [evals (atom 0)
                  result (bi/bisect-1
                          (fn [e]
                            (swap! evals inc)
                            (- e (* ecc (g/sin e)) m))
                          0.0
                          v/twopi)]
              [@evals result]))]
    (testing "for example, this took 51 evaluations of f."
      (let [[evals result] (kepler-1 0.99 0.01)]
        (is (= 58 evals))
        (is (ish? 0.34227031649177486
                  result)))))

  (letfn [(kepler-2 [ecc m]
            (let [evals (atom 0)
                  result (bi/bisect-2
                          (fn [e]
                            (swap! evals inc)
                            (- e (* ecc (g/sin e)) m))
                          0.0
                          v/twopi
                          1e-15)]
              [@evals result]))]
    (testing "for example, this took 51 evaluations of f."
      (let [[evals result] (kepler-2 0.99 0.01)]
        (is (= 58 evals))
        (is (ish? 0.34227031649177486
                  result))))))
