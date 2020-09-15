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

(ns sicmutils.numerical.unimin.brent-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.numerical.unimin.brent :as b]))

(deftest brent-tests
  (testing "basic brent minimization"
    (is (ish?
         {:result 2.000000000000032
          :value 0
          :iterations 10
          :fncalls 11
          :converged? true}
         (-> (fn [x] (g/square (- x 2)))
             (b/brent-min -1000 10))))))
