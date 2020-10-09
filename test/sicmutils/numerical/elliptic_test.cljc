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

(ns sicmutils.numerical.elliptic-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.numerical.elliptic :as e]
            [sicmutils.value :as v]))

(def ^:private near (v/within 1e-6))

(deftest elliptic-tests
  (testing "elliptic"
    (is (near 1.30567  (e/elliptic-f 1.2 (Math/sqrt 0.4)))))

  (testing "direct elliptic"
    (is (near 0.200212 (e/elliptic-f 0.2 0.4)))
    (is (near 0.841935 (e/elliptic-f 0.8 0.7)))
    (is (near 0.303652 (e/elliptic-f 0.3 (Math/sqrt 0.8))))
    (is (near 0.300712 (e/elliptic-f 0.3 0.4)))
    (is (near 0.738059 (e/elliptic-f 0.7 0.8))))

  (testing "general pendulum periods"
    (let [period (fn [theta_0]
                   (/ (* 8 (e/elliptic-f (/ theta_0 2) (/ (Math/sin (/ theta_0 2)))))
                      (* (Math/sqrt (* 2 9.8))
                         (Math/sqrt (- 1 (Math/cos theta_0))))))]
      (is (near 2.009916 (period 0.15)))
      (is (near 2.018438 (period 0.30)))
      (is (near 2.032791 (period 0.45)))
      (is (near 2.053204 (period 0.60)))
      (is (near 2.080013 (period 0.75)))
      (is (near 2.113680 (period 0.90)))
      (is (near 2.154814 (period 1.05)))
      (is (near 2.204206 (period 1.20)))
      (is (near 2.262882 (period 1.35)))
      (is (near 2.332176 (period 1.50)))
      (is (near 2.413836 (period 1.65)))
      (is (near 2.510197 (period 1.80)))
      (is (near 2.624447 (period 1.95))))))
