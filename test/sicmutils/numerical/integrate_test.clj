;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.numerical.integrate-test
  (:require [clojure.test :refer :all]
            [sicmutils.value :as v]
            [sicmutils.numerical.integrate :refer :all]))

(def ^:private near (v/within 1e-6))

(def ^:private natural-log (partial definite-integral / 1.))

(def ^:private sine (partial definite-integral #(Math/cos %) 0.))

(defn bessel-j0 [x]
  (/ (definite-integral #(Math/cos (- (* x (Math/sin %)))) 0. Math/PI) Math/PI))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (definite-integral #(* % %) 0. 1.)))
    (is (near 0.5 (definite-integral identity 0. 1.)))
    (is (near 3 (definite-integral (constantly 1.0) 0. 3.)))
    (is (near 0 (definite-integral (constantly 0.0) 0. 1000.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 0 (sine Math/PI)))
    (is (near 1 (sine (/ Math/PI 2))))
    (is (near 0.7651976 (bessel-j0 1)))
    (is (near -0.2459358 (bessel-j0 10)))))
