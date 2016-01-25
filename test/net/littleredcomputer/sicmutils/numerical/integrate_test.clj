;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns net.littleredcomputer.sicmutils.numerical.integrate-test
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.sicmutils.value :as v]
            [net.littleredcomputer.sicmutils.numerical.integrate :refer :all]))

(def ^:private near (v/within 1e-6))

(def ^:private natural-log (partial integrate / 1.))

(def ^:private sine (partial integrate #(Math/cos %) 0.))

(defn bessel-j0 [x]
  (/ (integrate #(Math/cos (- (* x (Math/sin %)))) 0. Math/PI) Math/PI))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (integrate #(* % %) 0. 1.)))
    (is (near 0.5 (integrate identity 0. 1.)))
    (is (near 3 (integrate (constantly 1.0) 0. 3.)))
    (is (near 0 (integrate (constantly 0.0) 0. 1000.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 0 (sine Math/PI)))
    (is (near 1 (sine (/ Math/PI 2))))
    (is (near 0.7651976 (bessel-j0 1)))
    (is (near -0.2459358 (bessel-j0 10)))
    ))
