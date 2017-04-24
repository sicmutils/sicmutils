;
; Copyright (C) 2017 Colin Smith.
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

(ns sicmutils.calculus.manifold-test
  (:require
    [clojure.test :refer :all]
    [sicmutils.calculus.manifold :refer :all]))

(deftest rectangular
  (testing "points"
    (let [rect (->Rectangular nil)]
      (is (= 1 1)))))

(deftest cartesian-plane
  (testing "R2"
    (let [R-family (make-manifold-family "R(%d)")
          Rn (attach-patch R-family :origin)
          Rnc (attach-coordinate-system Rn :rectangular :origin ->Rectangular)
          R2 (make-manifold Rnc 2)]
      (is (= 1 1)))))