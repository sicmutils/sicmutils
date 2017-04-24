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
  (:refer-clojure :exclude [* - / + ref zero? partial])
  (:require
    [clojure.test :refer :all]
    [sicmutils.env :refer :all]
    ;; objects in manifold ns will need to graduate to env
    [sicmutils.calculus.manifold :refer :all]))


(deftest rectangular
  (testing "R2"
    (let [rect (->Rectangular R2)]
      (testing "check-coordinates"
        (is (not (check-cooridinates rect (up 1))))
        (is (check-cooridinates rect (up 1 2)))
        (is (not (check-cooridinates rect (up 1 2 3))))
        (is (thrown? UnsupportedOperationException (check-cooridinates rect 99))))
      (testing "coords->point"
        (let [p (coords->point rect (up 3 4))]
          (is (= (up 3 4) (get-coordinates p rect (fn do-not-call [] (throw (IllegalStateException.)))) ))
          (is (check-point rect p)))))))

