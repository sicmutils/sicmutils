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

(ns sicmutils.mechanics.rotation-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :refer :all]
             [function :refer :all]
             [numbers]
             [simplify :refer [pe hermetic-simplify-fixture]]
             [structure :refer :all]]
            [sicmutils.mechanics.rotation :refer :all]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest hello
  (let [P (up 'x 'y 'z)]
    (is (= '(up x (+ (* y (cos a)) (* -1 z (sin a))) (+ (* y (sin a)) (* z (cos a))))
           (simplify ((Rx 'a) P))))
    (is (= '(up x (+ (* y (cos a)) (* -1 z (sin a))) (+ (* y (sin a)) (* z (cos a))))
           (simplify (* (rotate-x-matrix 'a) P))))
    (is (= '(up 0 0 0) (simplify (- ((Rx 'a) P) (* (rotate-x-matrix 'a) P)))))
    (is (= '(up 0 0 0) (simplify (- ((Ry 'a) P) (* (rotate-y-matrix 'a) P)))))
    (is (= '(up 0 0 0) (simplify (- ((Rz 'a) P) (* (rotate-z-matrix 'a) P)))))
    ))

(deftest rotation-from-structure-tests
  (testing "function - rotate about x axis"
    (is (= (up 0 0 1) ((Rx 'pi-over-2) (up 0 1 0))))
    (is (= (up 'x (- 'z) 'y) ((Rx 'pi-over-2) (up 'x 'y 'z))))))
