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
  (:refer-clojure :exclude [+ - * /])
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing use-fixtures]])
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.mechanics.rotation :as r]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest hello
  (let [P (up 'x 'y 'z)]
    (is (= '(up x (+ (* y (cos a)) (* -1 z (sin a))) (+ (* y (sin a)) (* z (cos a))))
           (g/simplify ((r/Rx 'a) P))))
    (is (= '(up x (+ (* y (cos a)) (* -1 z (sin a))) (+ (* y (sin a)) (* z (cos a))))
           (g/simplify (* (r/rotate-x-matrix 'a) P))))
    (is (= '(up 0 0 0) (g/simplify (- ((r/Rx 'a) P) (* (r/rotate-x-matrix 'a) P)))))
    (is (= '(up 0 0 0) (g/simplify (- ((r/Ry 'a) P) (* (r/rotate-y-matrix 'a) P)))))
    (is (= '(up 0 0 0) (g/simplify (- ((r/Rz 'a) P) (* (r/rotate-z-matrix 'a) P)))))
    ))

(deftest rotation-from-structure-tests
  (testing "function - rotate about x axis"
    (is (= (up 0 0 1) ((r/Rx 'pi-over-2) (up 0 1 0))))
    (is (= (up 'x (- 'z) 'y) ((r/Rx 'pi-over-2) (up 'x 'y 'z))))))
