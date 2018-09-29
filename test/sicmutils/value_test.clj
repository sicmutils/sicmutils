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

(ns sicmutils.value-test
  (:require [clojure.test :refer :all]
            [sicmutils.value :refer :all])
  (:import (clojure.lang PersistentVector)))

(deftest kinds
  (is (= Long (kind 1)))
  (is (= Double (kind 1.0)))
  (is (= PersistentVector (kind [1 2]))))

(deftest argument-kinds
  (let [L Long
        V PersistentVector]
    (is (= [L] (argument-kind 1)))
    (is (= [L L L] (argument-kind 1 2 3)))
    (is (= [V] (argument-kind [2 3])))
    (is (= [V V] (argument-kind [1] [3 4])))))
