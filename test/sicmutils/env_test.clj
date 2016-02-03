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

(ns sicmutils.env-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]))

(deftest partial-shim
  (testing "partial also works the way Clojure defines it"
    (is (= 10 ((partial + 9) 1)))
    (is (= 5 ((partial max 4) 5)))
    (is (= 4 ((partial max 4) 2)))))

(deftest ref-shim
  (testing "works for structures"
    (is (= 2 (ref (up 1 2 3) 1)))
    (is (= 3 (ref (down (up 1 2) (up 3 4)) 1 0))))
  (testing "works clojure-style"
    (let [r (ref [])
          s (ref {} :meta {:a "apple"})]
      (is (= [] @r))
      (is (= [99] (dosync (alter r conj 99))))
      (is (= {:b 88} (dosync (alter s assoc :b 88)))))))
