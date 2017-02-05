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

(ns sicmutils.examples.top-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.examples.top :refer :all]))

(deftest Top
  (let [state (up 't (up 'theta 'phi 'psi) (up 'thetadot 'phidot 'psidot))]
    (is (= '(+ (* 1/2 (expt (sin theta) 2) A (expt phidot 2))
               (* 1/2 (expt (cos theta) 2) C (expt phidot 2))
               (* (cos theta) C phidot psidot)
               (* 1/2 A (expt thetadot 2))
               (* 1/2 C (expt psidot 2))
               (* -1 (cos theta) gMR))
           (simplify ((L-axisymmetric 'A 'C 'gMR) state))))))
