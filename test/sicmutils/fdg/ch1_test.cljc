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

(ns sicmutils.fdg.ch1-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [sicmutils.env :as e :refer [+ - * /
                                         D simplify compose
                                         up down
                                         sin cos square]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn Lfree
  [mass]
  (fn [[t q v]]
    (* (/ 1 2) mass (square v))))

(defn sphere->R3
  [R]
  (fn [[t [theta phi] v]]
    (up (* R (sin theta) (cos phi))                         ; x
        (* R (sin theta) (sin phi))                         ; y
        (* R (cos theta)))))                                ; z

(defn Lsphere
  [m R]
  (compose (Lfree m) (e/F->C (sphere->R3 R))))

(defn L2
  [mass metric]
  (fn [place velocity]
    (* (/ 1 2) mass ((metric velocity velocity) place))))

(deftest chapter-one
  (is (= '(+ (* (/ 1 2) (expt R 2) m (expt phidot 2) (expt (sin theta) 2))
             (* (/ 1 2) (expt R 2) m (expt thetadot 2)))
         (v/freeze
          (simplify
           ((Lsphere 'm 'R)
            (up 't (up 'theta 'phi) (up 'thetadot 'phidot))))))))
