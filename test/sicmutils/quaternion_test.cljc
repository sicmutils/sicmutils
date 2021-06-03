;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.quaternion-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.function :as f]
            [sicmutils.generators :as sg]
            [sicmutils.laws :as sl]
            [sicmutils.generic :as g]
            [sicmutils.structure :as s]
            [sicmutils.quaternion :as q]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(defn v:make-unit
  "TODO move this to a collections, or vector namespace?"
  [v]
  (g/* (g/invert (g/abs v))
		   v))

(deftest basic-tests
  (is (= '(up theta
              (up x y (sqrt (+ (* -1 (expt x 2))
                               (* -1 (expt y 2))
                               1))))
         (v/freeze
          (g/simplify
           (q/->angle-axis
            (q/angle-axis->
             'theta
             ['x 'y (g/sqrt
                     (g/- 1 (g/square 'x) (g/square 'y)))]))))))


  (is (= (s/up 0 (s/up 0 0 0))
         (g/simplify
          (let [theta 'theta
                v (s/up 'x 'y 'z)
                axis (v:make-unit v)
                [theta' axis'] (-> (q/angle-axis-> theta axis)
                                   (q/->rotation-matrix)
                                   (q/rotation-matrix->)
                                   (q/->angle-axis))]
            (s/up (g/- theta' theta)
                  (g/- axis' axis))))))

  ;; But look at (show-notes) to see the assumptions.
  ;;
  ;; Indeed:
  (is (= (s/up 2.0
               (s/up -0.5345224838248488
                     -1.0690449676496976
                     -1.6035674514745464))
         (let [theta -1
               v (s/up 1 2 3)
               axis (v:make-unit v)
               [theta' axis'] (-> (q/angle-axis-> theta axis)
                                  (q/->rotation-matrix)
                                  (q/rotation-matrix->)
                                  (q/->angle-axis))]
           (s/up (g/- theta' theta)
                 (g/- axis' axis))))))

(deftest arithmetic-tests
  #_(sl/field 100 quat-gen "quaternions" :skew? true))
