;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.mechanics.rotation-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.generic :as g :refer [+ - * / cos sin]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.mechanics.rotation :as r]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest rotation-tests
  (let [P (up 'x 'y 'z)]
    (testing "rotate about x axis"
      (is (= (up 0 0 1)
             ((r/Rx 'pi-over-2) (up 0 1 0))))

      (is (= (up 'x (- 'z) 'y)
             ((r/Rx 'pi-over-2) (up 'x 'y 'z)))))

    (is (= '(up x
                (+ (* y (cos a)) (* -1 z (sin a)))
                (+ (* y (sin a)) (* z (cos a))))
           (simplify
            ((r/Rx 'a) P))
           (simplify
            (* (r/rotate-x-matrix 'a) P)))
        "Rotation example; applying `(r/Rx 'a)` is identical to multiplication
        by the rotation matrix.")

    (testing "rotation functions match rotation matrix multiplication."
      (is (= (up 0 0 0)
             (g/simplify
              (- ((r/Rx 'a) P)
                 (* (r/rotate-x-matrix 'a) P)))))

      (is (= (up 0 0 0)
             (g/simplify
              (- ((r/Ry 'a) P)
                 (* (r/rotate-y-matrix 'a) P)))))

      (is (= (up 0 0 0)
             (g/simplify
              (- ((r/Rz 'a) P)
                 (* (r/rotate-z-matrix 'a) P)))))))

  (testing "alternate definitions of rotation functions"
    (letfn [(rotate-x-2 [c s]
              (let [tuple (r/rotate-x-tuple-2 c s)]
                (fn [v] (* tuple v))))
            (rotate-x [angle]
              (rotate-x-2 (cos angle)
                          (sin angle)))]
      (is (= (up 0 0 0)
             (g/simplify
              ((- (r/Rx 'a) (rotate-x 'a))
               (up 'x 'y 'z))))
          "r/Rx matches alternative definition from scmutils."))

    (letfn [(rotate-y-2 [c s]
              (let [tuple (r/rotate-y-tuple-2 c s)]
                (fn [v] (* tuple v))))

            (rotate-y [angle]
              (rotate-y-2 (cos angle) (sin angle)))]
      (is (= (up 0 0 0)
             (g/simplify
              ((- (r/Ry 'a) (rotate-y 'a))
               (up 'x 'y 'z))))
          "r/Ry matches alternative definition from scmutils."))

    (letfn [(rotate-z-2 [c s]
              (let [tuple (r/rotate-z-tuple-2 c s)]
                (fn [v] (* tuple v))))

            (rotate-z [angle]
              (rotate-z-2 (cos angle) (sin angle)))]
      (is (= (up 0 0 0)
             (g/simplify
              ((- (r/Rz 'a) (rotate-z 'a))
               (up 'x 'y 'z))))
          "r/Rz matches alternative definition from scmutils."))))
