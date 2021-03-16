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

(ns sicmutils.calculus.basis-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :as c
             #?(:clj :refer :cljs :refer-macros) [let-coordinates]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.abstract.function :refer [literal-function]]
            [sicmutils.generic :as g]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

(deftest basis-tests
  (testing "vector-basis->dual"
    (let-coordinates [[theta phi] m/S2-spherical]
      (let [e0 (vf/components->vector-field
                (up (literal-function 'e0t '(-> (UP* Real 2) Real))
                    (literal-function 'e0p '(-> (UP* Real 2) Real)))
                S2-spherical)
            e1 (vf/components->vector-field
                (up (literal-function 'e1t '(-> (UP* Real 2) Real))
                    (literal-function 'e1p '(-> (UP* Real 2) Real)))
                S2-spherical)
            edual (b/vector-basis->dual (down e0 e1) S2-spherical)]
        (is (= (up (down 1 0)
                   (down 0 1))
               (g/simplify
                ((edual (down e0 e1))
                 ((m/point S2-spherical)
                  (up 'theta0 'phi0)))))))))

  (testing "Jacobian"
    (let [v   (vf/literal-vector-field 'v m/R2-rect)
          vjp (g/* (b/Jacobian
                    (b/coordinate-system->basis m/R2-polar)
                    (b/coordinate-system->basis m/R2-rect))
                   ((ff/coordinate-basis-oneform-fields m/R2-rect) v))]
      (is (= '(up
               (/ (+ (* x (v↑0 (up x y))) (* y (v↑1 (up x y))))
                  (sqrt (+ (expt x 2) (expt y 2))))
               (/ (+ (* x (v↑1 (up x y))) (* -1 y (v↑0 (up x y))))
                  (+ (expt x 2) (expt y 2))))
             (v/freeze
              (g/simplify
               (vjp ((m/point m/R2-rect) (up 'x 'y))))))))))
