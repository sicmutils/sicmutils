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

(ns sicmutils.calculus.bianchi-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.connection :as conn]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.curvature :as curv]
            [sicmutils.calculus.manifold :as m :refer [R3-rect]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest first-bianchi-identity
  (let [C     (cov/symmetrize-Cartan
               (conn/literal-Cartan 'C R3-rect))
        omega (ff/literal-oneform-field 'omega R3-rect)
        X     (vf/literal-vector-field 'X R3-rect)
        Y     (vf/literal-vector-field 'Y R3-rect)
        Z     (vf/literal-vector-field 'Z R3-rect)]
    (is (zero?
         ((+ ((curv/Riemann (cov/covariant-derivative C)) omega X Y Z)
             ((curv/Riemann (cov/covariant-derivative C)) omega Z X Y)
             ((curv/Riemann (cov/covariant-derivative C)) omega Y Z X))
          (m/typical-point R3-rect))))))
