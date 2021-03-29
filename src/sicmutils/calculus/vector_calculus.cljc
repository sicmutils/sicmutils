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

(ns sicmutils.calculus.vector-calculus
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.hodge-star :as hs]
            [sicmutils.calculus.metric :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.structure :as s]))

;; Traditional vector calculus operators
;;
;; TODO move the other ones here!

(defn gradient [metric basis]
  (f/compose (m/raise metric basis) d))

(defn curl [metric orthonormal-basis]
  (let [star  (hs/Hodge-star metric orthonormal-basis)
        sharp (m/raise metric orthonormal-basis)
        flat  (m/lower metric)]
    (f/compose sharp star ff/d flat)))

(defn divergence [metric orthonormal-basis]
  (let [star (hs/Hodge-star metric orthonormal-basis)
        flat (e/lower metric)]
    (f/compose star ff/d star flat)))

(defn Laplacian [metric orthonormal-basis]
  (f/compose (divergence metric orthonormal-basis)
             (gradient metric orthonormal-basis)))

(defn coordinate-system->Lame-coefficients [coordinate-system]
  (let [gij (m/coordinate-system->metric-components coordinate-system)]
    (assert (matrix/diagonal? gij))
    (let [n (:dimension (m/manifold coordinate-system))]
      (s/generate n ::s/down
                  (fn [i]
                    (g/sqrt (get-in gij [i i])))))))

(defn coordinate-system->orthonormal-vector-basis [coordsys]
  (let [vector-basis (vf/coordinate-system->vector-basis coordsys)
        Lame-coefs   (coordinate-system->Lame-coefficients coordsys)
        n (:dimension (m/manifold coordsys))]
    (s:generate n ::s/down
                (fn [i]
                  (* (nth vector-basis i)
                     (/ 1 (f/compose (ref Lame-coefs i)
                                     (m/chart coordsys))))))))
