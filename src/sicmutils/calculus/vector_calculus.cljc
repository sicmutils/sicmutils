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
            [sicmutils.calculus.manifold :as cm]
            [sicmutils.calculus.metric :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]))

;; ## Traditional vector calculus operators, defined using `D` and in FDG
;; ## appropriate manner.

(def ^{:doc "Operator that takes a function `f` and returns a new function that
  calculates the [Gradient](https://en.wikipedia.org/wiki/Gradient) of `f`.

  The related [[D]] operator returns a function that produces a structure of the
  opposite orientation as [[Grad]]. Both of these functions use forward-mode
  automatic differentiation."}
  Grad
  (-> (fn [f]
        (f/compose s/opposite
                   (g/partial-derivative f [])))
      (o/make-operator 'Grad)))

(defn gradient [metric basis]
  (f/compose (m/raise metric basis) ff/d))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Divergence](https://en.wikipedia.org/wiki/Divergence) of
  `f` at its input point.

 The divergence is a one-level contraction of the gradient."}
  Div
  (-> (f/compose g/trace Grad)
      (o/make-operator 'Div)))

(defn divergence [metric orthonormal-basis]
  (let [star (hs/Hodge-star metric orthonormal-basis)
        flat (m/lower metric)]
    (f/compose star ff/d star flat)))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Curl](https://en.wikipedia.org/wiki/Curl_(mathematics)) of `f`
  at its input point.

  `f` must be a function from $\\mathbb{R}^3 \\to \\mathbb{R}^3$."}
  Curl
  (-> (fn [f-triple]
        (let [[Dx Dy Dz] (map partial [0 1 2])
              fx (f/get f-triple 0)
              fy (f/get f-triple 1)
              fz (f/get f-triple 2)]
          (s/up (g/- (Dy fz) (Dz fy))
                (g/- (Dz fx) (Dx fz))
                (g/- (Dx fy) (Dy fx)))))
      (o/make-operator 'Curl)))

(defn curl [metric orthonormal-basis]
  (let [star  (hs/Hodge-star metric orthonormal-basis)
        sharp (m/raise metric orthonormal-basis)
        flat  (m/lower metric)]
    (f/compose sharp star ff/d flat)))


(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Vector
  Laplacian](https://en.wikipedia.org/wiki/Laplace_operator#Vector_Laplacian) of
  `f` at its input point."}
  Lap
  (-> (f/compose g/trace (g/* Grad Grad))
      (o/make-operator 'Lap)))

(defn Laplacian [metric orthonormal-basis]
  (f/compose (divergence metric orthonormal-basis)
             (gradient metric orthonormal-basis)))

(defn coordinate-system->Lame-coefficients [coordinate-system]
  (let [gij (m/coordinate-system->metric-components coordinate-system)]
    (assert (matrix/diagonal? gij))
    (let [n (:dimension (cm/manifold coordinate-system))]
      (s/generate n ::s/down
                  (fn [i]
                    (g/sqrt (get-in gij [i i])))))))

(defn coordinate-system->orthonormal-vector-basis [coordsys]
  (let [vector-basis (vf/coordinate-system->vector-basis coordsys)
        Lame-coefs (coordinate-system->Lame-coefficients coordsys)
        n (:dimension (cm/manifold coordsys))]
    (s/generate n ::s/down
                (fn [i]
                  (* (nth vector-basis i)
                     (/ 1 (f/compose
                           (get Lame-coefs i)
                           (cm/chart coordsys))))))))
