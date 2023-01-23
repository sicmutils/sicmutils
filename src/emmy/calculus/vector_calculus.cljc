#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.vector-calculus
  "This namespace contains vector calculus operators, in versions built on top
  of [[derivative/D]] _and_ in Functional Differential Geometry style.

  The former transform functions of scalars or vectors, while the latter take a
  metric and basis."
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.calculus.basis :as b]
            [emmy.calculus.covariant :as cov]
            [emmy.calculus.derivative :as d]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.hodge-star :as hs]
            [emmy.calculus.manifold :as cm]
            [emmy.calculus.metric :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :as f]
            [emmy.generic :as g :refer [- * /]]
            [emmy.matrix :as matrix]
            [emmy.operator :as o]
            [emmy.structure :as s]))

;; Traditional vector calculus operators, defined in two different styles. See
;; the namespace comment for a basic sketch.

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

(defn gradient
  "[[gradient]] implements equation (10.3) in Functional Differential Geometry,
  defined on page 154."
  [metric basis]
  (f/compose (m/raise metric basis) ff/d))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Divergence](https://en.wikipedia.org/wiki/Divergence) of
  `f` at its input point.

 The divergence is a one-level contraction of the gradient."}
  Div
  (-> (f/compose g/trace Grad)
      (o/make-operator 'Div)))

(defn divergence
  "Both arities of [[divergence]] are defined on page 156 of Functional Differential Geometry."
  ([Cartan]
   (let [basis (cov/Cartan->basis Cartan)
         nabla (cov/covariant-derivative Cartan)]
     (fn [v]
       (fn [point]
         (b/contract (fn [ei wi]
                       ((wi ((nabla ei) v)) point))
                     basis)))))

  ([metric orthonormal-basis]
   (let [star (hs/Hodge-star metric orthonormal-basis)
         flat (m/lower metric)]
     (f/compose star ff/d star flat))))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Curl](https://en.wikipedia.org/wiki/Curl_(mathematics)) of `f`
  at its input point.

  `f` must be a function from $\\mathbb{R}^3 \\to \\mathbb{R}^3$."}
  Curl
  (-> (fn [f-triple]
        (let [[Dx Dy Dz] (map d/partial [0 1 2])
              fx (f/get f-triple 0)
              fy (f/get f-triple 1)
              fz (f/get f-triple 2)]
          (s/up (- (Dy fz) (Dz fy))
                (- (Dz fx) (Dx fz))
                (- (Dx fy) (Dy fx)))))
      (o/make-operator 'Curl)))

(defn curl
  "[[curl]] implements equation (10.7) of Functional Differential Geometry,
  defined on page 155."
  [metric orthonormal-basis]
  (let [star  (hs/Hodge-star metric orthonormal-basis)
        sharp (m/raise metric orthonormal-basis)
        flat  (m/lower metric)]
    (f/compose sharp star ff/d flat)))


(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Vector
  Laplacian](https://en.wikipedia.org/wiki/Laplace_operator#Vector_Laplacian) of
  `f` at its input point."}
  Lap
  (-> (f/compose g/trace (* Grad Grad))
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
