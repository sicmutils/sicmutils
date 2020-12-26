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

(ns sicmutils.calculus.derivative
  (:refer-clojure :rename {partial core-partial}
                  #?@(:cljs [:exclude [partial]]))
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang Fn MultiFn))))

(defn- replace-tag-fn
  ([f old new]
   (replace-tag-fn f old new {}))
  ([f old new meta]
   (-> (fn [& args]
         (let [eps (d/fresh-tag)]
           (-> (apply f (map #(d/replace-tag % old eps) args))
               (d/replace-tag old new)
               (d/replace-tag eps old))))
       (with-meta (assoc meta :arity (f/arity f))))))

(defn- extract-tangent-fn
  ([f tag]
   (extract-tangent-fn f tag {}))
  ([f tag meta]
   (-> (fn [& args]
         (let [eps (d/fresh-tag)]
           (-> (apply f (map #(d/replace-tag % tag eps) args))
               (d/extract-tangent tag)
               (d/replace-tag eps tag))))
       (with-meta (assoc meta :arity (f/arity f))))))

(extend-protocol d/IPerturbed
  #?(:clj Fn :cljs function)
  (replace-tag [f old new] (replace-tag-fn f old new))
  (extract-tangent [f tag] (extract-tangent-fn f tag))

  #?@(:cljs
      [MetaFn
       (replace-tag [f old new]
                    (replace-tag-fn
                     (.-afn f) old new (.-meta f)))
       (extract-tangent [f tag]
                        (extract-tangent-fn
                         (.-afn f) tag (.-meta f)))])

  MultiFn
  (replace-tag [f old new] (replace-tag-fn f old new))
  (extract-tangent [f tag] (extract-tangent-fn f tag)))

(defn derivative [f]
  (fn [x]
    (let [tag (d/fresh-tag)]
      (-> (f (d/bundle x 1 tag))
          (d/extract-tangent tag)))))

;; ## Multivariable Calculus

(defn- euclidean-structure [selectors f]
  (letfn [(structural-derivative [g v]
            (cond (s/structure? v)
                  (s/opposite v
                              (map-indexed
                               (fn [i v_i]
                                 (structural-derivative
                                  (fn [w]
                                    (g (assoc v i w)))
                                  v_i))
                               v))
                  (v/numerical? v) ((derivative g) v)

                  :else
                  (u/illegal (str "bad structure " g ", " v))))
          (a-euclidean-derivative [v]
            (cond (s/structure? v)
                  (structural-derivative
                   (fn [w]
                     (f (if (empty? selectors)
                          w
                          (assoc-in v selectors w))))
                   (get-in v selectors))

                  (empty? selectors)
                  ((derivative f) v)

                  :else
                  (u/illegal (str "Bad selectors " f selectors v))))]
    a-euclidean-derivative))

(defn- multivariate-derivative [f selectors]
  (let [a (f/arity f)
        d (core-partial euclidean-structure selectors)
        make-df #(with-meta % {:arity a :from :multivariate-derivative})]
    (condp = a
      [:exactly 0] (make-df (constantly 0))
      [:exactly 1] (make-df (d f))
      [:exactly 2] (make-df (fn [x y]
                              ((d (fn [[x y]] (f x y)))
                               (matrix/seq-> [x y]))))
      [:exactly 3] (make-df (fn [x y z]
                              ((d (fn [[x y z]] (f x y z)))
                               (matrix/seq-> [x y z]))))
      [:exactly 4] (make-df (fn [w x y z]
                              ((d (fn [[w x y z]] (f w x y z)))
                               (matrix/seq-> [w x y z]))))
      [:between 1 2] (make-df (fn
                                ([x] ((d f) x))
                                ([x y]
                                 ((d (fn [[x y]] (f x y)))
                                  (matrix/seq-> [x y])))))
      (make-df
       (fn [& xs]
         (when (empty? xs) (u/illegal "No args passed to derivative?"))
         (if (= (count xs) 1)
           ((d f) (first xs))
           ((d #(apply f %)) (matrix/seq-> xs))))))))

(doseq [t [::v/function ::s/structure]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (multivariate-derivative f selectors))

  (defmethod g/partial-derivative [t nil] [f _]
    (multivariate-derivative f [])))

(def derivative-symbol 'D)

(def ^{:doc "Derivative operator. Produces a function whose value at some point
  can multiply an increment in the arguments, to produce the best linear
  estimate of the increment in the function value."}
  D
  (o/make-operator #(g/partial-derivative % [])
                   derivative-symbol))

(defn partial
  "Partial differentiation of a function at the (zero-based) slot index
  provided."
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors)
                   `(~'partial ~@selectors)))

;; TODO note to Colin that I THINK we just flip a single outer layer here...
(def Grad
  (-> (fn [f]
        (f/compose s/opposite
                   (g/partial-derivative f [])))
      (o/make-operator 'Grad)))

(def ^{:doc "takes a 3d vector of functions on 3d space."}
  Div
  (-> (f/compose g/trace Grad)
      (o/make-operator 'Div)))

(def ^{:doc "takes a 3d vector of functions on 3d space."}
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

(def ^{:doc "takes a 3d vector of functions on 3d space."}
  Lap
  (-> (f/compose g/trace (g/square Grad))
      (o/make-operator 'Lap)))

(defn taylor-series
  "Returns a `Series` of the coefficients of the taylor series of the function `f`
  evaluated at `x`, with incremental quantity `dx`.

  NOTE: The `(constantly dx)` term is what allows this to work with arbitrary
  structures of `x` and `dx`. Without this wrapper, `((g/* dx D) f)` with `dx`
  == `(up 'dx 'dy)` would expand to this:

  `(fn [x] (* (s/up ('dx x) ('dy x))
              ((D f) x)))`

  `constantly` delays the interpretation of `dx` one step:

  `(fn [x] (* (s/up 'dx 'dy)
              ((D f) x)))`
  "
  [f x dx]
  (((g/exp (g/* (constantly dx) D)) f) x))
