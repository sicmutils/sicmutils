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
            [sicmutils.series :as series]
            [sicmutils.structure :as struct]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang MultiFn))))

(declare replace-differential-tag)

(defn replace-dx-function [f newtag oldtag]
  (fn [& args]
    (let [eps (d/make-differential-tag)]
      ((replace-differential-tag eps oldtag)
       ((replace-differential-tag oldtag newtag)
        (apply f (map (replace-differential-tag oldtag eps) args)))))))

(defn replace-differential-tag [oldtag newtag]
  (fn call [obj]
    (cond (d/differential-type? obj)
          (d/terms->differential
           (mapv (fn [term]
                   (let [tagv (d/tags term)]
                     (if (d/tag-in? tagv oldtag)
                       (d/make-term (-> (d/tags term)
                                        (d/drop-tag oldtag)
                                        (d/insert-tag newtag))
                                    (d/coefficient term))
                       term)))
                 (d/terms obj)))
          (struct/structure? obj) (struct/mapr call obj)
          (matrix/matrix? obj)    (matrix/fmap call obj)
          #_(comment
              (quaternion? obj)
              (quaternion
               (call (quaternion-ref obj 0))
               (call (quaternion-ref obj 1))
               (call (quaternion-ref obj 2))
               (call (quaternion-ref obj 3))))
          (series/series? obj)    (series/fmap call obj)
          (fn? obj)               (replace-dx-function obj newtag oldtag)
          (instance? MultiFn obj) (replace-dx-function obj newtag oldtag)
          (o/operator? obj)       (replace-dx-function obj newtag oldtag)
          :else obj)))

(declare extract-dx-part)

(defn extract-dx-function [dx f]
  (fn [& args]
    (let [internal-tag (d/make-differential-tag)]
      ((replace-differential-tag internal-tag dx)
       (extract-dx-part
        dx
        (apply f (map (replace-differential-tag dx internal-tag) args)))))))

(defn extract-dx-differential [dx obj]
  (if (d/differential-type? obj)
    (d/sum->differential
     (mapcat (fn [term]
               (let [tagv (d/tags term)]
                 (if (d/tag-in? tagv dx)
                   [(d/make-term (d/drop-tag tagv dx)
                                 (d/coefficient term))]
                   [])))
             (d/terms obj)))
    0))

(defn- extract-dx-part [dx obj]
  (letfn [(dist [obj]
            (cond (struct/structure? obj) (struct/mapr dist obj)
                  (matrix/matrix? obj)    (matrix/fmap dist obj)
                  (series/series? obj)    (series/fmap dist obj)
                  (fn? obj)               (extract-dx-function dx obj)
                  (instance? MultiFn obj) (extract-dx-function dx obj)
                  (o/operator? obj)       (extract-dx-function dx obj)
                  #_(comment
                      (quaternion? obj)
                      (quaternion (dist (quaternion-ref obj 0))
                                  (dist (quaternion-ref obj 1))
                                  (dist (quaternion-ref obj 2))
                                  (dist (quaternion-ref obj 3))))
                  (fn? obj) #(dist (obj %))
                  :else (extract-dx-differential dx obj)))]
    (dist obj)))

(defn derivative [f]
  (fn [x]
    (let [dx (d/make-differential-tag)]
      (extract-dx-part dx (f (d/make-x+dx x dx))))))

;; ## Multivariable Calculus

(defn- euclidean-structure [selectors f]
  (letfn [(structural-derivative [g v]
            (cond (struct/structure? v)
                  (struct/opposite v
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
            (cond (struct/structure? v)
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
      (fn [& xs]
        (when (empty? xs) (u/illegal "No args passed to derivative?"))
        (if (= (count xs) 1)
          ((d f) (first xs))
          ((d #(apply f %)) (matrix/seq-> xs)))))))

(doseq [t [::v/function ::struct/structure ::matrix/matrix]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (multivariate-derivative f selectors))

  (defmethod g/partial-derivative [t nil] [f _]
    (multivariate-derivative f [])))

;; TODO is this sound?? move to operator if so...

#_
(defmethod g/partial-derivative [::o/operator v/seqtype] [op selectors]
  (o/->Operator (g/partial-derivative (:o op) selectors)
                (:arity op)
                `(~'D ~(:name op))
                (:context op)))

(def derivative-symbol 'D)

(def D
  "Derivative operator. Produces a function whose value at some point can
  multiply an increment in the arguments, to produce the best linear estimate
  of the increment in the function value."
  (o/make-operator (fn [f]
                     (g/partial-derivative f []))
                   derivative-symbol))

(defn partial
  "Partial differentiation of a function at the (zero-based) slot index
  provided."
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors)
                   :partial-derivative))

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
