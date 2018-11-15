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

(ns sicmutils.generic
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.core.match :refer [match]])
  (:import (clojure.lang Keyword)))

(defprotocol IGenericType
  (generic-type [this]))

(defprotocol INumericType
  (zero? [this])
  (one? [this]))

(extend-type Object
  IGenericType
  (generic-type [o] (or (:type o) (type o)))
  INumericType
  (zero? [_] false)
  (one? [_] false))

(extend-type nil
  IGenericType
  (generic-type [_] nil))

(defn ^:private primitive-kind
  [a]
  (if (fn? a) :sicmutils.function/function
      (generic-type a)))

(defn argument-kind
  [& args]
  (mapv primitive-kind args))

;;; classifiers

(defmacro ^:private def-generic-function
  "Defines a mutlifn using the provided symbol. Arranges for the multifn
  to answer the :arity  and :name messages."
  [f a & b]
  (let [arity (if b `[:between ~a ~@b] [:exactly a])
        docstring (str "generic " f)]
    `(do
       (defmulti ~f ~docstring argument-kind)
       (defmethod ~f [Keyword] [k#] ({:arity ~arity
                                      :name '~f} k#)))))

(def-generic-function add 2)
(def-generic-function mul 2)
(def-generic-function sub 2)
(def-generic-function div 2)

(def-generic-function cos 1)
(def-generic-function sin 1)
(def-generic-function tan 1)
(def-generic-function asin 1)
(def-generic-function acos 1)
(def-generic-function atan 1 2)
(def-generic-function cross-product 2)
(def-generic-function negative? 1)
(def-generic-function transpose 1)
(def-generic-function magnitude 1)
(def-generic-function determinant 1)

(def-generic-function invert 1)
(def-generic-function negate 1)
(def-generic-function square 1)
(def-generic-function cube 1)
(def-generic-function exp 1)
(def-generic-function log 1)
(def-generic-function abs 1)
(def-generic-function sqrt 1)

(def-generic-function exact-divide 2)
(def-generic-function quotient 2)
(def-generic-function remainder 2)
(def-generic-function expt 2)
(def-generic-function gcd 2)

(def-generic-function zero-like 1)
(defmethod zero-like :default [_] 0)

(def-generic-function one-like 1)
(defmethod one-like :default [_] 1)

(def-generic-function Lie-derivative 1)

(defmulti partial-derivative argument-kind)
(defmulti simplify argument-kind)
(defmethod simplify :default [a] a)

(defmulti numerical? argument-kind)
(defmethod numerical? :default [_] false)

(defmulti exact? argument-kind)
(defmethod exact? :default [_] false)

(defmulti arity argument-kind)
(defmethod arity :default [a]
  (or (:arity a)
      (:arity (meta a))
      [:exactly 0]))

;; we record arities as a vector with an initial keyword:
;;   [:exactly m]
;;   [:between m n]
;;   [:at-least m]

(defn ^:private combine-arities
  "Find the joint arity of arities a and b, i.e. the loosest possible arity specification
  compatible with both. Throws if the arities are incompatible."
  [a b]
  (let [fail #(throw (IllegalArgumentException. (str "Incompatible arities: " a " " b)))]
    ;; since the combination operation is symmetric, sort the arguments
    ;; so that we only have to implement the upper triangle of the
    ;; relation.
    (if (< 0 (compare (first a) (first b)))
      (combine-arities b a)
      (match [a b]
             [[:at-least k] [:at-least k2]] [:at-least (max k k2)]
             [[:at-least k] [:between m n]] (let [m (max k m)]
                                              (cond (= m n) [:exactly m]
                                                    (< m n) [:between m n]
                                                    :else (fail)))
             [[:at-least k] [:exactly l]] (if (>= l k)
                                            [:exactly l]
                                            (fail))
             [[:between m n] [:between m2 n2]] (let [m (max m m2)
                                                     n (min n n2)]
                                                 (cond (= m n) [:exactly m]
                                                       (< m n) [:between m n]
                                                       :else (fail)))
             [[:between m n] [:exactly k]] (if (and (<= m k)
                                                    (<= k n))
                                             [:exactly k]
                                             (fail))
             [[:exactly k] [:exactly l]] (if (= k l) [:exactly k] (fail))))))

(defn joint-arity
  "Find the most relaxed possible statement of the joint arity of the given objects.
  If they are incompatible, an exception is thrown."
  [objects]
  (reduce combine-arities [:at-least 0] (map arity objects)))

(defmulti freeze argument-kind)

(defn abstract-quantity?
  [x]
  (= (:type x) :sicmutils.expression/numerical-expression))

(defn ^:private bin+ [a b]
  (cond (zero? a) b
        (zero? b) a
        :else (add a b)))

(defn + [& args]
  (reduce bin+ 0 args))

(defn ^:private bin- [a b]
  (cond (zero? b) a
        (zero? a) (negate b)
        :else (sub a b)))

(defn - [& args]
  (cond (nil? args) 0
        (nil? (next args)) (negate (first args))
        :else (bin- (first args) (reduce bin+ (next args)))))

(defn ^:private bin* [a b]
  (cond (one? a) b
        (one? b) a
        :else (mul a b)))

;;; In bin* we test for exact (numerical) zero
;;; because it is possible to produce a wrong-type
;;; zero here, as follows:
;;;
;;;               |0|             |0|
;;;       |a b c| |0|   |0|       |0|
;;;       |d e f| |0| = |0|, not  |0|
;;;
;;; We are less worried about the g/zero? below,
;;; because any invertible matrix is square.

(defn * [& args]
  (reduce bin* 1 args))

(defn ^:private bin-div [a b]
  (cond (one? b) a
        :else (div a b)))

(defn / [& args]
  (cond (nil? args) 1
        (nil? (next args)) (invert (first args))
        :else (bin-div (first args) (reduce bin* (next args)))))

(def divide /)

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [ε]
  (fn [x y] (< (Math/abs (double (- x y))) ε)))
