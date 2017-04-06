;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
  (:refer-clojure :rename {/ core-div}
                  :exclude [+ - *])
  (:require [sicmutils
             [value :as v]
             [expression :as x]])
  (:import [sicmutils.expression Expression]
           (clojure.lang Keyword)))

;;; classifiers

(defn literal-number?
  [x]
  (and (instance? Expression x) (v/numerical? x)))

(defn abstract-number?
  [x]
  (or (symbol? x) (literal-number? x)))

(defn abstract-quantity?
  [x]
  (and (instance? Expression x)
       (x/abstract? x)))

(defn numerical-quantity?
  [x]
  (or (number? x)
      (abstract-number? x)
      (v/numerical? x)))

(defmacro def-generic-function
  "Defines a mutlifn using the provided symbol. Arranges for the multifn
  to answer the :arity message, reporting either [:exactly a] or
  [:between a b], according to the arguments given."
  [f a & b]
  (let [arity (if b `[:between ~a ~@b] [:exactly a])
        docstring (str "generic " f)]
    `(do
       (defmulti ~f ~docstring v/argument-kind)
       (defmethod ~f [Keyword] [k#] ({:arity ~arity} k#)))))

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

(defmulti partial-derivative v/argument-kind)
(defmulti simplify v/argument-kind)

(defn ^:private bin+ [a b]
  (cond (and (number? a) (number? b)) (+' a b)
        (v/nullity? a) b
        (v/nullity? b) a
        :else (add a b)))

(defn + [& args]
  (reduce bin+ 0 args))

(defn ^:private bin- [a b]
  (cond (and (number? a) (number? b)) (-' a b)
        (v/nullity? b) a
        (v/nullity? a) (negate b)
        :else (sub a b)))

(defn - [& args]
  (cond (nil? args) 0
        (nil? (next args)) (negate (first args))
        :else (bin- (first args) (reduce bin+ (next args)))))

(defn ^:private bin* [a b]
  (cond (and (number? a) (number? b)) (*' a b)
        (and (number? a) (v/nullity? a)) (v/zero-like b)
        (and (number? b) (v/nullity? b)) (v/zero-like a)
        (v/unity? a) b
        (v/unity? b) a
        :else (mul a b)))

;;; In bin* we test for exact (numerical) zero
;;; because it is possible to produce a wrong-type
;;; zero here, as follows:
;;;
;;;               |0|             |0|
;;;       |a b c| |0|   |0|       |0|
;;;       |d e f| |0| = |0|, not  |0|
;;;
;;; We are less worried about the v/nullity? below,
;;; because any invertible matrix is square.

(defn * [& args]
  (reduce bin* 1 args))

(defn ^:private bin-div [a b]
  (cond (and (number? a) (number? b)) (core-div a b)
        (v/unity? b) a
        :else (div a b)))

(defn / [& args]
  (cond (nil? args) 1
        (nil? (next args)) (invert (first args))
        :else (bin-div (first args) (reduce bin* (next args)))))

(def divide /)

(v/add-object-symbols! {+ '+ * '* - '- / (symbol "/")
                        exact-divide 'exact-divide quotient 'quotient remainder 'remainder
                        invert 'invert negate 'negate square 'square cube 'cube
                        expt 'expt exp 'exp log 'log sqrt 'sqrt abs 'abs negative? 'negative?
                        sin 'sin cos 'cos tan 'tan asin 'asin acos 'acos atan 'atan
                        partial-derivative 'partial
                        cross-product 'cross-product
                        simplify 'simplify})
