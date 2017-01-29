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
  (:refer-clojure :rename {/ core-div
                           zero? core-zero?}
                  :exclude [+ - *])
  (:require [sicmutils
             [value :as v]
             [expression :as x]])
  (:import [sicmutils.expression Expression]))

;;; classifiers
(defn zero?
  [x]
  (cond (number? x) (core-zero? x)
        (vector? x) (every? zero? x)
        :else (v/nullity? x)))

(defn one?
  [x]
  (or (and (number? x) (== x 1))
      (v/unity? x)))

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

(defmulti add v/argument-kind)
(defmethod add [:arity] [_] [:exactly 2])

(defmulti mul v/argument-kind)
(defmethod mul [:arity] [_] [:exactly 2])

(defmulti sub v/argument-kind)
(defmethod sub [:arity] [_] [:exactly 2])

(defmulti div v/argument-kind)
(defmethod div [:arity] [_] [:exactly 2])

(defmulti exact-divide v/argument-kind)
(defmethod exact-divide [:arity] [_] [:exactly 2])

(defmulti quotient v/argument-kind)
(defmethod quotient [:arity] [_] [:exactly 2])

(defmulti remainder v/argument-kind)
(defmethod remainder [:arity] [_] [:exactly 2])

(defmulti invert v/argument-kind)
(defmethod invert [:arity] [_] [:exactly 1])

(defmulti negate v/argument-kind)
(defmethod negate [:arity] [_] [:exactly 1])

(defmulti square v/argument-kind)
(defmethod square [:arity] [_] [:exactly 1])

(defmulti cube v/argument-kind)
(defmethod cube [:arity] [_] [:exactly 1])

(defmulti expt v/argument-kind)
(defmethod expt [:arity] [_] [:exactly 2])

(defmulti exp v/argument-kind)
(defmethod exp [:arity] [_] [:exactly 1])

(defmulti log v/argument-kind)
(defmethod log [:arity] [_] [:exactly 1])

(defmulti abs v/argument-kind)
(defmethod abs [:arity] [_] [:exactly 1])

(defmulti sqrt v/argument-kind)
(defmethod sqrt [:arity] [_] [:exactly 1])

(defmulti sin v/argument-kind)
(defmethod sin [:arity] [_] [:exactly 1])

(defmulti cos v/argument-kind)
(defmethod cos [:arity] [_] [:exactly 1])

(defmulti tan v/argument-kind)
(defmethod tan [:arity] [_] [:exactly 1])

(defmulti asin v/argument-kind)
(defmethod asin [:arity] [_] [:exactly 1])

(defmulti acos v/argument-kind)
(defmethod acos [:arity] [_] [:exactly 1])

(defmulti atan v/argument-kind)
(defmethod atan [:arity] [_] [:between 1 2])

(defmulti partial-derivative v/argument-kind)

(defmulti cross-product v/argument-kind)
(defmethod cross-product [:arity] [_] [:exactly 2])

(defmulti simplify v/argument-kind)

(defmulti negative? v/argument-kind)
(defmethod negative? [:arity] [_] [:exactly 1])

(defmulti transpose v/argument-kind)
(defmethod transpose [:arity] [_] [:exactly 1])

(defmulti magnitude v/argument-kind)
(defmethod magnitude [:arity] [_] [:exactly 1])

(defmulti determinant v/argument-kind)
(defmethod determinant [:arity] [_] [:exactly 1])

(defn ^:private bin+ [a b]
  (cond (and (number? a) (number? b)) (+' a b)
        (zero? a) b
        (zero? b) a
        :else (add a b)))

(defn + [& args]
  (reduce bin+ 0 args))

(defn ^:private bin- [a b]
  (cond (and (number? a) (number? b)) (-' a b)
        (zero? b) a
        (zero? a) (negate b)
        :else (sub a b)))

(defn - [& args]
  (cond (nil? args) 0
        (nil? (next args)) (negate (first args))
        :else (bin- (first args) (reduce bin+ (next args)))))

(defn ^:private bin* [a b]
  (cond (and (number? a) (number? b)) (*' a b)
        (and (number? a) (zero? a)) (v/zero-like b)
        (and (number? b) (zero? b)) (v/zero-like a)
        (one? a) b
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
;;; We are less worried about the zero? below,
;;; because any invertible matrix is square.

(defn * [& args]
  (reduce bin* 1 args))

(defn ^:private bin-div [a b]
  (cond (and (number? a) (number? b)) (core-div a b)
        (one? b) a
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
