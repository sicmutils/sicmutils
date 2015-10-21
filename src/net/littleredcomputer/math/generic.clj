;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns net.littleredcomputer.math.generic
  (:refer-clojure :rename {/ core-div
                           zero? core-zero?}
                  :exclude [+ - *])
  (:require [net.littleredcomputer.math
             [value :as v]
             [expression :as x]])
  (:import [net.littleredcomputer.math.expression Expression]))

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
(defmulti mul v/argument-kind)
(defmulti sub v/argument-kind)
(defmulti div v/argument-kind)
(defmulti invert v/argument-kind)
(defmulti negate v/argument-kind)
(defmulti square v/argument-kind)
(defmulti cube v/argument-kind)
(defmulti expt v/argument-kind)
(defmulti exp v/argument-kind)
(defmulti log v/argument-kind)
(defmulti abs v/argument-kind)
(defmulti sqrt v/argument-kind)
(defmulti sin v/argument-kind)
(defmulti cos v/argument-kind)
(defmulti tan v/argument-kind)
(defmulti asin v/argument-kind)
(defmulti acos v/argument-kind)
(defmulti partial-derivative v/argument-kind)
(defmulti cross-product v/argument-kind)
(defmulti simplify v/argument-kind)

(defn- bin+ [a b]
  (cond (and (number? a) (number? b)) (+' a b)
        (zero? a) b
        (zero? b) a
        :else (add a b)))

(defn + [& args]
  (reduce bin+ 0 args))

(defn- bin- [a b]
  (cond (and (number? a) (number? b)) (-' a b)
        (zero? b) a
        (zero? a) (negate b)
        :else (sub a b)))

(defn - [& args]
  (cond (nil? args) 0
        (nil? (next args)) (negate (first args))
        :else (bin- (first args) (reduce bin+ (next args)))))

(defn- bin* [a b]
  (cond (and (number? a) (number? b)) (*' a b)
        (and (zero? a)) (v/zero-like b)
        (and (zero? b)) (v/zero-like a)
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

(defn- bin-div [a b]
  (cond (and (number? a) (number? b)) (core-div a b)
        (one? b) a
        :else (div a b)))

(defn / [& args]
  (cond (nil? args) 1
        (nil? (next args)) (invert (first args))
        :else (bin-div (first args) (reduce bin* (next args)))))

(def divide /)
