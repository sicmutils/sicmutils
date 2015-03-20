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

(ns math.numbers
  (:refer-clojure :rename {zero? core-zero?
                           + core-+
                           - core--
                           * core-*
                           / core-div})
  (:require [math.generic :as g]
            [math.numsymb :as ns]
            [clojure.math.numeric-tower :as nt]))

(defn- make-numerical-combination
  ([operator] (make-numerical-combination operator false))
  ([operator commutative?]
     (fn [& operands]
       (ns/make-numsymb-expression operator
                                   (if commutative?
                                     (reverse operands)
                                     operands)))))

(defn- make-binary-operation [key operation commutative?]
  (g/defhandler key [number? number?] operation)
  (g/defhandler key [g/abstract-number? g/abstract-number?]
    (make-numerical-combination key))
  (g/defhandler key [number? g/abstract-number?]
    (make-numerical-combination key))
  (g/defhandler key [g/abstract-number? number?]
    (make-numerical-combination key commutative?)))

(defn- make-unary-operation [key operation]
  (g/defhandler key [number?] operation)
  (g/defhandler key [g/abstract-number?] (make-numerical-combination key)))

(defn- define-binary-operation
  [generic-operation core-operation]
  (defmethod generic-operation [Number Number] [a b] (core-operation a b)))

(defn define-unary-operation
  [generic-operation core-operation]
  (defmethod generic-operation Number [a] (core-operation a)))

(define-binary-operation g/add core-+)
(define-binary-operation g/sub core--)
(define-binary-operation g/mul core-*)
(define-binary-operation g/div core-div)
(define-unary-operation g/negate core--)
(define-unary-operation g/invert core-div)
(define-unary-operation g/sin #(Math/sin %))
(define-unary-operation g/cos #(Math/cos %))
(define-unary-operation g/tan #(Math/tan %))
(define-unary-operation g/square #(core-* % %))

(make-binary-operation :** nt/expt false)
(make-unary-operation :cube #(core-* % % %))
(make-unary-operation :abs nt/abs)
(make-unary-operation :sqrt nt/sqrt)
(make-unary-operation :log #(Math/log %))
(make-unary-operation :exp #(Math/exp %))

(println "numbers initialized")
