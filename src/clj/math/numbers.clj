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
                           - core--})
  (:require [math.generic :as g]
            [math.value :as v]
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

(defmethod g/tan java.lang.Number [a] (Math/tan a))
(defmethod g/tan :math.expression/numerical-expression [a] (ns/make-numsymb-expression :tan [a]))
(defmethod g/add [java.lang.Number java.lang.Number] [a b] (core-+ a b))
(defmethod g/add [:math.expression/numerical-expression
                  :math.expression/numerical-expression] [a b] (ns/make-numsymb-expression :+ [a b]))
(defmethod g/sub [java.lang.Number java.lang.Number] [a b] (core-- a b))
(defmethod g/sub [:math.expression/numerical-expression
                  :math.expression/numerical-expression] [a b] (ns/make-numsymb-expression :- [a b]))
(defmethod g/negate java.lang.Number [a] (core-- a))
(defmethod g/negate :math.expression/numerical-expression [a] (ns/make-numsymb-expression :negate [a]))
(derive clojure.lang.Symbol :math.expression/numerical-expression)
(derive java.lang.Number :math.expression/numerical-expression)

(make-binary-operation :* * true)
(make-binary-operation :div / false)
(make-binary-operation :** nt/expt false)
(make-unary-operation :sin #(Math/sin %))
(make-unary-operation :cos #(Math/cos %))
(make-unary-operation :square #(* % %))
(make-unary-operation :cube #(* % % %))
(make-unary-operation :abs nt/abs)
(make-unary-operation :invert /)
(make-unary-operation :sqrt nt/sqrt)
(make-unary-operation :log #(Math/log %))
(make-unary-operation :exp #(Math/exp %))

(println "numbers initialized")
