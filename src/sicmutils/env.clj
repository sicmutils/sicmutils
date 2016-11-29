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

(ns sicmutils.env
  (:refer-clojure :exclude [+ - * / zero?]
                  :rename {ref core-ref partial core-partial})
  (:import [sicmutils.matrix Matrix])
  (:require [sicmutils
             [generic :as g]
             [structure :as s]
             [numsymb]
             [numbers]
             [simplify :as simp]
             [expression]
             [function :as f]
             [complex :as c]
             [matrix :as matrix]
             [operator]
             [infix :as i]]
            [sicmutils.numerical
             [minimize :as min]
             [ode :as ode]
             [integrate :as intg]]
            [sicmutils.calculus.derivative :as d]
            [sicmutils.value :as v]))

(def + g/+)
(def - g/-)
(def * g/*)
(def / g/divide)
(def square g/square)
(def cube g/cube)
(def sqrt g/sqrt)
(def abs g/abs)
(def negate g/negate)
(def complex c/complex)
(def invert g/invert)
(def zero? g/zero?)

(def evolve ode/evolve)
(def state-advancer ode/state-advancer)

(defmacro literal-function
  ([f] `(f/literal-function ~f))
  ([f sicm-signature] `(f/literal-function ~f '~sicm-signature))
  ([f domain range] `(f/literal-function ~f ~domain ~range)))

(defmacro with-literal-functions
  [& args]
  `(f/with-literal-functions ~@args))

(def compose f/compose)

(def sin g/sin)
(def cos g/cos)
(def tan g/tan)
(def asin g/asin)
(def acos g/acos)
(def atan g/atan)
(def cot (g/divide g/cos g/sin))
(def csc (g/invert g/sin))
(def sec (g/invert g/cos))
(def exp g/exp)
(def log g/log)
(def expt g/expt)
(def simplify g/simplify)
(def cross-product g/cross-product)
(def print-expression simp/print-expression)

(defn ref
  "A shim so that ref can act like nth in SICM contexts, as clojure
  core ref elsewhere."
  [& args]
  (if (and (> (count args) 1)
           (sequential? (first args))
           (every? integer? (rest args)))
    (let [[a & indices] args]
      (if (instance? Matrix a)
       (matrix/matrix-get-in a indices)
       (get-in a indices)))
    (apply core-ref args)))

(defn partial
  "A shim. Dispatches to partial differentiation when all the arguments
  are integers; falls back to the core meaning (partial function application)
  otherwise."
  [& selectors]
  (if (every? integer? selectors)
    (apply d/∂ selectors)
    (apply core-partial selectors)))

(def up s/up)
(def down s/down)
(def transpose g/transpose)
(def determinant s/determinant)
(def component s/component)
(def structure? s/structure?)
(def orientation s/orientation)
(def structure->vector s/structure->vector)
(def vector->up s/vector->up)
(def vector->down s/vector->down)
(def m:transpose matrix/transpose)
(def compatible-shape s/compatible-shape)
(def mapr s/mapr)
(def s->m matrix/s->m)

(def matrix-by-rows matrix/by-rows)

(def D d/D)
(def ∂ d/∂)
(def pi Math/PI)
(def taylor-series-terms d/taylor-series-terms)

(def function? f/function?)

(def minimize min/minimize)

(def definite-integral intg/definite-integral)

(def ->infix i/->infix)
(def ->TeX i/->TeX)
(def ->JavaScript i/->JavaScript)

(def principal-value v/principal-value)
