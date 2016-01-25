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

(ns net.littleredcomputer.sicmutils.env
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [net.littleredcomputer.sicmutils
             [generic :as g]
             [structure :as s]
             [numsymb]
             [numbers]
             [simplify :as simp]
             [expression]
             [function :as f]
             [complex :as c]
             [operator]]
            [net.littleredcomputer.sicmutils.numerical.ode :as ode]
            [net.littleredcomputer.sicmutils.numerical.minimize :refer :all]
            [net.littleredcomputer.sicmutils.calculus.derivative :as d]
            [net.littleredcomputer.sicmutils.mechanics.lagrange :refer :all]
            [net.littleredcomputer.sicmutils.mechanics.hamilton :refer :all]
            [net.littleredcomputer.sicmutils.mechanics.rigid :refer :all]
            [net.littleredcomputer.sicmutils.mechanics.rotation :refer :all]))

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
(def zero? g/zero?)

(def evolve ode/evolve)
(def state-advancer ode/state-advancer)

(def literal-function f/literal-function)
(defmacro with-literal-functions
  [& args]
  `(f/with-literal-functions ~@args))
(def compose f/compose)

(def sin g/sin)
(def cos g/cos)
(def tan g/tan)
(def asin g/asin)
(def acos g/acos)
(def exp g/exp)
(def log g/log)
(def expt g/expt)
(def simplify g/simplify)
(def cross-product g/cross-product)
(def print-expression simp/print-expression)

(def up s/up)
(def down s/down)
(def transpose g/transpose)
(def determinant s/determinant)
(def component s/component)
(def structure? s/structure?)
(def orientation s/orientation)
(def dot-product s/dot-product)
(def structure->vector s/structure->vector)
(def vector->up s/vector->up)
(def vector->down s/vector->down)

(def D d/D)
(def ∂ d/partial)
(def partial d/partial)
(def pi Math/PI)
