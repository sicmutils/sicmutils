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

(ns net.littleredcomputer.math.env
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [net.littleredcomputer.math
             [generic :as g]
             [structure :as s]
             [numsymb]
             [numbers]
             [simplify :as simp]
             [expression]
             [function :as f]
             [operator]]
            [net.littleredcomputer.math.numerical.ode :as ode]
            [net.littleredcomputer.math.numerical.minimize :refer :all]
            [net.littleredcomputer.math.calculus.derivative :as d]
            [net.littleredcomputer.math.mechanics.lagrange :refer :all]
            [net.littleredcomputer.math.mechanics.hamilton :refer :all]
            [net.littleredcomputer.math.mechanics.rigid :refer :all]
            [net.littleredcomputer.math.mechanics.rotation :refer :all]))

(def + g/+)
(def - g/-)
(def * g/*)
(def / g/divide)
(def square g/square)
(def cube g/cube)
(def sqrt g/sqrt)
(def abs g/abs)
(def negate g/negate)

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
(def transpose s/transpose)
(def determinant s/determinant)
(def component s/component)
(def structure? s/structure?)
(def orientation s/orientation)
(def dot-product s/dot-product)

(def D d/D)
(def ∂ d/pd)
(def pd d/pd)
