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

(ns math.env
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :as g]
            [math.structure :as s]
            [math.numsymb]
            [math.numbers]
            [math.simplify]
            [math.expression]
            [math.numerical.ode]
            [math.function :as f]
            [math.operator]
            [math.numerical.ode :as ode]
            [math.calculus.derivative :as d]))

(def + g/+)
(def - g/-)
(def * g/*)
(def / g/divide)
(def square g/square)
(def cube g/cube)
(def sqrt g/sqrt)

(def sin g/sin)
(def cos g/cos)
(def tan g/tan)
(def asin g/asin)
(def acos g/acos)
(def exp g/exp)
(def log g/log)
(def expt g/expt)
(def simplify g/simplify)

(defmacro with-literal-functions
  [& args]
  `(f/with-literal-functions ~@args))

(def evolve ode/evolve)

(def up s/up)
(def down s/down)

(def D d/D)
