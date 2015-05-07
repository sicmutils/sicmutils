(ns math.env
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :as g]
            [math.structure :as s]
            [math.numsymb]
            [math.numbers]
            [math.simplify]
            [math.expression]
            [math.numerical.ode]
            [math.function]
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
(def evolve ode/evolve)
(def up s/up)
(def down s/down)

(def D d/D)
