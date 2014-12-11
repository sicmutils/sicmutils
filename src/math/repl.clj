(ns math.repl
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.numbers :refer :all]
            [math.expression :refer :all]
            [math.numerical.integrate :refer :all]
            [math.numerical.minimize :refer :all]
            [math.function :refer :all]
            [math.operator :refer :all]
            [math.value :as v]
            [math.calculus.derivative :refer :all]
            [math.mechanics.lagrange :refer :all]))
