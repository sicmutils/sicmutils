(ns math.repl
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [math.generic :refer :all]
            [math.structure :refer :all]
            [math.expression :refer :all]
            [math.numbers]
            [math.numsymb]
            [math.numerical.integrate :refer :all]
            [math.numerical.minimize :refer :all]
            [math.function :refer :all]
            [math.calculus.derivative :refer :all]
            [math.mechanics.lagrange :refer :all])
  (:gen-class))

(defn -main
  [& args]
  (prn "hello world"))