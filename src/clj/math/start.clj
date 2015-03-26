(ns math.start)

(defmacro math-ns
  [name & clauses]
  `(ns ~name
     (:refer-clojure :exclude [~'+ ~'- ~'* ~'/ ~'zero?])
     (:require [math.generic :refer :all]
               [math.structure :refer :all]
               [math.numsymb]
               [math.numbers]
               [math.simplify]
               [math.expression :refer :all]
               [math.numerical.ode :refer :all]
               [math.function :refer :all]
               [math.operator :refer :all]
               [math.calculus.derivative :refer :all]
               [math.mechanics.lagrange :refer :all]
               [math.mechanics.rotation :refer :all])
     ~@clauses))
