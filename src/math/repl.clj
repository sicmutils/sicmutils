(ns math.repl
  (:require [clojure.main :as m])
  (:gen-class))

(defn -main
  [& args]
  (prn "welcome.")
  (m/with-bindings
    (ns foo
      (:refer-clojure :exclude [+ - * / zero?])
      (:require [math.generic :refer :all]
                [math.structure :refer :all]
                [math.expression :refer :all]
                [math.numbers]
                [math.numerical.integrate :refer :all]
                [math.numerical.minimize :refer :all]
                [math.function :refer :all]
                [math.calculus.derivative :refer :all]
                [math.mechanics.lagrange :refer :all])
      )
    (m/repl)))
