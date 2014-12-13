(ns math.repl
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.main :as m])
  (:gen-class))

(defn- setup []
  (prn "SETUP MATH")
  ;(refer-clojure :exclude '[+ - * / zero?])
  (require '[math generic structure expression numbers numsymb
             function operator]
           '[math.numerical integrate minimize]
           '[math.calculus derivative]
           '[math.mechanics lagrange])

  (refer 'math.generic)
  (refer 'math.structure))

(defn -main
  [& args]
  (prn "welcome.")
  (m/with-bindings
    ;(ns math.repl (:refer-clojure :exclude '[+ - * / zero?]))
    (m/repl :init setup)))
