(ns math.examples.rigid-rotation-test
  (:refer-clojure :exclude [+ - * /])
  (:require [math.examples.rigid-rotation :as rigid]
            [clojure.test :refer :all]))

(deftest smoke
  (is (rigid/evolver 1 0.1 1 1.2 2 0.1 0.1 0.1 1 1 1)))
