(ns sicmutils.examples.pendulum-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.examples.pendulum :as pendulum]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest simple-pendulum
  (is (= '(+ (* 1/2 (expt l 2) m (expt thetadot 2)) (* (cos theta) g l m))
         (simplify ((pendulum/L 'm 'l 'g (fn [t] (up 0 0))) (up 't 'theta 'thetadot))))))
