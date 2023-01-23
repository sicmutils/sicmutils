#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.pendulum-test
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [emmy.env :refer [up simplify]]
            [emmy.examples.pendulum :as p]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest simple-pendulum
  (is (= '(+ (* (/ 1 2) (expt l 2) m (expt thetadot 2))
             (* g l m (cos theta)))
         (v/freeze
          (simplify
           ((p/L 'm 'l 'g (fn [_t] (up 0 0)))
            (up 't 'theta 'thetadot)))))))
