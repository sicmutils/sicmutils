#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.examples.pendulum-test
  (:require [clojure.test :refer [is deftest use-fixtures]]
            [sicmutils.env :refer [up simplify]]
            [sicmutils.examples.pendulum :as p]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest simple-pendulum
  (is (= '(+ (* (/ 1 2) (expt l 2) m (expt thetadot 2))
             (* g l m (cos theta)))
         (v/freeze
          (simplify
           ((p/L 'm 'l 'g (fn [_t] (up 0 0)))
            (up 't 'theta 'thetadot)))))))
