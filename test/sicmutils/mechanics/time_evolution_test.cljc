#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.time-evolution-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest]]
            [sicmutils.generic :as g :refer [+]]
            [sicmutils.mechanics.hamilton :as h]
            [sicmutils.mechanics.time-evolution :as te]
            [sicmutils.structure :as s :refer [up]]
            [sicmutils.value :as v]))

(deftest time-evolution-tests
  (is (= (h/->H-state (+ 't 'dt) (up 'x) (up 'p_x))
         ((te/shift-t 'dt) (h/->H-state 't (up 'x) (up 'p_x))))
      "time shifts work!")

  (let [H     (h/H-rectangular 'm (fn [_] 0))
        Hp    ((te/H->Hp 'dt) H)
        state (h/->H-state 't (up 'x) (up 'p_x))]
    (is (= '(/ (* (/ 1 2) (expt p_x 2)) m)
           (v/freeze
            (g/simplify
             (Hp state))))
        "this Hamiltonian is not time dependent!")))
