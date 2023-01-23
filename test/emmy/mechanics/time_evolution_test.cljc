#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.time-evolution-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest]]
            [emmy.generic :as g :refer [+]]
            [emmy.mechanics.hamilton :as h]
            [emmy.mechanics.time-evolution :as te]
            [emmy.structure :as s :refer [up]]
            [emmy.value :as v]))

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
