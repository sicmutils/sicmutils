#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.noether-test
  (:require [clojure.test :refer [is deftest]]
            [sicmutils.abstract.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.mechanics.lagrange :as l]
            [sicmutils.mechanics.noether :as n]
            [sicmutils.mechanics.rotation :as r]
            [sicmutils.structure :as s :refer [up]]
            [sicmutils.value :as v]))

(defn F-tilde [theta phi psi]
  (comp (r/Rx theta)
        (r/Ry phi)
        (r/Rz psi)
        l/coordinate))

(deftest noether-tests
  (is (= '(down (+ (* -1 m vy z) (* m vz y))
                (+ (* m vx z) (* -1 m vz x))
                (+ (* -1 m vx y) (* m vy x)))
         (v/freeze
          (g/simplify
           ((n/Noether-integral
             (l/L-central-rectangular 'm (f/literal-function 'Vr))
             F-tilde)
            (up 't
                (up 'x 'y 'z)
                (up 'vx 'vy 'vz))))))))
