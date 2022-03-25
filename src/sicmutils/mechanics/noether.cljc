#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.noether
  (:refer-clojure :exclude [partial])
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]))

;; ## Noether Theorem Support

;; F-tilde is a parametric coordinate transformation that given parameters takes
;; a state and returns transformed coordinates. F-tilde may take an arbitrary
;; number of real-valued parameters. F-tilde applied to zeros is the coordinate
;; selector: It takes a state and returns the coordinates. The hypothesis of
;; Noether's theorem is that the Lagrangian is invariant under the
;; transformation for all values of the parameters.

;; (D (lambda parms (compose L (F->C (apply F-tilde parms))))) = 0

(defn Noether-integral [L F-tilde]
  (let [min-arity (second (f/arity F-tilde))
        zeros     (repeat min-arity 0)]
    (g/* ((partial 2) L) (apply (D F-tilde) zeros))))
