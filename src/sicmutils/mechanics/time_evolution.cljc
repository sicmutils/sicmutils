#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.time-evolution
  (:require [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.mechanics.hamilton :as mh]))

;; ## Time Evolution Transformations

(defn shift-t [delta-t]
  (fn [[t q p]]
    (mh/->H-state
     (g/+ t delta-t) q p)))

(defn C->Cp [C]
  (fn [delta-t]
    (f/compose (C delta-t)
               (shift-t
                (g/- delta-t)))))

(defn H->Hp [delta-t]
  (fn [H]
    (f/compose H (shift-t
                  (g/- delta-t)))))
