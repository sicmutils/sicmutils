#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.time-evolution
  (:require [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.mechanics.hamilton :as mh]))

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
