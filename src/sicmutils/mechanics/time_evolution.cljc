;;
;; Copyright © 2022 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

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
                (- delta-t)))))

(defn H->Hp [delta-t]
  (fn [H]
    (f/compose H (shift-t
                  (- delta-t)))))
