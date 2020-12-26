;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.util.vector-set
  (:refer-clojure :rename {conj core-conj}
                  :exclude [contains? disj #?(:cljs conj)])
  (:require [sicmutils.util :as u]))

(def empty-set [])

(defn make [xs]
  (into [] (dedupe) (sort xs)))

(defn contains?
  "Return true if `t` is in the tag-set `ts`, false otherwise."
  [s x]
  (some #(= % x) s))

(defn disj
  "Return the tag set formed by dropping t from ts."
  [s x]
  (filterv #(not= % x) s))

(defn conj
  "Inserts tag into its appropriate sorted space in `tags`."
  [s x]
  (loop [s s, ret []]
    (cond (empty? s) (core-conj ret x)
          (< x (first s)) (into (core-conj ret x) s)
          (= x (first s))
          (u/illegal (str "elem " x "already present in " s))
          :else (recur (rest s)
                       (core-conj ret (first s))))))

(defn union
  "Returns a vector that contains the union of the sorted vectors `x` and `y`."
  [x y]
  (make (into x y)))

(defn intersection
  "Returns a vector that contains the intersection of the two sorted vectors `x`
  and `y`."
  [x y]
  (loop [i (long 0)
         j (long 0)
         r (transient [])]
    (let [xi (nth x i nil)
          yj (nth y j nil)]
      (cond (not (and xi yj)) (persistent! r)
            (< xi yj) (recur (inc i) j r)
            (> xi yj) (recur i (inc j) r)
            :else     (recur (inc i) (inc j) (conj! r xi))))))
