(ns math.struct
  (:require [math.generic :as g])
  (:gen-class))

(defn up [& xs] (with-meta (apply vector xs) {:type :up}))
(defn down [& xs] (with-meta (apply vector xs) {:type :down}))

(defn compatible-for-elementwise? [s t]
  "Given two structs, true iff the two structs are of the same
   orientation and length."
  (and (= (g/type s) (g/type t))
       (= (count s) (count t))))

(defn elementwise [op s t]
  (if (compatible-for-elementwise? s t)
    (with-meta (vec (map op s t)) {:type (:type (meta s))} )
    (throw (IllegalArgumentException.
            "incompatible for elementwise combination"))))

(def add (partial elementwise +))
(def sub (partial elementwise -))

(def operations {:add :up :up add})

;; idea: matrix squaring to get fibonacci numbers

