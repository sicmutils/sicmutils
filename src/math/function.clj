(ns math.function
  (:require [math.generic :as g]))

(defrecord Fn [name]
  g/Value
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] false)
  (exact? [x] false)
  (sort-key [x] 35)
  clojure.lang.IFn
  (invoke [f x] (list (.name f) x))
  ;;(applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  )

(defn literal-function [f] (Fn. f))
