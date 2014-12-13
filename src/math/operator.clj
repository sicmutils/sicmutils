(ns math.operator
  (:import (clojure.lang IFn))
  (:require [math.value :as v]))

(defrecord Operator [f name]
  v/Value
  (nullity? [_] false)
  (unity? [_] false)
  (zero-like [_] false)
  (exact? [_] false)
  (sort-key [_] 45)
  (freeze [o] (.name o))
  IFn
  (invoke [operator function]
    (let [operated-function ((:f operator) function)]
      (fn [& xs]
        (apply operated-function xs))))
  )

(defn make-operator
  [f name]
  (Operator. f name))

(defn operator?
  [x]
  (instance? Operator x))

(println "operator initialized")
