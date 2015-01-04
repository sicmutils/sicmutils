(ns math.operator
  (:require [math.value :as v]
            [math.generic :as g])
  (:import (clojure.lang IFn AFn)))

(defrecord Operator [f name]
  v/Value
  (sort-key [_] 45)
  (freeze [o] (.name o))
  IFn
  (invoke [operator function]
    (let [operated-function ((:f operator) function)]
      (fn [& xs]
        (apply operated-function xs))))
  (applyTo [operator fns]
    (AFn/applyToHelper operator fns))
  )

(defn make-operator
  [f name]
  (Operator. f name))

(defn operator?
  [x]
  (instance? Operator x))

(g/defhandler :simplify [#(instance? Operator %)] #(-> % :name g/simplify))

(println "operator initialized")

