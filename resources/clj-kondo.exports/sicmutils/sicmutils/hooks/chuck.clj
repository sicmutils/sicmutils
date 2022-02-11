(ns hooks.chuck
  (:require [clj-kondo.hooks-api :as api]))

(defn checking [{:keys [node]}]
  (let [[_ _ _ binding-vec & body] (:children node)
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   binding-vec
                   body))]
    {:node new-node}))
