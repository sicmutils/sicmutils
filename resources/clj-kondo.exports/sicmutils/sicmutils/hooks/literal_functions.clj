(ns hooks.literal-functions
  (:require [clj-kondo.hooks-api :as api]))

(defn literal-function
  "from sicmutils.env."
  [{:keys [node]}]
  (let [[_ f sig] (:children node)
        new-node (if (and (api/list-node? sig)
                          (= '-> (:value
                                  (first
                                   (:children sig)))))
                   (api/vector-node [f])
                   (api/vector-node [f sig]))]
    {:node new-node}))

(defn with-literal-functions [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        bindings (mapcat
                  (fn [sym-token]
                    (let [v (api/list-node
                             [(api/token-node 'quote)
                              sym-token])]
                      [sym-token v]))
                  (:children binding-vec))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node bindings)
                   body))]
    {:node new-node}))
