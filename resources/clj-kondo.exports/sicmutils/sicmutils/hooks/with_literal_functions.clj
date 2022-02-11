(ns hooks.with-literal-functions
  (:require [clj-kondo.hooks-api :as api]))

(defn with-literal-functions [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        bindings (mapcat
                  (fn [sym-token]
                    (let [sym (:value sym-token)
                          v (api/list-node
                             ['sicmutils.abstract.function/literal-function
                              (api/list-node
                               ['quote sym])])]
                      [sym-token v]))
                  (:children binding-vec))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node bindings)
                   body))]
    {:node new-node}))
