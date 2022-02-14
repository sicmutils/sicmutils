(ns hooks.sicmutils.util.def
  (:require [clj-kondo.hooks-api :as api]))

(defn import-def [{:keys [node]}]
  (let [[_ v sym] (:children node)
        sym (or sym (api/token-node
                     (symbol (name (:value v)))))
        new-node (api/list-node
                  [(api/token-node 'def)
                   sym v])]
    {:node new-node}))
