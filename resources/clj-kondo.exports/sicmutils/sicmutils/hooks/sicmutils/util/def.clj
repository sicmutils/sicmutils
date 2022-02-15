(ns hooks.sicmutils.util.def
  (:require [clj-kondo.hooks-api :as api]))

(defn import-def
  "Converts a node representing an invocation of
  the [[sicmutils.util.def/import-def]] macro into a matching `def` call."
  [{:keys [node]}]
  (let [[_ v sym] (:children node)
        sym (or sym (api/token-node
                     (symbol (name (:value v)))))
        new-node (api/list-node
                  [(api/token-node 'def)
                   sym v])]
    {:node new-node}))
