(ns hooks.chuck
  (:require [clj-kondo.hooks-api :as api]))

;; Taken from
;; https://github.com/gfredericks/test.chuck/blob/master/resources/clj-kondo.exports/com.gfredericks/test.chuck/clj_kondo/com/gfredericks/test/chuck/checking.clj.
;; I'll replace once I get the dependency pulled in properly.
(defn checking
  [{{:keys [children]} :node}]
  (let [[_checking desc & opt+bindings+body] children
        [opts binding-vec & body]             (if (api/vector-node? (first opt+bindings+body))
                                                (into [(api/map-node {})] opt+bindings+body)
                                                opt+bindings+body)]
    (when-not (even? (count (:children binding-vec)))
      (throw (ex-info "checking requires an even number of bindings" {})))
    {:node (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node (into [(api/token-node (symbol (gensym "_checking-desc"))) desc]
                                    (:children binding-vec)))
             opts
             body))}))
