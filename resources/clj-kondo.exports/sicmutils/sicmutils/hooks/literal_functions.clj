(ns hooks.literal-functions
  (:require [clj-kondo.hooks-api :as api]))

(defn arrow-form? [signature]
  (and (api/list-node? signature)
       (= '-> (:value
               (first
                (:children signature))))))

;; TODO
;; - better rules one
;; - bootstrap-repl!
;; - tidy and document all of these, check with borkdude
;; - review PR
;; - probably fix differential test namespace?
;; - add the github actions

(defn literal-function
  "from sicmutils.env."
  [{:keys [node]}]
  (let [[_ f sig-or-domain range] (:children node)
        new-node (cond range
                       (api/vector-node [f sig-or-domain range])

                       (arrow-form? sig-or-domain)
                       (api/vector-node [f])

                       :else (api/vector-node [f sig-or-domain]))]
    {:node new-node}))

(defn binding-pair [entry]
  (cond (api/token-node? entry)
        (let [v (api/list-node
                 [(api/token-node 'quote)
                  entry])]
          [entry v])

        (and (or (api/list-node? entry)
                 (api/vector-node? entry))
             (= (count (:children entry)) 3))
        (let [[sym domain range] (:children entry)]
          [sym (api/vector-node [domain range])])

        :else (throw
               (ex-info
                "unknown literal function type, TODO note about required format" {}))))

(defn with-literal-functions [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        bindings (mapcat binding-pair
                         (:children binding-vec))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node bindings)
                   body))]
    {:node new-node}))
