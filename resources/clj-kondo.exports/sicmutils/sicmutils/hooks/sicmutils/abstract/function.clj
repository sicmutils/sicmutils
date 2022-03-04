(ns hooks.sicmutils.abstract.function
  (:require [clj-kondo.hooks-api :as api]))

(defn- arrow-form?
  "Returns true if the supplied node is a list node of the form `(-> ...)`, false
  otherwise."
  [signature]
  (and (api/list-node? signature)
       (= '-> (:value
               (first
                (:children signature))))))

(defn literal-function
  "Lints the macro version of `literal-function`, living in `sicmutils.env`.

  If the signature consists of a list-node starting with `->` it's treated as
  quoted. Else, it's emitted in a vector-node with the `f` entry."
  [{:keys [node]}]
  (let [[_ f sig-or-domain range] (:children node)
        new-node (cond range
                       (api/vector-node [f sig-or-domain range])

                       (arrow-form? sig-or-domain)
                       (api/vector-node [f])

                       :else (api/vector-node [f sig-or-domain]))]
    {:node new-node}))

(defn binding-pair
  "Given an entry in the binding vector of a call to `with-literal-functions`,
  returns a 2-vector pair of lintable entries in a `let` form.

  Invalid entries return an empty vector after triggering a linter error."
  [entry]
  (cond (and (api/token-node? entry)
             (simple-symbol? (:value entry)))
        (let [v (api/list-node
                 [(api/token-node 'quote) entry])]
          [entry v])

        (and (or (api/list-node? entry)
                 (api/vector-node? entry))
             (= (count (:children entry)) 3))
        (let [[sym domain range] (:children entry)]
          [sym (api/vector-node [domain range])])

        :else
        (do (api/reg-finding!
             (assoc (meta entry)
                    :message (str "Bindings must be either bare symbols or "
                                  "3-vectors of the form [sym domain range]. "
                                  "Received: "
                                  (pr-str (api/sexpr entry)))
                    :type :sicmutils.abstract.function/invalid-binding))
            [])))

(defn with-literal-functions
  "Converts a node representing an invocation of
  the [[sicmutils.abstract.function/with-literal-functions]] macro into a
  let-style representation of the requested bindings."
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        bindings (into []
                       (mapcat binding-pair)
                       (:children binding-vec))]
    {:node
     (api/list-node
      (list*
       (api/token-node 'let)
       (api/vector-node bindings)
       body))}))
