(ns hooks.sicmutils.calculus.coordinate
  (:require [clj-kondo.hooks-api :as api]))

(defn symbols-from-prototype
  "Generates a list of lintable symbols from the supplied argument prototype. The
  prototype is allowed to be a vector, a list like `(up x y)` or a bare symbol.
  Anything else causes an exception.

  Nested structures are fine! The return value is a flat sequence."
  [p]
  (cond (and (api/list-node? p)
             ('#{up down} (:value (first (:children p)))))
        (mapcat symbols-from-prototype (rest (:children p)))

        (api/vector-node? p)
        (mapcat symbols-from-prototype (:children p))

        (and (api/token-node? p) (symbol? (:value p)))
        [p]

        :else
        (do (api/reg-finding!
             (assoc (meta p)
                    :message (str "Bindings must be either a vector or list "
                                  "(optionally beginning with `up` or `down`) "
                                  "or a bare symbol. Received: "
                                  (pr-str (api/sexpr p)))
                    :type :sicmutils.calculus.coordinate/invalid-binding))
            [])))

(defn coordinate-name->vf-name
  "Given a token-node of coordinate symbolic name, returns a token-node containing
  the symbolic name of the associated coordinate basis vector field."
  [n]
  (api/token-node
   (symbol (str "d:d" (:value n)))))

(defn coordinate-name->ff-name
  "Given a token-node of coordinate symbolic name, returns a token-node containing
  the symbolic name of the associated coordinate basis one-form vector field."
  [n]
  (api/token-node
   (symbol (str \d (:value n)))))

(defn quotify
  "Given some node, returns a 2-vector representing a pair of entries in a
  let-style binding vector. The value is a quoted node `n`."
  [n]
  [n (api/list-node [(api/token-node 'quote) n])])

(defn let-coordinates
  "Converts a node representing an invocation of
  the [[sicmutils.calculus.coordinate/let-coordinates]] macro into a let-style
  representation of the requested bindings."
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)]
    (when-not (even? (count (:children binding-vec)))
      (throw
       (ex-info "let-coordinates requires an even number of bindings." {})))

    (let [pairs        (partition 2 (:children binding-vec))
          prototypes   (map first pairs)
          c-systems    (map second pairs)
          system-names (map (comp api/token-node symbol name :value) c-systems)
          coord-names  (mapcat symbols-from-prototype prototypes)
          vf-names     (map coordinate-name->vf-name coord-names)
          ff-names     (map coordinate-name->ff-name coord-names)
          new-node (api/list-node
                    (list*
                     (api/token-node 'let)
                     (api/vector-node
                      (concat
                       (interleave system-names c-systems)
                       (mapcat quotify coord-names)
                       (mapcat quotify vf-names)
                       (mapcat quotify ff-names)))
                     (api/vector-node
                      (concat system-names coord-names vf-names ff-names))
                     body))]
      {:node new-node})))

(defn using-coordinates
  "Converts a node representing an invocation of
  the [[sicmutils.calculus.coordinate/using-coordinates]] macro into a let-style
  representation of the requested bindings."
  [{:keys [node]}]
  (let [[sym prototype system & body] (:children node)]
    (let-coordinates
        {:node (api/list-node
                (list* sym
                       (api/vector-node [prototype system])
                       body))})))

(defn ->declare
  "Given some token-node, returns a 2-vector representing a pair of entries in a
  let-style binding vector. The value is a quoted node `n`."
  [n]
  (api/list-node
   [(api/token-node 'declare)
    n]))

(defn define-coordinates
  "Converts a node representing an invocation of
  the [[sicmutils.calculus.coordinate/define-coordinates]] macro into a series
  of declarations for all generated `def` forms, plus a final vector of all
  entries.

  This last vector will silence any 'not used' warnings."
  [{:keys [node]}]
  (let [[_ prototype system] (:children node)
        sys-name (api/token-node (symbol (name (:value system))))
        coord-names (symbols-from-prototype prototype)
        vf-names (map coordinate-name->vf-name coord-names)
        ff-names (map coordinate-name->ff-name coord-names)
        syms (concat [sys-name] coord-names vf-names ff-names)
        new-node (api/list-node
                  (concat
                   [(api/token-node 'do)
                    (->declare sys-name)]
                   (map ->declare coord-names)
                   (map ->declare vf-names)
                   (map ->declare ff-names)
                   [(api/vector-node syms)]))]
    {:node new-node}))
