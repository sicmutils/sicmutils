(ns hooks.let-coordinates
  (:require [clj-kondo.hooks-api :as api]))

;; from coordinate
(defn ^:no-doc symbols-from-prototype
  "Generates a list of symbols from the supplied argument prototype. The
  prototype is allowed to be a vector, a list like `(up x y)` or a bare symbol.
  Anything else causes an exception.

  Nested structures are fine! The return value is a flat sequence."
  [p]
  (cond (and (api/list-node? p)
             ('#{up down} (:value (first (:children p)))))
        (mapcat symbols-from-prototype (rest (:children p)))

        (api/vector-node? p) (mapcat symbols-from-prototype (:children p))
        (and (api/token-node? p)
             (symbol? (:value p)))
        [p]
        :else (throw
               (ex-info (str "Invalid coordinate prototype: " p) {}))))

;; from vector-field
(defn ^:no-doc coordinate-name->vf-name
  "From the name `n` of a coordinate, produce the name of the coordinate basis
  vector field (as a symbol)"
  [n]
  (api/token-node
   (symbol (str "d:d" (:value n)))))

;; from form-field
(defn ^:no-doc coordinate-name->ff-name
  "From the name of a coordinate, produce the name of the coordinate basis
  one-form field (as a symbol)"
  [n]
  (api/token-node
   (symbol (str \d (:value n)))))

(defn quotify [n]
  [n (api/list-node [(api/token-node 'quote) n])])

(defn let-coordinates [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)]
    (when-not (even? (count (:children binding-vec)))
      (throw (ex-info "checking requires an even number of bindings" {})))
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

(defn using-coordinates [{:keys [node]}]
  (let [[sym prototype system & body] (:children node)]
    (let-coordinates
        {:node (api/list-node
                (list* sym
                       (api/vector-node [prototype system])
                       body))})))

(defn ->def [n]
  (api/list-node
   [(api/token-node 'def)
    n
    (api/list-node [(api/token-node 'quote) n])]))

(defn define-coordinates [{:keys [node ns]}]
  (let [[_ prototype system] (:children node)
        sys-name (api/token-node (symbol (name (:value system))))
        coord-names (symbols-from-prototype prototype)
        vf-names (map coordinate-name->vf-name coord-names)
        ff-names (map coordinate-name->ff-name coord-names)
        new-node (api/list-node
                  (concat
                   [(api/token-node 'do)
                    (api/list-node
                     [(api/token-node 'def) sys-name system])]
                   (map ->def coord-names)
                   (map ->def vf-names)
                   (map ->def ff-names)))]
    {:node new-node}))
