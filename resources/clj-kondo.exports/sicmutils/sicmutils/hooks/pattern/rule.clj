(ns hooks.pattern.rule
  (:require [clj-kondo.hooks-api :as api]))

;; ## Pattern Linting
;;
;; Lint the first argument of the `pattern` and macros by treating everything as
;; quoted, EXCEPT:
;;
;; - anything inside an [[unquote?]] or [[unquote-splice?]]form
;; - restrictions, which appear in the third-and-further position of `(? sym
;;   ..)` or `(?? sym ...)` forms
;;
;; Additionally, check that:
;;
;; - `($$ ...)` forms contain only a single symbol, nothing else
;; - `(?? ...)` and `(? ...)` begin with a bare symbol, nothing else.

(defn unquote?
  "Returns true if the supplied `node` is an `:unquote` node, false otherwise."
  [node]
  (= :unquote (:tag node)))

(defn unquote-splice?
  "Returns true if the supplied `node` is an `:unquote-splicing` node, false
  otherwise."
  [node]
  (= :unquote-splicing (:tag node)))

(def ^{:doc "Returns true if the supplied `node` is either of the unquote
forms (`:unquote` or `:unquote-splicing`, false otherwise.)"}
  any-unquote?
  (some-fn unquote? unquote-splice?))

(def ^{:doc "Returns true if the supplied `node` is a collection, ie, a list,
  vector or map, false otherwise."}
  coll-node?
  (some-fn api/list-node? api/vector-node? api/map-node?))

(defn binding-form?
  "Returns true if the node references a list of the form `(? ...)`, `(?? ...)`
  or `($$ ...)`, false otherwise."
  [node]
  (and (api/list-node? node)
       (#{'? '?? '$$}
        (:value
         (first (:children node))))))

(defn segment-marker?
  "Returns true if the supplied node begins a segment binding, false otherwise."
  [sym]
  (contains? #{'?? '$$} (:value sym)))

(defn walk-node
  "NOTE side effects a"
  [f node]
  (let [tree  (tree-seq coll-node? :children node)
        xform (mapcat (fn [x]
                        (when-let [result (f x)]
                          [result])))]
    (into [] xform tree)))

(defn restrictions
  "If the supplied node is a [[binding-form?]], emits an `api/vector-node`
  containing the restrictions."
  [node]
  (when (binding-form? node)
    (let [[_ _ & restrictions] (:children node)]
      (when (seq restrictions)
        (api/vector-node restrictions)))))

(defn unquotes
  "If the supplied node is an [[any-unquote?]] acts as identity. Else, returns
  nil."
  [node]
  (when (any-unquote? node)
    node))

(defn lint-binding-form!
  "If the supplied `node` is a [[binding-form?]], registers findings for invalid
  syntax. Else, returns nil with no side effects."
  [node]
  (when (binding-form? node)
    (let [[sym binding & restrictions] (:children node)]
      (when-not (or (simple-symbol? (:value binding))
                    (any-unquote? binding))
        (let [{:keys [row col]} (meta binding)]
          (api/reg-finding!
           {:message
            "Binding variable must be a non-namespaced symbol."
            :type :sicmutils.pattern/binding-sym
            :row row
            :col col})))

      (when (segment-marker? sym)
        (doseq [r restrictions]
          (let [{:keys [row col]} (meta r)]
            (api/reg-finding!
             {:message
              (str "Restrictions are (currently) ignored on "
                   (:value sym) " binding forms.")
              :type :sicmutils.pattern/ignored-restriction
              :row row
              :col col})))))))

(defn pattern-unquotes
  "Given a node representing a pattern, returns a vector of

  - all unquoted forms in the supplied pattern
  - all restrictions encountered in any binding form.

  Optionally takes an `effect!` that will run on every node encountered during
  tree-walking of the supplied `node`."
  ([node] (pattern-unquotes node identity))
  ([node effect!]
   (let [tree  (tree-seq coll-node? :children node)
         xform (mapcat (fn [x]
                         (effect! x)
                         (or (when-let [v (restrictions x)]
                               [v])
                             (if (any-unquote? x)
                               [x]
                               []))))]
     (into [] xform tree))))

(defn pattern
  "Converts a node representing an invocation of the [[sicmutils.rule/pattern]]
  macro into a vector of all unquoted forms.

  As a side effect, registers any issue with binding forms encountered in the
  pattern using [[lint-binding-form!]]"
  [{:keys [node]}]
  (let [[_ form pred] (:children node)
        to-check (pattern-unquotes form lint-binding-form!)]
    {:node
     (api/vector-node
      (if pred
        (conj to-check pred)
        to-check))}))

;; ### Consequences
;;
;; Consequence forms also use `(? ..)` and `(?? ...)` as a way to splice in
;; matched bindings. Unlike the pattern uses of these, restrictions are NOT
;; allowed.

(defn binding-field
  "If the supplied node is a [[binding-form?]], emits an `api/vector-node`
  containing the restrictions."
  [node]
  (when (binding-form? node)
    (let [[_ binding] (:children node)]
      (when-not (symbol? (:value binding))
        binding))))

(defn lint-consequence!
  "If the supplied `node` is a [[binding-form?]], registers findings for invalid
  syntax (interpreting the binding forms as consequence matchers, not patterns).

  Else, returns nil with no side effects."
  [node]
  (when (binding-form? node)
    (let [[_ binding & restrictions] (:children node)]
      (when (qualified-symbol? (:value binding))
        (let [{:keys [row col]} (meta binding)]
          (api/reg-finding!
           {:message
            "Binding variable must be a non-namespaced symbol."
            :type :sicmutils.pattern/binding-sym
            :row row
            :col col})))

      (doseq [r restrictions]
        (let [{:keys [row col]} (meta r)]
          (api/reg-finding!
           {:message
            (str "Restrictions are not allowed in consequence binding forms.")
            :type :sicmutils.pattern/consequence-restriction
            :row row
            :col col}))))))

(defn consequence
  "Converts a node representing an invocation of
  the [[sicmutils.rule/consequence]] macro into a vector of all unquoted forms.

  As a side effect, registers any issue with binding forms encountered in the
  pattern using [[lint-consequence!]]"
  [{:keys [node]}]
  (let [[_ form] (:children node)]
    {:node
     (api/vector-node
      (pattern-unquotes form lint-consequence!))}))

(defn template [{:keys [node]}]
  (let [[_ m-or-form form-or-nil] (:children node)]
    {:node
     (if form-or-nil
       (api/vector-node
        (into [m-or-form] (pattern-unquotes form-or-nil)))
       (api/vector-node
        (pattern-unquotes m-or-form)))}))

(defn process-rule [args]
  (condp = (count args)
    2 (let [[pattern consequent-fn] args]
        (api/vector-node
         (list*
          (api/list-node
           [consequent-fn (api/map-node {})])
          (pattern-unquotes pattern))))

    3 (let [[pattern pred skel] args]
        (api/vector-node
         (concat
          [(api/list-node
            [pred (api/map-node {})])]
          (pattern-unquotes pattern)
          (pattern-unquotes skel))))

    (throw (ex-info "wrong number of arguments for rule" {}))))

(defn rule [{:keys [node]}]
  {:node (process-rule
          (rest (:children node)))})

(defn ruleset [{:keys [node]}]
  (when-not (zero? (mod (dec (count (:children node))) 3))
    (throw (ex-info "ruleset requires bindings in groups of 3" {})))
  (let [inputs (partition 3 (rest (:children node)))
        rules  (map process-rule inputs)]
    {:node (api/vector-node rules)}))
