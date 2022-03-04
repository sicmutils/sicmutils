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

(def ^{:doc "Given any number of nil-or-result producing functions, returns a
  new function (of a single `node`) that will

- try all functions from left to right on the `node`
- return the first non-nil result encountered."}
  match-first
  some-fn)

(defn match-any
  "Given any number of nil-or-result producing functions, returns a new function
  of `node` that will produce a nested series of `api/vector-node`s containing
  all non-nil results from applying any of the supplied functions to `node`."
  ([] (constantly nil))
  ([l] l)
  ([l r]
   (fn [node]
     (let [l' (l node)
           r' (r node)]
       (if (and l' r')
         (api/vector-node [l' r'])
         (or l' r')))))
  ([l r & more]
   (reduce match-any l (cons r more))))

(defn walk-node
  "Given a function `f` of a node and some `node`, returns a vector of all nodes
  in the syntax tree represented by `node` for which `(f sub-node)` returns a
  non-nil result.

  NOTE: The result is an eagerly-evaluated vector, so `f` is permitted to have
  side effects.

  NOTE: Use [[match-any]] and [[match-first]] to build up more sophisticated
  values of `f`."
  [f node]
  (let [tree  (tree-seq coll-node? :children node)
        xform (mapcat (fn [x]
                        (when-let [result (f x)]
                          [result])))]
    (into [] xform tree)))

(defn restrictions
  "If the supplied node is a [[binding-form?]], emits an `api/vector-node`
  containing the restrictions. Else, returns `nil`."
  [node]
  (when (binding-form? node)
    (let [[_ _ & restrictions] (:children node)]
      (when (seq restrictions)
        (api/vector-node restrictions)))))

(defn unquotes
  "If the supplied node is an [[any-unquote?]], acts as identity. Else, returns
  `nil`."
  [node]
  (when (any-unquote? node)
    node))

(defn- reg-binding-sym!
  "This function is shared between binding and consequence checkers, with
  different guards in each case."
  [binding]
  (api/reg-finding!
   (assoc (meta binding)
          :message
          (str "Binding variable "
               (pr-str (api/sexpr binding))
               " must be a non-namespaced symbol or non-symbol form.")
          :type :sicmutils.pattern/binding-sym)))

(defn lint-binding-form!
  "If the supplied `node` is a [[binding-form?]], registers findings for invalid
  syntax.

  [[lint-binding-form!]] returns nil for any input."
  [node]
  (when (binding-form? node)
    (let [[sym binding & restrictions] (:children node)]
      (when-not (or (simple-symbol? (:value binding))
                    (any-unquote? binding))
        (reg-binding-sym! binding))

      (when (segment-marker? sym)
        (doseq [r restrictions]
          (api/reg-finding!
           (assoc (meta r)
                  :message
                  (str "Restrictions are (currently) ignored on "
                       (:value sym) " binding forms: "
                       (pr-str (api/sexpr r)))
                  :type :sicmutils.pattern/ignored-restriction)))))))

(defn pattern-vec
  "Given a node representing a pattern form, (and, optionally, a node representing
  a predicate function `f`), returns a vector of all checkable entries in the
  pattern.

  These are

  - all unquoted forms
  - all restrictions on binding forms

  During the syntax tree walk, [[pattern-vec]] will trigger a side effect of
  linting all binding forms it sees using [[lint-binding-form!]]"
  ([node] (pattern-vec node nil))
  ([node pred]
   (let [f (match-first lint-binding-form! restrictions unquotes)
         to-check (walk-node f node)]
     (if pred
       (conj to-check pred)
       to-check))))

(defn pattern
  "Converts a node representing an invocation of the [[pattern.rule/pattern]]
  macro into a vector-node of all lintable forms.

  As a side effect, registers any issue with binding forms encountered in the
  pattern using [[lint-binding-form!]]"
  [{:keys [node]}]
  (let [[_ form pred] (:children node)]
    {:node
     (api/vector-node
      (pattern-vec form pred))}))

;; ### Consequences
;;
;; Consequence forms also use `(? ..)` and `(?? ...)` as a way to splice in
;; matched bindings. Unlike the pattern uses of these, restrictions are NOT
;; allowed.

(defn binding-field
  "If the supplied node is a [[binding-form?]] and the binding entry is NOT
  symbolic, emits the binding node.

  Else, returns `nil`."
  [node]
  (when (binding-form? node)
    (let [[_ binding] (:children node)]
      (when-not (symbol? (:value binding))
        binding))))

(defn lint-consequence!
  "If the supplied `node` is a [[binding-form?]], registers findings for invalid
  syntax (interpreting the binding forms as consequence matchers, not patterns).

  [[lint-consequence!]] returns nil for any input."
  [node]
  (when (binding-form? node)
    (let [[_ binding & restrictions] (:children node)]
      (when (qualified-symbol? (:value binding))
        (reg-binding-sym! binding))
      (doseq [r restrictions]
        (api/reg-finding!
         (assoc (meta r)
                :message
                (str (str "Restrictions are not allowed in consequence bindings: "
                          (pr-str (api/sexpr r))))
                :type :sicmutils.pattern/consequence-restriction))))))

(defn consequence-vec
  "Given a node representing a consequence form, (the right side of a rule),
  returns a vector of all checkable entries in the consequence.

  These are

  - all unquoted forms
  - any non-symbolic binding field
  - any restrictions on binding forms (which are invalid to use here, but still
    deserve linting if they are ALSO non-existent or something!)

  During the syntax tree walk, [[consequence-vec]] will trigger a side effect of
  linting all binding forms it sees using [[lint-consequence!]]"
  [form]
  (let [f (match-first
           lint-consequence!
           (match-any binding-field restrictions)
           unquotes)]
    (walk-node f form)))

(defn consequence
  "Converts a node representing an invocation of the [[pattern.rule/consequence]]
  macro into a vector-node of all lintable forms.

  As a side effect, registers any issue with binding forms encountered in the
  pattern using [[lint-consequence!]]"
  [{:keys [node]}]
  (let [[_ form] (:children node)]
    {:node
     (api/vector-node
      (consequence-vec form))}))

(defn template
  "Converts a node representing an invocation of the [[pattern.rule/template]]
  macro into a vector-node of all lintable forms.

  These include the optional first argument `m` and anything returned
  by `([[consequence-vec]] <form-argument>)`."
  [{:keys [node]}]
  (let [[_ m-or-form form-or-nil] (:children node)]
    {:node
     (if form-or-nil
       (api/vector-node
        (conj (consequence-vec form-or-nil) m-or-form))
       (api/vector-node
        (consequence-vec m-or-form)))}))

(defn- process-rule
  "Given the 2- or 3- arguments supplied to the [[pattern.rule/rule]] macro,
  returns an `api/vector-node` of all lintable forms.

  The `pattern` argument is processed using [[pattern-vec]], while the
  `skeleton` argument is processed using [[consequence-vec]].

  The `pred` or `consequent-fn` arguments are presented as fn invocation forms
  like `(fn {})` to prevent 'unused fn' warnings."
  [args]
  (condp = (count args)
    2 (let [[pattern consequent-fn] args]
        (api/vector-node
         (conj (pattern-vec pattern)
               (api/list-node
                [consequent-fn (api/map-node {})]))))

    3 (let [[pattern pred skel] args]
        (api/vector-node
         (concat
          [(api/list-node
            [pred (api/map-node {})])]
          (pattern-vec pattern)
          (consequence-vec skel))))

    ;; This should never happen.
    (throw (ex-info "wrong number of arguments for rule" {}))))

(defn rule
  "Converts a node representing an invocation of the [[pattern.rule/rule]] macro
  into a vector-node of all lintable forms."
  [{:keys [node]}]
  {:node (process-rule
          (rest (:children node)))})

(defn ruleset
  "Converts a node representing an invocation of the [[pattern.rule/ruleset]]
  macro into a vector-node of all lintable forms."
  [{:keys [node]}]
  (let [binding-count (dec (count (:children node)))]
    (when-not (zero? (mod binding-count 3))
      (api/reg-finding!
       (assoc (meta (first (:children node)))
              :message
              (str "ruleset requires bindings in groups of 3. Received "
                   binding-count " bindings.")
              :type :sicmutils.pattern/ruleset-args))))
  (let [inputs (partition 3 (rest (:children node)))
        rules  (map process-rule inputs)]
    {:node (api/vector-node rules)}))
