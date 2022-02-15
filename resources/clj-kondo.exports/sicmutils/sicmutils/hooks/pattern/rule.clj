(ns hooks.pattern.rule
  (:require [clj-kondo.hooks-api :as api]))

;; TODO this is dumb because we are using the same code here to syntax check
;; inputs and outputs of rules.
;;
;; TODO add linting of

#_
((rule (+ (? (fn [x] x)) 10) => 12) '(+ 12 11))

;; check that, if we have a binding form on the LEFT side of the rule, that only
;; a simple symbol is allowed in the first position.
;;
;; On the RIGHT side, in a consequence, we want to check that there is only ONE
;; item in there; and that we leave it OUT if it's a symbol, leave it in
;; otherwise.
;;
;; it would be super helpful to tell people the rules with the linter.
;;
;; - you are only allowed to have a single function in the consequence ? or ?? spot
;; - lint all restrictions...
;;
;; etc

(defn restriction
  [node]
  (let [[sym binding & restriction] (:children node)
        restriction (if (api/token-node? binding)
                      restriction
                      (cons binding restriction))]
    (when (and (#{'? '??} (:value sym))
               (seq restriction))
      [(api/vector-node restriction)])))

(defn extract-unquotes
  ([node]
   (let [coll-node? (some-fn api/list-node? api/vector-node? api/map-node?)
         atom? (some-fn api/token-node? api/string-node? api/keyword-node?)
         tree (tree-seq coll-node? :children node)]
     (mapcat (fn [x]
               (or (restriction x)
                   (if (or (atom? x) (coll-node? x))
                     []
                     [x])))
             tree))))

(defn pattern [{:keys [node]}]
  (let [[_ form pred] (:children node)]
    {:node
     (api/vector-node
      (into (extract-unquotes form)
            (when pred [pred])))}))

(defn consequence [{:keys [node]}]
  (let [[_ form] (:children node)]
    {:node
     (api/vector-node
      (extract-unquotes form))}))

(defn template [{:keys [node]}]
  (let [[_ m-or-form form-or-nil] (:children node)]
    {:node
     (if form-or-nil
       (api/vector-node
        (into [m-or-form] (extract-unquotes form-or-nil)))
       (api/vector-node
        (extract-unquotes m-or-form)))}))

(defn process-rule [args]
  (condp = (count args)
    2 (let [[pattern consequent-fn] args]
        (api/vector-node
         (list*
          (api/list-node
           [consequent-fn (api/map-node {})])
          (extract-unquotes pattern))))

    3 (let [[pattern pred skel] args]
        (api/vector-node
         (concat
          [(api/list-node
            [pred (api/map-node {})])]
          (extract-unquotes pattern)
          (extract-unquotes skel))))

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
