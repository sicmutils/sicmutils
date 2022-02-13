(ns hooks.rule
  (:require [clj-kondo.hooks-api :as api]))

(defn extract-unquotes [node]
  (let [coll-node? (some-fn api/list-node? api/vector-node? api/map-node?)
        atom? (some-fn api/token-node? api/string-node? api/keyword-node?)
        tree (tree-seq coll-node? :children node)]
    (remove (some-fn coll-node? atom?) tree)))

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
          [(api/list-node
            [consequent-fn (api/map-node {})])]
          (extract-unquotes pattern))))

    3 (let [[pattern pred skel] args]
        (api/vector-node
         (concat
          [(api/list-node
            [pred (api/map-node {})])]
          (extract-unquotes pattern)
          (extract-unquotes skel))))

    (throw (ex-info "wrong number of arguments for rule" {}))))

;; TODO in all of these fuckers we DO want to also keep items that are past the
;; first two items in `(? x ....)` and `(?? x ...)` for later. The restrictions.
;;
;; TODO then we have to get the namespace working in the other one, to kill redefined var.

(defn rule [{:keys [node]}]
  {:node (process-rule
          (rest (:children node)))})

(defn ruleset [{:keys [node]}]
  (when-not (zero? (mod (dec (count (:children node))) 3))
    (throw (ex-info "ruleset requires bindings in groups of 3" {})))
  (let [inputs (partition 3 (rest (:children node)))
        rules  (map process-rule inputs)]
    {:node (api/vector-node rules)}))
