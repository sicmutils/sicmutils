(ns math.simplify
  (:require [math.poly :as poly]
            [clojure.walk :as w]
            [clojure.pprint :as pp]))

(defn symbol-generator [k]
  (let [i (atom -1)]
    #(->> (swap! i inc) (str k) symbol)))

(defn analyzer
  [symbol-generator expr-> ->expr known-operations]
  ; "if a tree falls in the forest" functional code
  (let [base-simplify                                       ;#(if (seq? %) (expr-> % ->expr) %)
        (fn [f] (prn "bs" f) (if (seq? f) (-> f (expr-> ->expr)) f))]
    (fn [expr]
     (let [xmap (transient {})
           mapx (transient {})
           auxorder (transient [])
           auxiliarize (fn [node]
                         (cond (vector? node) node
                               (seq? node) (let [v (xmap node)]
                                             (cond (false? v) node
                                                   (some? v) v
                                                   :else
                                                   ; at this point, we have not
                                                   ; seen the expression, so we
                                                   ; have to decide if we want
                                                   ; to assign an aux variable to
                                                   ; it.
                                                   (do (prn "fn" (first node) "ko" known-operations)
                                                       (if (known-operations (first node))
                                                         ; if the head of the expr
                                                         ; is one of the known functions
                                                         ; then this node is ok so we
                                                         ; set it to false in the map.
                                                         (do
                                                           (conj! xmap [node false])
                                                           node)
                                                         (let [g (symbol-generator)
                                                               simpx (map base-simplify (rest node))
                                                               rhs (cons (first node) simpx)
                                                               ;rhs node
                                                               ]
                                                           (do (prn "node" node)
                                                               (prn "rest-node" (rest node))
                                                               (prn "type simpx" (type simpx))
                                                               (prn "tf" (type (first simpx)))
                                                               (prn "simpx" simpx))
                                                           (conj! xmap [node g])
                                                           (conj! mapx [g rhs])
                                                           (conj! auxorder g)
                                                           g
                                                           )
                                                         ))
                                                   ))
                               :else node))
           simplified-auxiliaries true]

       [(w/postwalk auxiliarize expr) (persistent! xmap) (persistent! mapx)]))))
