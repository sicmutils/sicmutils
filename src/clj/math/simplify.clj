(ns math.simplify
  (:require [clojure.walk :as w]
            [math.numsymb :as sym]
            [math.expression :as x]
            [math.generic :as g]))

(defn symbol-generator
  "Returns a function which generates a sequence of symbols
  staring with the initial prefix."
  [k]
  (let [i (atom -1)]
    #(->> (swap! i inc) (str k) symbol)))

(defn analyzer
  [symbol-generator expr-> ->expr known-operations]
  (let [base-simplify #(expr-> % ->expr)
        printing-base-simplify (fn [x]
                                 (prn "into simp" x)
                                 (let [s (base-simplify x)]
                                   (prn "out of simp" s)
                                   s))
        ]
    (fn [expr]
      ; "if a tree falls in the forest" functional code
      (let [xmap (transient {})
           mapx (transient {})
           auxorder (transient [])
           auxiliarize (fn [node]
                         (prn "auxiliarize" node)
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
                                                   (if (known-operations (first node))
                                                     ; if the head of the expr
                                                     ; is one of the known functions
                                                     ; then this node is ok so we
                                                     ; set it to false in the map.
                                                     ; TODO: this does not take into account the "no nonintgegral exponents" rule
                                                     ; TODO: which might also have to deal w/ negative exponents... does our poly
                                                     ; TODO: library even handle that? some unit tests would be helpful.
                                                     (do
                                                       (conj! xmap [node false])
                                                       node)

                                                     (let [g (symbol-generator)
                                                           simpx (map printing-base-simplify node)
                                                           ; XXX doall is only for debugging
                                                           rhs (doall (sym/apply-by-symbol (first simpx) (rest simpx)))]
                                                       (prn "mapping" g "to" rhs)
                                                       (prn )
                                                       (conj! xmap [node g])
                                                       (conj! mapx [g rhs])
                                                       (conj! auxorder g)
                                                       g))))
                               :else node))
           simplified-auxiliaries true]
        (prn "analyzing" expr)
       [(w/postwalk auxiliarize expr) (persistent! xmap) (persistent! mapx)]))))

(doseq [predicate [number?
                   symbol?
                   nil?]]
  (g/defhandler :simplify [predicate] identity))