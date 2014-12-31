(ns math.simplify
  (:require [math.numsymb :as sym]))

(defn symbol-generator
  "Returns a function which generates a sequence of symbols
  staring with the initial prefix."
  [k]
  (let [i (atom -1)]
    #(->> (swap! i inc) (str k) symbol)))

(defn analyzer
  [symbol-generator expr-> ->expr known-operations]
  (let [base-simplify #(expr-> % ->expr)]
    (fn [expr]
      (letfn [(simplify-expression
                [expr]
                (let [[expr-map analyzed-expr] (analyze {} expr)]
                  (->> analyzed-expr base-simplify (backsubstitute expr-map))))
              (analyze
                [expr-map expr]
                (if (and (sequential? expr)
                         (not (= (first expr) 'quote)))
                  (let [[expr-map analyzed-expr] (map-with-state expr-map analyze expr)]
                    ;; at this point all subexpressions are canonical
                    (if (and (known-operations (sym/operator analyzed-expr))
                             true #_"this is where the exponent integrality test would go")
                      [expr-map analyzed-expr]
                      (if-let [existing-expr (expr-map analyzed-expr)]
                        [expr-map existing-expr]
                        (new-kernels expr-map analyzed-expr))))
                  [expr-map expr]))
              (new-kernels
                [expr-map expr]
                (let [simplified-expr (map base-simplify expr)]
                  (if-let [v (sym/symbolic-operator (sym/operator simplified-expr))]
                    (let [w (apply v (sym/operands simplified-expr))]
                      (if (and (sequential? w)
                               (= (sym/operator w) (sym/operator simplified-expr)))
                        (add-symbols expr-map w)
                        (analyze expr-map w)))
                    (add-symbols expr-map simplified-expr))
                  ))
              (add-symbols
                [expr-map expr]
                (let [[new-expr-map new-expr] (map-with-state expr-map add-symbol expr)]
                  (add-symbol new-expr-map new-expr)))
              (add-symbol
                [expr-map expr]
                (if (and (sequential? expr)
                         (doall expr)
                         (not (= (first expr) 'quote)))
                  (if-let [existing-expr (expr-map expr)]
                    [expr-map existing-expr]
                    (let [newvar (symbol-generator)]
                      [(conj expr-map [expr newvar]) newvar]))
                  [expr-map expr]))
              (backsubstitute
                [expr-map expr]
                (let [mapx (into {} (for [[k v] expr-map] [v k]))
                      bsub (fn bsub [v]
                             (cond (sequential? v) (doall (map bsub v))
                                   (symbol? v) (let [w (mapx v)]
                                                 (if w (bsub w) v))
                                   :else v))]
                  (bsub expr)))
              (map-with-state
                ;; Maps f over coll while maintaining state. The function
                ;; f is called with [state, v] for each value v in col, and
                ;; is expected to return a pair containing the new state and
                ;; (f v). The result is a pair with the final state and
                ;; the sequence of the values of (f v).
                [initial-state f coll]
                (reduce
                  (fn [[state acc] val]
                    (let [[new-state f-val] (f state val)]
                      [new-state (conj acc f-val)]))
                  [initial-state []]
                  coll))
              ]
        (simplify-expression expr)))))

