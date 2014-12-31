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
      ; "if a tree falls in the forest" functional code
      (let [xmap (transient {})]
        (letfn [(simplify-expression
                  [expr]
                  (-> expr analyze base-simplify (backsubstitute (persistent! xmap))))
                (analyze
                  [expr]
                  (if (and (sequential? expr)
                           (not (= (first expr) 'quote)))
                    (let [analyzed-expr (map analyze expr)]
                      ;; at this point all subexpressions are canonical
                      (if (and (known-operations (sym/operator analyzed-expr))
                               true #_"this is where the exponent integrality test would go")
                        analyzed-expr
                        (or (xmap analyzed-expr) (new-kernels analyzed-expr))))
                    expr))
                (new-kernels
                  [expr]
                  (let [simplified-expr (map base-simplify expr)]
                    (if-let [v (sym/symbolic-operator (sym/operator simplified-expr))]
                      (let [w (apply v (sym/operands simplified-expr))]
                        (if (and (sequential? w)
                                 (= (sym/operator w) (sym/operator simplified-expr)))
                          (add-symbols! w)
                          (analyze w)))
                      (add-symbols! simplified-expr))
                    ))
                (add-symbols!
                  [expr]
                  (let [new (map add-symbol! expr)]
                    (add-symbol! new)))
                (add-symbol!
                  [expr]
                  (if (and (sequential? expr)
                           (doall expr)
                           (not (= (first expr) 'quote)))
                    (or (xmap expr)
                        (let [newvar (symbol-generator)]
                          (conj! xmap [expr newvar])
                          newvar))
                    expr))
                (backsubstitute
                  [expr xmap]
                  (let [mapx (into {} (for [[k v] xmap] [v k]))
                        bsub (fn bsub [v]
                               (cond (sequential? v) (doall (map bsub v))
                                     (symbol? v) (let [w (mapx v)]
                                                   (if w (bsub w) v))
                                     :else v))]
                    (bsub expr)))]
          ;; TODO: note that we only return the vector for debugging & testing.
          ;; once backsubstitute is implemented, we won't need this and can
          ;; perhaps get rid of the call to (doall) in (add-symbol!).
          (simplify-expression expr))))))

