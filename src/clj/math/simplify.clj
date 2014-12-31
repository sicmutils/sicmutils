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
      (let [xmap (transient {})
            mapx (transient {})
            auxorder (transient []) #_"needed?" ]
        (letfn [(simplify-expression
                  [expr]
                  (backsubstitute (analyze-expression expr)))
                (analyze-expression
                  [expr]
                  (base-simplify (analyze expr)))
                (analyze
                  [expr]
                  (ianalyze expr))
                (ianalyze
                  [expr]
                  (if (and (sequential? expr)
                           (not (= (first expr) 'quote)))
                    (let [sexpr (map ianalyze expr)]
                      ;; at this point all subexpressions are canonical
                      (if (and (known-operations (sym/operator sexpr))
                               true #_"this is where the exponent integrality test would go")
                        sexpr
                        (or (xmap sexpr) (new-kernels sexpr))))
                    expr))
                (new-kernels
                  [expr]
                  (let [sexpr (map base-simplify expr)]
                    (if-let [v (sym/symbolic-operator (sym/operator sexpr))]
                      (let [w (apply v (sym/operands sexpr))]
                        (if (and (sequential? w)
                                 (= (sym/operator w) (sym/operator sexpr)))
                          (add-symbols! w)
                          (ianalyze w)))
                      (add-symbols! sexpr))
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
                          (conj! mapx [newvar expr])
                          (conj! auxorder newvar)
                          newvar))
                    expr))
                (backsubstitute
                  [expr]
                  expr)
                ]
          ;; TODO: note that we only return the vector for debugging & testing.
          ;; once backsubstitute is implemented, we won't need this and can
          ;; perhaps get rid of the call to (doall) in (add-symbol!).
          [(simplify-expression expr) (persistent! xmap) (persistent! mapx)])))))

