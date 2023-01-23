#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.analyze
  "This namespace defines an API for working with 'expression analyzers' and
  the [[ICanonicalize]] protocol.

  Expression analyzers find canonical forms of inputs over limited vocabularies
  of operations. For example, a polynomial analyzer will expose operations like
  addition, subtraction, multiplication, and exponentiation by positive
  integers (but not division).

  An expression containing only these operations and symbols can then be
  converted to and from a polynomial canonical form, which in this example would
  have the effect of grouping like terms; a rational function backend would
  include the division operation and be capable of cancellation. Canonicalizing
  an expression with respect to an analyzer is therefore effected by a
  round-trip to and from the canonical form."
  (:require [emmy.expression :as x]
            [emmy.numsymb :as sym]
            [emmy.value :as v]))

;; ## General Recursive Simplifier Maker
;;
;; The heading, and the following block of comments, is based on the narrative
;; in `simplify/simplify.scm` of the scmutils repository.
;;
;; Given a set of operations, the protocols and functions in this namespace
;; allow you to define recursive simplifiers that simplify expressions involving
;; these operations, treating other combinations as atomic.
;;
;; To break an expression up into manipulable and nonmanipulable parts with
;; respect to a set of algebraic operators. This is done by the introduction of
;; auxiliary variables.
;;
;; For example, the equation
;;
;;    I = Is (exp((V2 - V3)/Vt) - 1) ;; I, V2, V3
;;
;; can be broken into three equations:
;;
;;    I + Is = Is*X                  ;; I, X
;;    V2/Vt - V3/Vt = Y              ;; V2, V3, Y
;;    X = (exp Y)                    ;; X, Y
;;
;; where X and Y are new variables. The first two parts contain only addition,
;; subtraction, multiplication, and division and the third is not expressible in
;; terms of those operations.
;;
;;
;; ## Implementation

(def ^{:dynamic true
       :doc "Exponential expressions with non-integer exponents must become
       kernels, because they cannot become polynomial exponentials.

       To disable this guard, bind this variable to `false`."}
  *inhibit-expt-simplify*
  true)

;; ### Utilities

(defn- make-vcompare
  "Returns
  a [Comparator](https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html)
  function taking account of the input variable set `var-set` in the following
  way:

  If both inputs to the comparator are in `var-set,` or both are not, then the
  results are as `clojure.core/compare` would return. But if one is in `var-set`
  and the other is not, then the other will always compare greater.

  In this way, expressions produced by the simplifier will have simple variables
  sorted earlier than expressions involving those variables."
  [var-set]
  (fn [v w]
    (cond
      (var-set v) (if (var-set w)
                    (compare v w)
                    -1)
      (var-set w) 1
      :else (compare v w))))

(defn monotonic-symbol-generator
  "Returns a function which generates a sequence of symbols with the given
  `prefix` with the property that later symbols will sort after earlier symbols.

  This is important for the stability of the simplifier. (If we just used
  `clojure.core/gensym`, then a temporary symbol like `G__1000` will sort
  earlier than `G__999`. This will trigger errors at unpredictable times,
  whenever `clojure.core/gensym` returns two symbols that cross an
  order-of-magnitude boundary.)"
  [prefix]
  (let [count (atom -1)]
    (fn [] (symbol
           #?(:clj
              (format "%s%016x" prefix (swap! count inc))

              :cljs
              (let [i (swap! count inc)
                    suffix (-> (.toString i 16)
                               (.padStart 16 "0"))]
                (str prefix suffix)))))))

(defprotocol ICanonicalize
  "[[ICanonicalize]] captures the methods exposed by a Emmy analyzer backend."
  (expression->
    [analyzer x continue]
    [analyzer x continue compare-fn]
    "Invokes `continue` with two arguments:

  - A version of `x` converted to the canonical form represented by `analyzer`
  - A (sorted by `compare-fn`) sequence of variables found in `x`.

  `compare-fn` is used to sort variables. Defaults
  to [[clojure.core/compare]].")

  (->expression [analyzer b variables]
    "Convert a canonical form `b` back to S-expression form.

    Each [[ICanonicalize]] instance uses `variables` in different ways. The
    `variables` sequence is typically obtained from the continuation invoked
    by [[expression->]], so these functions are complementary.")

  (known-operation? [analyzer x]
    "Returns true if the symbolic operation `x` is considered fundamental by
    `analyzer`, false otherwise."))

(defn make-analyzer
  "Make-analyzer takes an analyzer `backend` (which implements [[ICanonicalize]])
  and returns a dictionary with the apparatus necessary to prepare expressions
  for analysis by replacing subexpressions formed from operations unknown to the
  analyzer with generated symbols, and backsubstituting after analysis is
  complete.

  For example, in the case of polynomial canonical form, we would replace a
  subexpression like `(sin x)` with a gensym, before entry, since the `sin`
  operation is not available to the polynomial canonicalizer, and restore it
  afterwards."
  ([backend]
   (make-analyzer backend (monotonic-symbol-generator "-g-")))
  ([backend symbol-generator]
   (let [ref #?(:clj ref :cljs atom)
         alter #?(:clj alter :cljs swap!)
         ref-set #?(:clj ref-set :cljs reset!)
         expr->var (ref {})
         var->expr (ref {})
         compare-fn (atom compare)]
     (letfn [(v-compare [v1 v2]
               (@compare-fn v1 v2))

             (unquoted-list? [expr]
               (and (sequential? expr)
                    (not (= (first expr) 'quote))))

             ;; Prepare for new analysis
             (new-analysis! []
               (reset! compare-fn compare)
               (#?(:clj dosync :cljs do)
                (ref-set expr->var {})
                (ref-set var->expr {}))
               nil)

             (ianalyze [expr]
               (if (unquoted-list? expr)
                 (let [analyzed-expr (doall (map ianalyze expr))]
                   (if (and (known-operation? backend (sym/operator analyzed-expr))
                            (not (and *inhibit-expt-simplify*
                                      (sym/expt? analyzed-expr)
                                      (not (v/integral?
                                            (second
                                             (sym/operands analyzed-expr)))))))
                     analyzed-expr
                     (if-let [existing-expr (@expr->var analyzed-expr)]
                       existing-expr
                       (new-kernels analyzed-expr))))
                 expr))

             (analyze [expr]
               (let [vcompare (make-vcompare (x/variables-in expr))]
                 (reset! compare-fn vcompare))
               (ianalyze expr))


             ;; NOTE: use `doall` to force the variable-binding side effects
             ;; of `base-simplify`.
             (new-kernels [expr]
               (let [simplified-expr (doall (map base-simplify expr))
                     op (sym/operator simplified-expr)]
                 (if-let [v (sym/symbolic-operator op)]
                   (let [w (apply v (sym/operands simplified-expr))]
                     (if (and (sequential? w)
                              (= (sym/operator w) op))
                       (add-symbols! w)
                       (ianalyze w)))
                   (add-symbols! simplified-expr))))

             (add-symbol! [expr]
               (if (unquoted-list? expr)
                 ;; in a transaction, probe and maybe update the expr->var->expr
                 ;; maps.
                 ;;
                 ;; NOTE: Make sure to use the FROZEN version of the expression
                 ;; as the key!
                 (let [expr-k (v/freeze expr)]
                   (#?(:clj dosync :cljs identity)
                    (if-let [existing-expr (@expr->var expr-k)]
                      existing-expr
                      (let [var (symbol-generator)]
                        (alter expr->var assoc expr-k var)
                        (alter var->expr assoc var expr)
                        var))))
                 expr))

             (add-symbols! [expr]
               ;; NOTE: FORCE the side effect of binding all symbols.
               (let [new (doall (map add-symbol! expr))]
                 (add-symbol! new)))

             (backsubstitute [expr]
               (cond (sequential? expr) (doall
                                         (map backsubstitute expr))
                     (symbol? expr) (if-let [w (@var->expr expr)]
                                      (backsubstitute w)
                                      expr)
                     :else expr))

             (base-simplify [expr]
               (if (unquoted-list? expr)
                 (expression-> backend
                               expr
                               #(->expression backend %1 %2)
                               v-compare)
                 expr))

             (analyze-expression [expr]
               (binding [sym/*incremental-simplifier* false]
                 (base-simplify
                  (analyze expr))))

             ;; Simplify relative to existing tables.
             (simplify-expression [expr]
               (backsubstitute
                (analyze-expression expr)))

             ;; Default simplifier
             (simplify [expr]
               (new-analysis!)
               (simplify-expression
                (x/expression-of expr)))]
       {:simplify
        (fn [expr]
          (if (x/literal? expr)
            (x/fmap simplify expr)
            (simplify expr)))

        :simplify-expression
        (fn [expr]
          (if (x/literal? expr)
            (x/fmap simplify-expression expr)
            (simplify-expression expr)))

        :initializer new-analysis!
        :analyze-expression analyze-expression
        :get-var->expr (fn [] @var->expr)
        :get-expr->var (fn [] @expr->var)}))))

;; These functions allow you to take different pieces of the analyzer apart.

(defn default-simplifier
  "Given an `analyzer` instance created with [[make-analyzer]], returns a
  simplifier (a function of S-expression => simplified S-expression) that will
  reset its internal symbolic bindings at every invocation.

  Equivalent to:

  ```clojure
  (let [new-analysis! (initializer analyzer)
        simplify (expression-simplifier analyzer)]
    (fn [expr]
      (new-analysis!)
      (simplify expr)))
  ```

  See [[expression-simplifier]] for a version that will assign the same symbol
  to every expression it sees more than once."
  [analyzer]
  (:simplify analyzer))

(defn expression-simplifier
  "Given an `analyzer` instance created with [[make-analyzer]], returns a
  simplifier (a function of S-expression => simplified S-expression) that will
  NOT reset its internal symbolic bindings across invocations.

  This can be useful if the analyzer backend has any sort of memoization or
  caching of expressions.

  Pass `analyzer` to [[initializer]] to create a function that, when called,
  will explicitly reset the internal cache:

  ```clojure
  (def reset-analyzer! (initializer analyzer))
  (def simplify (expression-simplifier analyzer))

  (reset-analyzer!)
  (simplify <expr>)
  ```

  See [[default-simplifier]] for a version that will reset its internal variable
  assignment cache at each invocation."
  [analyzer]
  (:simplify-expression analyzer))

(defn initializer
  "Given an `analyzer` instance created with [[make-analyzer]], returns a function
  of no arguments that, when called, will reset the analyzer's internal caches
  of symbol => subexpression and subexpression => symbol."
  [analyzer]
  (:initializer analyzer))

(defn expression-analyzer
  "Given an `analyzer` instance created with [[make-analyzer]], returns a function
  that will take a symbolic expression, and return a simplified expression with
  any subexpression NOT supported by the analyzer backend replaced by a
  generated symbol.

  Any replaced subexpression will map to the SAME symbol over repeated
  invocations, unless you call the resetting function generated by passing
  `analyzer` to [[initializer]].

  For example:

  ```clojure
  (let [a  (poly-analyzer)
        ea (expression-analyzer a)]
    (ea '(+ x x x (sin x) (sin x))))
  ;;=> (+ (* 3 x) (* 2 -s-0000000000000000))
  ```"
  [analyzer]
  (:analyze-expression analyzer))

(defn auxiliary-variable-fetcher
  "Given an `analyzer` instance created with [[make-analyzer]], returns a function
  of no arguments that, when called, will return the analyzer's current map of
  generated symbol => subexpression.

  Call the no-argument function returned by passing `analyzer`
  to [[initializer]] to reset the table.

  For example:

  ```clojure
  (def a (poly-analyzer))
  (def ea (expression-analyzer a))

  (def get-tables (auxiliary-variable-fetcher a))
  (def reset-tables! (initializer a))

  (ea '(+ x x x (sin x) (sin x)))
  ;;=> (+ (* 3 x) (* 2 -s-0000000000000000))

  (get-tables)
  ;;=> {'-s-0000000000000000 '(sin x)}

  (reset-tables!)
  (get-tables)
  ;;=> {}
  ```"
  [analyzer]
  (:get-var->expr analyzer))
