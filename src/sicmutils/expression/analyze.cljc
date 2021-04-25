;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with thixs code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.expression.analyze
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
  (:require [sicmutils.expression :as x]
            [sicmutils.numsymb :as sym]
            [sicmutils.value :as v]))

(defn- make-vcompare
  "Returns
  a [Comparator](https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html)
  function taking account of the input variable set `var-set` in the following
  way:

  If both inputs to the comparator are in `var-set,` or both are not, then the
  results are as core compare would return. But if one is in `var-set` and the
  other is not, then the other will always compare greater.

  In this way, expressions produced by the simplifier will have simple variables
  sorted earlier than expressions involving those variables."
  [var-set]
  (fn [v w]
    (cond
      (var-set v) (if (var-set w) (compare v w) -1)
      (var-set w) 1
      :else (compare v w))))

(defprotocol ICanonicalize
  "[[ICanonicalize]] captures the methods exposed by a SICMUtils analyzer backend."
  (expression->
    [analyzer x continue]
    [analyzer x continue compare-fn]
    "Invokes `continue` with

  - A version of `x` converted to the canonical form represented by `analyzer`
  - A (sorted) sequence of variables found in `x`

  `compare-fn` is used to sort variables. Defaults to `clojure.core/compare`.")

  (->expression [analyzer b variables]
    "Convert a canonical form `b` back to S-expression form.

    Each [[ICanonicalize]] instance uses `variables` in different ways. The
    `variables` sequence is typically obtained from the continuation invoked
    by [[expression->]], so these functions are complementary.")

  (known-operation? [analyzer x]
    "Returns true if the symbolic operation `x` is considered fundamental by
    `analyzer`, false otherwise.")

  (new-variables [analyzer n]
    "Returns `n` new symbolic variables used to represent values outside the
     domain of computation for `analyzer`"))

(defn make-analyzer
  "Make-analyzer takes an analyzer `backend` (which implements [[ICanonicalize]])
  and provides the apparatus necessary to prepare expressions for analysis by
  replacing subexpressions formed from operations unknown to the analyzer with
  generated symbols, and backsubstituting after analysis is complete.

  For example, in the case of polynomial canonical form, we would replace a
  subexpression like `(sin x)` with a gensym, before entry, since the sine
  operation is not available to the polynomial canonicalizer, and restore it
  afterwards."
  [backend symbol-generator]
  (let [ref #?(:clj ref :cljs atom)
        alter #?(:clj alter :cljs swap!)
        expr->var (ref {})
        var->expr (ref {})]
    (fn [expr]
      (let [vless? (make-vcompare (x/variables-in expr))]
        (letfn [(analyze [expr]
                  (if (and (sequential? expr)
                           (not (= (first expr) 'quote)))
                    (let [analyzed-expr (map analyze expr)]
                      (if (and (known-operation? backend (sym/operator analyzed-expr))
                               (or (not (= 'expt (sym/operator analyzed-expr)))
                                   (integer? (second (sym/operands analyzed-expr)))))
                        analyzed-expr
                        (if-let [existing-expr (@expr->var analyzed-expr)]
                          existing-expr
                          (new-kernels analyzed-expr))))
                    expr))
                (new-kernels [expr]
                  ;; use doall to force the variable-binding side effects of
                  ;; base-simplify
                  (let [simplified-expr (doall (map base-simplify expr))]
                    (if-let [v (sym/symbolic-operator (sym/operator simplified-expr))]
                      (let [w (apply v (sym/operands simplified-expr))]
                        (if (and (sequential? w)
                                 (= (sym/operator w) (sym/operator simplified-expr)))
                          (add-symbols! w)
                          (analyze w)))
                      (add-symbols! simplified-expr))))

                (add-symbols! [expr]
                  (add-symbol!
                   ;; FORCE the side effect of binding all symbols.
                   (doall (map add-symbol! expr))))

                (add-symbol! [expr]
                  (let [expr-k (v/freeze expr)]
                    (if (and (sequential? expr)
                             (not= (first expr) 'quote))
                      ;; in a transaction, probe and maybe update the
                      ;; expr->var->expr maps.
                      ;;
                      ;; NOTE: Make sure to use the FROZEN version of the
                      ;; expression as the key.
                      (#?(:clj dosync :cljs identity)
                       (if-let [existing-expr (@expr->var expr-k)]
                         existing-expr
                         (let [var (symbol-generator)]
                           (alter expr->var assoc expr-k var)
                           (alter var->expr assoc var expr)
                           var)))
                      expr)))

                (backsubstitute [expr]
                  (cond (sequential? expr) (map backsubstitute expr)
                        (symbol? expr) (if-let [w (@var->expr (v/freeze expr))]
                                         (backsubstitute w)
                                         expr)
                        :else expr))

                (base-simplify [expr]
                  ;; TODO NOTE that this was so important!
                  (let [cont #(->expression backend %1 %2)]
                    (expression-> backend expr cont vless?)))]
          (-> expr analyze base-simplify backsubstitute))))))

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
