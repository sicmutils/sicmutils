;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with thixs code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.analyze
  (:require [sicmutils.expression :as x]
            [sicmutils.numsymb :as sym]))

(defn ^:private make-vcompare
  "Returns a Comparator function taking account of the input variable set in the
  following way: if both inputs to the comparator are in var-set, or both are
  not, then the results are as core compare would return. But if one is in
  var-set and the other is not, then the other will always compare greater. In
  this way, expressions produced by the simplifier will have simple variables
  sorted earlier than expressions involving those variables."
  [var-set]
  (fn [v w]
    (cond
      (var-set v) (if (var-set w) (compare v w) -1)
      (var-set w) 1
      :else (compare v w))))

(defprotocol ICanonicalize
  "ICanonicalize captures the methods exposed by a scmutils analyzer backend.
  Analyzer backends find canonical forms of inputs over limited vocabularies of
  operations. For example, a polynomial backend will expose operations like
  addition, subtraction, multiplication, and exponentiation by positive
  integers (but not division). An expression containing only these operations
  and symbols can then be converted to and from a polynomial canonical form,
  which in this example would have the effect of grouping like terms; a
  rational function backend would include the division operation and be capable
  of cancellation. Canonicalizing an expression with respect to an analyzer is
  therefore effected by a round-trip to and from the canonical form."
  (expression->
    [this x continue]
    [this x continue compare-variable]
    "Convert an expression to the canonical form represented by this analyzer,
    and invoke continue with the canonicalized input and a (sorted) sequence of
    variables found in the original expression.")
  (->expression [this b variables]
    "Convert a canonical form back to S-expression form. The variable list is
    typically obtained from the continuation invoked by expression->, so these
    functions are complementary.")
  (known-operation? [this x]
    "A function deciding if an operation is considered fundamental by the
     canonical form")
  (new-variables [this n]
    "Produce n new variables to represent values outside the domain of
     computation for this analyzer"))

(defn make-analyzer
  "Make-analyzer takes an analyzer backend (which implements ICanonicalize) and
  provides the apparatus necessary to prepare expressions for analysis by
  replacing subexpressions formed from operations unknown to the analyzer with
  generated symbols, and backsubstituting after analysis is complete. (For
  example, in the case of polynomial canonical form, we would replace a
  subexpression like (sin x) with a gensym, before entry, since the sine
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
                  (if (and (sequential? expr)
                           (not (= (first expr) 'quote)))
                    ;; in a transaction, probe and maybe update the expr->var->expr maps
                    (#?(:clj dosync :cljs identity)
                     (if-let [existing-expr (@expr->var expr)]
                       existing-expr
                       (let [var (symbol-generator)]
                         (alter expr->var assoc expr var)
                         (alter var->expr assoc var expr)
                         var)))
                    expr))

                (backsubstitute [expr]
                  (cond (sequential? expr) (map backsubstitute expr)
                        (symbol? expr) (if-let [w (@var->expr expr)]
                                         (backsubstitute w)
                                         expr)
                        :else expr))
                (base-simplify [expr]
                  (expression-> backend expr #(->expression backend %1 %2) vless?))]
          (-> expr analyze base-simplify backsubstitute))))))

(defn monotonic-symbol-generator
  "Returns a function which generates a sequence of symbols with the given
  prefix with the property that later symbols will sort after earlier symbols.
  This is important for the stability of the simplifier. (If we just used
  gensym, then a temporary symbol like G__1000 will sort earlier than G__999,
  and this will happen at unpredictable times.)"
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
