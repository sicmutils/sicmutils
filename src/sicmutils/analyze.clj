;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.analyze
  (:require [sicmutils
             [numsymb :as sym]]
            [sicmutils.expression :as x])
  (:import (java.util Comparator)))

(defn ^:private make-vcompare
  [var-set]
  "Returns a Comparator taking account of the input variable set in the following way:
  if both inputs to the comparator are in var-set, or both are not, then the results
  are as core compare would return. But if one is in var-set and the other is not, then
  the other will always compare greater. In this way, expressions produced by the simplifier
  will have simple variables sorted earlier than expressions involving those variables."
  (reify Comparator
    (compare [_ v w]
      (cond
        (var-set v) (if (var-set w) (compare v w) -1)
        (var-set w) 1
        :else (compare v w)))))

(defprotocol IAnalyze
  (expression-> [this x ->x] [this x ->x compare-variable])
  (->expression [this b variables])
  (known-operation? [this x]))

(defn analyzer
  [a symbol-generator]
  (let [expr->var (ref {})
        var->expr (ref {})]
    (fn [expr]
      (let [vless? (make-vcompare (x/variables-in expr))]
        (letfn [(analyze [expr]
                  (if (and (sequential? expr)
                           (not (= (first expr) 'quote)))
                    (let [analyzed-expr (map analyze expr)]
                      (if (and (known-operation? a (sym/operator analyzed-expr))
                               (or (not (= 'expt (sym/operator analyzed-expr)))
                                   (integer? (second (sym/operands analyzed-expr)))))
                        analyzed-expr
                        (if-let [existing-expr (@expr->var analyzed-expr)]
                          existing-expr
                          (new-kernels analyzed-expr))))
                    expr))
                (new-kernels [expr]
                  ;; use doall to force the variable-binding side effects of base-simplify
                  (let [simplified-expr (doall (map base-simplify expr))]
                    (if-let [v (sym/symbolic-operator (sym/operator simplified-expr))]
                      (let [w (apply v (sym/operands simplified-expr))]
                        (if (and (sequential? w)
                                 (= (sym/operator w) (sym/operator simplified-expr)))
                          (add-symbols! w)
                          (analyze w)))
                      (add-symbols! simplified-expr))))
                (add-symbols! [expr]
                  (->> expr (map add-symbol!) add-symbol!))
                (add-symbol! [expr]
                  (if (and (sequential? expr)
                           (not (= (first expr) 'quote)))
                    (dosync                                 ; in a transaction, probe and maybe update the expr->var->expr maps
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
                  (expression-> a expr #(->expression a %1 %2) vless?))]
          (-> expr analyze base-simplify backsubstitute))))))

(defn monotonic-symbol-generator
  "Returns a function which generates a sequence of symbols with the given
  prefix with the property that later symbols will sort after earlier symbols.
  This is important for the stability of the simplifier. (If we just used
  gensym, then a temporary symbol like G__1000 will sort earlier than G__999,
  and this will happen at unpredictable times.)"
  [prefix]
  (let [count (atom -1)]
    (fn [] (symbol (format "%s%016x" prefix (swap! count inc))))))
