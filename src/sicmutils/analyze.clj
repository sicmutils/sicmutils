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
             [numsymb :as sym]]))

(defn analyzer
  [symbol-generator expr-> ->expr known-operations]
  (let [expr->var (ref {})
        var->expr (ref {})]
    (fn [expr]
      (letfn [(analyze [expr]
                (if (and (sequential? expr)
                         (not (= (first expr) 'quote)))
                  (let [analyzed-expr (map analyze expr)]
                    (if (and (known-operations (sym/operator analyzed-expr))
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
                  (dosync ; in a transaction, probe and maybe update the expr->var->expr maps
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
              (base-simplify [expr] (expr-> expr ->expr))]
        (-> expr analyze base-simplify backsubstitute)))))

(defn monotonic-symbol-generator
  "Returns a function which generates a sequence of symbols with the given
  prefix with the property that later symbols will sort after earlier symbols.
  This is important for the stability of the simplifier. (If we just used
  gensym, then a temporary symbol like G__1000 will sort earlier than G__999,
  and this will happen at unpredictable times.)"
  [prefix]
  (let [count (atom -1)]
    (fn [] (symbol (format "%s%016x" prefix (swap! count inc))))))
