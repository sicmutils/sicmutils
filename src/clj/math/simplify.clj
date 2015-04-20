;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.simplify
  (:require [clojure.walk :refer [postwalk]]
            [clojure.pprint :as pp]
            [math.numsymb :as sym]
            [math.polynomial :as poly]
            [math.value :as v]
            [math.generic :as g]
            [math.expression :as x]
            [math.rules :as rules]
            [pattern.rule :as rule])
  (:import (math.expression Expression))
  )

(defn symbol-generator
  "Returns a function which generates a sequence of symbols
  staring with the initial prefix."
  [fmt]
  (let [i (atom -1)]
    #(->> (swap! i inc) (format fmt) symbol)))

(defn- map-with-state
  "Maps f over coll while maintaining state. The function
  f is called with [state, v] for each value v in col, and
  is expected to return a pair containing the new state and
  (f v). The result is a pair with the final state and
  the sequence of the values of (f v)."
  [f initial-state coll]
  (reduce
    (fn [[state acc] val]
      (let [[new-state f-val] (f state val)]
        [new-state (conj acc f-val)]))
    [initial-state []]
    coll))

(defn analyzer
  [symbol-generator expr-> ->expr known-operations]
  ;; TODO: we haven't recorded variable order, so expressions can get scrambled
  ;; as a result of sorting by generated-symbol-name. The solution is to communicate
  ;; the existing order of subexpressions to the polynomial simplifier.
  ;; TODO: we went through a lot of trouble to thread expr-map through all of
  ;; these functions, but we notice that in 1.8 (volatile!) is coming to
  ;; Clojure by way of stateful transducers, and I'd like to use that technique
  ;; instead.
  (fn [expr]
    (letfn [(simplify-expression
              [expr]
              (let [[expr-map analyzed-expr] (analyze {} expr)]
                (->> analyzed-expr base-simplify (backsubstitute expr-map))))
            (analyze
              [expr-map expr]
              (if (and (sequential? expr)
                       (not (= (first expr) 'quote)))
                (let [[expr-map analyzed-expr] (map-with-state analyze expr-map expr)]
                  ;; at this point all subexpressions are canonical TODO: is this true?
                  (if (and (known-operations (sym/operator analyzed-expr))
                           (or (not (= 'expt (sym/operator analyzed-expr)))
                               (integer? (second (sym/operands analyzed-expr)))))
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
                  (add-symbols expr-map simplified-expr))))
            (add-symbols
              [expr-map expr]
              (apply add-symbol (map-with-state add-symbol expr-map expr)))
            (add-symbol
              [expr-map expr]
              (if (and (sequential? expr)
                       (not (= (first expr) 'quote)))
                (if-let [existing-expr (expr-map expr)]
                  [expr-map existing-expr]
                  (let [newvar (symbol-generator)]
                    [(conj expr-map [expr newvar]) newvar]))
                [expr-map expr]))
            (backsubstitute
              [expr-map expr]
              (let [mapx (inverse-map expr-map)
                    bsub (fn bsub [v]
                           (cond (sequential? v) (map bsub v)
                                 (symbol? v) (let [w (mapx v)]
                                               (if w (bsub w) v))
                                 :else v))]
                (bsub expr)))
            (base-simplify
              [expr]
              (expr-> expr ->expr))
            (inverse-map
              [m]
              (into {} (for [[k v] m] [v k])))
            ]
      (simplify-expression expr))))

(def ^:private poly-analyzer
  (analyzer (symbol-generator "-s-%05d") poly/expression-> poly/->expression poly/operators-known))

(def ^:private simplify-and-flatten poly-analyzer)

(defn- simplify-until-stable
  [rule-simplify canonicalize]
  (fn simplify [expression]
    (let [new-expression (rule-simplify expression)]
      (if (= expression new-expression)
        expression
        (let [canonicalized-expression (canonicalize new-expression)]
          (cond (= canonicalized-expression expression) expression
                (g/zero? (poly-analyzer `(- ~expression ~canonicalized-expression))) canonicalized-expression
                :else (simplify canonicalized-expression)))))))

(defn simplify-and-canonicalize
  [rule-simplify canonicalize]
  (fn simplify [expression]
    (let [new-expression (rule-simplify expression)]
      (if (= expression new-expression)
        expression
        (canonicalize new-expression)))))

(def ^:private sin-sq->cos-sq-simplifier
  (simplify-and-canonicalize rules/sin-sq->cos-sq simplify-and-flatten))
(def ^:private sincos-simplifier
  (simplify-and-canonicalize rules/sincos-flush-ones simplify-and-flatten))
(def ^:private square-root-simplifier
  (simplify-and-canonicalize rules/simplify-square-roots simplify-and-flatten))

;; looks like we might have the modules inverted: rulesets will need some functions from the
;; simplification library, so this one has to go here. Not ideal the way we have split things
;; up, but at least things are beginning to simplify adequately.

(def sincos-cleanup
  (let [at-least-two? #(and (number? %) (>= % 2))]
    (simplify-and-canonicalize
      (rule/rule-simplifier
        (rule/ruleset
          (+ (:?? a1) (:? a) (:?? a2) (* (:?? b1) (expt (cos (:? x)) (:? n at-least-two?)) (:?? b2)) (:?? a3))
          #(g/zero? (poly-analyzer `(~'+ (~'* ~@(% 'b1) ~@(% 'b2) (~'expt (~'cos ~(% 'x)) ~(- (% 'n) 2))) ~(% 'a))))
          (+ (:?? a1) (:?? a2) (:?? a3) (* (:? a) (expt (sin (:? x)) 2)))

          (+ (:?? a1) (* (:?? b1) (expt (cos (:? x)) (:? n at-least-two?)) (:?? b2)) (:?? a2) (:? a) (:?? a3))
          #(g/zero? (poly-analyzer `(~'+ (~'* ~@(% 'b1) ~@(% 'b2) (~'expt (~'cos ~(% 'x)) ~(- (% 'n) 2))) ~(% 'a))))
          (+ (:?? a1) (:?? a2) (:?? a3) (* (:? a) (expt (sin (:? x)) 2)))))
      simplify-and-flatten)))

(def simplify-expression-1
  #(-> %
       simplify-and-flatten
       sin-sq->cos-sq-simplifier
       sincos-simplifier
       sincos-cleanup
       square-root-simplifier
       rules/divide-numbers-through
       ;rules/cancel-within-fractions
       simplify-and-flatten))

(def simplify-expression (simplify-until-stable simplify-expression-1 simplify-and-flatten))



(defn- fixup-symbols
  [xs]
  (postwalk (fn [x] (cond (symbol? x) (let [sym-ns (namespace x)
                                            sym-name (name x)]
                                        ;; kind of a hack, but we don't want a circular dependency
                                        ;; here.
                                        (if (and (= sym-ns "math.generic")
                                                 (= sym-name "divide"))
                                          '/
                                          (symbol sym-name)))
                          :else x)) xs))

(defmethod g/simplify :math.expression/numerical-expression
  [a]
  (-> a v/freeze simplify-expression fixup-symbols))

(defmethod g/simplify :default [a] a)
(defmethod g/simplify clojure.lang.Var [a] (-> a meta :name))

(def print-expression #(-> % g/simplify pp/pprint))
(def pe print-expression)
