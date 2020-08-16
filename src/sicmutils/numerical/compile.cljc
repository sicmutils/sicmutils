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
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.numerical.compile
  (:require #?(:cljs [goog.string :refer [format]])
            [clojure.set :as set]
            [clojure.walk :as w]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.structure :as struct]
            [sicmutils.util.stopwatch :as us]
            [taoensso.timbre :as log]))

(def ^:private compiled-function-whitelist {'up `struct/up
                                            'down `struct/down
                                            'cos #(Math/cos %)
                                            'sin #(Math/sin %)
                                            'tan #(Math/tan %)
                                            '+ +
                                            '- -
                                            '* *
                                            '/ /
                                            'expt #(Math/pow %1 %2)
                                            'sqrt #(Math/sqrt %)})

(def ^:private compiled-function-cache (atom {}))

(defn ^:private construct-state-function-exp
  "Given a state model (a structure which is in the domain and range
  of the function) and its body, produce a function of the flattened
  form of the argument structure as a sequence.
  FIXME: give an example here, since nobody could figure out what's
  going on just by reading this"
  [generic-parameters state-model body]
  `(fn [~(into [] (concat (-> state-model flatten) generic-parameters))]
     ~(w/postwalk-replace compiled-function-whitelist body)))

(defn ^:private discard-unreferenced-variables
  [expression var-to-expr continue]
  (let [subexpr-vars (into #{} (keys var-to-expr))
        variables-in-x (x/variables-in expression)]
    (loop [variables-required #{}
           new-variables (set/intersection subexpr-vars variables-in-x)]
      (if (empty? new-variables)
        ;; we're done. prune and sort the variable list for the consumer.
        (continue
          expression
          (sort-by first (filter #(variables-required (first %)) var-to-expr)))
        ;; not yet. We found new variables in the last pass. See if the
        ;; expressions they refer to flush out variables that we haven't seen.
        (recur (set/union variables-required new-variables)
               (let [new-vs (into #{} (mapcat #(x/variables-in (var-to-expr %)) new-variables))]
                 (set/difference (set/intersection new-vs subexpr-vars) new-variables)))))))

(defn extract-common-subexpressions
  "Considers an S-expression from the point of view of optimizing
  its evaluation by isolating common subexpressions into auxiliary
  variables. The continuation is called with two arguments: a
  new equivalent expression with possibly some subexpressions replaced
  by new variables (delivered by the supplied generator) and a seq
  of pairs of [aux variable, subexpression] used to reconstitute the
  value."
  [expression symbol-generator continue]
  (loop [x expression
         expr-to-var {}]
    (let [cs (atom {})
          increment (fnil inc 0)]
      ;; cs maps subexpressions to the number of times we have seen the
      ;; expression.
      (w/postwalk (fn [e]
                    (when (and (seq? e) (not (expr-to-var e)))
                      (swap! cs update e increment))
                    e)
                  x)

      (let [new-syms (into {} (for [[k v] @cs :when (> v 1)] [k (symbol-generator)]))]
        (if (empty? new-syms)
          (discard-unreferenced-variables x
                                          (into {} (for [[expr var] expr-to-var] [var expr]))
                                          continue)
          (let [joint-syms (into expr-to-var new-syms)]
            (recur (w/postwalk-replace joint-syms x) joint-syms)))))))

(defn ^:private initialize-cs-variables
  "Given a list of pairs of (symbol, expression) construct a
  binding vector (this is just a one-level flattening of the
  input)."
  [syms]
  (reduce (fn [v [sym x]] (conj (conj v sym) x)) [] syms))

(defn common-subexpression-elimination
  "Given an expression and a table of common subexpressions, create a
  let statement which assigns the subexpressions to the values of
  dummy variables generated for the purpose of holding these values;
  the body of the let statement will be x with the subexpressions
  replaced by the dummy variables."
  [x & {:keys [symbol-generator]
        :or {symbol-generator gensym}}]
  (extract-common-subexpressions
    x
    symbol-generator
    (fn [new-expression new-vars]
      (if (> (count new-vars) 0)
        (do
          (log/info (format "common subexpression elimination: %d expressions" (count new-vars)))
          `(let ~(initialize-cs-variables new-vars) ~new-expression))
        new-expression))))

(defn ^:private compile-state-function2
  [f parameters initial-state]
  (let [sw (us/stopwatch)
        generic-parameters (for [_ parameters] (gensym 'p))
        generic-initial-state (struct/mapr (fn [_] (gensym 'y)) initial-state)
        g (apply f generic-parameters)
        compiled-function (->> generic-initial-state
                               g
                               g/simplify
                               common-subexpression-elimination
                               (construct-state-function-exp
                                generic-parameters
                                generic-initial-state)
                               eval)]
    (log/info "compiled state function in" (us/repr sw))
    compiled-function))

(defn compile-state-function
  [f parameters initial-state]
  (if-let [cached (@compiled-function-cache f)]
    (do
      (log/info "compiled state function cache hit")
      cached)
    (let [compiled-function (compile-state-function2 f parameters initial-state)]
      (swap! compiled-function-cache assoc f compiled-function)
      compiled-function)))

(defn ^:private construct-univariate-function-exp
  [x body]
  `(fn [~x] ~(w/postwalk-replace compiled-function-whitelist body)))

(defn ^:private compile-univariate-function2
  [f]
  (let [sw (us/stopwatch)
        var (gensym 'x)
        compiled-function (->> var
                               f
                               g/simplify
                               common-subexpression-elimination
                               (construct-univariate-function-exp var)
                               eval)]
    (log/info "compiled univariate function in" (us/repr sw))
    compiled-function))

(defn compile-univariate-function
  [f]
  (if-let [cached (@compiled-function-cache f)]
    (do
      (log/info "compiled univariate function cache hit")
      cached)
    (let [compiled-function (compile-univariate-function2 f)]
      (swap! compiled-function-cache assoc f compiled-function)
      compiled-function)))
