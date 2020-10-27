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
  "This namespace compiles generic functions down into fast, native functions."
  (:require #?(:cljs [goog.string :refer [format]])
            [clojure.set :as set]
            [clojure.walk :as w]
            [sci.core :as sci]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.structure :as struct]
            [sicmutils.util.stopwatch :as us]
            [taoensso.timbre :as log]))

(def ^:private compiled-fn-whitelist
  {'up struct/up
   'down struct/down
   '+ +
   '- -
   '* *
   '/ /
   'cos #(Math/cos %)
   'sin #(Math/sin %)
   'tan #(Math/tan %)
   'acos #(Math/acos %)
   'asin #(Math/asin %)
   'atan #(Math/atan %)
   'expt #(Math/pow %1 %2)
   'sqrt #(Math/sqrt %)
   'abs (fn [^double x] (Math/abs x))
   'log #(Math/log %)
   'exp #(Math/exp %)})

(def ^:private fn-cache (atom {}))

;; ## Function Compilation

(def ^:dynamic *mode* :sci)

(def ^:private sci-context
  (sci/init
   {:bindings compiled-fn-whitelist}))

;; ### State Functions

(defn- compile-state-native
  "Given a state model (a structure which is in the domain and range
  of the function) and its body, produce a function of the flattened
  form of the argument structure as a sequence.

  FIXME: give an example here, since nobody could figure out what's
  going on just by reading this"
  [params state-model body]
  (eval
   `(fn [~(into [] (flatten state-model))
        ~(into [] params)]
      ~(w/postwalk-replace compiled-fn-whitelist body))))

(defn- compile-state-sci [params state-model body]
  (sci/eval-form (sci/fork sci-context)
                 `(fn [~(into [] (flatten state-model))
                      ~(into [] params)]
                    ~body)))

;; ### Non-State Functions

(defn- compile-native [x body]
  (let [body (w/postwalk-replace compiled-fn-whitelist body)]
    (eval `(fn [~x] ~body))))

(defn- compile-sci [x body]
  (sci/eval-form (sci/fork sci-context)
                 `(fn [~x] ~body)))

(defn- discard-unreferenced-variables
  [expression var->expr continue]
  (let [subexpr-vars   (into #{} (keys var->expr))
        variables-in-x (x/variables-in expression)]
    (loop [variables-required #{}
           new-variables (set/intersection subexpr-vars variables-in-x)]
      (if (empty? new-variables)
        ;; we're done. prune and sort the variable list for the consumer.
        (continue
         expression
         (sort-by first (filter #(variables-required (first %)) var->expr)))
        ;; not yet. We found new variables in the last pass. See if the
        ;; expressions they refer to flush out variables that we haven't seen.
        (recur (set/union variables-required new-variables)
               (let [new-vs (into #{} (mapcat #(x/variables-in (var->expr %)) new-variables))]
                 (set/difference (set/intersection new-vs subexpr-vars) new-variables)))))))

(defn extract-common-subexpressions
  "Considers an S-expression from the point of view of optimizing its evaluation
  by isolating common subexpressions into auxiliary variables. The continuation
  is called with two arguments: a new equivalent expression with possibly some
  subexpressions replaced by new variables (delivered by the supplied generator)
  and a seq of pairs of [aux variable, subexpression] used to reconstitute the
  value.

  If `:deterministic? true` is supplied, the function will assign aux variables
  by sorting the string representations of each term before assignment.
  Otherwise, the nondeterministic order of hash maps inside this function won't
  guarantee a consistent variable naming convention in the returned function.
  For tests, set `:deterministic? true`."
  [expression symbol-generator continue & {:keys [deterministic?]}]
  (let [pairs (if deterministic?
                (partial sort-by (comp str vec first))
                identity)]
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
        (let [new-syms (into {} (for [[k v] (pairs @cs) :when (> v 1)] [k (symbol-generator)]))]
          (if (empty? new-syms)
            (discard-unreferenced-variables x
                                            (into {} (for [[expr var] expr-to-var] [var expr]))
                                            continue)
            (let [joint-syms (into expr-to-var new-syms)]
              (recur (w/postwalk-replace joint-syms x) joint-syms))))))))

(defn ^:private initialize-cs-variables
  "Given a list of pairs of (symbol, expression) construct a
  binding vector (this is just a one-level flattening of the
  input)."
  [syms]
  (reduce (fn [v [sym x]] (conj (conj v sym) x)) [] syms))

(defn common-subexpression-elimination
  "Given an expression and a table of common subexpressions, create a let
  statement which assigns the subexpressions to the values of dummy variables
  generated for the purpose of holding these values; the body of the let
  statement will be x with the subexpressions replaced by the dummy variables.

  If `:deterministic? true` is supplied, the function will assign variable names
  by sorting the string representations of each term before assignment.
  Otherwise, the nondeterministic order of hash maps inside this function won't
  guarantee a consistent variable naming convention in the returned function.
  For tests, set `:deterministic? true`."
  [x & {:keys [symbol-generator deterministic?]
        :or {symbol-generator gensym}}]
  (extract-common-subexpressions
   x
   symbol-generator
   (fn [new-expression new-vars]
     (if (> (count new-vars) 0)
       (do
         (log/info (format "common subexpression elimination: %d expressions" (count new-vars)))
         `(let ~(initialize-cs-variables new-vars) ~new-expression))
       new-expression))
   :deterministic? deterministic?))

(defn- compile-state-fn* [f params initial-state]
  (let [sw             (us/stopwatch)
        mode           *mode*
        generic-params (for [_ params] (gensym 'p))
        generic-state  (struct/mapr (fn [_] (gensym 'y)) initial-state)
        g              (apply f generic-params)
        body           (common-subexpression-elimination
                        (g/simplify (g generic-state)))
        compiler       (if (= mode :native)
                         compile-state-native
                         compile-state-sci)
        compiled-fn    (compiler generic-params generic-state body)]
    (log/info "compiled state function in" (us/repr sw))
    compiled-fn))

(defn compile-state-fn
  [f params initial-state]
  (if-let [cached (@fn-cache f)]
    (do
      (log/info "compiled state function cache hit")
      cached)
    (let [compiled (compile-state-fn* f params initial-state)]
      (swap! fn-cache assoc f compiled)
      compiled)))

(defn- compile-univariate-fn*
  [f]
  (let [sw       (us/stopwatch)
        mode     *mode*
        var      (gensym 'x)
        body     (common-subexpression-elimination
                  (g/simplify (f var)))
        compiled (if (= mode :native)
                   (compile-native var body)
                   (compile-sci var body))]
    (log/info "compiled univariate function in " (us/repr sw) " with mode " mode)
    compiled))

(defn compile-univariate-fn [f]
  (if-let [cached (@fn-cache f)]
    (do
      (log/info "compiled univariate function cache hit")
      cached)
    (let [compiled (compile-univariate-fn* f)]
      (swap! fn-cache assoc f compiled)
      compiled)))
