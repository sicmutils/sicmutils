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

(ns sicmutils.numerical.compile
  (:require [clojure.walk :refer [postwalk-replace]]
            [sicmutils
             [structure :as struct]
             [generic :as g]]
            [clojure.walk :as w]
            [clojure.tools.logging :as log])
  (:import (com.google.common.base Stopwatch)))

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

(defn- construct-state-function-exp
  "Given a state model (a structure which is in the domain and range
  of the function) and its body, produce a function of the flattened
  form of the argument structure as a sequence.
  FIXME: give an example here, since nobody could figure out what's
  going on just by reading this"
  [generic-parameters state-model body]
  `(fn [~(into [] (concat (-> state-model flatten) generic-parameters))]
     ~(postwalk-replace compiled-function-whitelist body)))

(defn extract-common-subexpressions
  "Given an S-expression, return a vector of subexpressions which occur
  more than once within it. (Here, symbols and other primitive elements
  do not count as subexpressions.)"
  [x]
  (let [cs (atom {})]
    ;; cs maps subexpressions to the number of times we have seen the
    ;; expression.
    (w/postwalk (fn [e]
                  (when (seq? e)
                    (swap! cs update-in [e] (fnil inc 0)))
                  e)
                x)
    (into [] (for [[k v] @cs :when (> v 1)] k))))

(defn ^:private initialize-cs-variables
  "Given a list of pairs of (symbol, expression) construct a
  binding vector (this is just a one-level flattening of the
  input)."
  [syms]
  (reduce (fn [v [x sym]] (conj (conj v sym) x)) [] syms))

(defn ^:private cs-body
  [syms x]
  (w/postwalk (fn [e]
                (or (syms e) e))
              x))

(defn common-subexpression-elimination
  "Given an expression and a table of common subexpressions, create a
  let statement which assigns the subexpressions to the values of
  dummy variables generated for the purpose of holding these values;
  the body of the let statement will be x with the subexpressions
  replaced by the dummy variables."
  [x & {:keys [symbol-generator]
        :or {symbol-generator gensym}}]
  (let [cs (extract-common-subexpressions x)]
    (if (> (count cs) 0)
      (let [syms (into {} (for [x cs] [x (symbol-generator)]))]
        (log/info (format "common subexpression elimination: %d expressions" (count cs)))
        `(let ~(initialize-cs-variables syms)
           ~(cs-body syms x)))
      x)))

(defn- compile-state-function2
  [f parameters initial-state]
  (let [sw (Stopwatch/createStarted)
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
    (log/info "compiled state function in" (str sw))
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

(defn- construct-univariate-function-exp
  [x body]
  `(fn [~x] ~(postwalk-replace compiled-function-whitelist body)))

(defn- compile-univariate-function2
  [f]
  (let [sw (Stopwatch/createStarted)
        var (gensym 'x)
        compiled-function (->> var
                               f
                               g/simplify
                               common-subexpression-elimination
                               (construct-univariate-function-exp var)
                               eval)]
    (log/info "compiled univariate function in" (str sw))
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
