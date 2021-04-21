;;
;; Copyright © 2021 Sam Ritchie.
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
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.rule
  "Functions for writing and applying term-rewriting rules using the primitives
  defined in [[pattern.match]]."
  (:refer-clojure :exclude [replace while])
  (:require #?(:clj [potemkin :refer [import-def]])
            [pattern.consequence :as c]
            [pattern.match :as m]
            [pattern.syntax :as ps]
            [sicmutils.util :as u]
            #?(:cljs
               [sicmutils.util.def :as util.def
                :refer-macros [import-def]])))

;; Adapted from Scheme Rules. TODO update!

;; A rule is a pattern, a predicate and a handler. The pattern determines the
;; applicability of the rule, and the match bindings that enable said
;; applicability, and the handler can compute an arbitrary value from them. Once
;; constructed, a rule is a procedure that accepts a datum, and returns either
;; the datum if the pattern doesn't match or the value of the handler when
;; applied to the dictionary if it does. The input datum is used as the sentinel
;; value for failure because in the context of term rewriting, succeeding with
;; the input as the answer is equivalent to failing.
;;
;; TODO we are NOT doing this now, we are making it explicit.

;; The handler can reject a putative match by returning #f, which causes
;; backtracking into the matcher, and may cause the handler to be called again
;; with different bindings. If the handler always returns #f, the rule may fail
;; even though its pattern matched.
;;
;; ## Alexey Inspired Code
;;
;; Functional version of rules, no macro.
;;
;; To allow the handler to cause its rule to succeed with #f, we provide a
;; custom data structure in which that #f can be wrapped so it looks like a true
;; value to the matcher combinators. It is then unwrapped by `interpret-success'
;; in the rule procedure.

;; Two predicates for rules.
(def => (constantly true))
(def !=> (constantly false))

;; This is how we record a MATCH failure.

(import-def c/succeed)
(import-def m/failure)
(import-def m/failed?)

(defn pattern*
  "Function version of pattern builder."
  ([form]
   (m/matcher form))
  ([form pred]
   (if pred
     (m/matcher form pred)
     (m/matcher form))))

(defmacro pattern
  "Generates a pattern matcher from a template. TODO describe the language here!"
  ([form]
   `(pattern*
     ~(ps/compile-pattern form)))
  ([form pred]
   `(pattern*
     ~(ps/compile-pattern form)
     ~@(when pred [pred]))))

(defmacro consequence
  "Generates a rule handler from a skeleton form. TODO describe the language
  here!"
  [form]
  (c/compile-skeleton form))

(defn rule*
  "Function version of [[rule]].

  Accepts a `match` pattern and a handler...

  - if the match fails, returns `failure`.
  - if the handler returns `nil` or `false`, returns `failure`.
  - if you WANT to return `nil` or `false` explicitly, use [[return]].

  Basically we have an [[match/matcher]], and then the `handler` argument gets
  fed the dictionary of matches if the match succeeds (might be empty!)

  TODO use [[consequence]] if you want."
  [match handler]
  (let [match (if (fn? match)
                match
                (m/matcher match))]
    (fn [data]
      (let [result (match data)]
        (if (m/failed? result)
          m/failure
          (c/unwrap
           (or (handler result)
               m/failure)))))))

(defn- compile-rule
  "Rule takes a match pattern and substitution pattern, compiles each of these and
  returns a function which may be applied to a form and (optionally) a success
  continuation.

  The function will try to match the pattern and, if successful, _and_ the
  bindings satisfy the supplied predicate, will call the continuation with the
  result of the substitution.

  TODO note that in the two arg case, you give a function of the bindings, and
  that you can use [[consequence]] if you like.

  TODO NOTE that if the consequent-fn in the two arg case returns falsey, the
  whole thing fails."
  ([p consequent-fn]
   `(rule* (pattern ~p)
           ~consequent-fn))

  ([p pred skeleton]
   `(rule* (pattern ~p ~pred)
           (consequence ~skeleton))))

(defmacro rule
  ([pattern consequent-fn]
   (compile-rule pattern consequent-fn))
  ([pattern pred skeleton]
   (compile-rule pattern pred skeleton)))

;; ## Alexey Combinators, Term Rewriting
;;
;; Rule combinators
;;
;; Various patterns of rule application captured as combinators that take rules
;; and produce rules (to wit, procedures that accept one input and return the
;; result of transforming it, where returning the input itself signals match
;; failure).

(defn pass [data] data)

(defn fail [_] failure)

(defn return
  "Returns a rule that always replaces its input with `x`."
  [x]
  (fn [_] x))

(defn branch
  "Returns a rule that calls `succeed-r` with `(r data)` if successful, `fail-r`
  with `data` if failed."
  [r succeed-r fail-r]
  (fn [data]
    (let [result (r data)]
      (if (failed? result)
        (fail-r data)
        (succeed-r result)))))

(defn choice*
  "Apply several rules in series, returning the first success."
  [rules]
  (fn [data]
    (loop [rules rules]
      (if (empty? rules)
        failure
        (let [answer ((first rules) data)]
          (if (failed? answer)
            (recur (rest rules))
            answer))))))

(defn choice
  "Apply several rules in series, returning the first success."
  ([] fail)
  ([r] r)
  ([r & rs]
   (choice* (cons r rs))))

(defn pipe*
  "Apply several rules in series, threading the results. Takes a collection of
  rules."
  [rules]
  (fn [data]
    (reduce (fn [prev r]
              (let [result (r prev)]
                (if (failed? result)
                  (reduced failure)
                  result)))
            data
            rules)))

(defn pipe
  "Apply several rules in series, threading the results."
  ([] pass)
  ([r] r)
  ([r & rs]
   (pipe* (cons r rs))))

(defn n-times
  "Returns a rule that applies the rule `r` `n` times, failing out if any
  application fails."
  [n r]
  (pipe* (repeat n r)))

(defn attempt? [f]
  (::attempt? (meta f)))

(defn as-attempt [f]
  (vary-meta f assoc ::attempt? true))

(defn attempt
  "Returns a rule which will attempt `r` and return its input if `r` fails."
  [r]
  (if (attempt? r)
    r
    (as-attempt
     (choice r pass))))

(defn predicate
  "Returns a rule that will pass the data on unchanged if `(f data)` returns true,
  fails otherwise."
  [f]
  (fn [data]
    (if (f data)
      data
      failure)))

(defn guard
  "Build a strategy which applies `s` to `t` iff `p` is true for `t`."
  [f r]
  (attempt
   (pipe (predicate f) r)))

(defn iterated
  "Apply one rule repeatedly until it doesn't match anymore."
  [r]
  (fn [data]
    (let [result (r data)]
      (if (failed? result)
        data
        (recur result)))))

(defn while
  "Returns a new rule which repeatedly applies `r` until `f` returns `true`
  between two successive values."
  [f r]
  (fn rec [data]
    ((pipe (attempt r)
           (fn [data*]
             (if (f data data*)
               (rec data*)
               data*)))
     data)))

(defn until
  "Returns a new rule which repeatedly applies `r` until `f` returns `false`
  between two successive values."
  [f r]
  (fn [data]
    (let [data* ((attempt r) data)]
      (if (f data data*)
        data*
        (recur data*)))))

(defn fixed-point
  "Apply a rule until it returns its input."
  [r]
  (fn [data]
    (let [result (r data)]
      (if (= data result)
        data
        (recur result)))))

(defn trace
  "Returns a rule that calls `f` with its input and output values."
  ([r]
   (trace r prn))
  ([r f]
   (let [id (gensym "t_")]
     (fn [data]
       (f {:id id, :in data})
       (let [result (r data)]
         (f {:id id, :out result})
         result)))))

;; ## Expression Matchers
;;
;; TODO add trace, check for more matchers!!

(defn- try-subexpressions [the-rule expr]
  (cond (sequential? expr)
        (let [processed (map the-rule expr)]
          (if (= expr processed)
            expr
            (if (vector? expr)
              (vec processed)
              processed)))

        (map? expr)
        (let [processed (u/map-vals the-rule expr)]
          (if (= expr processed)
            expr
            processed))

        :else expr))

(defn bottom-up
  "Apply one rule to all subexpressions of the input, bottom-up."
  [the-rule]
  (let [r (attempt the-rule)]
    (fn rec [expression]
      (r (try-subexpressions rec expression)))))

(defn top-down
  "NOTE: Actually top down AND bottom up!"
  [the-rule]
  (let [r (attempt the-rule)]
    (fn rec [expr]
      (r (try-subexpressions rec (r expr))))))

(defn iterated-bottom-up
  "Iterate one rule to convergence on all subexpressions of the input,
  bottom up.

  Note that subexpressions of a result returned by one invocation of the rule
  may admit additional invocations, so we need to recur again after every
  successful transformation."
  [the-rule]
  (let [r   (attempt the-rule)
        rec (atom nil)]
    (letfn [(rec* [expr]
              (let [processed (try-subexpressions @rec expr)
                    answer (r processed)]
                (if (= answer processed)
                  answer
                  (@rec answer))))]
      (reset! rec (memoize rec*))
      @rec)))

(defn iterated-top-down
  "Iterate one rule to convergence on all subexpressions of the input, applying
  it on the way down as well as back up."
  [the-rule]
  (let [r   (attempt the-rule)
        rec (atom nil)]
    (letfn [(rec* [expr]
              (let [answer (r expr)]
                (if (= answer expr)
                  (let [processed (try-subexpressions @rec expr)
                        answer (r processed)]
                    (if (= answer processed)
                      answer
                      (@rec answer)))
                  (@rec answer))))]
      (reset! rec (memoize rec*))
      @rec)))

;; ## Our API
;;
;; The original, good stuff.

(defn ruleset*
  "Function version of ruleset. These really want to be rules that can fail."
  [& rules]
  (attempt
   (apply choice rules)))

(defmacro ruleset
  "Ruleset compiles rules, predicates and consequences (triplet-wise) into a
  function which acts like a single rule (as `rule` would produce) which acts by
  invoking the success continuation with the consequence of the first successful
  rule whose patterns match and satisfy the predicate. If no rules match, the
  failure continuation is invoked."
  [& patterns-and-consequences]
  {:pre (zero? (mod (count patterns-and-consequences) 3))}
  (let [rule-inputs (partition 3 patterns-and-consequences)
        rules       (mapv #(apply compile-rule %) rule-inputs)]
    `(ruleset* ~@rules)))

(defn rule-simplifier
  "Transform the supplied rules into a function of expressions which will
  arrange to apply each of the rules in the ruleset to all the component parts
  of the expression in depth order, then simplifies the result; the process is
  continued until a fixed point of the simplification process is achieved."
  [& rules]
  (iterated-bottom-up
   (apply pipe (map attempt rules))))

(defn term-rewriting
  "Alias for `rule-simplifier`..."
  [& rules]
  (apply rule-simplifier rules))
