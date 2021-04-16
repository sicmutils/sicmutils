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
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.rule
  "Functions for writing and applying term-rewriting rules using the primitives
  defined in [[pattern.match]]."
  (:require [pattern.match :as m]
            [pattern.syntax :as ps]))

;; Adapted from Scheme Rules:

;; A rule is a pattern, a predicate and a handler. The pattern determines the
;; applicability of the rule, and the match bindings that enable said
;; applicability, and the handler can compute an arbitrary value from them. Once
;; constructed, a rule is a procedure that accepts a datum, and returns either
;; the datum if the pattern doesn't match or the value of the handler when
;; applied to the dictionary if it does. The input datum is used as the sentinel
;; value for failure because in the context of term rewriting, succeeding with
;; the input as the answer is equivalent to failing.

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

(defn succeed [x]
  {::succeed x})

(defn- interpret-success [x]
  (if (map? x)
    (::succeed x x)
    x))

(defn make-rule
  "The value to return on failure can be overridden to distinguish idempotent
  success from actual failure, should that be important."
  ([pattern handler]
   (let [match (m/pattern->combinators pattern)
         ;; TODO tidy this up!
         handler (if (fn? handler)
                   handler
                   (constantly handler))]
	   (fn call
       ([data]
        (call data data))
       ([data fail-token]
        (letfn [(cont [frame xs]
                  (and (empty? xs)
                       (handler frame)))]
          (interpret-success
           (or (match {} [data] cont)
               (succeed fail-token)))))))))

(defn- compile-pattern
  "Replace `pattern` with code that will construct the equivalent form with
  variable predicate values exposed to evaluation (see above)."
  [pattern]
  (cond (keyword? pattern)    pattern
        (symbol? pattern)     (list 'quote pattern)
        (ps/unquote? pattern) (ps/unquoted-form pattern)

        ;; TODO if we have unquote splice, we'll have to do the same trick here
        ;; to flatten the pattern down.
        (sequential? pattern)
        (if (or (ps/element? pattern)
                (ps/segment? pattern)
                (ps/reverse-segment? pattern))
          (let [[k sym & preds] pattern]
            `(list ~k '~sym ~@preds))

          (cons 'list (map compile-pattern pattern)))

        :else pattern))

(defn compile-predicate [pred]
  (if (= pred '=>)
    `(constantly true)
    pred))

(defn- lookup [m x]
  (let [f (if (symbol? x)
            `(quote ~x)
            x)]
    (list f m)))

(defn- compile-skeleton
  "Compiles a skeleton expression (written as a pattern), by returning a code
  fragment which will replace instances of variable and segment references in
  the skeleton with values provided by the frame referred to by `frame-sym`.

  The form is meant to be evaluated in an environment where `frame-sym` is bound
  to a mapping of pattern variables to their desired substitutions.

  NOTE: The difference from the original stuff is, here, we have a nice
  dictionary data structure, so the final function just takes that.

  NOTE: reverse segments don't appear in the final bit! just do the normal."
  [frame-sym skel]
  (letfn [(compile [elem]
            (cond (or (ps/element? elem)
                      (ps/segment? elem))
                  (let [v (ps/variable-name elem)]
                    (lookup frame-sym v))

                  (sequential? elem)
                  (compile-sequential elem)

                  :else `'~elem))

          (compile-sequential [xs]
            (let [[acc pending] (reduce
                                 (fn [[acc pending] item]
                                   (let [compiled (compile item)]
                                     (if (ps/segment? item)
                                       (if (empty? pending)
                                         [(conj acc compiled) []]
                                         [(conj acc pending compiled) []])
                                       [acc (conj pending compiled)])))
                                 [[] []]
                                 xs)]
              (cond (and (empty? acc)
                         (empty? pending)) '()
                    (empty? acc)     `(list ~@pending)
                    (empty? pending) `(concat ~@acc)
                    :else `(concat ~@(conj acc pending)))))]
    (compile skel)))

(defn- compile-rule
  "Rule takes a match pattern and substitution pattern, compiles each of these and
  returns a function which may be applied to a form and (optionally) a success
  continuation.

  The function will try to match the pattern and, if successful, _and_ the
  bindings satisfy the supplied predicate, will call the continuation with the
  result of the substitution.

  TODO note that in the two arg case, you give a function of the bindings.

  TODO NOTE that if the consequent-fn in the two arg case returns falsey, the
  whole thing fails."
  ([pattern consequent-fn]
   (let [pattern-expr (compile-pattern pattern)]
     `(make-rule ~pattern-expr ~consequent-fn)))

  ([pattern predicate skeleton]
   (let [pattern-expr     (compile-pattern pattern)
         pred-expr        (compile-predicate predicate)
         frame-sym        (gensym)
         skeleton-expr    (compile-skeleton frame-sym skeleton)]
     `(make-rule ~pattern-expr
                 (fn [~frame-sym]
                   (when-let [result# (~pred-expr ~frame-sym)]
                     (let [~frame-sym (if (map? result#)
                                        (merge ~frame-sym result#)
                                        ~frame-sym)]
                       (succeed ~skeleton-expr))))))))

(defmacro rule
  ([pattern consequent-fn]
   (compile-rule pattern consequent-fn))
  ([pattern predicate? skeleton]
   (compile-rule pattern predicate? skeleton)))

;; ## Alexey Combinators, Term Rewriting
;;
;; Rule combinators
;;
;; Various patterns of rule application captured as combinators that
;; take rules and produce rules (to wit, procedures that accept one
;; input and return the result of transforming it, where returning
;; the input itself signals match failure).

(def ^:no-doc sentinel
  #?(:cljs (NeverEquiv.)
     :clj (Object.)))

(defn rule-list
  "Apply several rules in series, returning the first success."
  [rules]
  (fn call
    ([data] (call data data))
    ([data fail-token]
     (loop [rules rules]
       (if (empty? rules)
         fail-token
         (let [answer ((first rules) data sentinel)]
           (if (= answer sentinel)
             (recur (rest rules))
             answer)))))))

(defn in-order
  "Apply several rules in series, threading the results.

  This one will only fail if ALL of the rules fail."
  [& rules]
  (fn call
    ([data] (call data data))
    ([data fail-token]
     (let [[acc fail?]
           (reduce
            (fn [[acc fail?] r]
              (let [result (r acc sentinel)]
                (if (= result sentinel)
                  [acc fail?]
                  [result false])))
            [data true]
            rules)]
       (if fail? fail-token acc)))))

(defn iterated
  "Apply one rule repeatedly until it doesn't match anymore.

  The `fail-token` here only returns if we fail to match even a single time."
  [the-rule]
  (fn call
    ([data]
     (call data data))
    ([data fail-token]
     (let [attempts (iterate #(the-rule % sentinel)
                             (the-rule data sentinel))]
       (reduce (fn [l r]
                 (if (= r sentinel)
                   (reduced l)
                   r))
               fail-token
               attempts)))))

;; ## Expression Matchers

(defn- try-subexpressions [the-rule expr]
  (if (sequential? expr)
    (let [subexpressions-tried (map the-rule expr)]
      (if (every? true? (map = expr subexpressions-tried))
        expr
        subexpressions-tried))
    expr))

(defn on-subexpressions
  "Apply one rule to all subexpressions of the input, bottom-up."
  [the-rule]
  (fn on-expr [expression]
    (the-rule
     (try-subexpressions on-expr expression))))

(defn iterated-on-subexpressions
  "Iterate one rule to convergence on all subexpressions of the input,
  bottom up.

  Note that subexpressions of a result returned by one invocation of the rule
  may admit additional invocations, so we need to recur again after every
  successful transformation."
  [the-rule]
  (fn on-expr [expr]
    (let [subexpressions-done (try-subexpressions on-expr expr)
          answer (the-rule subexpressions-done)]
      (if (= answer subexpressions-done)
        answer
        (on-expr answer)))))

(defn top-down
  "Iterate one rule to convergence on all subexpressions of the input, applying
  it on the way down as well as back up."
  [the-rule]
  (fn on-expr [expr]
    (let [answer (the-rule expr)]
      (if (= answer expr)
        (let [subexpressions-done (try-subexpressions on-expr expr)
              answer (the-rule subexpressions-done)]
          (if (= answer subexpressions-done)
            answer
            (on-expr answer)))
        (on-expr answer)))))

;; ## Our API
;;
;; The original, good stuff.

(defn make-ruleset [rules]
  (rule-list rules))

(defmacro ruleset
  "Ruleset compiles rules, predicates and consequences (triplet-wise) into a
  function which acts like a single rule (as `rule` would produce) which acts by
  invoking the success continuation with the consequence of the first successful
  rule whose patterns match and satisfy the predicate. If no rules match, the
  failure continuation is invoked.

  TODO note that currently we ONLY allow triplets, but we really should allow
  pairs... take an explicit list."
  [& patterns-and-consequences]
  {:pre (zero? (mod (count patterns-and-consequences) 3))}
  (let [rule-inputs (partition 3 patterns-and-consequences)
        rules       (mapv #(apply compile-rule %) rule-inputs)]
    `(rule-list ~rules)))

(defn rule-simplifier
  "Transform the supplied rules into a function of expressions which will
  arrange to apply each of the rules in the ruleset to all the component parts
  of the expression in depth order, then simplifies the result; the process is
  continued until a fixed point of the simplification process is achieved."
  [& the-rules]
  (iterated-on-subexpressions
   (rule-list the-rules)))

(defn term-rewriting
  "Alias for `rule-simplifier`..."
  [& rules]
  (apply rule-simplifier rules))
