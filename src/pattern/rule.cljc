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
  (:require [pattern.match :as m]))

;; TODO NOTE! NOW we have rules, predicates and skeletons, in GJS land.

(defn- compile-pattern
  "Replace `pattern` with code that will construct the equivalent form with
  variable predicate values exposed to evaluation (see above).

  TODO make this extensible."
  [pattern]
  (cond (keyword? pattern)   pattern
        (symbol? pattern)    (list 'quote pattern)
        (m/splice? pattern)  (m/spliced-form pattern)

        (or (m/element? pattern)
            (m/segment? pattern)
            (m/reverse-segment? pattern))
        (let [[k sym & preds] pattern]
          `(list ~k '~sym ~@preds))

        (sequential? pattern) (cons 'list (map compile-pattern pattern))

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
            (cond (or (m/element? elem)
                      (m/segment? elem))
                  (let [v (m/variable-name elem)]
                    (lookup frame-sym v))

                  (sequential? elem)
                  (compile-sequential elem)

                  :else `'~elem))

          (compile-sequential [xs]
            (let [[acc pending] (reduce
                                 (fn [[acc pending] item]
                                   (let [compiled (compile item)]
                                     (if (m/segment? item)
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
     `(let [matcher# (m/pattern->matcher ~pattern-expr)]
        (fn [data# success#]
          (if-let [frame# (m/match matcher# data#)]
            (when-let [result# (~consequent-fn frame#)]
              (success# result#)))))))

  ([pattern predicate skeleton]
   (let [pattern-expr     (compile-pattern pattern)
         pred-expr        (compile-predicate predicate)
         frame-sym        (gensym)
         skeleton-expr    (compile-skeleton frame-sym skeleton)]
     `(let [matcher# (m/pattern->matcher ~pattern-expr)]
        (fn [data# success#]
          (if-let [~frame-sym (m/match matcher# data# ~pred-expr)]
            (success# ~skeleton-expr)))))))

(defmacro rule
  ([pattern consequent-fn]
   (compile-rule pattern consequent-fn))
  ([pattern predicate? skeleton]
   (compile-rule pattern predicate? skeleton)))

(defmacro ruleset
  "Ruleset compiles rules, predicates and consequences (triplet-wise)
  into a function which acts like a single rule (as rule would
  produce) which acts by invoking the success continuation with the
  consequence of the first successful rule whose patterns match and
  satisfy the predicate. If no rules match, the failure continuation
  is invoked.

  TODO note that currently we ONLY allow triplets, but we really should allow
  pairs... take an explicit list."
  [& patterns-and-consequences]
  {:pre (zero? (mod (count patterns-and-consequences) 3))}
  (let [rule-inputs (partition 3 patterns-and-consequences)
        rules       (mapv #(apply compile-rule %) rule-inputs)]
    `(let [rules# ~rules]
       (fn [data# continue# fail#]
         (or (some #(% data# continue#) rules#)
             (fail# data#))))))

;; TODO how can we cache these??

(defn- try-rulesets
  "Execute the supplied rulesets against expression in series. The first ruleset
  to succeed in rewriting an expression will cause the success continuation to
  be invoked and the process will stop. If no ruleset succeeds, the original
  expression is returned."
  [[ruleset & rulesets] expression succeed]
  (if ruleset
    (ruleset expression succeed #(try-rulesets rulesets % succeed))
    expression))

(defn rule-simplifier
  "Transform the supplied rulesets into a function of expressions
  which will arrange to apply each of the rules in the ruleset to all
  the component parts of the expression in depth order, then
  simplifies the result; the process is continued until a fixed point
  of the simplification process is achieved."
  [& rulesets]
  (fn simplifier [expression]
    (let [simplified (if (sequential? expression)
                       (map simplifier expression)
                       expression)]
      (try-rulesets rulesets simplified simplifier))))


;; TODO: do we want the top down stuff here too??
;; https://github.com/axch/rules/blob/master/term-rewriting.scm
