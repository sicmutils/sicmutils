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

(ns pattern.rule
  (:require [pattern.match :refer :all]
            [clojure.spec.alpha :as s]))

;; Inspired by Gerald Jay Sussman's lecture notes for MIT 6.945
;; http://groups.csail.mit.edu/mac/users/gjs/6.945/

(defn ^:private compile-consequence
  "Compiles a consequence (written as a pattern), by returning a code
  fragment which will replace instances of variable and segment
  references in the consequence with values provided by the frame
  referred to by frame-symbol. The form is meant to be evaluated in an
  environment where frame-symbol is bound to a mapping of pattern
  variables to their desired substitutions."
  [frame-symbol consequence]
  (cond
    (segment-reference? consequence)
    (let [v (variable consequence)
          function-of-frame (if (symbol? v) `(quote ~v) v)]
      `(~function-of-frame ~frame-symbol))

    (variable-reference? consequence)
    (let [v (variable consequence)
          function-of-frame (if (symbol? v) `(quote ~v) v)]
      `(list (~function-of-frame ~frame-symbol)))

    (seq? consequence)
    `(list (concat ~@(map
                      (partial compile-consequence frame-symbol)
                      consequence)))

    :else `(list '~consequence)))

(defn ^:private expose-predicate
  "This is currently a little tricky to explain. A variable pattern in
  a ruleset might be written (:? 'a integer?). If we were to quote the
  entire ruleset, then the constraint part--integer?--would have the
  form of a symbol, and not what we want, which is the function object
  bound to integer?. Expose-predicate processes a form into code that
  will return the variable pattern in a form in which the predicate
  function is 'exposed' to evaluation."
  [form]
  `(list :? '~(second form) ~(nth form 2)))

(defn ^:private prepare-pattern
  "Replace form with code that will construct the equivalent form with
  variable predicate values exposed to evaluation (see above)."
  [form]
  (cond (variable-reference-with-predicate? form) (expose-predicate form)
        (list? form) (cons 'list (map prepare-pattern form))
        (symbol? form) `(quote ~form)
        :else form))

(defn ^:private rule-body
  "Rule takes a match pattern and substitution pattern, compiles each
  of these and returns a function which may be applied to a form
  and (optionally) a success continuation. The function will try to
  match the pattern and, if successful, _and_ the bindings satisfy the
  supplied predicate, will call the continuation with the result of
  the substitution."
  [pattern predicate? consequence]
  (let [prepared-pattern (prepare-pattern pattern)
        frame-symbol (gensym)
        compiled-consequence (compile-consequence frame-symbol consequence)
        replace-if (if (= predicate? '=>) `(constantly true) predicate?)]
    `(let [matcher# (pattern->matcher ~prepared-pattern)]
       (fn [data# continue#]
         (if-let [~frame-symbol (match matcher# data# ~replace-if)]
           (continue# (first ~compiled-consequence)))))))

(defmacro rule
  [pattern predicate? consequence]
  (rule-body pattern predicate? consequence))

(s/def ::pattern list?)
(s/def ::consequence any?)
(s/def ::predicate (s/or :any #(= % '=>) :predicate any?))
(s/def ::rule (s/cat :pattern ::pattern
                     :predicate ::predicate
                     :consequence ::consequence))
(s/def ::ruleset (s/cat :name (s/? string?)
                        :rules (s/+ ::rule)))



(defmacro ruleset
  "Ruleset compiles rules, predicates and consequences (triplet-wise)
  into a function which acts like a single rule (as rule would
  produce) which acts by invoking the success continuation with the
  consequence of the first successful rule whose patterns match and
  satisfy the predicate. If no rules match, the failure continuation
  is invoked."
  [& rules]
  (let [rs (s/conform ::ruleset rules)]
    (when (= rs :s/invalid)
      (throw (ex-info "invalid input" (s/explain-data ::rule rules))))
    (let [rules (for [{:keys [pattern predicate consequence]} (:rules rs)]
                  (rule-body pattern (second predicate) consequence))
          rs-name (or (:name rs) (gensym "ruleset-"))]
      (println "compiling " rs-name)
      `(let [rules# ~(into [] rules)]
         (fn [data# continue# fail#]
           (or (some #(% data# continue#) rules#)
               (fail# data#)))))))

(defn ^:private try-rulesets
  "Execute the supplied rulesets against expression in order. The
  first ruleset to succeed in rewriting an expression will cause
  the success continuation to be invoked and the process will stop.
  If no ruleset succeeds, the original expression is returned."
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
    (let [simplified (if (seq? expression)
                       (map simplifier expression)
                       expression)]
      (try-rulesets rulesets simplified simplifier))))
