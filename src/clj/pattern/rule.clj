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

(ns pattern.rule
  (require [pattern.match :refer :all]))

;; Inspired by Gerald Jay Sussman's lecture notes for MIT 6.945
;; http://groups.csail.mit.edu/mac/users/gjs/6.945/

(defn- compile-consequence
  "Compiles a consequence (written as a pattern), by returnin a code
  fragment which will replace instances of variable and segment
  references in the consequence with values provided by the frame
  referred to by frame-symbol. The form is meant to be evaluated in an
  environment where frame-symbol is bound to a mapping of pattern
  variables to their desired substitutions."
  [frame-symbol consequence]
  (cond (variable-reference? consequence)
        `(list (~frame-symbol '~(variable consequence)))
        (segment-reference? consequence)
        `(~frame-symbol '~(variable consequence))
        (seq? consequence)
        `(list (concat ~@(map
                          (partial compile-consequence frame-symbol)
                          consequence)))
        :else `(list '~consequence)))

(defn- expose-predicate
  "This is currently a little tricky to explain. A variable pattern in
  a ruleset might be written (:? 'a integer?). If we were to quote the
  entire ruleset, then the constraint part--integer?--would have the
  form of a symbol, and not what we want, which is the function object
  bound to integer?. Expose-predicate processes a form into code that
  will return the variable pattern in a form in which the predicate
  function is 'exposed' to evaluation."
  [form]
  `(list :? '~(second form) ~(nth form 2)))

(defn- prepare-pattern
  "Replace form with code that will construct the equivalent form with
  variable predicate values exposed to evaluation (see above)."
  [form]
  (cond (variable-reference-with-predicate? form) (expose-predicate form)
        (list? form) (cons 'list (map prepare-pattern form))
        (symbol? form) `(quote ~form)
        :else form))

(defmacro rule
  "Rule takes a match pattern and substitution pattern, compiles each
  of these and returns a function which may be applied to a form
  and (optionally) a success continuation. The function will try to
  match the pattern and, if successful, _and_ the bindings satisfy the
  supplied predicate, will call the continuation with the result of
  the substituion."
  [pattern predicate? consequence]
  (let [prepared-pattern (prepare-pattern pattern)
        frame-symbol (gensym)
        compiled-consequence (compile-consequence frame-symbol consequence)]
    `(let [matcher# (pattern->matcher ~prepared-pattern)]
       (fn apply#
         ([data#] (apply# data# identity))
         ([data# continue#]
            (if-let [~frame-symbol (match matcher# data#)]
              (if (~predicate? ~frame-symbol)
                (continue# (first ~compiled-consequence)))))))))

(defmacro ruleset
  "Ruleset compiles rules, predicates and consequences (triplet-wise)
  into a function which acts like a single rule (as rule would
  produce) which acts by returning the consequence of the first
  successful rule, whose pattern-matches satisfy the predicate,
  or nil if none are applicable."
  [& patterns-and-consequences]
  (let [[p pred c & pcs] patterns-and-consequences]
    (if p
      (let [prepared-pattern  p]
        `(fn apply#
          ([data#] (apply# data# identity identity))
          ([data# continue#] (apply# data# continue# identity))
          ([data# continue# fail#]
             (let [R# (rule ~prepared-pattern ~pred ~c)]
               (or (R# data# continue#)
                   ((ruleset ~@pcs) data# continue# fail#)
                   (fail# data#))))))
      `(fn [data# _# fail#]
         (fail# data#)))))

(defn- try-rulesets
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
