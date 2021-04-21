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

;; ## Rules
;;
;; A 'rule' is a combination of a possibly-failing matcher (see
;; `sicmutils.match`), an optional matcher predicate that can introduce NEW
;; bindings, and a consequence function (see `sicmutils.consequence`)
;;
;; - the matcher (and its predicate) determine whether or not the rule applies
;;   to its data input, and
;; - The consequence can either fail, or successfully return some arbitrary
;;   value or transformation of the bindings from the successful match
;;
;; The final rule is a function from a data input to either failure or a
;; successful transformation.
;;
;; This namespace defines:
;;
;; - the machinery required to create rules, in both function and macro form
;; - some basic rules
;; - combinators to build more advanced rules and term rewriting systems out of
;;   basic rules. These are called "rule combinators".
;;
;; ### Defining Rules
;;
;; Rules are composed from matchers and consequences; these functions are
;; politely aliased into this namespace to make it more self contained.

(def ^{:doc "Convenient predicate that always passes."}
  => (constantly true))

(def ^{:doc "Predicate that fails for all inputs."}
  !=> (constantly false))

(import-def c/succeed)
(import-def m/failure)
(import-def m/failed?)
;;
;; [[pattern*]] is effectively an alias for [[match/matcher]]; the macro
;; form, [[pattern]], is able to take a pattern with un-quoted forms like `?x`.
;; See [[syntax/compile-pattern]] for details.
;;
;; NOTE: These should almost certainly live in `sicmutils.match`. Can we alias a
;; macro into this namespace with Potemkin?

(defn pattern*
  "Builds the pattern portion of a rule from the supplied pattern form or matcher
  combinator and optional predicate `pred`.

  See [[pattern.syntax]] for the allowed syntax pattern, or [[pattern.match]]
  for details on matcher combinators.

  See [[match/matcher]] for more detailed documentation."
  ([form]
   (m/matcher form))
  ([form pred]
   (if pred
     (m/matcher form pred)
     (m/matcher form))))

(defmacro pattern
  "Takes an unevaluated pattern form (or matcher combinator) and an optional
  predicate `pred`, and returns a matcher appropriate for passing to [[rule*]]."
  ([form]
   `(pattern*
     ~(ps/compile-pattern form)))
  ([form pred]
   `(pattern*
     ~(ps/compile-pattern form)
     ~@(when pred [pred]))))

(defmacro consequence
  "Accepts a skeleton expression `skel` and returns a function from a pattern
  matcher's binding map to a data structure of identical shape to `skel`, with:

  - all variable binding forms replaced by their entries in the binding map
  - same with any segment binding form, with the added note that these should
    be spliced in
  - any `unquote` or `unquote-splicing` forms respected

  NOTE: reverse-segment variables are NOT evaluated here; these currently only
  apply when matching an already-bound segment variable."
  [form]
  (c/compile-skeleton form))

(defn rule*
  "Functional version of [[rule]]. See [[rule]] for documentation."
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
  "Returns compiled, macro-ready input for [[rule*]] based on the contract
  described by [[rule]]."
  ([p consequent-fn]
   `(rule* (pattern ~p)
           ~consequent-fn))

  ([p pred skeleton]
   `(rule* (pattern ~p ~pred)
           (consequence ~skeleton))))

(defmacro rule
  "Accepts either:

  - A pattern written using the syntax from `pattern.syntax` and a consequence
    function from binding map => failure or return form, or
  - A pattern, predicate and a consequence _skeleton_,

  And returns a rule. A rule is a function from some data object to either

  - A special `failure` singleton (test for this with [[failed?]]), or
  - A successful transformation provided by a consequence function.

  In the 2-argument case, you must provide an explicit function of the binding
  map. A return of `failure`, `nil` or `false` will cause the whole rule to
  fail. To successfully return `nil` or `false`, wrap the result in [[succeed]].

  Notes for the 3-argument case:


  - If the predicate returns `nil`, `false` or `failure`, the rule fails.

  - The predicate can succeed by returning anything else. If the return value is
    a map, the rule will call the consequence function with this map merged in to
    the bindings.

  - the third form is a consequence 'skeleton' instead of an explicit function
    See [[consequence]] for details."
  ([pattern consequent-fn]
   (compile-rule pattern consequent-fn))
  ([pattern pred skeleton]
   (compile-rule pattern pred skeleton)))

;; ## Rules, Rule Combinators
;;
;; The following section defines a small set of basic rules, as well as a series
;; of rule "combinators". These are functions that take one or more rules as
;; inputs and return a new rule.

(defn pass
  "Rule that always succeeds by returning its input data unchanged."
  [data] data)

(defn fail
  "Rule that always fails with an explicit `failure`, no matter the input."
  [_] failure)

(defn predicate
  "Returns a rule that will pass its input data on unchanged if `(f data)` returns
  true and fail otherwise."
  [f]
  (fn [data]
    (if (f data)
      data
      failure)))

(defn return
  "Returns a rule that matches any input and always returns `x`."
  [x]
  (fn [_] x))

(defn branch
  "Takes a rule `r` and returns a new rule that calls `r` with its input.

  The returned rule returns:

  - `(succeed-r (r data)) if `(r data)` is successful,
  - `(fail-r data) otherwise."
  [r succeed-r fail-r]
  (fn [data]
    (let [result (r data)]
      (if (failed? result)
        (fail-r data)
        (succeed-r result)))))

(defn choice*
  "Identical to the multi-arity [[choice]], but accepts an explicit sequence."
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
  "Accepts any number of `rules` and returns a new `rule` that attempts to apply
  each rule in `rules` to its input data. Returns the first non-failing rule's
  result, or `failure` if no rule succeeds.

  NOTE: The zero-arity `(choice)` returns [[fail]], a rule that fails for any
  input.

  See [[choice*]] for an identical function that accepts an explicit sequence."
  ([] fail)
  ([r] r)
  ([r & rs]
   (choice* (cons r rs))))

(defn pipe*
  "Identical to the multi-arity [[pipe]], but accepts an explicit sequence."
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
  "Accepts any number of `rules` and returns a new `rule` that attempts to pipe
  its input `data` through each rule in `rules`. Only succeeds if every rule
  succeeds on the previous rule's successful output.

  NOTE: The zero-arity `(pipe)` returns [[pass]], a rule that succeeds for any
  input by returning the input unchanged.

  See [[pipe*]] for an identical function that accepts an explicit sequence."
  ([] pass)
  ([r] r)
  ([r & rs]
   (pipe* (cons r rs))))

(defn n-times
  "Returns a rule that applies the rule `r` iteratively `n` times to the input
  data, failing if any application fails.

  For example, these forms are equivalent, except that the [[n-times]] version
  will fail immediately if any application fails vs passing on its failure:

  ```clojure
  (n-times 3 my-rule)
  (fn [data]
    (my-rule (my-rule (my-rule data))))
  ```"
  [n r]
  (pipe* (repeat n r)))

(defn attempt?
  "Returns `true` if `r` was marked as an 'attempt' rule, ie, a rule that will
  never fail, but return its input on a failed match."
  [r]
  (::attempt? (meta r) false))

(defn as-attempt
  "Marks the supplied rule as an 'attempt' rule that won't fail."
  [r]
  (vary-meta r assoc ::attempt? true))

(defn attempt
  "Takes a rule `r` and returns a new rule that return either `(r data)` if `r` is
  successful, or its original input on failure.

  NOTE that the returned rule will never fail! This makes it inappropriate to
  use with [[choice]], for example, if you expect any rule supplied after this
  one to ever be matched. [[attempt]] rules are great choices for the final rule
  passed to [[choice]], however."
  [r]
  (if (attempt? r)
    r
    (as-attempt
     (choice r pass))))

(defn guard
  "Takes a predicate function `f` and a rule `r`, and returns a new rule that will
  return `(r data)` if `(f data)` is true, `data` otherwise.

  NOTE that the returned rule will never fail."
  [f r]
  (attempt
   (pipe (predicate f) r)))

(defn iterated
  "Similar to `clojure.core/iterate` for rule application.

  Takes a rule `r` and returns a new rule that will the first non-failing
  result of the sequence `[data (r data) (r (r data)) ...]`

  This might be `data` itself if `r` fails on first application. This means that
  the returned rule will never fail."
  [r]
  (as-attempt
   (fn [data]
     (let [result (r data)]
       (if (failed? result)
         data
         (recur result))))))

(defn while
  "Returns a new rule which repeatedly applies `r` as long as `f` continues to
  return `true` between the input and output of the rule `r` applied iteratively
  to the input `data`.

  See [[until]] for a similar function that treats its predicate differently."
  [f r]
  (as-attempt
   (fn rec [data]
     ((pipe (attempt r)
            (fn [data*]
              (if (f data data*)
                (rec data*)
                data*)))
      data))))

(defn until
  "Returns a new rule which repeatedly applies `r` until `f` returns `true`
  between the input and output of the rule `r` applied iteratively to the input
  `data`, signaling completion.

  See [[while]] for a similar function that treats its predicate differently."
  [f r]
  (as-attempt
   (fn [data]
     (let [data* ((attempt r) data)]
       (if (f data data*)
         data*
         (recur data*))))))

(defn fixed-point
  "Takes a rule `r` and returns a new rule that applies `r` to `data` iteratively
  until (= input (r input))."
  [r]
  (until = r))

(defn trace
  "Takes a rule `r` and returns a new version of `r` tagged with a unique `id`.
  The returned rule calls the side-effecting `f` with

  ```clojure
  {:id id, :in data}
  ```

  Before calling `r` with `data`, and calls `f` with

  ```clojure
  {:id id, :out (r data)}
  ```

  when the rule returns."
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
;; The next group of rule combinators are designed to accept an expression built
;; out of Clojure data structures and apply the rule in either depth-first or
;; breadth-first fashion.

(defn- try-subexpressions
  "Given a rule `the-rule` and a possibly-nested expression `expr`, attempts to
  apply `the-rule` to all subexpressions in breadth-first order. If the
  transformed form is equivalent, returns its input so that [[identical?]]
  checks before and after don't break.

  Descends correctly into vectors, sequences and dictionaries.

  NOTE: [[try-subexpressions]] assumes that [[the-rule]] will always succeed,
  returning its input on a failed match."
  [the-rule expr]
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
  "Given some rule `the-rule`, returns a new rule that accepts potentially nested
  `data` and applies `the-rule` to all subexpressions in depth-first order, from
  the leaves on up.

  The transformation is applied a single time to all subexpressions.
  See [[iterated-bottom-up]] for a version that will iterate to convergence."
  [the-rule]
  (let [r (attempt the-rule)]
    (as-attempt
     (fn rec [expression]
       (r (try-subexpressions rec expression))))))

(defn top-down
  "Given some rule `the-rule`, returns a new rule that accepts potentially nested
  `data` and applies `the-rule` to all subexpressions on the way down AND back
  up a traversal. This is a sort of hybrid of breadth-first, depth-first.

  The transformation is applied a single time to all subexpressions.
  See [[iterated-top-down]] for a version that will iterate to convergence."
  [the-rule]
  (let [r (attempt the-rule)]
    (as-attempt
     (fn rec [expr]
       (r (try-subexpressions rec (r expr)))))))

(defn iterated-bottom-up
  "Version of [[bottom-up]] that iterates on each subexpression to convergence
  before each subexpression returns. Any change in a subexpression triggers a
  new iterated-bottom-up replacement of that subexpression.

  The returned rule keeps an internal memoization cache and will return
  immediately for subexpressions it's seen before."
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
      (as-attempt @rec))))

(defn iterated-top-down
  "Version of [[top-down]] that iterates on each subexpression to convergence
  before each subexpression returns. Any change in a subexpression triggers a
  new iterated-top-down replacement of that subexpression.

  The returned rule keeps an internal memoization cache and will return
  immediately for subexpressions it's seen before."
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
      (as-attempt @rec))))

;; ## Term Rewriting
;;
;; Term-rewriting systems often declare sets of rules meant to be attempted one
;; after the other, just like [[choice]]; but failure for term-rewriting should
;; return the term with no transformation.

(defn ruleset*
  "Given some number of `rules`, returns a new rule that will act like [[choice]]
  and attempt to apply each rule to the input data, returning the first match.

  If all `rules` fail, the returned rule will return its input `data`.

  See [[ruleset]] for a macro that allows inline rule definition."
  [& rules]
  (attempt
   (apply choice rules)))

(defmacro ruleset
  "Accepts triplets of the form:

  <pattern> <predicate> <consequence-template>

  and returns a new rule that will attempt to match the rules compiled from each
  triplet in sequence, returning the filled-in `<consequence-template>` of the
  first successful match.

  If none of the rules match, the returned rule returns its input data
  unchanged.

  See [[ruleset*]] for a function version that takes explicit
  already-constructed rules."
  [& patterns-and-consequences]
  {:pre (zero? (mod (count patterns-and-consequences) 3))}
  (let [inputs (partition 3 patterns-and-consequences)
        rules  (map #(apply compile-rule %) inputs)]
    `(ruleset* ~@rules)))

(defn rule-simplifier
  "Given some number of `rules`, returns a new rule that will attempt to apply
  each rule to its input expression (and every subexpression of the input,
  bottom up), iterating until no rule causes any change in any level of the
  supplied expression."
  [& rules]
  (iterated-bottom-up
   (apply pipe (map attempt rules))))

(defn term-rewriting
  "Alias for [[rule-simplifier]]."
  [& rules]
  (apply rule-simplifier rules))
