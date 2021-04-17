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

(ns pattern.match
  "Implementation of a pattern matching system inspired by [Gerald Jay Sussman's
  lecture notes for MIT
  6.945](http://groups.csail.mit.edu/mac/users/gjs/6.945/).
  See [[pattern.rule]] for a higher-level API.

  [[pattern.match]] is spiritually similar to Alexey
  Radul's [Rules](https://github.com/axch/rules) library for Scheme."
  (:refer-clojure :exclude [sequence]
                  :rename {or core:or
                           and core:and
                           not core:not})
  (:require [pattern.syntax :as s]))

;; ### Design notes:
;;
;; we probably do want to keep vector vs sequence matchers separate, etc...
;; right now we just have a `list` matcher. We want a vector matcher. We don't
;; need specific keyword matchers etc... For that, you can just use a predicate.
;;
;; `Map` matcher too! We definitely want a dictionary matcher.
;;
;; TODO: We ALSO want to convert all of the rules to `?x` syntax
;; TODO: we ALSO want to remove `:??`, and make it `??` like the original.
;;
;; # Pattern Matcher
;;
;; This is the start of the pattern matching library. Here's the high level
;; design:
;;
;; This namespace defines the contract for a "matcher combinator". The most
;; primitive matcher type is a function, but then we can combine these into
;; matchers on lists... well, a list matcher is really the only combinator right
;; now.
;;
;; There are rule combinators of course!
;;
;;
;; A match procedure takes a data item, a dictionary, and a success
;; continuation. The dictionary accumulates the assignments of match variables
;; to values found in the data. The success continuation takes the new
;; dictionary as an argument.
;;
;; If a match procedure fails it returns nil, or false.
;;
;; ### Notes on our Implementation
;;
;; A "Frame" here is an environment of bindings.
;;
;; A "matcher" is a function of:
;;
;; - a frame
;; - some datum, a data item
;; - a continuation to call if success is achieved
;;
;; The "succeed" continuation takes the frame and the REST of the forms in
;; the "datum"... which I think in this case is a data expression, just anything
;; at all.
;;
;;
;; ## Combinators
;;
;; TODO note constraints, and that this is the base set of pattern matcher
;; combinators you can build other ones out of.
;;
;; TODO note ANY combinator can be an underscore, maybe?
;;
;; TODO note this too - a constraint is... well, just a predicate, I guess. This
;; is the predicate that always returns true.

;; ### Basic Matchers

(defn fail
  "Matcher which will fail for any input."
  [_ _ _])

(defn pass
  "Matcher that succeeds (with no new bindings) for any input, passing along its
  input frame."
  [frame _ succeed]
  (succeed frame))

(defn with-frame
  "Takes a `new-frame` of bindings and returns a matcher that will ignore its
  input and always succeed by replacing the current map of bindings with
  `new-frame`."
  [new-frame]
  (fn [_ _ succeed]
    (succeed new-frame)))

(defn update-frame
  "Takes a function from `frame` to a new frame (or false) and any number of
  arguments `args`, and returns a matcher that will ignore its input and

  - succeed with `(apply f frame args)` if that value is truthy,
  - fail otherwise."
  [f & args]
  (fn [frame _ succeed]
    (when-let [new-frame (apply f frame args)]
      (succeed new-frame))))

(defn predicate
  "Takes a predicate function `pred` and returns a matcher that succeeds (with no
  new bindings) if its data input passes the predicate, fails otherwise."
  [pred]
  (fn predicate-match [frame xs succeed]
    (core:and (pred xs)
              (succeed frame))))

(defn frame-predicate
  "Takes a predicate function `pred` and returns a matcher that succeeds (with no
  new bindings) if its data input passes the predicate, fails otherwise."
  [pred]
  (fn frame-pred [frame _ succeed]
    (core:and (pred frame)
              (succeed frame))))

(defn eq
  "Takes some input `x` and returns a matcher which succeeds if its data input is
  equal to `x` (via `=` or the optional `eq-fn` argument). Fails otherwise.

  The frame is not modified."
  ([x] (eq x =))
  ([x eq-fn]
   (predicate
    (fn [other]
      (eq-fn x other)))))

(defn bind
  "Takes a binding variable `sym` and an optional predicate `pred`, and returns a
  matcher that binds its input to `sym` in the returned `frame`.

  The binding only occurs if `input` passes `pred`.

  If `sym` is already present in `frame`, the matcher only succeeds if the
  values are equal, fails otherwise."
  ([sym]
   (bind sym (fn [_] true)))
  ([sym pred]
   (fn bind-match [frame data succeed]
     (when (pred data)
       (if-let [binding (frame sym)]
         (core:and (= binding data)
                   (succeed frame))
         (succeed (assoc frame sym data)))))))

;; ### Matcher Combinators
;;
;; We can declare this, TODO, this will make sure we can take patterns too.

(declare pattern->combinators)

(defn match-when
  "Returns a matcher that passes its `frame` on to `success-pattern` if `pred`
  succeeds on its data input, fails otherwise."
  [pred success-pattern]
  (let [match (pattern->combinators success-pattern)]
    (fn [frame xs success]
      (when (pred xs)
        (match frame xs success)))))

(defn match-if
  "Returns a matcher that passes its `frame` on to `success-pattern` if `pred`
  succeeds on its data input, `fail-pattern` otherwise.

  If no `fail-matcher` is supplied, the behavior is equivalent
  to [[match-when]]."
  ([pred success-pattern]
   (match-when pred success-pattern))
  ([pred success-pattern fail-pattern]
   (let [s-match (pattern->combinators success-pattern)
         f-match (pattern->combinators fail-pattern)]
     (fn [frame xs success]
       (if (pred xs)
         (s-match frame xs success)
         (f-match frame xs success))))))

(defn or
  "Takes a sequence of patterns, and returns a matcher that will apply its
  arguments to each matcher in turn. Returns the value of the first pattern that
  succeeds."
  ([] fail)
  ([pattern] (pattern->combinators pattern))
  ([pattern & more]
   (let [matchers (map pattern->combinators (cons pattern more))]
     (fn call [frame xs succeed]
       (some #(% frame xs succeed)
             matchers)))))

(defn and
  "Takes a sequence of patterns and returns a matcher that will apply its
  arguments to the first pattern;

  If that match succeeds, the next pattern will be called with the new, returned
  frame (and the original data and success continuation).

  The returned matcher succeeds only of all patterns succeed, and returns the
  value of the final pattern."
  ([] pass)
  ([pattern] (pattern->combinators pattern))
  ([pattern & more]
   (let [matchers (map pattern->combinators (cons pattern more))]
     (fn [frame xs succeed]
       (reduce (fn [acc matcher]
                 (if acc
                   (matcher acc xs succeed)
                   (reduced acc)))
               frame
               matchers)))))

(defn not
  "Takes a `pattern` and returns a matcher that will apply its arguments to the
  `pattern`. The returned pattern will succeed with the original frame if
  `pattern` fails, and fail if `pattern` succeeds."
  [pattern]
  (let [match (pattern->combinators pattern)]
    (fn [frame xs succeed]
      (when-not (match frame xs succeed)
        (succeed frame)))))

;; ### Lists and Segments

;; Segment variables introduce some additional trouble. Unlike other matchers, a
;; segment variable is not tested against a fixed datum that it either matches
;; or not, but against a list such that it may match any prefix. This means that
;; in general, segment variables must search, trying one match and possibly
;; backtracking.
;;
;; There are, however, two circumstances when the search can be avoided:
;;
;; - if the variable is already bound, the bound value needs to be checked
;;   against the data, but no guessing as to how much data to consume is
;;   required.
;;
;; - Also, if the segment variable is the last matcher in its enclosing
;;   list (which actually happens quite often!) then the list matcher already
;;   knows how much data must be matched, and no search is needed.
;;
;;
;; NOTE: The original does some clever stuff when we have an explicit LIST we're
;; using for matching. Instead of storing the actual prefix as a binding, the other systems store:
;;
;; - a pointer to the beginning of the prefix
;; - a pointer to the END of the prefix

(defn as-segment-matcher [f]
  (vary-meta f assoc ::segment? true))

(defn segment-matcher? [f]
  (::segment? (meta f) false))

(defn segment
  "Takes a binding variable `sym` and returns a matcher that calls its success
  continuation with successively longer prefixes of its (sequential) data input
  bound to `sym` inside the frame.

  If `sym` is already present in the frame, the returned matcher only succeeds
  if the bound value is a prefix of the data argument `xs`.

  NOTE: the returned matcher will call its success continuation with TWO
  arguments; the new frame and the remaining elements in `xs`. This is a
  different contract than all other matchers, making `segment` appropriate for
  use inside `sequence`."
  [sym]
  (as-segment-matcher
   (fn segment-match [frame xs succeed]
     (let [xs (core:or xs [])]
       (when (sequential? xs)
         (if-let [binding (frame sym)]
           (let [binding-count (count binding)]
             (when (= (take binding-count xs) binding)
               (succeed frame (drop binding-count xs))))
           (loop [prefix []
                  suffix xs]
             (core:or (succeed (assoc frame sym prefix) suffix)
                      (core:and (seq suffix)
                                (recur (conj prefix (first suffix))
                                       (next suffix)))))))))))

(defn- entire-segment
  "Similar to [[segment]], but matches the entire remaining sequential argument
  `xs`. Fails if its input is not sequential, or `sym` is already bound to some
  other variable or non-equal sequence.

  Calls its continuation with the new frame and `nil`, always."
  [sym]
  (as-segment-matcher
   (fn entire-segment-match [frame xs succeed]
     (let [xs (core:or xs [])]
       (when (sequential? xs)
         (if-let [binding (frame sym)]
           (when (= xs binding)
             (succeed frame nil))
           (succeed (assoc frame sym xs) nil)))))))

(defn reverse-segment
  "Returns a matcher that takes a binding variable `sym`, and succeeds if it's
  called with a sequential data argument with a prefix that is the REVERSE of
  the sequence bound to `sym` in `frame`.

  Fails if any of the following are true:

  - `sym` is not bound in the frame
  - `sym` is bound to something other than a vector prefix created by `segment`
  - the data argument does not have a prefix matching the reverse of vector
    bound to `sym`."
  [sym]
  (as-segment-matcher
   (fn reverse-segment-match [frame xs succeed]
     (let [xs (core:or xs [])]
       (when (sequential? xs)
         (when-let [binding (frame sym)]
           (when (vector? binding)
             (let [binding-count (count binding)
                   reversed      (rseq binding)]
               (when (= (take binding-count xs) reversed)
                 (succeed frame (drop binding-count xs)))))))))))

(defn sequence*
  "Version of [[sequence]] that takes an explicit sequence of `patterns`, vs the
  multi-arity version. See [[sequence]] for documentation."
  [patterns]
  (fn sequence-match [frame xs succeed]
    (when (sequential? xs)
      (letfn [(step [frame items matchers]
                (letfn [(try-elem [matcher]
                          (matcher frame
                                   (first items)
                                   (fn [new-frame]
                                     (step new-frame
                                           (next items)
                                           (next matchers)))))

                        (try-segment [matcher]
                          (matcher frame
                                   items
                                   (fn [new-frame new-xs]
                                     (step new-frame
                                           new-xs
                                           (next matchers)))))]
                  (cond matchers (let [m (first matchers)]
                                   (if (segment-matcher? m)
                                     (try-segment m)
                                     (core:and (seq items)
                                               (try-elem m))))

                        (seq items) false
                        :else (succeed frame))))]
        (let [matchers (map pattern->combinators patterns)]
          (step frame xs matchers))))))

(defn sequence
  "Takes a sequence of patterns and returns a matcher that accepts a sequential
  data input, and attempts to match successive items (or segments) in the
  sequence with the supplied patterns.

  The returned matcher succeeds if `patterns` can consume all elements, fails
  otherwise (or of any of the supplied patterns fails on its argument).

  On success, the returned matcher calls its success continuation with a frame
  processed by each pattern in sequence."
  [& patterns]
  (sequence* patterns))

;; ## Pattern Matching Compiler
;;
;; This next section takes patterns described using the syntax in
;; `pattern.syntax`, and compiles these into matcher combinators.
;;
;; TODO: Can we open this up and make the compiler generic and extensible?
;;
;; TODO continue docs from here.

(defn pattern->combinators
  "Given a pattern (which is essentially a form consisting of constants mixed with
  pattern variables) returns a match combinator for the pattern.

  TODO this is a good place to open up dispatch, as Alexey does, and make new,
  extensible pattern syntax."
  [pattern]
  (cond (s/binding? pattern)
        (bind (s/variable-name pattern)
              (s/restriction pattern))

        (s/segment? pattern)
        (segment (s/variable-name pattern))

        (s/reverse-segment? pattern)
        (reverse-segment (s/variable-name pattern))

        (s/wildcard? pattern) pass

        (fn? pattern) pattern

        (sequential? pattern)
        (if (empty? pattern)
          (eq pattern)
          (sequence*
           (concat (map pattern->combinators (butlast pattern))
                   (let [p (last pattern)]
                     [(if (s/segment? p)
                        (entire-segment (s/variable-name p))
                        (pattern->combinators p))]))))

        :else (eq pattern)))

;; ## Making toplevel matchers out of patterns
;;
;; What do we have to this point? We have a collection of matcher combinators,
;; and a soon-to-be-open system for turning a pattern into a matcher. Rules
;; BUILD on these, but we are still low level!
;;
;; TODO note that this is a higher level place for passing either patterns OR
;; already built matchers. They are all the same. We have our low level
;; combinators; now we want to build matchers.

;; This is something that's available

(defn matcher
  "Returns a function of the data that...

  TODO could by (match-and match (update-frame f)), test that this is true.

  TODO note that this is the place to bump up to a better API."
  ([pattern]
   (let [match (pattern->combinators pattern)]
     (fn [data]
       (match {} data identity))))
  ([pattern pred]
   (let [match (pattern->combinators pattern)
         success (fn [frame]
                   (when-let [m (pred frame)]
                     (if (map? m)
                       (merge frame m)
                       frame)))]
     (fn [data]
       (match {} data success)))))

(defn match
  "TODO note that this is a high level wrapper."
  ([pattern data]
   ((matcher pattern) data))
  ([pattern data pred]
   ((matcher pattern pred) data)))

(defn foreach-matcher
  "TODO note that this calls `f` with each frame, for side effects."
  [pattern f]
  (let [match (pattern->combinators pattern)]
    (fn [data]
      (let [cont (fn
                   ([frame]
                    (f frame)
                    false)
                   ([frame xs]
                    (f frame xs)
                    false))]
        (match {} data cont)))))

(defn foreach [pattern f data]
  ((foreach-matcher pattern f) data))

(defn all-results-matcher
  "Returns a function of `data`... TODO describe"
  [pattern]
  (let [match (pattern->combinators pattern)]
    (fn [data]
      (let [results (atom [])
            cont (fn
                   ([frame]
                    (swap! results conj frame)
                    false)
                   ([frame xs]
                    (swap! results conj [frame xs])
                    false))]
        (match {} data cont)
        @results))))

(defn all-results [pattern data]
  ((all-results-matcher pattern) data))
