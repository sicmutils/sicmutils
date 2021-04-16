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

(ns pattern.match
  "Implementation of a pattern matching system inspired by [Gerald Jay Sussman's
  lecture notes for MIT
  6.945](http://groups.csail.mit.edu/mac/users/gjs/6.945/).
  See [[pattern.rule]] for a higher-level API.

  [[pattern.match]] is spiritually similar to Alexey
  Radul's [Rules](https://github.com/axch/rules) library for Scheme."
  (:require [pattern.syntax :as s]))

;; ### Design notes:
;;
;; we probably do want to keep vector vs sequence matchers separate, etc...
;; right now we just have a `list` matcher. We want a vector matcher. We don't
;; need specific keyword matchers etc... For that, you can just use a predicate.
;;
;; `Map` matcher too! We definitely want a dictionary matcher.
;;
;; unquote DONE, and unquote-splice too. We have `unquote` now.
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

(def ^{:doc "Predicate that returns `true` for any input."}
  no-constraint
  (constantly true))

(defn predicate
  "Takes a predicate and returns a matcher that fails if the predicate fails when
  applied to the first item in its datum argument, or succeeds with `(next xs)`
  and no new bindings if the predicate passes.

  TODO note that this applies to sequential stuff."
  [pred]
  (fn predicate-match [frame xs succeed]
    (and (sequential? xs)
         (pred (first xs))
         (succeed frame (next xs)))))

(defn match-eq
  "Returns a matcher which succeeds iff the head of the data is equal to thing.
  The frame is not modified.

  NOTE that this is like match:equal. Also note eq-fn etc."
  ([thing]
   (match-eq thing =))
  ([thing eq-fn]
   (predicate
    (fn [x]
      (eq-fn thing x)))))

(defn match-element
  "If:

  - there is more matchable data (with a first entry that satisfies the optional
  constraint predicate), and
  - the variable is not bound in the frame

  This combinator will succeed by

  - consuming the next item in the data, and
  - producing a frame in which the data seen is bound to the pattern variable.

  If the variable is bound, then the value seen must match the binding to
  succeed (the frame is not modified in this case)."
  ([sym]
   (match-element sym no-constraint))
  ([sym predicate?]
   (fn element-match [frame data succeed]
     (when (and (sequential? data)
                (seq data))
       (let [[x & xs] data]
         (when (predicate? x)
           (if-let [binding (frame sym)]
             (and (= binding x)
                  (succeed frame xs))
             (succeed (assoc frame sym x) xs))))))))

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

(defn match-segment
  "Takes a segment variable and binds successively longer prefixes to the symbol,
  calling the continuation with each one to see if it succeeds.

  If the segment is already present in the frame, it only succeeds if the
  current prefix of `xs` matches the already bound prefix."
  [sym]
  (fn segment-match [frame xs succeed]
    (let [xs (or xs [])]
      (when (sequential? xs)
        (if-let [binding (frame sym)]
          (let [binding-count (count binding)]
            (when (= (take binding-count xs) binding)
              (succeed frame (drop binding-count xs))))
          (loop [prefix []
                 suffix xs]
            (or (succeed (assoc frame sym prefix) suffix)
                (and (seq suffix)
                     (recur (conj prefix (first suffix))
                            (next suffix))))))))))

(defn reverse-segment
  "Succeeds if the symbol is ALREADY bound, and the next block matches the reverse
  of the binding."
  [sym]
  (fn reverse-segment-match [frame xs succeed]
    (let [xs (or xs [])]
      (when (sequential? xs)
        (when-let [binding (frame sym)]
          (let [binding-count (count binding)
                reversed      (rseq binding)]
            (when (= (take binding-count xs) reversed)
              (succeed frame (drop binding-count xs)))))))))

(defn- match-final-segment
  "Version of `match-segment` that does no searching."
  [sym]
  (fn final-segment-match [frame xs succeed]
    (let [xs (or xs [])]
      (when (sequential? xs)
        (if-let [binding (frame sym)]
          (when (= xs binding)
            (succeed frame nil))
          (succeed (assoc frame sym xs) nil))))))

(defn match-list
  "Takes a sequence of matchers and returns a NEW matcher that will try them one
  at a time.

  Each matcher is called with a success continuation that attempts to match the
  new frame and returned `xs` against the remaining matchers.

  If the matcher list runs out, AND there are remaining items, the whole
  returned matcher fails!

  If the matchers, running all together, match everything, then the FULL success
  continuation `succeed` is called with a single item dropped.

  TODO tidy this up once I stare at use cases."
  [matchers]
  (fn list-match [frame xs succeed]
    (when (sequential? xs)
      (letfn [(step [frame items matchers]
                (cond matchers ((first matchers)
                                frame
                                items
                                (fn [new-frame new-xs]
                                  (step new-frame new-xs (next matchers))))

                      (seq items) false

                      :else (succeed frame (next xs))))]
        ;; NOTE this `(first xs)` is the only weird part for me... why is the
        ;; list matcher living in a list?
        ;;
        ;; NOTE this is super weird! The whole thing overall needs a list of
        ;; tokens, so it can't just MATCH ever.
        (step frame (first xs) matchers)))))

;; ## Pattern Matching Compiler
;;
;; A comment in `rules` states: The compiler is a generic operator, allowing the
;; syntax to be extended. NOTE that this last part is not yet true, but TODO
;; make it true.

(defn pattern->combinators
  "Given a pattern (which is essentially a form consisting of constants mixed with
  pattern variables) returns a match combinator for the pattern.

  TODO this is a good place to open up dispatch, as Alexey does, and make new,
  extensible syntax for matchers."
  [pattern]
  (cond (s/element? pattern)
        (match-element (s/variable-name pattern)
                       (s/restriction pattern))

        (s/segment? pattern)
        (match-segment (s/variable-name pattern))

        (s/reverse-segment? pattern)
        (reverse-segment (s/variable-name pattern))

        (sequential? pattern)
        (if (empty? pattern)
          (match-eq pattern)
          (match-list
           ;; NOTE: The final element can go faster, that's why we do this.
           (concat (map pattern->combinators (butlast pattern))
                   (let [p (last pattern)]
                     [(if (s/segment? p)
                        (match-final-segment (s/variable-name p))
                        (pattern->combinators p))]))))

        (fn? pattern) pattern

        :else (match-eq pattern)))

;; ## Making toplevel matchers out of patterns
;;
;; What do we have to this point? We have a collection of matcher combinators,
;; and a soon-to-be-open system for turning a pattern into a matcher. Rules
;; BUILD on these, but we are still low level!

;; This is something that's available

(defn matcher
  "Returns a function of the data that "
  ([pattern] (matcher pattern no-constraint))
  ([pattern pred]
   (let [match   (pattern->combinators pattern)
         success (fn [frame tail]
                   (when-let [m (and (empty? tail)
                                     (pred frame))]
                     (if (map? m)
                       (merge frame m)
                       frame)))]
     (fn [data]
       (match {} [data] success)))))

(defn foreach-matcher
  "TODO Calls `f` with each frame (and optionally tail), for side effects."
  [pattern]
  (let [match (pattern->combinators pattern)]
    (fn [data f & {:keys [include-tails?]}]
      (letfn [(cont [frame xs]
                (if include-tails?
                  (f frame xs)
                  (f frame)))]
        (match {} [data] cont)))))

(defn all-results-matcher
  "Returns a function of `data`... TODO describe"
  [pattern]
  (let [match (pattern->combinators pattern)]
    (fn [data & {:keys [include-tails?]}]
      (let [results (atom [])
            cont    (fn [frame xs]
                      (if include-tails?
                        (swap! results conj [frame xs])
                        (swap! results conj frame))
                      false)]
        (match {} [data] cont)
        @results))))

;; ## Higher Level API
;;
(defn match
  "Convenience function for applying a match combinator to some data.

  Primes the process with an empty frame and supplies a continuation
  which will:

   return the pattern bindings if the match is successful,
   nil otherwise.

  If predicate is supplied, then the resulting frame of a match must satisfy
  this predicate. Otherwise we continue searching."
  ([pattern data]
   ((matcher pattern) data))
  ([pattern data predicate]
   ((matcher pattern predicate) data)))
