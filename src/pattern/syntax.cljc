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

(ns pattern.syntax
  "This namespace contains code to match and parse the default pattern and
  skeleton syntax provided in the library."
  (:require [sicmutils.util :as u]))

;; ### Notes
;;
;; The current version of the library declares the following syntax rules:
;;
;; - `_` is a wildcard matcher that succeeds with anything and introduces no new
;;   bindings.
;;
;; - `:x` or `?x` trigger an unrestricted binding match. It will match anything
;;   and introduce a new binding from that symbol to the matched value.
;;
;; - `(:? <binding> <predicates...>)` triggers a binding iff all of the
;;   predicate functions appearing after the binding pass for the candidate
;;
;; - `(:?? <binding>)`, `??x` or `:x*` (a keyword ending with a *) inside of a
;;   sequence matches a _segment_ of the list whose length isn't fixed. Segments
;;   will attempt to succed with successively longer prefixes of the remaining
;;   items in the list.
;;
;; - `(:$$ <binding>)`, `$$x` or `:x$` (a keyword ending with `$`) will only
;;   match _after_ the same binding has already succeeded with a segment. If it
;;   has, - this will match a segment equal to the _reverse_ of the
;;   already-bound segment.
;;
;; - Any sequential entry, like a list or a vector, triggers a `sequence` match.
;;   This will attempt to match a sequence, and only pass if its matcher
;;   arguments are able to match all entries in the sequence.
;;
;; I'm planning on killing the keyword forms of the bindings, since that
;; prevents us from matching keywords, of course. That is fine for `sicmutils`,
;; because symbolic expressions never contain these, but the pattern matching
;; code wants to be a lot more general!
;;
;;
;; ## Non-Macro Syntax

(defn- suffix
  "Returns the final character of the string, symbol or keyword."
  [kwd]
  (let [s (name kwd)
        c (count s)]
    (.charAt ^String s (dec c))))

(defn restricted?
  "Returns true if `pattern` is a binding pattern with restriction predicates,
  false otherwise."
  [pattern]
  (and (sequential? pattern)
       (> (count pattern) 2)))

(defn wildcard?
  "Returns true if `pattern` matches the wildcard character `_`, false otherwise."
  [pattern]
  (= pattern '_))

(defn binding?
  "Returns true if `pattern` is a binding variable reference, false otherwise.

  A binding variable is either:

  - A keyword not ending in `$` or `*` (these signal [[segment?]])
  - A symbol starting with a single `?` character
  - A sequence of the form `(:? <binding> ...)`."
  [pattern]
  (or (and (keyword? pattern)
           (not (#{\* \$} (suffix pattern))))

      (and (simple-symbol? pattern)
           (u/re-matches? #"\?[^\?].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) :?))))

(defn segment?
  "Returns true if `pattern` is a segment variable reference, false otherwise.

  A segment binding variable is either:

  - A keyword ending with `*`
  - A symbol starting with `??`
  - A sequence of the form `(:?? <binding>)`."
  [pattern]
  (or (and (keyword? pattern)
           (= \* (suffix pattern)))

      (and (simple-symbol? pattern)
           (u/re-matches? #"\?\?[^\?].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) :??))))

(defn reverse-segment?
  "Returns true if `pattern` is a reversed-segment variable reference, false
  otherwise.

  A reverse-segment binding variable is either:

  - A keyword ending `$`
  - A symbol starting with `$$`
  - A sequence of the form `(:$$ <binding>)`."
  [pattern]
  (or (and (keyword? pattern)
           (= \$ (suffix pattern)))

      (and (simple-symbol? pattern)
           (u/re-matches? #"\$\$[^\$].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) :$$))))

(defn variable-name
  "Given a variable, segment or reverse segment binding form, returns the binding
  variable.

  NOTE that [[variable-name]] will not guard against incorrect inputs."
  [pattern]
  (if (or (keyword? pattern)
          (simple-symbol? pattern))
    pattern
    (second pattern)))

(defn restriction
  "If `pattern` is a variable binding form in a pattern with restriction predicates,
  returns a predicate that only returns true if all of the predicates pass for
  its input, false otherwise.

  If `pattern` has no restrictions or is some other input type, returns a else
  returns a predicate that will always return `true`."
  [pattern]
  (let [no-constraint (fn [_] true)]
    (if (or (keyword? pattern)
            (simple-symbol? pattern))
      no-constraint
      (if-let [fs (seq (drop 2 pattern))]
        (apply every-pred fs)
        no-constraint))))

;; ## Pattern Compilation
;;
;; [[compile-pattern]] below allows a macro to take a pattern form with binding
;; symbols unquoted. The disadvantage of a macro is that a user can't usually do
;; things like splice in bindings that are in the current scope.
;;
;; To fix this, we handle `unquote` and `unquote-splicing` directly.

(defn unquote?
  "Returns true if `pattern` is a form that should be included with no quoting
  into the returned pattern, false otherwise."
  [pattern]
  (and (sequential? pattern)
       (= (first pattern)
          'clojure.core/unquote)))

(defn unquote-splice?
  "Returns true if `pattern` is a sequence form that should be spliced directly
  into the returned pattern, false otherwise."
  [pattern]
  (and (sequential? pattern)
       (= (first pattern)
          'clojure.core/unquote-splicing)))

(defn unquoted-form
  "Given a `pattern` that responds `true` to [[unquote?]] or [[unquote-splice?]],
  returns the form from that pattern."
  [pattern]
  (second pattern))

(defn- splice-reduce
  "Helper function for reducing over a sequence that might contain forms that need
  to be spliced into the resulting sequence. This is a sort of helper for a
  guarded `mapcat`.

  Takes a sequence `xs` and mapping function `f` and returns a sequence of
  sequences that, if concatenated together, would be identical to

  ```clojure
  (map f xs)
  ```

  Where any `x` such that `(splice? x)` returns true would have its sequential
  value `(f x)` spliced into the result.

  For example:

  ```clojure
  (let [f (fn [x] (if (odd? x)  [x x x] x))]
    (splice-reduce odd? f (range 5)))

  ;;=> [[0] [1 1 1] [2] [3 3 3] [4]]
  ```"
  [splice? f xs]
  (let [[acc pending] (reduce
                       (fn [[acc pending] x]
                         (if (splice? x)
                           (if (empty? pending)
                             [(conj acc (f x)) []]
                             [(conj acc pending (f x)) []])
                           [acc (conj pending (f x))]))
                       [[] []]
                       xs)]
    (if (empty? pending)
      acc
      (conj acc pending))))

(defn compile-pattern
  "Given a pattern with unquoted binding forms and, potentially, `~` and `~@`
  entries, returns a pattern appropriately quoted such that it can be evaluated
  by the Clojure reader.

  Changes:

  - `(:? x) => (list :? 'x)`
  - any unquoted symbol is quoted
  - Any form unquoted like `~x` is left UNquoted
  - Any form marked `~@(1 2 3)` is spliced in directly

  These rules proceed recursively down into map, vector and sequential data
  structures. (Recursion only pushes down into values for map-shaped patterns.)"
  [pattern]
  (letfn [(compile-sequential [xs]
            (let [acc (splice-reduce
                       unquote-splice? compile-pattern xs)]
              (if (vector? xs)
                (into [] cat acc)
                (cons `list (apply concat acc)))))]

    (cond (symbol? pattern) (list 'quote pattern)

          (or (unquote? pattern)
              (unquote-splice? pattern))
          (unquoted-form pattern)

          (sequential? pattern)
          (if (or (binding? pattern)
                  (segment? pattern)
                  (reverse-segment? pattern))
            (let [[k sym & preds] pattern]
              `(list ~k '~sym ~@preds))
            (compile-sequential pattern))

          (map? pattern)
          (u/map-vals compile-pattern pattern)

          :else pattern)))

(defn- apply-form
  "Given symbols `f` representing a function and `m` representing its argument,
  returns a form that represents function application.

  Symbols are quoted, [[unquote?]] forms are included without quote and all
  other forms are left untouched."
  [f x]
  (let [f (cond (symbol? f) `(quote ~f)
                (unquote? f) (unquoted-form f)
                :else f)]
    (list f x)))

;; ### Skeleton
;;
;; A Skeleton is a template form that we can transform into a function of a
;; matcher's binding map, called a "consequence". The function should take the
;; binding map and return a copy of the skeleton with:
;;
;; - all variable binding forms replaced by their entries in the binding map
;; - same with any segment binding form, with the added note that these should
;;   be spliced in
;; - any `unquote` or `unquote-splicing` forms respected
;;
;; Any non-binding symbol will be quoted.
;;
;; ### Consequence Functions
;;
;; The contract for a "consequence" function is that it can return `false` or
;; `nil` to signal failure. But what if the function wants to succeed with those
;; values?
;;
;; Wrapping a return value with [[succeed]] will allow a successful return of
;; those values. This only matters for skeleton compilation if the skeleton is
;; identical to `nil` or `false`. In those cases, the returned function will
;; produced `(succeed nil)` or `(succeed false)`.

(defn succeed
  "Wraps the argument `x` in a form that will always successfully return from a
  consequence function, whatever its value.

  Use [[succeed]] to return `nil` or `false` from a consequence function. For
  all other return values, returning `(succeed x)` is identical to returning
  `x`"
  [x]
  {::succeed x})

(defn ^:no-doc unwrap
  "Given a form returned by a consequence function, unwraps the top level
  `succeed` wrapper if present to return the final value."
  [x]
  (if (map? x)
    (::succeed x x)
    x))

(defn compile-skeleton
  "Takes a skeleton expression `skel` and returns a form that will evaluate to a
  function from a pattern matcher's binding map to a data structure of identical shape to `skel`, with

  - all variable binding forms replaced by their entries in the binding map
  - same with any segment binding form, with the added note that these should
    be spliced in
  - any `unquote` or `unquote-splicing` forms respected

  NOTE: reverse-segment variables are NOT evaluated here; these currently only
  apply when matching an already-bound segment variable."
  [skel]
  (let [frame-sym (gensym)]
    (letfn [(compile-sequential [xs]
              (let [acc (splice-reduce (some-fn segment? unquote-splice?)
                                       compile xs)]
                (cond (empty? acc) ()
                      (= 1 (count acc)) (first acc)
                      :else `(concat ~@acc))))

            (compile [form]
              (cond (or (binding? form)
                        (segment? form))
                    (let [v (variable-name form)]
                      (apply-form v frame-sym))

                    (symbol? form) (list 'quote form)

                    (unquote? form)
                    (unquoted-form form)

                    (unquote-splice? form)
                    (into [] (unquoted-form form))

                    (map? form)
                    (u/map-vals compile form)

                    (vector? form)
                    `(vec ~(compile-sequential form))

                    (sequential? form)
                    `(seq ~(compile-sequential form))

                    :else form))]
      (if skel
        `(fn [~frame-sym]
           ~(compile skel))
        `(fn [_#]
           (succeed ~skel))))))
