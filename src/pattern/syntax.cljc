#_"SPDX-License-Identifier: GPL-3.0"

(ns pattern.syntax
  "The syntax namespace defines the default syntax for patterns corresponding to
  the matcher combinators defined in [[pattern.match]]."
  (:require [emmy.util :as u]))

;; ### Notes
;;
;; The current version of the library declares the following syntax rules:
;;
;; - `_` is a wildcard matcher that succeeds with anything and introduces no new
;;   bindings.
;;
;; - `?x` triggers an unrestricted binding match. It will match anything and
;;   introduce a new binding from that symbol to the matched value.
;;
;; - `(? <binding> <predicates...>)` triggers a binding iff all of the predicate
;;   functions appearing after the binding pass for the candidate
;;
;; - `(?? <binding>)` or `??x` inside of a sequence matches a _segment_ of the
;;   list whose length isn't fixed. Segments will attempt to succed with
;;   successively longer prefixes of the remaining items in the list.
;;
;; - `($$ <binding>)` or `$$x` will only match _after_ the same binding has
;;   already succeeded with a segment. If it has, - this will match a segment
;;   equal to the _reverse_ of the already-bound segment.
;;
;; - Any sequential entry, like a list or a vector, triggers a `sequence` match.
;;   This will attempt to match a sequence, and only pass if its matcher
;;   arguments are able to match all entries in the sequence.
;;
;;
;; ## Non-Macro Syntax

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

  - A symbol starting with a single `?` character
  - A sequence of the form `(? <binding> ...)`."
  [pattern]
  (or (and (simple-symbol? pattern)
           (u/re-matches? #"^\?[^\?].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) '?))))

(defn segment?
  "Returns true if `pattern` is a segment variable reference, false otherwise.

  A segment binding variable is either:

  - A symbol starting with `??`
  - A sequence of the form `(?? <binding>)`."
  [pattern]
  (or (and (simple-symbol? pattern)
           (u/re-matches? #"^\?\?[^\?].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) '??))))

(defn reverse-segment?
  "Returns true if `pattern` is a reversed-segment variable reference, false
  otherwise.

  A reverse-segment binding variable is either:

  - A symbol starting with `$$`
  - A sequence of the form `(:$$ <binding>)`."
  [pattern]
  (or (and (simple-symbol? pattern)
           (u/re-matches? #"^\$\$[^\$].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) '$$))))

(defn variable-name
  "Given a variable or segment binding form, returns the binding variable.

  NOTE that [[variable-name]] will not guard against incorrect inputs."
  [pattern]
  (if (simple-symbol? pattern)
    pattern
    (second pattern)))

(defn reverse-segment-name
  "Given a REVERSE-segment name, either extracts the symbol from a pattern
  like `(:$$ x)`, or transforms symbols like `$$x` into `??x`."
  [pattern]
  (if (simple-symbol? pattern)
    (symbol
     (str "??" (subs (name pattern) 2)))
    (second pattern)))

(defn restriction
  "If `pattern` is a variable binding form in a pattern with restriction predicates,
  returns a predicate that only returns true if all of the predicates pass for
  its input, false otherwise.

  If `pattern` has no restrictions or is some other input type, returns a
  predicate that will always return `true`."
  [pattern]
  (let [no-constraint (fn [_] true)]
    (if (simple-symbol? pattern)
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

(defn splice-reduce
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

  - `(? x) => (list '? 'x)`
  - any bare symbol is quoted
  - Any form unquoted like `~x` is left UNquoted, even in the symbol spot of `(?
    ~sym ...)`
  - Any form marked `~@[1 2 3]` is spliced in directly, EVEN in the symbol spot
    of `(? ~@sym ...)`

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
              (if (unquote-splice? sym)
                `(list* '~k (concat ~(unquoted-form sym) [~@preds]))
                (let [sym (if (unquote? sym)
                            (unquoted-form sym)
                            `(quote ~sym))]
                  `(list '~k ~sym ~@preds))))
            (compile-sequential pattern))

          (map? pattern)
          (u/map-vals compile-pattern pattern)

          :else pattern)))
