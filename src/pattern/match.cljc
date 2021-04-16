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
  Radul's [Rules](https://github.com/axch/rules) library for Scheme.")

;; # Pattern Matcher
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
;; TODO missing elements:
;;
;; match:predicate

;; ### Constraints
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

;; ## Lists and Segments

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

;; TODO note match:segment in matcher.scm Gotta go over this again and
;; guarantee that it is actually doing the right thing.

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
        (step frame (first xs) matchers)))))

;; ## Accessors

(defn- keyword-suffix
  "Returns the final character of the supplied keyword `kwd`."
  [kwd]
  (let [s (name kwd)
        c (count s)]
    (.charAt ^String s (dec c))))

(defn restricted?
  "TODO let's allow multiple restrictions, and combine them together with
  every-pred. This has to happen in `restriction`."
  [pattern]
  (and (sequential? pattern)
       (> (count pattern) 2)))

(defn splice? [pattern]
  (and (sequential? pattern)
       (= (first pattern) :splice)))

(defn spliced-form [pattern]
  (second pattern))

(defn element?
  "Returns true if `pattern` is a variable reference (i.e., it looks like `(:?
  ...)`) or is a simple keyword (not ending in `$` or `*`), false otherwise."
  [pattern]
  (or (and (keyword? pattern)
           (not (#{\* \$} (keyword-suffix pattern))))

      (and (sequential? pattern)
           (= (first pattern) :?))))

(defn element-with-restriction?
  "Returns true if `pattern` is a variable reference and is also equipped with a
  constraint on matched values, false otherwise."
  [pattern]
  (and (element? pattern)
       (restricted? pattern)))

(defn segment?
  "Returns true if `pattern` is a segment reference (i.e., it looks like `(:??
  ...)`) or is a keyword ending in `*`, false otherwise."
  [pattern]
  (or (and (keyword? pattern)
           (= \* (keyword-suffix pattern)))

      (and (sequential? pattern)
           (= (first pattern) :??))))

(defn reverse-segment?
  "Returns true if x is a REVERSED segment reference (i.e., it looks like `(:$$
  ...)`) or is a keyword ending in `$`, false otherwise."
  [pattern]
  (or (and (keyword? pattern)
           (= \$ (keyword-suffix pattern)))

      (and (sequential? pattern)
           (= (first pattern) :$$))))

(defn variable-name
  "Returns the variable contained in a variable or segment reference form.

  TODO should we always convert these to keywords? That would certainly make it
  easier to look stuff up in frames..."
  [pattern]
  (if (keyword? pattern)
    pattern
    (second pattern)))

(defn- restriction
  "If `pattern` is a variable reference in a pattern with a constraint,
  returns that constraint; else returns a stock function which enforces no
  constraint at all.

  Multiple constraints are allowed."
  [pattern]
  (if (keyword? pattern)
    no-constraint
    (if-let [fs (seq (drop 2 pattern))]
      (apply every-pred fs)
      no-constraint)))

(defn pattern->matcher
  "Given a pattern (which is essentially a form consisting of constants mixed with
  pattern variables) returns a match combinator for the pattern.

  TODO this is a good place to open up dispatch, as Alexey does, and make new,
  extensible syntax for matchers."
  [pattern]
  (cond (element? pattern)
        (match-element (variable-name pattern)
                       (restriction pattern))

        (segment? pattern)
        (match-segment (variable-name pattern))

        (reverse-segment? pattern)
        (reverse-segment (variable-name pattern))

        (sequential? pattern)
        (if (empty? pattern)
          (match-eq pattern)
          (match-list
           ;; NOTE: The final element can go faster, that's why we do this.
           (concat (map pattern->matcher (butlast pattern))
                   (let [p (last pattern)]
                     [(if (segment? p)
                        (match-final-segment (variable-name p))
                        (pattern->matcher p))]))))

        (fn? pattern) pattern

        :else (match-eq pattern)))

;; ## Higher Level API

(defn match
  "Convenience function for applying a match combinator to some data.

  Primes the process with an empty frame and supplies a continuation
  which will:

   return the pattern bindings if the match is successful,
   nil otherwise.

  If predicate is supplied, then the resulting frame of a match must satisfy
  this predicate. Otherwise we continue searching."
  ([matcher data]
   (match matcher data no-constraint))
  ([matcher data predicate]
   (let [receive (fn [frame data]
                   (when-let [m (and (empty? data)
                                     (predicate frame))]
                     (if (map? m)
                       (merge frame m)
                       frame)))]
     (matcher {} (list data) receive))))

(defn foreach
  "TODO Calls `f` with each frame (and optionally tail), for side effects."
  [f matcher data & {:keys [include-tails?]}]
  (matcher {}
           data
           (fn [frame xs]
             (if include-tails?
               (f frame xs)
               (f frame))
             false)))

(defn all-results-matcher
  ([matcher input & {:keys [include-tails?]}]
   (let [results  (atom [])
         callback (if include-tails?
                    #(swap! results conj [%1 %2])
                    #(swap! results conj %1))]
     (foreach callback matcher input :include-tails? include-tails?)
     @results)))
