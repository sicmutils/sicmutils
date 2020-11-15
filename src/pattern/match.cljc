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

(ns pattern.match)

;; # Pattern Matcher
;;
;; Inspired by Gerald Jay Sussman's lecture notes for MIT 6.945
;; http://groups.csail.mit.edu/mac/users/gjs/6.945/
;;
;; Notes on the implementation.
;;
;; A "Frame" here is an environment of bindings.
;;
;; A "Matcher" is a function of:
;;
;; - frame
;; - some datum
;; - a continuation to call if success is achieved
;;
;; The "succeed" continuation takes the frame and the REST of the forms.
;;
;; TODO missing elements:
;;
;; match:eqv
;; match:predicate

(def ^:private zero [{} nil])

(def ^:private no-constraint (constantly true))

(defn predicate
  "If the predicate succeeds on the head, calls the continuation with the frame
  and tail, else fails."
  [pred]
  (fn [frame xs succeed]
    (and (sequential? xs)
         (pred (first xs))
         (succeed frame (next xs)))))

;; TODO note match:equal in matcher.scm.
(defn match-one
  "Combinator which succeeds iff the head of the data is equal to thing.

  The frame is not modified."
  ([thing] (match-one thing =))
  ([thing eq-fn]
   (predicate #(eq-fn thing %))))

;; TODO note match:element in scheme.
(defn match-var
  "If:

  - the variable is not bound in the frame
  - there is more matchable data (that satisfies the optional constraint
    predicate)

  This combinator will succeed by

  - consuming the next item in the data and
  - producing a frame in which the data seen is bound to the pattern variable.

  If the variable is bound, then the value seen must match the binding to
  succeed (the frame is not modified in this case).

  TODO note that this is DIFFERENT than what we find in scmutils! There, we
  apply the predicate BEFORE the lookup. Test with both. Does this matter?"
  ([var]
   (match-var var no-constraint))
  ([var predicate?]
   (fn [frame data succeed]
     (when (sequential? data)
       (when-let [[x & xs] data]
         (if-let [binding (frame var)]
           (and (= binding x)
                (succeed frame xs))
           (when (predicate? x)
             (succeed (assoc frame var x) xs))))))))

;; TODO note match:segment in matcher.scm. Gotta go over this again and
;; guarantee that it is actually doing the right thing.
(defn match-segment [var]
  (fn [frame xs succeed]
    (when (sequential? xs)
      (if-let [binding (frame var)]
        ;; the segment value is bound.
        (let [binding-count (count binding)]
          ;; succeed when the counts match... no actual equality check. TODO is
          ;; that okay? NO, TODO we need to fix this!
          (when (= (take binding-count xs) binding)
            (succeed frame (drop binding-count xs))))
        ;; the segment value is unbound. Try the match with successively longer
        ;; prefixes grabbed; fail when we run out.
        (loop [before [] after xs]
          (or (succeed (assoc frame var before) after)
              (when-not (empty? after)
                (recur (conj before (first after))
                       (next after)))))))))

;; TODO test!

(defn match-list [matchers]
  (fn [frame xs succeed]
    (if (sequential? xs)
      (let [step (fn step
                   [frame as matchers]
                   (cond matchers ((first matchers) frame as
                                   #(step %1 %2 (next matchers)))
                         (not (empty? as)) false
                         :else (succeed frame (next xs))))]
        (step frame (first xs) matchers)))))

;; TODO match:reverse-segment


;; TODO note, this is match:element?
(defn variable-reference?
  "True if x is a variable reference (i.e., it looks like (:? ...)) or
  is a simple keyword"
  [x]
  (or (keyword? x)
      (and (sequential? x)
           (= (first x) :?))))

;; TODO note, this is (every-pred match:element? match:restricted?)
(defn variable-reference-with-predicate?
  "True if x is a variable reference and is also equipped with a
  constraint on matched values"
  [x]
  (and (variable-reference? x)
       (sequential? x)
       (> (count x) 2)))

;; TODO match:segment?
(defn segment-reference?
  "True if x is a segment reference (i.e., it looks like (:?? ...))
  or is a keyword ending in `*`"
  [x]
  (or (and (keyword? x)
           (let [s (name x)
                 c (count s)]
             (= \* (nth s (dec c)))))
      (and (sequential? x)
           (= (first x) :??))))

;; TODO match:variable-name
(defn variable
  "Return the variable contained in a variable or segment reference
  form"
  [x]
  (if (keyword? x)
    x
    (second x)))

;; TODO: match:restriction
(defn- variable-constraint
  "If x is a variable reference in a pattern with a constraint,
  returns that constraint; else returns a stock function which
  enforces no constraint at all."
  [x]
  (if (keyword? x)
    no-constraint
    (nth x 2 no-constraint)))

;; TODO match:reverse-segment? missing

;; TODO note match:->combinators
(defn pattern->matcher
  "Given a pattern (which is essentially a form consisting of
  constants mixed with pattern variables) returns a match combinator
  for the pattern."
  [pattern]
  (cond
    (variable-reference? pattern) (match-var (variable pattern)
                                             (variable-constraint pattern))
    (segment-reference? pattern)  (match-segment (variable pattern))
    ;; TODO add match:reverse-segment?
    ;; TODO check empty list - does this clause cover or do we need a separate?
    (sequential? pattern) (match-list (map pattern->matcher pattern))

    :else (match-one pattern)))

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
                   (when (and (empty? data)
                              (predicate frame))
                     frame))]
     (matcher {} (list data) receive))))
