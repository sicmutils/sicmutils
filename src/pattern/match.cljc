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

(def ^:private zero [{} nil])

(defn match-one
  "Combinator which succeeds iff the head of the data is equal to
  thing. The frame is not modified."
  [thing]
  (fn [frame xs succeed]
    (and (sequential? xs)
         (= (first xs) thing)
         (succeed frame (next xs)))))

(defn match-var
  "If the variable is not bound in the frame, and there is more
  matchable data (that satisfies the optional constraint predicate),
  this combinator will succeed by consuming the next item in the data
  and produces a frame in which the data seen is bound to the pattern
  variable. If the variable is bound, then the value seen must match
  the binding to succeed (the frame is not modified in this case)."
  ([var]
   (match-var var (constantly true)))
  ([var predicate?]
   (fn [frame data succeed]
     (if (sequential? data)
       (if-let [[x & xs] data]
         (if-let [binding (frame var)]
           (and (= binding x) (succeed frame xs))
           (if (predicate? x)
             (succeed (assoc frame var x) xs))))))))

(defn match-segment [var]
  (fn [frame xs succeed]
    (if-let [binding (frame var)]
      ;; the segment value is bound.
      (let [binding-count (count binding)]
        (if (= (take binding-count xs) binding)
          (succeed frame (drop binding-count xs))))
      ;; the segment value is unbound.
      (loop [before [] after xs]
        (or (succeed (assoc frame var before) after)
            (if-not (empty? after)
              (recur (conj before (first after)) (next after))))))))

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

(defn variable-reference?
  "Returns true if x is a variable reference (i.e., it looks like (:? ...)) or is
  a simple keyword, false otherwise."
  [x]
  (or (keyword? x)
      (and (sequential? x)
           (= (first x) :?))))

(defn variable-reference-with-predicate?
  "Returns true if x is a variable reference and is also equipped with a
  constraint on matched values, false otherwise."
  [x]
  (and (variable-reference? x)
       (sequential? x)
       (> (count x) 2)))

(defn segment-reference?
  "Returns true if x is a segment reference (i.e., it looks like (:?? ...)) or is
  a keyword ending in `*`, false otherwise."
  [x]
  (or (and (keyword? x)
           (let [s (name x)
                 c (count s)]
             (= \* (nth s (dec c)))))
      (and (sequential? x)
           (= (first x) :??))))

(def ^:private no-constraint (constantly true))

(defn variable
  "Returns the variable contained in a variable or segment reference form."
  [x]
  (if (keyword? x) x
      (second x)))

(defn- variable-constraint
  "If x is a variable reference in a pattern with a constraint,
  returns that constraint; else returns a stock function which
  enforces no constraint at all."
  [x]
  (if (keyword? x) no-constraint
      (nth x 2 no-constraint)))

(defn pattern->matcher
  "Given a pattern (which is essentially a form consisting of
  constants mixed with pattern variables) returns a match combinator
  for the pattern."
  [pattern]
  (cond
    (segment-reference? pattern) (match-segment (variable pattern))
    (variable-reference? pattern) (match-var (variable pattern)
                                             (variable-constraint pattern))
    (sequential? pattern) (match-list (map pattern->matcher pattern))
    :else (match-one pattern)))

(defn match
  "Convenience function for applying a match combinator to some data.

  Primes the process with an empty frame and supplies a continuation
  which will return the pattern bindings if the match is successful,
  nil otherwise. If predicate is supplied, then the resulting frame of
  a match must satisfy this predicate; otherwise we continue searching."
  ([matcher data]
   (match matcher data nil))
  ([matcher data predicate]
   (let [receive (fn [frame data]
                   (if (and (empty? data)
                            (or (not predicate) (predicate frame)))
                     frame))]
     (matcher {} (list data) receive))))
