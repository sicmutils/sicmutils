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
  "Syntax for rules to get converted to matchers."
  (:require [sicmutils.util :as u]))

;; ## Syntax of patterns

;; The syntax of patterns is given by a compiler that turns a pattern expression
;; into a matcher combinator (which will often be a list matcher closed over
;; other matchers).

(defn- keyword-suffix
  "Returns the final character of the supplied keyword `kwd`."
  [kwd]
  (let [s (name kwd)
        c (count s)]
    (.charAt ^String s (dec c))))

(defn restricted?
  "Allows multiple restrictions!"
  [pattern]
  (and (sequential? pattern)
       (> (count pattern) 2)))

(defn unquote? [pattern]
  (and (sequential? pattern)
       (= (first pattern) 'clojure.core/unquote)))

(defn unquote-splice? [pattern]
  (and (sequential? pattern)
       (= (first pattern)
          'clojure.core/unquote-splicing)))

(defn unquoted-form [pattern]
  (second pattern))

(defn wildcard?
  "Returns true if `pattern` matches the wildcard `_`, false otherwise."
  [pattern]
  (= pattern '_))

(defn binding?
  "Returns true if `pattern` is a variable reference (i.e., it looks like `(:?
  ...)`) or is a simple keyword (not ending in `$` or `*`), false otherwise."
  [pattern]
  (or (and (keyword? pattern)
           (not (#{\* \$} (keyword-suffix pattern))))

      (and (simple-symbol? pattern)
           (u/re-matches? #"\?[^\?].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) :?))))

(defn segment?
  "Returns true if `pattern` is a segment reference (i.e., it looks like `(:??
  ...)`) or is a keyword ending in `*`, false otherwise."
  [pattern]
  (or (and (keyword? pattern)
           (= \* (keyword-suffix pattern)))

      (and (simple-symbol? pattern)
           (u/re-matches? #"\?\?[^\?].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) :??))))

(defn reverse-segment?
  "Returns true if x is a REVERSED segment reference (i.e., it looks like `(:$$
  ...)`) or is a keyword ending in `$`, or a symbol starting with $$. false
  otherwise."
  [pattern]
  (or (and (keyword? pattern)
           (= \$ (keyword-suffix pattern)))

      (and (simple-symbol? pattern)
           (u/re-matches? #"\$\$[^\$].*" (name pattern)))

      (and (sequential? pattern)
           (= (first pattern) :$$))))

(defn variable-name
  "Returns the variable contained in a variable or segment reference form."
  [pattern]
  (if (or (keyword? pattern)
          (symbol? pattern))
    pattern
    (second pattern)))

(defn restriction
  "If `pattern` is a variable reference in a pattern with a constraint,
  returns that constraint; else returns a stock function which enforces no
  constraint at all.

  Multiple constraints are allowed."
  [pattern]
  (let [no-constraint (constantly true)]
    (if (or (keyword? pattern)
            (symbol? pattern))
      no-constraint
      (if-let [fs (seq (drop 2 pattern))]
        (apply every-pred fs)
        no-constraint))))
