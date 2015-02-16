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

(ns math.value
  (:refer-clojure :rename {zero? core-zero?})
  (:require [clojure.tools.logging :as log]))

(defprotocol Value
  (numerical? [this])
  (nullity? [this])
  (unity? [this])
  (zero-like [this])
  (exact? [this])
  (compound? [this])
  (sort-key [this])
  (freeze [this])
  (arity-of [this])
  )

(defn arity
  [f]
  ;; this whole function is deeply bogus. We will have to spend some time
  ;; figuring out how to deal with arity in a more precise and defensive
  ;; way. TODO: implement arity metadata for structs, and throw exceptions
  ;; if reflection-determined arity for a function has any ambiguity at
  ;; all.
  (or (:arity f)
      (:arity (meta f))
      (cond (symbol? f) 0
            (satisfies? Value f) (arity-of f)
            (ifn? f) (let [^"[java.lang.reflect.Method" ms (.getDeclaredMethods (class f))
                           arities (into #{} (map #(alength (.getParameterTypes %)) ms))]
                       (if (> (count arities) 1)
                         (let [smallest-nonzero-arity (reduce min (disj arities 0))]
                           (log/warn "guessing that arity of" f "is" smallest-nonzero-arity)
                           smallest-nonzero-arity)
                         (first arities)))
            :else 0)))

(def machine-epsilon
  (loop [e 1.0]
    (if (not= 1.0 (+ 1.0 (/ e 2.0)))
      (recur (/ e 2.0))
      e)))

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [^double ε]
  (fn [^double x ^double y] (< (Math/abs (- x y)) ε)))

(println "value initialized")
