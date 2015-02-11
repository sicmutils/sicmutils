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
  (:refer-clojure :rename {zero? core-zero?}))

(defprotocol Value
  (numerical? [this])
  (nullity? [this])
  (unity? [this])
  (zero-like [this])
  (exact? [this])
  (compound? [this])
  (sort-key [this])
  (freeze [this])
  )

(defn arity
  [f]
  {:pre [(ifn? f)]}
  ;; this whole function is deeply bogus. We will have to spend some time
  ;; figuring out how to deal with arity in a more precise and defensive
  ;; way. TODO: implement arity metadata for structs, and throw exceptions
  ;; if reflection-determined arity for a function has any ambiguity at
  ;; all.
  #_(prn "seeking arity of" f (meta f))
  (or (:arity (meta f))
      (let [^"[java.lang.reflect.Method" ms (.getDeclaredMethods (class f))
            ^"java.lang.reflect.Method" m (first ms)
            p (.getParameterTypes m)]
        #_(prn "shortcut arity failed on" f "returning" (alength p))
        #_(prn "methods" (map  #(alength (.getParameterTypes %)) ms))
        (alength p)
        )))

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
