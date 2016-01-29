;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns sicmutils.value
  (:refer-clojure :rename {zero? core-zero?})
  (:import (clojure.lang RestFn MultiFn)
           (java.lang.reflect Method)))

(defprotocol Value
  (numerical? [this])
  (nullity? [this])
  (unity? [this])
  (zero-like [this])
  (one-like [this])
  (exact? [this])
  ;; Freezing an expression means removing wrappers and other metadata
  ;; from subexpressions, so that the result is basically a pure
  ;; S-expression with the same structure as the input. Doing this will
  ;; rob an expression of useful information fur further computation; so
  ;; this is intended to be done just before simplification and printing, to
  ;; simplify those processes.
  (freeze [this])
  (arity [this])
  (kind [this]))

(declare primitive-arity primitive-kind)

(def ^:private object-name-map (atom {}))

(extend-type Object
  Value
  (numerical? [_] false)
  (nullity? [_] false)
  (unity? [o] (when (number? o) (== o 1)))
  (exact? [o] (or (integer? o) (ratio? o)))
  (zero-like [_] 0)
  (one-like [_] 1)
  (freeze [o] (cond
                (vector? o) (mapv freeze o)
                (sequential? o) (map freeze o)
                :else (or (@object-name-map o) o)))
  (arity [o] (primitive-arity o))
  (kind [o] (primitive-kind o)))

(extend-type nil
  Value
  (freeze [_] nil))

(defn add-object-symbols!
  [o->syms]
  (swap! object-name-map into o->syms))

(def ^:private primitive-arity
  "Computing arities of clojure functions is a bit complicated.
  It involves reflection, so the results are definitely worth
  memoizing."
  (memoize
   (fn [f]
     (or (:arity f)
         (:arity (meta f))
         (cond (symbol? f)
               [:exactly 0]

               (fn? f)
               (let [^"[java.lang.reflect.Method" methods (.getDeclaredMethods (class f))
                     ;; tally up arities of invoke, doInvoke, and getRequiredArity methods
                     ^RestFn rest-fn f
                     facts (group-by first
                                     (for [^Method m methods]
                                       (condp = (.getName m)
                                         "invoke" [:invoke (alength (.getParameterTypes m))]
                                         "doInvoke" [:doInvoke true]
                                         "getRequiredArity" [:getRequiredArity (.getRequiredArity rest-fn)])))]
                 (cond
                   ;; Rule one: if all we have is one single case of invoke, then the
                   ;; arity is the arity of that method. This is the common case.
                   (and (= 1 (count facts))
                        (= 1 (count (:invoke facts))))
                   [:exactly (second (first (:invoke facts)))]
                   ;; Rule two: if we have exactly one doInvoke and getRequiredArity,
                   ;; then the arity at least the result of .getRequiredArity.
                   (and (= 2 (count facts))
                        (= 1 (count (:doInvoke facts)))
                        (= 1 (count (:getRequiredArity facts))))
                   [:at-least (second (first (:getRequiredArity facts)))]
                   ;; Rule three: if we have invokes for the arities 0..3, getRequiredArity
                   ;; says 3, and we have doInvoke, then we consider that this function
                   ;; was probably produced by Clojure's core "comp" function, and
                   ;; we somewhat lamely consider the arity of the composed function 1.
                   (and (= #{0 1 2 3} (into #{} (map second (:invoke facts))))
                        (= 3 (second (first (:getRequiredArity facts))))
                        (:doInvoke facts))
                   [:exactly 1]
                   :else (throw (IllegalArgumentException. (str "arity? " f " " facts)))))

               (instance? clojure.lang.MultiFn f)
               (f :arity)

               :else [:exactly 1])))))

(defn joint-arity
  "Find the most relaxed possible statement of the joint arity of the objects
  xs. If they are incompatible, an exception is thrown."
  [arities]
  (let [arity-fail #(throw (IllegalArgumentException.
                            (str "Incompatible arities: " arities)))]
    (reduce (fn [[joint-qualifier joint-value] [qualifier value]]
              (if (= joint-qualifier :exactly)
                (if (= qualifier :exactly)
                  (if (= joint-value value)  ;; exactly/exactly: counts must match
                    [joint-qualifier joint-value]
                    (arity-fail))
                  (if (>= joint-value value) ;; exactly/at-least: exactly count must >= at-least
                    [joint-qualifier joint-value]
                    (arity-fail)))
                (if (= qualifier :exactly)
                  (if (>= value joint-value) ;; at-least/exactly
                    [qualifier value]
                    (arity-fail))
                  [:at-least (max joint-value value)])))
            [:at-least 0]
            arities)))

(defn- primitive-kind
  [a]
  (cond
    (or (fn? a) (= (class a) MultiFn)) ::function
    (keyword? a) a
    :else (type a)))

(defn argument-kind
  [a & as]
  (cond (some? as) (mapv kind (cons a as))
        (nil? a) nil
        :else (kind a)))

(def machine-epsilon
  (loop [e 1.0]
    (if (not= 1.0 (+ 1.0 (/ e 2.0)))
      (recur (/ e 2.0))
      e)))

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [^double ε]
  (fn [^double x ^double y] (< (Math/abs (- x y)) ε)))
