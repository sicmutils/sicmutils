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
  (numerical? [_] false)
  (unity? [o] (when (number? o) (== o 1)))
  (exact? [o] (or (integer? o) (ratio? o)))
  (arity [o] (primitive-arity o))
  (zero-like [o] (cond (fn? o) (with-meta
                                 (constantly 0)
                                 {:arity (arity o)
                                  :from :object-zero-like})
                       :else 0))
  (one-like [_] 1)
  (freeze [o] (cond
                (vector? o) (mapv freeze o)
                (sequential? o) (map freeze o)
                :else (or (@object-name-map o) o)))
  (kind [o] (primitive-kind o)))

(extend-type nil
  Value
  (freeze [_] nil)
  (numerical? [_] nil)
  (kind [_] nil))

(defn add-object-symbols!
  [o->syms]
  (swap! object-name-map into o->syms))

;; we record arities as a vector with an initial keyword:
;;   [:exactly m]
;;   [:between m n]
;;   [:at-least m]

(def ^:private reflect-on-arity
  "Returns the arity of the function f.
  Computing arities of clojure functions is a bit complicated.
  It involves reflection, so the results are definitely worth
  memoizing."
  (memoize
   (fn [f]
     (let [^"[java.lang.reflect.Method" methods (.getDeclaredMethods (class f))
                     ;; tally up arities of invoke, doInvoke, and
                     ;; getRequiredArity methods. Filter out invokeStatic.
                     ^RestFn rest-fn f
                     facts (group-by first
                                     (for [^Method m methods
                                           :let [name (.getName m)]
                                           :when (not= name "invokeStatic")]
                                       (condp = name
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
                   ;; and possibly an invokeStatic, then the arity at
                   ;; least the result of .getRequiredArity.
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
                   :else (throw (IllegalArgumentException. (str "arity? " f " " facts))))))))

(defn ^:private primitive-arity
  "Return the cached or obvious arity of the object if we know it.
  Otherwise delegate to the heavy duty reflection, if we have to."
  [f]
  (or (:arity f)
      (:arity (meta f))
      (cond (symbol? f) [:exactly 0]
            (fn? f) (reflect-on-arity f)
            ;; If f is a multifunction, then we expect that it has a multimethod
            ;; responding to the argument :arity, which returns the arity.
            (instance? MultiFn f) (f :arity)
            ;; Faute de mieux, we assume the function is unary. Most math functions are.
            :else [:exactly 1])))

(defn ^:private combine-arities
  "Find the joint arity of arities a and b, i.e. the loosest possible arity specification
  compatible with both. Throws if the arities are incompatible."
  [a b]
  (let [fail #(throw (IllegalArgumentException. (str "Incompatible arities: " a " " b)))]
    ;; since the combination operation is symmetric, sort the arguments
    ;; so that we only have to implement the upper triangle of the
    ;; relation.
    (if (< 0 (compare (first a) (first b)))
     (combine-arities b a)
     (case (first a)
       :at-least (let [k (second a)]
                   (case (first b)
                    :at-least [:at-least (max k (second b))]
                    :between (let [m (max k (second b))
                                   n (nth b 2)]
                               (cond (= m n) [:exactly m]
                                     (< m n) [:between m n]
                                     :else (fail)))
                    :exactly (let [l (second b)]
                               (if (>= l k)
                                [:exactly l]
                                (fail)))))
       :between (let [[m n] (rest a)]
                  (case (first b)
                   :between (let [[m2 n2] (rest b)
                                  m (max m m2)
                                  n (min n n2)]
                              (cond (= m n) [:exactly m]
                                    (< m n) [:between m n]
                                    :else (fail)))
                   :exactly (let [k (second b)]
                              (if (and (<= m k)
                                       (<= k n))
                                [:exactly k]
                                (fail)))
                   ))
       :exactly (let [k1 (second a)
                      k2 (second b)]
                  (if (= k1 k2) [:exactly k1] (fail)))))))

(defn joint-arity
  "Find the most relaxed possible statement of the joint arity of the given arities.
  If they are incompatible, an exception is thrown."
  [arities]
  (reduce combine-arities [:at-least 0] arities))

(defn ^:private primitive-kind
  [a]
  (cond
    (or (fn? a) (= (class a) MultiFn)) ::function
    (keyword? a) a
    :else (type a)))

(def argument-kind #(mapv kind %&))

(def machine-epsilon
  (loop [e 1.0]
    (if (not= 1.0 (+ 1.0 (/ e 2.0)))
      (recur (/ e 2.0))
      e)))

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [^double ε]
  (fn [^double x ^double y] (< (Math/abs (- x y)) ε)))

(def twopi (* 2 Math/PI))

(defn principal-value
  [cuthigh]
  (let [cutlow (- cuthigh twopi)]
    (fn [x]
      (if (and (<= cutlow x) (< x cuthigh))
        x
        (let [y (- x (* twopi (Math/floor (/ x twopi))))]
          (if (< y cuthigh)
            y
            (- y twopi)))))))
