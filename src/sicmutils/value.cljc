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

(ns sicmutils.value
  (:refer-clojure :rename {zero? core-zero?})
  (:require [sicmutils.util :as u]
            #?(:clj [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]]))
  #?(:clj
     (:import (clojure.lang RestFn MultiFn Keyword Symbol)
              (java.lang.reflect Method))))

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
  ;; rob an expression of useful information for further computation; so
  ;; this is intended to be done just before simplification and printing, to
  ;; simplify those processes.
  (freeze [this])
  (kind [this]))

(declare arity primitive-kind)

(def ^:private object-name-map (atom {}))

#?(:cljs
   (def ^:private ratio? (constantly false)))

(extend-protocol Value
  #?(:clj Number :cljs number)
  (nullity? [x] (core-zero? x))
  (unity? [x] (== 1 x))
  (zero-like [_] 0)
  (one-like [_] 1)
  (freeze [x] x)
  (exact? [x] (or (integer? x) (ratio? x)))
  (numerical? [_] true)
  (kind [x] (type x))

  nil
  (freeze [_] nil)
  (numerical? [_] false)
  (nullity? [_] true)
  (unity?[_] false)
  (kind [_] nil)

  #?(:clj Object :cljs default)
  (nullity? [o] false)
  (numerical? [_] false)
  (unity? [o] false)
  (exact? [o] false)
  (zero-like [o] (cond (instance? Symbol o) 0
                       (or (fn? o) (instance? MultiFn o)) (with-meta
                                                            (constantly 0)
                                                            {:arity (arity o)
                                                             :from :object-zero-like})

                       :else (u/unsupported (str "zero-like: " o))))
  (one-like [o] (u/unsupported (str "one-like: " o)))
  (freeze [o] (cond
                (vector? o) (mapv freeze o)
                (sequential? o) (map freeze o)
                :else (or (and (instance? MultiFn o)
                               (if-let [m (get-method o [Keyword])]
                                 (m :name)))
                          (@object-name-map o)
                          o)))
  (kind [o] (primitive-kind o)))

(defn add-object-symbols!
  [o->syms]
  (swap! object-name-map into o->syms))

;; we record arities as a vector with an initial keyword:
;;   [:exactly m]
;;   [:between m n]
;;   [:at-least m]

#?(:clj
   (defn jvm-arity [f]
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
         :else (u/illegal (str "arity? " f " " facts)))))

   :cljs
   (do
     (defn variadic?
       "Returns true if the supplied function is variadic, false otherwise."
       [f]
       (boolean (.-cljs$lang$maxFixedArity f)))

     (defn exposed-arities
       "When CLJS functions have different arities, the function is represented as a js
  object with each arity storied under its own key."
       [f]
       (let [parse (fn [s]
                     (when-let [arity (re-find (re-pattern #"invoke\$arity\$\d+") s)]
                       (js/parseInt (subs arity 13))))
             arities (->> (map parse (js-keys f))
                          (concat [(.-cljs$lang$maxFixedArity f)])
                          (remove nil?)
                          (into #{}))]
         (if (empty? arities)
           [(alength f)]
           (sort arities))))

     (defn js-arity
       "Returns a data structure indicating the arity of the supplied function."
       [f]
       (let [arities (exposed-arities f)]
         (cond (variadic? f)
               (if (= [0 1 2 3] arities)
                 ;; Rule 3, where we assume that any function that's variadic and
                 ;; that has defined these particular arities is a "compose"
                 ;; function... and therefore takes a single argument.
                 [:exactly 1]

                 ;; this case is where we know we have variadic args, so we set
                 ;; a minimum. This could break if some arity was missing
                 ;; between the smallest and the variadic case.
                 [:at-least (first arities)])

               ;; This corresponds to rule 1 in the JVM case. We have a single
               ;; arity and no evidence of a variadic function.
               (= 1 (count arities)) [:exactly (first arities)]

               ;; This is a departure from the JVM rules. A potential error here
               ;; would occur if someone defined arities 1 and 3, but missed 2.
               :else [:between
                      (first arities)
                      (last arities)])))))

(def ^:private reflect-on-arity
  "Returns the arity of the function f.
  Computing arities of clojure functions is a bit complicated.
  It involves reflection, so the results are definitely worth
  memoizing."
  (memoize
   #?(:cljs js-arity :clj jvm-arity)))

(defn arity
  "Return the cached or obvious arity of the object if we know it.
  Otherwise delegate to the heavy duty reflection, if we have to."
  [f]
  (or (:arity f)
      (:arity (meta f))
      (cond (symbol? f) [:exactly 0]
            ;; If f is a multifunction, then we expect that it has a multimethod
            ;; responding to the argument :arity, which returns the arity.
            (instance? MultiFn f) (f :arity)
            (fn? f) (reflect-on-arity f)
            ;; Faute de mieux, we assume the function is unary. Most math functions are.
            :else [:exactly 1])))

(defn ^:private combine-arities
  "Find the joint arity of arities a and b, i.e. the loosest possible arity specification
  compatible with both. Throws if the arities are incompatible."
  [a b]
  (let [fail #(u/illegal (str "Incompatible arities: " a " " b))]
    ;; since the combination operation is symmetric, sort the arguments
    ;; so that we only have to implement the upper triangle of the
    ;; relation.
    (if (< 0 (compare (first a) (first b)))
      (combine-arities b a)
      (match [a b]
             [[:at-least k] [:at-least k2]] [:at-least (max k k2)]
             [[:at-least k] [:between m n]] (let [m (max k m)]
                                              (cond (= m n) [:exactly m]
                                                    (< m n) [:between m n]
                                                    :else (fail)))
             [[:at-least k] [:exactly l]] (if (>= l k)
                                            [:exactly l]
                                            (fail))
             [[:between m n] [:between m2 n2]] (let [m (max m m2)
                                                     n (min n n2)]
                                                 (cond (= m n) [:exactly m]
                                                       (< m n) [:between m n]
                                                       :else (fail)))
             [[:between m n] [:exactly k]] (if (and (<= m k)
                                                    (<= k n))
                                             [:exactly k]
                                             (fail))
             [[:exactly k] [:exactly l]] (if (= k l) [:exactly k] (fail))))))

(defn joint-arity
  "Find the most relaxed possible statement of the joint arity of the given arities.
  If they are incompatible, an exception is thrown."
  [arities]
  (reduce combine-arities [:at-least 0] arities))

(defn ^:private primitive-kind
  [a]
  (cond
    (or (fn? a) (instance? MultiFn a)) ::function
    :else (or (:type a)
              (type a))))

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
