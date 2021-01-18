
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology

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
;;

(ns sicmutils.function
  (:refer-clojure :rename {get core-get
                           get-in core-get-in}
                  #?@(:cljs [:exclude [get get-in]]))
  (:require [clojure.core.match :refer [match]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang RestFn Fn MultiFn Keyword Symbol Var)
              (java.lang.reflect Method))))

;; ## Function Algebra
;;
;; this namespace extends the sicmutils generic operations to Clojure functions
;; and multimethods. (Of course, this includes the generic operations
;; themselves!)

;; ### Utilities

(defprotocol IArity
  (arity [_]
    "Return the cached or obvious arity of the object if we know it. Otherwise
    delegate to the heavy duty reflection, if we have to."))

(extend-protocol IArity
  #?(:clj Object :cljs default)
  (arity [o]
    (or (:arity (meta o))
        ;; Faute de mieux, we assume the function is unary. Most math functions
        ;; are.
        [:exactly 1]))

  Symbol
  (arity [_] [:exactly 0])

  MultiFn
  ;; If f is a multifunction, then we expect that it has a multimethod
  ;; responding to the argument :arity, which returns the arity.
  (arity [f] (f :arity)))

(defn function? [f]
  (isa? (v/kind f) ::v/function))

(defn with-arity
  "Appends the supplied `arity` to the metadata of `f`, knocking out any
  pre-existing arity notation.

  Optionally accepts a third parameter `m` of metadata to attach to the return
  function, in addition to the new `:arity` key."
  ([f arity]
   (with-arity f arity {}))
  ([f arity m]
   (let [new-meta (-> (meta f)
                      (merge m)
                      (assoc :arity arity))]
     (if new-meta
       (with-meta f new-meta)))))

(defn compose
  "Compose is like Clojure's standard comp, but for this system we
  like to know the arity of our functions, so that we can calculate
  their derivatives with structure, etc. The arity of a composition is
  simply the arity of its rightmost (that is, first to be applied)
  function term."
  [& fns]
  (let [a (arity (last fns))]
    (with-meta (apply comp fns) {:arity a})))

(defn get
  "For non-functions, acts like [[clojure.core/get]]. For function
  arguments (anything that responds true to [[function?]]), returns

  (comp #(clojure.core/get % k) f)

  If `not-found` is supplied it's passed through to the
  composed [[clojure.core/get]]."
  ([f k]
   (if (function? f)
     (compose #(get % k) f)
     (core-get f k)))
  ([f k not-found]
   (if (function? f)
     (compose #(get % k not-found) f)
     (core-get f k not-found))))

(defn get-in
  "For non-functions, acts like [[clojure.core/get-in]]. For function
  arguments (anything that responds true to [[function?]]), returns

  (comp #(clojure.core/get-in % ks) f)

  If `not-found` is supplied it's passed through to the
  composed [[clojure.core/get-in]]."
  ([f ks]
   (if (function? f)
     (compose #(get-in % ks) f)
     (core-get-in f ks)))
  ([f ks not-found]
   (if (function? f)
     (compose #(get-in % ks not-found) f)
     (core-get-in f ks not-found))))

(defn- zero-like [f]
  (let [meta {:arity (arity f)
              :from :zero-like}]
    (-> (fn [& args]
          (v/zero-like (apply f args)))
        (with-meta meta))))

(defn- one-like [f]
  (let [meta {:arity (arity f)
              :from :one-like}]
    (-> (fn [& args]
          (v/one-like (apply f args)))
        (with-meta meta))))

(def ^{:doc "Returns its argument."}
  I
  identity)

(defn- identity-like [f]
  (let [meta {:arity (arity f)
              :from :identity-like}]
    (with-meta identity meta)))

(defn arg-shift
  "Takes a function `f` and a sequence of `shifts`, and returns a new function
  that adds each shift to the corresponding argument of `f`. Too many or two few
  shifts are ignored.

  ((arg-shift square 3) 4) ==> 49
  ((arg-shift square 3 2 1) 4) ==> 49"
  [f & shifts]
  (let [shifts (concat shifts (repeat 0))]
    (-> (fn [& xs]
          (apply f (map g/+ xs shifts)))
        (with-meta {:arity (arity f)}))))

(defn arg-scale
  "Takes a function `f` and a sequence of `factors`, and returns a new function
  that multiplies each factor by the corresponding argument of `f`. Too many or
  two few factors are ignored.

  ((arg-scale square 3) 4) ==> 144
  ((arg-scale square 3 2 1) 4) ==> 144"
  [f & factors]
  (let [factors (concat factors (repeat 1))]
    (-> (fn [& xs]
          (apply f (map g/* xs factors)))
        (with-meta {:arity (arity f)}))))

(extend-protocol v/Value
  MultiFn
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [f] (zero-like f))
  (one-like [f] (one-like f))
  (identity-like [f] (identity-like f))
  (exact? [f] (compose v/exact? f))
  (freeze [f]
    (if-let [m (get-method f [Keyword])]
      (m :name)
      (core-get @v/object-name-map f f)))
  (kind [o] ::v/function)

  #?(:clj Fn :cljs function)
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [f] (zero-like f))
  (one-like [f] (one-like f))
  (identity-like [f] (identity-like f))
  (exact? [f] (compose v/exact? f))
  (freeze [f] (core-get @v/object-name-map f f))
  (kind [_] ::v/function)

  Var
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [f] (zero-like f))
  (one-like [f] (one-like f))
  (identity-like [f] (identity-like f))
  (exact? [f] (compose v/exact? f))
  (freeze [f] (core-get @v/object-name-map @f f))
  (kind [_] ::v/function)

  #?@(:cljs
      [MetaFn
       (zero? [_] false)
       (one? [_] false)
       (identity? [_] false)
       (zero-like [f] (zero-like f))
       (one-like [f] (one-like f))
       (identity-like [f] (identity-like f))
       (exact? [f] (compose v/exact? f))
       (freeze [f] (core-get @v/object-name-map f f))
       (kind [_] ::v/function)]))

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
                                 :when (not (#{"withMeta" "meta" "invokeStatic"} name))]
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

(def ^{:private true
       :doc "Returns the arity of the function f. Computing arities of clojure
  functions is a bit complicated. It involves reflection, so the results are
  definitely worth memoizing."}
  reflect-on-arity
  (memoize
   #?(:cljs js-arity :clj jvm-arity)))

#?(:clj
   (extend-protocol IArity
     Fn
     (arity [f] (:arity (meta f) (reflect-on-arity f))))

   :cljs
   (extend-protocol IArity
     function
     (arity [f] (reflect-on-arity f))

     MetaFn
     (arity [f] (:arity (meta f) (reflect-on-arity f)))))

(defn combine-arities
  "Find the joint arity of arities a and b, i.e. the loosest possible arity specification
  compatible with both. Throws if the arities are incompatible."
  ([] [:at-least 0])
  ([a] a)
  ([a b]
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
              [[:exactly k] [:exactly l]] (if (= k l) [:exactly k] (fail)))))))

(defn joint-arity
  "Find the most relaxed possible statement of the joint arity of the given arities.
  If they are incompatible, an exception is thrown."
  [arities]
  (reduce combine-arities arities))

(defn seq-arity
  "Returns the most general arity compatible with all entries in the supplied
  sequence of values."
  [xs]
  (transduce (map arity) combine-arities xs))

;; ## Generic Implementations
;;
;; A `::cofunction` is a type that we know how to combine with a function in a
;; binary operation.

(derive ::v/scalar ::cofunction)

(defn- unary-operation
  "For a unary operator (like sqrt), returns a function of one function which when
  called will apply the operation to the result of the original function (so
  that ((unary-operation sqrt) f) x) will return
  (sqrt (f x))."
  [operator]
  (-> (partial comp operator)
      (with-meta {:arity [:exactly 1]})))

(defn coerce-to-fn
  ([x arity]
   (if (v/numerical? x)
     (-> (constantly x)
         (with-meta {:arity arity}))
     x)))

(defn- binary-operation
  "For a given binary operator (like +), returns a function of two functions which
  will produce the pointwise operation of the results of applying the two
  functions to the input. That is, (binary-operation +) applied to f and g will
  produce a function which computes (+ (f x) (g x)) given x as input."
  [operator]
  (letfn [(h [f g]
            (let [f-arity (if (v/numerical? f) (arity g) (arity f))
                  g-arity (if (v/numerical? g) f-arity   (arity g))
                  f1      (coerce-to-fn f f-arity)
                  g1      (coerce-to-fn g g-arity)
                  arity (joint-arity [f-arity g-arity])]
              (-> (condp = arity
                    [:exactly 0]
                    #(operator (f1) (g1))
                    [:exactly 1]
                    #(operator (f1 %) (g1 %))
                    [:exactly 2]
                    #(operator (f1 %1 %2) (g1 %1 %2))
                    [:exactly 3]
                    #(operator (f1 %1 %2 %3) (g1 %1 %2 %3))
                    [:exactly 4]
                    #(operator (f1 %1 %2 %3 %4) (g1 %1 %2 %3 %4))
                    [:exactly 5]
                    #(operator (f1 %1 %2 %3 %4 %5) (g1 %1 %2 %3 %4 %5))
                    [:exactly 6]
                    #(operator (f1 %1 %2 %3 %4 %5 %6) (g1 %1 %2 %3 %4 %5 %6))
                    [:exactly 7]
                    #(operator (f1 %1 %2 %3 %4 %5 %6 %7) (g1 %1 %2 %3 %4 %5 %6 %7))
                    [:exactly 8]
                    #(operator (f1 %1 %2 %3 %4 %5 %6 %7 %8) (g1 %1 %2 %3 %4 %5 %6 %7 %8))
                    [:exactly 9]
                    #(operator (f1 %1 %2 %3 %4 %5 %6 %7 %8 %9) (g1 %1 %2 %3 %4 %5 %6 %7 %8 %9))
                    [:exactly 10]
                    #(operator (f1 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10) (g1 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10))
                    [:at-least 0]
                    #(operator (apply f1 %&) (apply g1 %&))
                    (u/illegal (str  "unsupported arity for function arithmetic " arity)))
                  (with-meta {:arity arity}))))]
    (with-meta h {:arity [:exactly 2]})))

(defn- defunary
  [generic-op]
  (let [unary-op (unary-operation generic-op)]
    (defmethod generic-op [::v/function] [a] (unary-op a))))

(defn- defbinary
  "Given a generic and binary function operation,
  define the multimethods necessary to introduce this operation
  to function arguments."
  ([generic-op]
   (defbinary generic-op generic-op))
  ([generic-op binary-op]
   (let [binop (binary-operation binary-op)]
     (doseq [signature [[::v/function ::v/function]
                        [::v/function ::cofunction]
                        [::cofunction ::v/function]]]
       (defmethod generic-op signature [a b] (binop a b))))))

(defbinary g/add g/+)
(defbinary g/sub g/-)
(defbinary g/mul g/*)
(defunary g/invert)
(defbinary g/div g/divide)
(defbinary g/expt)

(defunary g/negate)
(defunary g/negative?)
(defunary g/abs)
(defunary g/sqrt)

(defbinary g/quotient)
(defbinary g/remainder)
(defbinary g/modulo)

(defunary g/sin)
(defunary g/cos)
(defunary g/tan)
(defunary g/asin)
(defunary g/acos)

(defunary g/atan)
(defbinary g/atan)

(defunary g/sinh)
(defunary g/cosh)
(defunary g/tanh)

(defunary g/square)
(defunary g/cube)

(defunary g/exp)
(defunary g/log)

(comment
  "This comment expands on a comment from scmutils, function.scm, in the
  definition of `transpose-defining-relation`:

  $T$ is a linear transformation

  $$T : V -> W$$

  the transpose of $T$ is

  $$T^t : (W -> R) -> (V -> R)$$

  \\forall a \\in V, g \\in (W -> R),

  T^t : g \\to g \\circ T

  ie:

  (T^t(g))(a) = g(T(a))")
(defmethod g/transpose [::v/function] [f]
  (fn [g]
    (fn [a]
      (g (f a)))))

(defunary g/determinant)
(defunary g/trace)

(defbinary g/gcd)
(defbinary g/lcm)
(defbinary g/exact-divide)

(defunary g/dimension)
(defbinary g/dot-product)
(defbinary g/inner-product)
(defbinary g/outer-product)
(defbinary g/cross-product)

;; Complex Operations

(defbinary g/make-rectangular)
(defbinary g/make-polar)
(defunary g/real-part)
(defunary g/imag-part)
(defunary g/magnitude)
(defunary g/angle)
(defunary g/conjugate)
