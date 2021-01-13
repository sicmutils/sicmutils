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
  (:refer-clojure :rename {zero? core-zero?
                           number? core-number?
                           = core=
                           compare core-compare}
                  #?@(:cljs [:exclude [zero? number? = compare]]))
  (:require [sicmutils.util :as u]
            #?(:clj [potemkin :refer [import-def]])
            #?@(:cljs
                [[goog.array :as garray]
                 [goog.math.Long]
                 [goog.math.Integer]]))
  #?(:clj
     (:import (clojure.lang BigInt PersistentVector Sequential Symbol))))

(defprotocol Numerical
  (numerical? [_]))

(extend-protocol Numerical
  #?(:clj Object :cljs default)
  (numerical? [_] false))

(defprotocol Value
  (zero? [this])
  (one? [this])
  (identity? [this])
  (zero-like [this])
  (one-like [this])
  (identity-like [this])
  (exact? [this])
  (freeze [this]
    "Freezing an expression means removing wrappers and other metadata from
  subexpressions, so that the result is basically a pure S-expression with the
  same structure as the input. Doing this will rob an expression of useful
  information for further computation; so this is intended to be done just
  before simplification and printing, to simplify those processes.")
  (kind [this]))

(def argument-kind #(mapv kind %&))

(def object-name-map (atom {}))

(def seqtype #?(:clj Sequential :cljs ::seq))

;; Allows multimethod dispatch to seqs in CLJS.
#?(:cljs
   (do
     (derive IndexedSeq ::seq)
     (derive PersistentVector ::seq)
     (derive LazySeq ::seq)))

;; Smaller inheritance tree to enabled shared implementations between numeric
;; types that represent mathematical integers.

(derive ::native-integral ::integral)
(derive ::integral ::real)
(derive ::floating-point ::real)
(derive ::real ::number)

(defn native-integral?
  "Returns true if x is an integral number that Clojure's math operations work
  with, false otherwise."
  [x]
  (isa? (kind x) ::native-integral))

(defn integral?
  "Returns true if x is an integral number, false otherwise."
  [x]
  (isa? (kind x) ::integral))

(defn real?
  "Returns true if `x` is either an integral number or a floating point number (ie,
  in the numeric tower but not complex), false otherwise."
  [x]
  (isa? (kind x) ::real))

(defn number?
  "Returns true if `x` is any number type in the numeric tower:

  - integral
  - floating point
  - complex

  false otherwise."
  [x]
  (isa? (kind x) ::number))

;; `::scalar` is a thing that symbolic expressions AND actual numbers both
;; derive from.
(derive ::number ::scalar)

(defn scalar?
  "Returns true for anything that derives from `::scalar`, ie, any numeric type in
  the numeric tower that responds true to [[number?]], plus symbolic expressions
  generated [[sicmutils.abstract.number/literal-number]],

  false otherwise."
  [x]
  (isa? (kind x) ::scalar))

#?(:clj
   (do
     (derive Number ::real)
     (derive Double ::floating-point)
     (derive Float ::floating-point)
     (derive BigDecimal ::floating-point)
     (derive Integer ::native-integral)
     (derive Long ::native-integral)
     (derive BigInt ::native-integral)
     (derive BigInteger ::native-integral))

   :cljs
   (do (derive js/Number ::real)
       (derive js/BigInt ::integral)
       (derive goog.math.Integer ::integral)
       (derive goog.math.Long ::integral)))

(extend-protocol Numerical
  #?(:clj Number :cljs number)
  (numerical? [_] true)

  #?@(:clj
      [java.lang.Double
       (numerical? [_] true)

       java.lang.Float
       (numerical? [_] true)]))

(extend-protocol Value
  #?(:clj Number :cljs number)
  (zero? [x] (core-zero? x))
  (one? [x] (== 1 x))
  (identity? [x] (== 1 x))
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [x] x)
  (exact? [x] (or (integer? x) #?(:clj (ratio? x))))
  (kind [x] #?(:clj (type x)
               :cljs (if (exact? x)
                       ::native-integral
                       ::floating-point)))

  #?(:clj Boolean :cljs boolean)
  (zero? [x] false)
  (one? [x] false)
  (identity? [x] false)
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [x] x)
  (exact? [x] false)
  (kind [x] (type x))

  #?@(:clj
      [java.lang.Double
       (zero? [x] (core-zero? x))
       (one? [x] (== 1 x))
       (identity? [x] (== 1 x))
       (zero-like [_] 0.0)
       (one-like [_] 1.0)
       (identity-like [_] 1.0)
       (freeze [x] x)
       (exact? [x] false)
       (kind [x] (type x))

       java.lang.Float
       (zero? [x] (core-zero? x))
       (one? [x] (== 1 x))
       (identity? [x] (== 1 x))
       (zero-like [_] 0.0)
       (one-like [_] 1.0)
       (identity-like [_] 1.0)
       (freeze [x] x)
       (exact? [x] false)
       (kind [x] (type x))])

  nil
  (zero? [_] true)
  (one?[_] false)
  (identity?[_] false)
  (zero-like [o] (u/unsupported "nil doesn't support zero-like."))
  (one-like [o] (u/unsupported "nil doesn't support one-like."))
  (identity-like [o] (u/unsupported "nil doesn't support identity-like."))
  (freeze [_] nil)
  (kind [_] nil)

  PersistentVector
  (zero? [v] (every? zero? v))
  (one? [_] false)
  (identity? [_] false)
  (zero-like [v] (mapv zero-like v))
  (one-like [o] (u/unsupported (str "one-like: " o)))
  (identity-like [o] (u/unsupported (str "identity-like: " o)))
  (exact? [v] (every? exact? v))
  (freeze [v] (mapv freeze v))
  (kind [v] (type v))

  #?(:clj Object :cljs default)
  (zero? [o] false)
  (one? [o] false)
  (identity? [o] false)
  (zero-like [o] (u/unsupported (str "zero-like: " o)))
  (one-like [o] (u/unsupported (str "one-like: " o)))
  (identity-like [o] (u/unsupported (str "identity-like: " o)))
  (exact? [o] false)
  (freeze [o] (if (sequential? o)
                (map freeze o)
                (get @object-name-map o o)))
  (kind [o] (:type o (type o))))

(defn exact-zero?
  "Returns true if the supplied argument is an exact numerical zero, false
  otherwise."
  [n]
  (and (number? n)
       (exact? n)
       (zero? n)))

;; Override equiv for numbers.
(defmulti = argument-kind)

;; These two constitute the default cases.
(defmethod = [::number ::number] [l r]
  #?(:clj  (== l r)
     :cljs (identical? l r)))

(defmethod = :default [l r]
  (if (or (isa? (kind l) ::number)
          (isa? (kind r) ::number))
    false
    (core= l r)))

#?(:cljs
   ;; These definitions are required for the protocol implementation below.
   (do
     (defmethod = [::native-integral js/BigInt] [l r]
       (js*  "~{} == ~{}" l r))

     (defmethod = [js/BigInt ::native-integral] [l r]
       (js*  "~{} == ~{}" l r))

     (doseq [[from to f] [[goog.math.Long goog.math.Integer u/int]
                          [::native-integral goog.math.Integer u/int]
                          [::native-integral goog.math.Long u/long]
                          [goog.math.Long js/BigInt u/bigint]
                          [goog.math.Integer js/BigInt u/bigint]]]
       (defmethod = [from to] [l r] (core= (f l) r))
       (defmethod = [to from] [l r] (core= l (f r))))

     (extend-protocol IEquiv
       number
       (-equiv [this other]
         (cond (core-number? other) (identical? this other)
               (numerical? other)   (= this other)
               :else false))

       goog.math.Integer
       (-equiv [this other]
         (if (core= goog.math.Integer (type other))
           (.equals this other)
           (= this other)))

       goog.math.Long
       (-equiv [this other]
         (if (core= goog.math.Long (type other))
           (.equals this other)
           (= this other))))))

#?(:cljs
   (extend-type js/BigInt
     IEquiv
     (-equiv [this o]
       (let [other (.valueOf o)]
         (if (u/bigint? other)
           (js*  "~{} == ~{}" this other)
           (= this other))))

     IPrintWithWriter
     (-pr-writer [x writer opts]
       (let [rep (if (<= x (.-MAX_SAFE_INTEGER js/Number))
                   (str x)
                   (str "\"" x "\""))]
         (write-all writer "#sicm/bigint " rep)))))

#?(:cljs
   ;; goog.math.{Long, Integer} won't compare properly using <, > etc unless they
   ;; can convert themselves to numbers via `valueOf.` This extension takes care of
   ;; that modification.
   (do
     (extend-type goog.math.Long
       Object
       (valueOf [this] (.toNumber this)))

     (extend-type goog.math.Integer
       Object
       (valueOf [this] (.toNumber this)))))

#?(:cljs
   (extend-protocol IComparable
     number
     (-compare [this o]
       (let [other (.valueOf o)]
         (if (real? other)
           (garray/defaultCompare this other)
           (throw (js/Error. (str "Cannot compare " this " to " o))))))

     js/BigInt
     (-compare [this o]
       (let [other (.valueOf o)]
         (if (real? other)
           (garray/defaultCompare this other)
           (throw (js/Error. (str "Cannot compare " this " to " o))))))

     goog.math.Integer
     (-compare [this o]
       (let [other (.valueOf o)]
         (cond (instance? goog.math.Integer other) (.compare this other)
               (real? other) (garray/defaultCompare this other)
               :else (throw (js/Error. (str "Cannot compare " this " to " o))))))

     goog.math.Long
     (-compare [this o]
       (let [other (.valueOf o)]
         (cond (instance? goog.math.Long other) (.compare this other)
               (real? other) (garray/defaultCompare this other)
               :else (throw (js/Error. (str "Cannot compare " this " to " o))))))))

#?(:cljs
   ;; Clojurescript-specific implementations of Value.
   (let [big-zero (js/BigInt 0)
         big-one (js/BigInt 1)]

     (extend-protocol Numerical
       js/BigInt
       (numerical? [_] true)

       goog.math.Integer
       (numerical? [_] true)

       goog.math.Long
       (numerical? [_] true))

     (extend-protocol Value
       js/BigInt
       (zero? [x] (js*  "~{} == ~{}" big-zero x))
       (one? [x] (js*  "~{} == ~{}" big-one x))
       (identity? [x] (js*  "~{} == ~{}" big-one x))
       (zero-like [_] big-zero)
       (one-like [_] big-one)
       (identity-like [_] big-one)
       (freeze [x]
         ;; Bigint freezes into a non-bigint if it can be represented as a
         ;; number; otherwise, it turns into its own literal.
         (if (<= x (.-MAX_SAFE_INTEGER js/Number))
           (js/Number x)
           x))
       (exact? [_] true)
       (kind [_] js/BigInt)

       goog.math.Integer
       (zero? [x] (.isZero x))
       (one? [x] (core= (.-ONE goog.math.Integer) x))
       (identity? [x] (core= (.-ONE goog.math.Integer) x))
       (zero-like [_] (.-ZERO goog.math.Integer))
       (one-like [_] (.-ONE goog.math.Integer))
       (identity-like [_] (.-ONE goog.math.Integer))
       (freeze [x] x)
       (exact? [_] true)
       (kind [_] goog.math.Integer)

       goog.math.Long
       (zero? [x] (.isZero x))
       (one? [x] (core= (.getOne goog.math.Long) x))
       (identity? [x] (core= (.getOne goog.math.Long) x))
       (zero-like [_] (.getZero goog.math.Long))
       (one-like [_] (.getOne goog.math.Long))
       (identity-like [_] (.getOne goog.math.Long))
       (freeze [x] x)
       (exact? [x] true)
       (kind [_] goog.math.Long))))

(defn kind-predicate
  "Returns a predicate that returns true if its argument matches the supplied
  kind-keyword `k`, false otherwise."
  [x]
  (let [k (kind x)]
    (fn [x2] (isa? (kind x2) k))))

#?(:clj (import-def core-compare)
   :cljs
   (defn ^number compare
     "Comparator. Clone of [[cljs.core/compare]] that works with the expanded
      SICMUtils numeric tower.

  Returns a negative number, zero, or a positive number when x is logically
  'less than', 'equal to', or 'greater than' y. Uses IComparable if available
  and google.array.defaultCompare for objects of the same type and special-cases
  nil to be less than any other object."
     [x y]
     (cond
       (identical? x y) 0
       (nil? x)         -1
       (nil? y)         1
       (core-number? x) (let [yv (.valueOf y)]
                          (if (real? yv)
                            (garray/defaultCompare x yv)
                            (throw (js/Error. (str "Cannot compare " x " to " y)))))

       (satisfies? IComparable x)
       (-compare x y)

       :else
       (if (and (or (string? x) (array? x) (true? x) (false? x))
                (identical? (type x) (type y)))
         (garray/defaultCompare x y)
         (throw (js/Error. (str "Cannot compare " x " to " y)))))))

(defn add-object-symbols!
  [o->syms]
  (swap! object-name-map into o->syms))

(def machine-epsilon
  (loop [e 1.0]
    (if (core= 1.0 (+ e 1.0))
      (* e 2.0)
      (recur (/ e 2.0)))))

(def sqrt-machine-epsilon
  (Math/sqrt machine-epsilon))

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
