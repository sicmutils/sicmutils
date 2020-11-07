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

(ns sicmutils.series
  (:refer-clojure :exclude [identity])
  (:require [sicmutils.series.impl :as i]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn Seqable Sequential))))

;; Missing:
;;
;; - TODO function-> takes the constant term and generates a power series.
;; - TODO ->function, turn into a power series
;; - TODO rename `starting-with`, add a power series version.
;; - TODO finish `impl` move of tests
;; - TODO figure out which fns only make sense between the SAME type...

;; # Power Series
;;
;; This namespace contains an implementation of two data types:
;;
;; - `Series`, which represents a generic infinite series of arbitrary values, and
;; - `PowerSeries`, a series that represents a power series in a single
;;   variable; in other words, a series where the nth entry is interpreted as
;;   the coefficient of $x^n$:
;;
;; [a b c d ...] == $a + bx + cx^2 + dx^3 + ...$
;;
;; TODO note that we can see `sicmutils.series.impl` for Power Serious etc.

(declare s-zero s-one)

(deftype Series [xs]
  v/Value
  (nullity? [_] false)
  (unity? [_] false)
  (zero-like [_] s-zero)
  (one-like [_] s-one)
  (numerical? [_] false)
  (freeze [_]
    (let [xform (comp (take 4)
                      (map g/simplify))
          prefix (sequence xform xs)]
      `[~'Series ~@prefix ~'...]))
  (kind [_] ::series)

  Object
  (toString [S] (str (v/freeze S)))

  #?@
  (:clj
   [Sequential

    Seqable
    (seq [_] xs)

    IFn
    ;; Invoking a series uses `value` from above to generate a new series.
    (invoke [_]
            (Series. (i/value xs [])))
    (invoke [_ a]
            (Series. (i/value xs [a])))
    (invoke [_ a b]
            (Series. (i/value xs [a b])))
    (invoke [_ a b c]
            (Series. (i/value xs [a b c])))
    (invoke [_ a b c d]
            (Series. (i/value xs [a b c d])))
    (invoke [_ a b c d e]
            (Series. (i/value xs [a b c d e])))
    (invoke [_ a b c d e f]
            (Series. (i/value xs [a b c d e f])))
    (invoke [_ a b c d e f g]
            (Series. (i/value xs [a b c d e f g])))
    (invoke [_ a b c d e f g h]
            (Series. (i/value xs [a b c d e f g h])))
    (invoke [_ a b c d e f g h i]
            (Series. (i/value xs [a b c d e f g h i])))
    (invoke [_ a b c d e f g h i j]
            (Series. (i/value xs [a b c d e f g h i j])))
    (invoke [_ a b c d e f g h i j k]
            (Series. (i/value xs [a b c d e f g h i j k])))
    (invoke [_ a b c d e f g h i j k l]
            (Series. (i/value xs [a b c d e f g h i j k l])))
    (invoke [_ a b c d e f g h i j k l m]
            (Series. (i/value xs [a b c d e f g h i j k l m])))
    (invoke [_ a b c d e f g h i j k l m n]
            (Series. (i/value xs [a b c d e f g h i j k l m n])))
    (invoke [_ a b c d e f g h i j k l m n o]
            (Series. (i/value xs [a b c d e f g h i j k l m n o])))
    (invoke [_ a b c d e f g h i j k l m n o p]
            (Series. (i/value xs [a b c d e f g h i j k l m n o p])))
    (invoke [_ a b c d e f g h i j k l m n o p q]
            (Series. (i/value xs [a b c d e f g h i j k l m n o p q])))
    (invoke [_ a b c d e f g h i j k l m n o p q r]
            (Series. (i/value xs [a b c d e f g h i j k l m n o p q r])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s]
            (Series. (i/value xs [a b c d e f g h i j k l m n o p q r s])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t]
            (Series. (i/value xs [a b c d e f g h i j k l m n o p q r s t])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
            (Series. (i/value xs (concat [a b c d e f g h i j k l m n o p q r s t] rest))))
    (applyTo [s xs] (AFn/applyToHelper s xs))]

   :cljs
   [ISequential

    ISeqable
    (-seq [_] xs)

    IPrintWithWriter
    (-pr-writer [x writer _]
                (write-all writer
                           "#object[sicmutils.series.Series \""
                           (.toString x)
                           "\"]"))

    IFn
    (-invoke [_]
             (Series. (i/value xs [])))
    (-invoke [_ a]
             (Series. (i/value xs [a])))
    (-invoke [_ a b]
             (Series. (i/value xs [a b])))
    (-invoke [_ a b c]
             (Series. (i/value xs [a b c])))
    (-invoke [_ a b c d]
             (Series. (i/value xs [a b c d])))
    (-invoke [_ a b c d e]
             (Series. (i/value xs [a b c d e])))
    (-invoke [_ a b c d e f]
             (Series. (i/value xs [a b c d e f])))
    (-invoke [_ a b c d e f g]
             (Series. (i/value xs [a b c d e f g])))
    (-invoke [_ a b c d e f g h]
             (Series. (i/value xs [a b c d e f g h])))
    (-invoke [_ a b c d e f g h i]
             (Series. (i/value xs [a b c d e f g h i])))
    (-invoke [_ a b c d e f g h i j]
             (Series. (i/value xs [a b c d e f g h i j])))
    (-invoke [_ a b c d e f g h i j k]
             (Series. (i/value xs [a b c d e f g h i j k])))
    (-invoke [_ a b c d e f g h i j k l]
             (Series. (i/value xs [a b c d e f g h i j k l])))
    (-invoke [_ a b c d e f g h i j k l m]
             (Series. (i/value xs [a b c d e f g h i j k l m])))
    (-invoke [_ a b c d e f g h i j k l m n]
             (Series. (i/value xs [a b c d e f g h i j k l m n])))
    (-invoke [_ a b c d e f g h i j k l m n o]
             (Series. (i/value xs [a b c d e f g h i j k l m n o])))
    (-invoke [_ a b c d e f g h i j k l m n o p]
             (Series. (i/value xs [a b c d e f g h i j k l m n o p])))
    (-invoke [_ a b c d e f g h i j k l m n o p q]
             (Series. (i/value xs [a b c d e f g h i j k l m n o p q])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r]
             (Series. (i/value xs [a b c d e f g h i j k l m n o p q r])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s]
             (Series. (i/value xs [a b c d e f g h i j k l m n o p q r s])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
             (Series. (i/value xs [a b c d e f g h i j k l m n o p q r s t])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
             (Series. (i/value xs (concat [a b c d e f g h i j k l m n o p q r s t] rest))))]))

#?(:clj
   (defmethod print-method Series [^Series s ^java.io.Writer w]
     (.write w (str "#object[sicmutils.series.Series \""
                    (.toString s)
                    "\"]"))))

(def s-zero (Series. (i/->series [0])))
(def s-one (Series. (i/->series [1])))

;; ### Power Series
;;
;; The primary difference here is the `IFn` implementation; application of a
;; power series multiples each coefficient by a successively larger power of
;; its (single, for now) argument.
;;
;; TODO Modify this description once we implement multivariable power series!

(declare zero one)

(deftype PowerSeries [xs]
  v/Value
  (nullity? [_] false)
  (unity? [_] false)
  (zero-like [_] zero)
  (one-like [_] one)
  (numerical? [_] false)
  (freeze [_]
    (let [xform  (comp (take 4) (map g/simplify))
          prefix (sequence xform xs)]
      `[~'PowerSeries ~@prefix ~'...]))
  (kind [_] ::power-series)

  Object
  (toString [S] (str (v/freeze S)))

  #?@
  (:clj
   [Sequential

    Seqable
    (seq [_] xs)

    IFn
    (invoke [_ a] (Series. (i/p-value xs a)))]

   :cljs
   [ISequential

    ISeqable
    (-seq [_] xs)

    IFn
    (-invoke [_ a] (Series. (i/p-value xs a)))

    IPrintWithWriter
    (-pr-writer [this writer _]
                (write-all writer
                           "#object[sicmutils.series.PowerSeries \""
                           (.toString this)
                           "\"]"))]))

#?(:clj
   (defmethod print-method PowerSeries [^PowerSeries s ^java.io.Writer w]
     (.write w (str "#object[sicmutils.series.PowerSeries \""
                    (.toString s)
                    "\"]"))))

;; ## Series API

(defn series?
  "Returns true if `s` is either a `Series` or a `PowerSeries`, false otherwise."
  [s]
  (or (instance? Series s)
      (instance? PowerSeries s)))

(defn power-series?
  "Returns true if `s` is specifically a `PowerSeries`, false otherwise."
  [s]
  (instance? PowerSeries s))

(defn- -make
  "Takes a series?-true object and returns the appropriate, more specific
  constructor."
  [s]
  (if (power-series? s)
    ->PowerSeries
    ->Series))

(defn- kind->make
  "Takes a keyword - either ::series or ::power-series - and returns the
  appropriate series constructor. Throws if neither of these are supplied."
  [kind]
  (case kind
    ::series ->Series
    ::power-series ->PowerSeries
    (u/illegal (str "Unsupported kind: " kind))))

(defn series*
  "Given a sequence, returns a new `Series` object that wraps that
  sequence (potentially padding its tail with zeros if it's finite)."
  [prefix]
  (->Series (i/->series prefix)))

(defn series
  "Return a `Series` starting with the supplied values. The remainder of the
  series will be filled with the zero-value corresponding to the first of the
  given values.

  If you have a sequence already, prefer `series*`"
  [& prefix]
  (series* prefix))

(defn power-series*
  "Given a sequence, returns a new `PowerSeries` object that wraps that
  sequence (potentially padding its tail with zeros if it's finite)."
  [prefix]
  (->PowerSeries (i/->series prefix)))

(defn power-series
  "Return a `PowerSeries` starting with the supplied values. The remainder of the
  series will be filled with the zero-value corresponding to the first of the
  given values.

  If you have a sequence already, prefer `power-series*`"
  [& prefix]
  (power-series* prefix))

;; ## Higher Level Constructors

(def zero (power-series* [0]))
(def one (power-series* [1]))
(def identity (power-series* [0 1]))

(defn constant
  ([c] (power-series* [c]))
  ([c kind] ((kind->make kind) (i/->series [c]))))

(defn generate
  "Returns a `PowerSeries` generated by (f i) for i in 0, 1, ..."
  ([f] (->PowerSeries (map f (range))))
  ([f kind]
   ((kind->make kind) (map f (range)))))

(defn ->function
  "Accepts a `Series` or `PowerSeries` and coerces the input to a `PowerSeries`
  without any application. Any other series will error."
  [s]
  (cond (power-series? s) s
        (series? s) (->PowerSeries (seq s))
        :else (u/illegal "non-series provided to ->function.")))

(defn partial-sums
  "Returns a series (of the same type as the input) of partial sums of the terms
  in the supplied series."
  [s]
  ((-make s) (reductions g/+ s)))

(defn fmap
  "Returns a new series generated by applying the supplied `f` to each element in
  the input series `s`.

  NOTE scmutils calls this `series:elementwise`."
  [f s]
  ((-make s) (map f s)))

(defn sum
  "Returns the sum of all elements in the input series `s` up to order
  `n` (inclusive). For example:

  (sum (series 1 1 1 1 1 1 1) 3)
  ;; => 4

  NOTE that `sum` sums the first `n + 1` terms, since series starts with an
  order 0 term."
  [s n]
  (transduce (take (inc n)) g/+ (seq s)))

(defn inflate
  "Accepts an input series `s` and an exponent `n`, and expands the series in the
  `n`th power of its argument. Every term `i` maps to position `i*n`, with zeros
  padded in the new missing slots.

  For example:

  (inflate identity 3)
  ;; => (series 0 0 0 1)

  (take 6 (inflate (generate inc) 3))
  ;; => (1 0 2 0 3 0)

  NOTE this operation makes sense as described for a `PowerSeries`, but still
  works with `Series` objects."
  [s n]
  (if (<= n 1)
    s
    (let [zero  (v/zero-like (first s))
          zeros (repeat (dec n) zero)]
      ((-make s)
       (->> (map cons s (repeat zeros))
            (apply concat))))))

;; TODO these next three ONLY make sense for power series!

#_
(defn compose [s t]
  ((-make s) (i/revert (seq s))))

#_
(defn revert [s]
  ((-make s) (i/revert (seq s))))

#_
(defn integrate [s]
  ((-make s) (i/integral (seq s))))


;; ## Series Wrappers

(def exp-series (->PowerSeries i/expx))
(def sin-series (->PowerSeries i/sinx))
(def cos-series (->PowerSeries i/cosx))
(def tan-series (->PowerSeries i/tanx))
(def sec-series (->PowerSeries i/secx))

(def asin-series (->PowerSeries i/asinx))
(def acos-series (->PowerSeries i/acosx))
(def atan-series (->PowerSeries i/atanx))
(def acot-series (->PowerSeries i/acotx))

(def sinh-series (->PowerSeries i/sinhx))
(def cosh-series (->PowerSeries i/coshx))
(def tanh-series (->PowerSeries i/tanhx))
(def asinh-series (->PowerSeries i/asinhx))
(def atanh-series (->PowerSeries i/atanhx))

(def log1+x-series (->PowerSeries i/log1+x))
(def log1-x-series (->PowerSeries i/log1-x))

(defn binomial-series [n]
  (->PowerSeries (i/binomial n)))

;; ## Series (vs PowerSeries)

(def fib-series (->Series i/fib))
(def catalan-series (->Series i/catalan))

;; ## Generic Implementations
;;
;; Next, we implement all generic operations for `Series` and `PowerSeries`. A
;; key idea here is that all "coefficients" of a series must be some kind
;; derived from `::coseries`. This is /not/ true in scmutils; in that library,
;; anything that responds false to `series?` is game for interaction with series
;; objects.
;;
;; NOTE This might be the right way to go. Feel free to experiment.

(derive ::x/numerical-expression ::coseries)

;; All generic methods use the sequence-based operations we defined above on the
;; series wrapped by each sequence, then packages the series back up.

(doseq [[ctor kind] [[->Series ::series]
                     [->PowerSeries ::power-series]]]
  (defmethod g/add [kind kind] [s t]
    (ctor (i/seq:+ (seq s) (seq t))))

  (defmethod g/add [::coseries kind] [c s]
    (ctor (i/c+seq c (seq s))))

  (defmethod g/add [kind ::coseries] [s c]
    (ctor (i/seq+c (seq s) c)))

  (defmethod g/negate [kind] [s]
    (ctor (i/negate (seq s))))

  (defmethod g/sub [kind kind] [s t]
    (ctor (i/seq:- (seq s) (seq t))))

  (defmethod g/sub [::coseries kind] [c s]
    (ctor (i/c-seq c (seq s))))

  (defmethod g/sub [kind ::coseries] [s c]
    (ctor (i/seq-c (seq s) c)))

  (defmethod g/mul [kind kind] [s t]
    (ctor (i/seq:* (seq s) (seq t))))

  (defmethod g/mul [::coseries kind] [c s]
    (ctor (i/c*seq c (seq s))))

  (defmethod g/mul [kind ::coseries] [s c]
    (ctor (i/seq*c (seq s) c)))

  (defmethod g/square [kind] [s]
    (let [xs (seq s)]
      (ctor (i/seq:* xs xs))))

  (defmethod g/cube [kind] [s]
    (let [xs (seq s)]
      (ctor (i/seq:* (i/seq:* xs xs) xs))))

  (defmethod g/expt [kind ::v/native-integral] [s e]
    (ctor (i/expt (seq s) e)))

  (defmethod g/invert [kind] [s]
    (ctor (i/invert (seq s))))

  (defmethod g/div [::coseries kind] [c s]
    (ctor (i/c-div-seq c (seq s))))

  (defmethod g/div [kind ::coseries] [s c]
    (ctor (i/seq-div-c (seq s) c)))

  (defmethod g/div [kind kind] [s t]
    (ctor (i/div (seq s) (seq t))))

  (defmethod g/sqrt [kind] [s]
    (ctor (i/sqrt (seq s))))

  (defmethod g/simplify [kind] [s]
    (map g/simplify (seq s))))


;; TODO document, Power Series Only!

(defmethod g/exp [::power-series] [s]
  (->PowerSeries (i/compose i/expx (seq s))))

(defmethod g/cos [::power-series] [s]
  (->PowerSeries (i/compose i/cosx (seq s))))

(defmethod g/sin [::power-series] [s]
  (->PowerSeries (i/compose i/sinx (seq s))))

(defmethod g/tan [::power-series] [s]
  (->PowerSeries (i/compose i/tanx (seq s))))

(defmethod g/asin [::power-series] [s]
  (->PowerSeries (i/compose i/asinx (seq s))))

(defmethod g/acos [::power-series] [s]
  (->PowerSeries (i/compose i/acosx (seq s))))

(defmethod g/atan [::power-series] [s]
  (->PowerSeries (i/compose i/atanx (seq s))))

;; ## Derivatives

(defmethod g/partial-derivative [::series v/seqtype] [^Series s selectors]
  (->Series (map #(g/partial-derivative % selectors)
                 (.-xs s))))

(defmethod g/partial-derivative [::power-series v/seqtype] [^PowerSeries s selectors]
  (if (empty? selectors)
    (->PowerSeries (i/deriv (.-xs s)))
    (u/illegal
     (str "Cannot yet take partial derivatives of a power series: " s selectors))))
