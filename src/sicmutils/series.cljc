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

;; # Power Series
;;
;; This namespace contains an implementation of two data types:
;;
;; - `Series`, which represents a generic infinite series of arbitrary values, and
;; - `PowerSeries`, a series that represents a power series in a single
;;   variable; in other words, a series where the nth entry is interpreted as
;;   the coefficient of $x^n$:
;;
;; $$[a b c d ...] == $a + bx + cx^2 + dx^3 + ...$$
;;
;; Many of the functions below draw on the `sicmutils.series.impl` namespace,
;; which implements many of these operations on bare Clojure sequences.
;;
;; The implementation follows Doug McIlroy's beautiful paper, ["Power Series,
;; Power
;; Serious"](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).
;; Doug also has a 10-line version in Haskell on [his
;; website](https://www.cs.dartmouth.edu/~doug/powser.html).

(declare s-zero s-one s-identity series-value)

(deftype Series [xs]
  v/Value
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] s-zero)
  (one-like [_] s-one)

  ;; This is suspect, since [[Series]], unlike [[PowerSeries]], are general
  ;; infinite sequences and not necessarily interpreted as polynomials. This
  ;; decision follows `scmutils` convention.
  (identity-like [_] s-identity)

  (numerical? [_] false)
  (exact? [_] false)
  (freeze [_]
    (let [prefix (g/simplify (take 4 xs))]
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
            (Series. (series-value xs [])))
    (invoke [_ a]
            (Series. (series-value xs [a])))
    (invoke [_ a b]
            (Series. (series-value xs [a b])))
    (invoke [_ a b c]
            (Series. (series-value xs [a b c])))
    (invoke [_ a b c d]
            (Series. (series-value xs [a b c d])))
    (invoke [_ a b c d e]
            (Series. (series-value xs [a b c d e])))
    (invoke [_ a b c d e f]
            (Series. (series-value xs [a b c d e f])))
    (invoke [_ a b c d e f g]
            (Series. (series-value xs [a b c d e f g])))
    (invoke [_ a b c d e f g h]
            (Series. (series-value xs [a b c d e f g h])))
    (invoke [_ a b c d e f g h i]
            (Series. (series-value xs [a b c d e f g h i])))
    (invoke [_ a b c d e f g h i j]
            (Series. (series-value xs [a b c d e f g h i j])))
    (invoke [_ a b c d e f g h i j k]
            (Series. (series-value xs [a b c d e f g h i j k])))
    (invoke [_ a b c d e f g h i j k l]
            (Series. (series-value xs [a b c d e f g h i j k l])))
    (invoke [_ a b c d e f g h i j k l m]
            (Series. (series-value xs [a b c d e f g h i j k l m])))
    (invoke [_ a b c d e f g h i j k l m n]
            (Series. (series-value xs [a b c d e f g h i j k l m n])))
    (invoke [_ a b c d e f g h i j k l m n o]
            (Series. (series-value xs [a b c d e f g h i j k l m n o])))
    (invoke [_ a b c d e f g h i j k l m n o p]
            (Series. (series-value xs [a b c d e f g h i j k l m n o p])))
    (invoke [_ a b c d e f g h i j k l m n o p q]
            (Series. (series-value xs [a b c d e f g h i j k l m n o p q])))
    (invoke [_ a b c d e f g h i j k l m n o p q r]
            (Series. (series-value xs [a b c d e f g h i j k l m n o p q r])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s]
            (Series. (series-value xs [a b c d e f g h i j k l m n o p q r s])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t]
            (Series. (series-value xs [a b c d e f g h i j k l m n o p q r s t])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
            (Series. (series-value xs (concat [a b c d e f g h i j k l m n o p q r s t] rest))))
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
             (Series. (series-value xs [])))
    (-invoke [_ a]
             (Series. (series-value xs [a])))
    (-invoke [_ a b]
             (Series. (series-value xs [a b])))
    (-invoke [_ a b c]
             (Series. (series-value xs [a b c])))
    (-invoke [_ a b c d]
             (Series. (series-value xs [a b c d])))
    (-invoke [_ a b c d e]
             (Series. (series-value xs [a b c d e])))
    (-invoke [_ a b c d e f]
             (Series. (series-value xs [a b c d e f])))
    (-invoke [_ a b c d e f g]
             (Series. (series-value xs [a b c d e f g])))
    (-invoke [_ a b c d e f g h]
             (Series. (series-value xs [a b c d e f g h])))
    (-invoke [_ a b c d e f g h i]
             (Series. (series-value xs [a b c d e f g h i])))
    (-invoke [_ a b c d e f g h i j]
             (Series. (series-value xs [a b c d e f g h i j])))
    (-invoke [_ a b c d e f g h i j k]
             (Series. (series-value xs [a b c d e f g h i j k])))
    (-invoke [_ a b c d e f g h i j k l]
             (Series. (series-value xs [a b c d e f g h i j k l])))
    (-invoke [_ a b c d e f g h i j k l m]
             (Series. (series-value xs [a b c d e f g h i j k l m])))
    (-invoke [_ a b c d e f g h i j k l m n]
             (Series. (series-value xs [a b c d e f g h i j k l m n])))
    (-invoke [_ a b c d e f g h i j k l m n o]
             (Series. (series-value xs [a b c d e f g h i j k l m n o])))
    (-invoke [_ a b c d e f g h i j k l m n o p]
             (Series. (series-value xs [a b c d e f g h i j k l m n o p])))
    (-invoke [_ a b c d e f g h i j k l m n o p q]
             (Series. (series-value xs [a b c d e f g h i j k l m n o p q])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r]
             (Series. (series-value xs [a b c d e f g h i j k l m n o p q r])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s]
             (Series. (series-value xs [a b c d e f g h i j k l m n o p q r s])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
             (Series. (series-value xs [a b c d e f g h i j k l m n o p q r s t])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
             (Series. (series-value xs (concat [a b c d e f g h i j k l m n o p q r s t] rest))))]))

#?(:clj
   (defmethod print-method Series [^Series s ^java.io.Writer w]
     (.write w (str "#object[sicmutils.series.Series \""
                    (.toString s)
                    "\"]"))))

;; ### Power Series
;;
;; The primary difference from `Series` is the `IFn` implementation; application
;; of a power series multiples each coefficient by a successively larger power
;; of its (single, for now) argument.
;;
;; TODO Modify this description once we implement multivariable power series!

(declare zero one identity power-series-value)

(deftype PowerSeries [xs]
  v/Value
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] zero)
  (one-like [_] one)
  (identity-like [_] identity)
  (numerical? [_] false)
  (exact? [_] false)
  (freeze [_]
    (let [prefix (g/simplify (take 4 xs))]
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
    (invoke [_ a] (Series. (power-series-value xs a)))
    (applyTo [s xs] (AFn/applyToHelper s xs))]

   :cljs
   [ISequential

    ISeqable
    (-seq [_] xs)

    IFn
    (-invoke [_ a] (Series. (power-series-value xs a)))

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

;; ## Constructors

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

(def ^:private s-zero (series* [0]))
(def ^:private s-one (series* [1]))
(def ^:private s-identity (series* [0 1]))

;; These exposed objects are `PowerSeries` instances, because the concepts of
;; zero, one and identity don't make sense unless you interpret these as
;; coefficients on a power series.

(def zero (power-series* [0]))
(def one (power-series* [1]))
(def identity (power-series* [0 1]))

(defn constant
  "Returns a `PowerSeries` representing the supplied constant term.

  Optionally, pass `kind` of either `::series` or `::power-series` to specify
  the type of series returned."
  ([c] (power-series* [c]))
  ([c kind] ((kind->make kind) (i/->series [c]))))

(defn xpow
  "Returns a `PowerSeries` instance representing $x^n$."
  [n]
  {:pre [(>= n 0)]}
  (power-series* (concat (repeat n 0) [1])))

(defn generate
  "Returns a `PowerSeries` generated by (f i) for i in 0, 1, ...

  Optionally, pass `kind` of either `::series` or `::power-series` to specify
  the type of series returned."
  ([f] (->PowerSeries (map f (range))))
  ([f kind]
   ((kind->make kind) (map f (range)))))

(defn ->function
  "Accepts a `Series` or `PowerSeries` and coerces the input to a `PowerSeries`
  without any application. Returns the coerced `PowerSeries` instance.

  Supplying a non-series will throw."
  [s]
  (cond (power-series? s) s
        (series? s) (->PowerSeries (seq s))
        :else (u/illegal "non-series provided to ->function.")))

;; ## Application
;;
;; Given some power series $F$, we can "apply" the series to a value $x$ by
;; multiplying each entry $f_n$ by $x^n$:

(defn- power-series-value
  "Evaluates the power series, and converts it back down to a normal series."
  [f x]
  (let [one    (v/one-like x)
        powers (iterate #(g/* x %) one)]
    (map g/* f powers)))

;; Once a `PowerSeries` has been applied, it becomes a `Series`.
;;
;; What does it mean to apply a `Series`? The concept only makes sense if the
;; series contains "applicables", or objects that can act as functions
;; themselves.
;;
;; If it does, then application of a series to some argument list `xs` means
;; applying each series element to `xs`.
;;
;; One further wrinkle occurs if the applicable in some position returns a
;; series. `value` should combine all of these resulting series, with each
;; series shifted by its initial position in the first series.
;; Concretely, suppose that $F$ has the form:
;;
;; $$(x => (A1, A2, A3, ...), x => (B1, B2, B3, ...) x => (C1, C2, C3, ...), ...)$$

;; Then, this series applied to x should yield the series of values
;; (A1, (+ A2 B1), (+ A3 B2 C1), ...)
;;
;; Here's the implementation:

(defn- series-value [f xs]
  (letfn [(collect [f]
            (let [result (apply (first f) xs)]
              (if (series? result)
                (lazy-seq
                 (let [[r & r-tail] result]
                   (cons r (i/seq:+ r-tail (collect (rest f))))))

                ;; note that we have already realized first-result,
                ;; so it does not need to be behind lazy-seq.
                (cons result (lazy-seq (collect (rest f)))))))]
    (collect (seq f))))

(defn value
  "Returns the value of the supplied `Series` or `PowerSeries` applied to `xs`.

  If a `PowerSeries` is supplied, `xs` (despite its name) must be a single
  value. Returns a `Series` generated by multiplying each `i`th term in `s` by
  $x^i$, where $x$ is the `xs` argument.

  If a `Series` is supplied:

  Assumes that S is a series of applicables of arity equal to the count of `xs`.
  If, in fact, S is a series of series-valued applicables, then the result will
  be a sort of layered sum of the values.

  Concretely, suppose that S has the form:

    [x => [A1 A2 A3...], x => [B1 B2 B3...], x => [C1 C2 C3...], ...]

  Then, this series applied to x will yield the new series:

    [A1 (+ A2 B1) (+ A3 B2 C1) ...]

  The way to think about this is, that if a power series has some other series
  as the coefficient of the $x^n$ term, the series must shift by $n$ positions
  before being added into the final total."
  [s xs]
  (cond (power-series? s) (power-series-value s xs)
        (series? s) (series-value s xs)
        :else
        (u/illegal (str "value only works on `Series` or `PowerSeries`; received " s))))

(defn fmap
  "Returns a new series generated by applying the supplied `f` to each element in
  the input series `s`. The returned series will be the same type as the input
  series, either `Series` or `PowerSeries`.

  NOTE scmutils calls this `series:elementwise`."
  [f s]
  ((-make s) (map f s)))

(defn inflate
  "Accepts an input series `s` and an exponent `n`, and expands the series in the
  `n`th power of its argument. Every term `i` maps to position `i*n`, with zeros
  padded in the new missing slots.

  For example:

  (inflate identity 3)
  ;; => (series 0 0 0 1)

  (take 6 (inflate (generate inc) 3))
  ;; => (1 0 2 0 3 0)

  NOTE this operation makes sense as described for a `PowerSeries`, where each
  entry represents the coefficient of some power of `x`; functionally it still
  works with `Series` objects."
  [s n]
  (if (<= n 1)
    s
    (let [zero  (v/zero-like (first s))
          zeros (repeat (dec n) zero)]
      ((-make s)
       (->> (map cons s (repeat zeros))
            (apply concat))))))

(defn partial-sums
  "Returns a series (of the same type as the input) of partial sums of the terms
  in the supplied series `s`."
  [s]
  ((-make s) (reductions g/+ s)))

(defn sum
  "Returns the sum of all elements in the input series `s` up to order
  `n` (inclusive). For example:

  (sum (series 1 1 1 1 1 1 1) 3)
  ;; => 4

  NOTE that `sum` sums the first `n + 1` terms, since series starts with an
  order 0 term."
  [s n]
  (transduce (take (inc n)) g/+ (seq s)))

;; ## Power Series Specific Functions

(defn compose
  "Returns a new `PowerSeries` $U$ that represents the composition of the two
  input power series $S$ and $T$, where $U$ evaluates like:

  $$U(x) = S(T(x))$$"
  [s t]
  {:pre [(power-series? s)
         (power-series? t)]}
  (->PowerSeries
   (i/compose (seq s) (seq t))))

(defn revert
  "Returns a new `PowerSeries` $U$ that represents the compositional inverse (the
  'reversion') of the input power series $S$, satisfying:

  $$S(U(x)) = x$$"
  [s]
  {:pre [(power-series? s)]}
  (->PowerSeries (i/revert (seq s))))

(defn integral
  "Returns a `PowerSeries` $U$ that represents the definite integral of the input
  power series $S$ with constant term $c$:

  $$U = c + \\int_0^{\\infty} S$$"
  ([s] (integral s 0))
  ([s constant]
   {:pre [(power-series? s)]}
   (->PowerSeries (i/integral (seq s) constant))))

;; ## Built In Series
;;
;; The following section defines a number of built in series that come up often
;; enough to be included. There are, of course, far more! Please feel free to
;; open a PR if you have a series you think should be included.

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

(defn binomial-series
  "Returns a `PowerSeries` instance representing a
  [Binomial series](https://en.wikipedia.org/wiki/Binomial_series), ie, the
  taylor series of the function $f$ given by

  $$f(x) = (1 + x)^\\alpha$$"
  [alpha]
  (->PowerSeries (i/binomial alpha)))

;; ## Series (vs PowerSeries)
;;
;; These are interesting sequences, not taylor series, but nice to have in a
;; library like SICMUtils.

(def fib-series (->Series i/fib))
(def catalan-series (->Series i/catalan))

;; ## Generic Implementations
;;
;; This section installs `Series` and `PowerSeries` into the SICMUtils generic
;; arithmetic system.
;;
;; A key idea here is that all "coefficients" of a series must be some kind
;; derived from `::coseries`. This is /not/ true in the Scheme scmutils library;
;; in that library, anything that responds false to `series?` is game for
;; interaction with series objects.
;;
;; NOTE This might be the right way to go. Feel free to experiment.

(derive ::x/numeric ::coseries)
(derive ::v/number ::coseries)
(derive ::v/function ::coseries)

;; All generic methods:
;;
;; - unwrap the supplied series instances,
;; - operate directly on their backing sequences
;; - repackage the result using the appropriate constructor
;;
;; This section does /not/ define methods that coerce `Series` => `PowerSeries`
;; or vice versa. Users should do this explicitly.

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


;; ## Power Series Generic Extensions
;;
;; A `PowerSeries` is a single variable function; we extend the following
;; methods to `PowerSeries` in the same style as they're extended for functions.
;; Each of the following act like function composition, and compose their
;; operation with the function represented by the `PowerSeries`.
;;
;; If `s` is a `PowerSeries` that applies as `(s x)`, `(g/exp s)` returns a
;; series that represents `(g/exp (s x))`.

(defmethod g/exp [::power-series] [s]
  (->PowerSeries (i/compose i/expx (seq s))))

(defmethod g/cos [::power-series] [s]
  (->PowerSeries (i/compose i/cosx (seq s))))

(defmethod g/sin [::power-series] [s]
  (->PowerSeries (i/compose i/sinx (seq s))))

(defmethod g/tan [::power-series] [s]
  (->PowerSeries (i/compose i/tanx (seq s))))

(defmethod g/sec [::power-series] [s]
  (->PowerSeries (i/compose i/secx (seq s))))

(defmethod g/asin [::power-series] [s]
  (->PowerSeries (i/compose i/asinx (seq s))))

(defmethod g/acos [::power-series] [s]
  (->PowerSeries (i/compose i/acosx (seq s))))

(defmethod g/atan [::power-series] [s]
  (->PowerSeries (i/compose i/atanx (seq s))))

(defmethod g/cosh [::power-series] [s]
  (->PowerSeries (i/compose i/coshx (seq s))))

(defmethod g/sinh [::power-series] [s]
  (->PowerSeries (i/compose i/sinhx (seq s))))

(defmethod g/tanh [::power-series] [s]
  (->PowerSeries (i/compose i/tanhx (seq s))))

(defmethod g/asinh [::power-series] [s]
  (->PowerSeries (i/compose i/asinhx (seq s))))

(defmethod g/atanh [::power-series] [s]
  (->PowerSeries (i/compose i/atanhx (seq s))))

;; ## Derivatives
;;
;; For a `Series`, the derivative operation assumes that the series contains
;; applicables that can take their own partial derivatives.

(defmethod g/partial-derivative [::series v/seqtype] [^Series s selectors]
  (->Series (map #(g/partial-derivative % selectors)
                 (.-xs s))))

;; A `PowerSeries` is itself a single-variable function, so
;; `g/partial-derivative` simply takes the series derivative of the contained
;; sequence.

(defmethod g/partial-derivative [::power-series v/seqtype] [^PowerSeries s selectors]
  (if (empty? selectors)
    (->PowerSeries (i/deriv (.-xs s)))
    (u/illegal
     (str "Cannot yet take partial derivatives of a power series: " s selectors))))
