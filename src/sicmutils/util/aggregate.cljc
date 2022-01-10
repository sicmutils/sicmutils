;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.util.aggregate
  "Namespace with algorithms for aggregating sequences in various ways.

  Contains a number of algorithms for [compensated
  summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) of
  floating-point numbers."
  (:require [sicmutils.generic :as g]))

;; ## Summing Sequences of Numbers
;;
;; This is a fascinating topic, and my explorations have not yet done it
;; justice. This namespace starts with a number of functions designed to sum up
;; sequences of numbers in an extensible way.
;;
;; Much of the numerical physics simulation code in the library (everything
;; in [[sicmutils.numerical.quadrature]]) depends on the ability to sum up lists
;; of floating point numbers without the accumulation of error due to the
;; machine representation of the number.
;;
;; Here is the naive way to add up a list of numbers:

#_
(defn naive-sum [xs]
  (apply g/+ xs))

;; Simple! But watch it break:

(comment
  ;; This should be 1.0...
  (= 0.9999999999999999
     (naive-sum [1.0 1e-8 -1e-8])))

;; Algorithms called ['compensated
;; summation'](https://en.wikipedia.org/wiki/Kahan_summation_algorithm)
;; algorithms are the way around this problem. Instead of simply accumulating
;; the numbers as they come, compensated summation keeps track of the piece of
;; the sum that would get erased from the sum due to lack of precision.
;;
;; Because there are a few ways to do this, I chose to make the implementation
;; pluggable by following the `Aggregator` pattern
;; from [Algebird](https://github.com/twitter/algebird/blob/develop/algebird-core/src/main/scala/com/twitter/algebird/Aggregator.scala),
;;
;; In sicmutils I've decided to call this abstraction a "fold". I am here
;;
;; A fold is a combination of:
;;
;; - some initial value into which you want to aggregate
;; - a combining function of (accumulator, x) => accumulator
;; - a "present" function that converts the accumulator into a final value
;;
;; TODO continue from here... TODO this IS a fold in Algebird, not aggregator!
;;
;; TODO also note that I did the same thing for polynomial and rational function
;; interpolation, same style!! And rewrite those namespaces to use the same
;; thing? Benchmark?

;; I learned about "Kahan's summation trick" from `rational.scm` in the
;; `scmutils` package, where it shows up in the `sigma` function.

(defn naive-fold
  ([] 0.0)
  ([x] x)
  ([acc x]
   (+ acc x)))

;; TODO note that any monoid works!
;;
;; TODO
;;
;; - make a thing that can generate these.
;; - Dynamically bind it with nice metadata
;; - see if we can make
;;
;; TODO move these down after.

(defn fold->sum-fn
  ([fold]
   (fold->sum-fn fold fold fold))
  ([fold present]
   (fold->sum-fn fold fold present))
  ([init fold present]
   (fn ([xs]
       (present
        (reduce fold (init) xs)))
     ([f low high]
      (let [xs (range low high)]
        (transduce (map f) fold xs))))))

;; TODO is this good? check arities...

(defn fold->scan-fn
  ([fold]
   (fold->scan-fn fold fold fold))
  ([fold present]
   (fold->scan-fn fold fold present))
  ([init fold present]
   (fn scan
     ([xs]
      (->> (reductions fold (init) xs)
           (map present)
           (rest)))
     ([f low high]
      (scan
       (map f (range low high)))))))

(def ^{:doc "Sums either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  Using the generic [[sicmutils.generic/+]] function."
       :arglists '([xs], [f low high])}
  generic-sum
  (fold->sum-fn g/+))

;; ## Compensated Summation Folds

(defn kahan-fold
  "Implements a fold that tracks the summation of a sequence of floating point
  numbers, using Kahan's trick for maintaining stability in the face of
  accumulating floating point errors.

  - the 0-arity returns an initial accumulator
  - the 1-arity version takes an accumulator and returns a final value
  - the 2-arity version takes an accumulator and a new value and folds the value
    into the accumulator, returning a new accumulator.

  Because of this implementation, [[kahan-fold]] is suitable for use
  with [[clojure.core/transduce]]."
  ([] [0.0 0.0])
  ([[acc _]] acc)
  ([[acc c] x]
   (let [y (- x c)
         t (+ acc y)]
     [t (- (- t acc) y)])))

(defn kahan-babushka-neumaier-fold
  "Implements a fold that tracks the summation of a sequence of floating point
  numbers, using TODO update docs!!

  - the 0-arity returns an initial accumulator
  - the 1-arity version takes an accumulator and returns a final value
  - the 2-arity version takes an accumulator and a new value and folds the value
    into the accumulator, returning a new accumulator.

  Because of this implementation, [[kbn-fold]] is suitable for use
  with [[clojure.core/transduce]]."
  ([] [0.0 0.0])
  ([acc] (reduce + acc))
  ([[acc c] x]
   (let [acc+x (+ acc x)
         delta (if (>= (Math/abs ^double acc)
                       (Math/abs ^double x))
                 ;; If sum is bigger, low-order digits of `x` are lost.
                 (+ (- acc acc+x) x)

                 ;; else, low-order digits of `sum` are lost.
                 (+ (- x acc+x) acc))]
     [acc+x (+ c delta)])))

(def ^{:doc "Shorter alias for [[kahan-babushka-neumaier-fold]]."}
  kbn-fold
  kahan-babushka-neumaier-fold)

(defn kahan-babushka-klein-fold
  "Klein: https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements"
  ([] [0.0 0.0 0.0])
  ([acc] (reduce + acc))
  ([[acc cs ccs] x]
   (let [acc+x (+ acc x)
         ;; called `c` in the algo...
         delta (if (>= (Math/abs ^double acc)
                       (Math/abs ^double x))
                 (+ (- acc acc+x) x)
                 (+ (- x acc+x) acc))
         cs+delta (+ cs delta)
         cc (if (>= (Math/abs ^double cs)
                    (Math/abs ^double delta))
              (+ (- cs cs+delta) delta)
              (+ (- delta cs+delta) cs))]
     [acc+x cs+delta (+ ccs cc)])))

;; ## Note that this is a pattern...

;; reference from wiki pointed here:
;; https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.582.288&rep=rep1&type=pdf

;; ### Macro Version, turbo

(defn- klein-term [acc delta]
  `[sum# (+ ~acc ~delta)
    ~delta (if (>= (Math/abs ~(with-meta acc {:tag 'double}))
                   (Math/abs ~(with-meta delta {:tag 'double})))
             (+ (- ~acc sum#) ~delta)
             (+ (- ~delta sum#) ~acc))
    ~acc sum#])

(defn- kbk-foldn-body
  "Split out from the macro so we can use it to build SCI macros."
  [n]
  (let [syms   (into [] (repeatedly (inc n) gensym))
        prefix (pop syms)
        final  (peek syms)
        delta  (gensym)]
    `[([] [~@(repeat (inc n) 0.0)])
      ([accs#] (reduce + accs#))
      ([~syms ~delta]
       (let [~@(mapcat #(klein-term % delta) prefix)]
         [~@prefix (+ ~final ~delta)]))]))

;; TODO docs, implement in SCI.

(defmacro kbk-foldn [n]
  `(fn ~@(kbk-foldn-body n)))

;; ## Dynamically Bindable Summing

(def ^{:dynamic true
       :doc "boom, defaults to [[kahan-babushka-neumaier-fold]]."}
  *fold*
  kahan-babushka-neumaier-fold)

(defn sum
  "Sums either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  TODO fix, no longer necessarily using Kahan, note that this is using the
  dynamically bound `*fold*`.

  Using Kahan's summation trick behind the scenes to keep floating point errors
  under control.

  TODO update the docs, say that we use whatever is bound to `*fold*`."
  ([xs]
   (*fold*
    (reduce *fold* (*fold*) xs)))
  ([f low high]
   (let [xs (range low high)]
     (transduce (map f) *fold* xs))))

(defn scanning-sum
  "Returns every intermediate summation from summing either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  Using Kahan's summation trick behind the scenes to keep floating point errors
  under control."
  ([xs]
   (->> (reductions *fold* (*fold*) xs)
        (map *fold*)
        (rest)))
  ([f low high]
   (sum
    (map f (range low high)))))

;; ## Pairwise Summation

;; matches Julia's
(def ^:dynamic *cutoff* 128)

;; TODO https://hackage.haskell.org/package/math-functions-0.3.4.2/docs/src/Numeric.Sum.html#pairwiseSum

;; from that haskell code:

;; -- This approach is perhaps 10% faster than 'KBNSum', but has poorer
;; -- bounds on its error growth.  Instead of having roughly constant
;; -- error regardless of the size of the input vector, in the worst case
;; -- its accumulated error grows with /O(log n)/.

;; TODO add notes that we can combine this idea with the fold idea if someone
;; can figure out page 7 here
;; https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.582.288&rep=rep1&type=pdf
;;
;; add notes from wiki about pairwise sum error bounds.

(defn pairwise-sum
  "https://en.wikipedia.org/wiki/Pairwise_summation

  this works because we are not adding a progressively bigger sum to deltas, but
  adding numbers of same size."
  ([xs]
   (letfn [(f [v]
             (let [n (count v)]
               (if (<= n *cutoff*)
                 (reduce + v)
                 (let [split-idx (bit-shift-right n 1)
                       l (subvec v 0 split-idx)
                       r (subvec v split-idx)]
                   (+ (f l)
                      (f r))))))]
     (f (if (vector? xs)
          xs
          (into [] xs)))))
  ([f low high]
   (pairwise-sum
    (mapv f (range low high)))))

;; ## Monoids

(defn- combiner
  "If `stop?` is false, returns `f`. Else, returns a binary reducing function that
  returns a `reduced` value if its left argument returns `true` for `stop?`,
  else aggregates with `f`."
  [f stop?]
  (if stop?
    (fn [l r]
      (if (stop? l)
        (reduced l)
        (f l r)))
    f))

(defn monoid
  "Accepts a binary (associative) aggregation function `plus` and an identity
  element `id` and returns a multi-arity function that will combine its
  arguments via `plus`. A 0-arity call returns `id`.

  optionally takes an `annihilate?` function that should return true for any `x`
  such that `(plus x <any>) == x`.

  If the `annihilate?` function is supplied, then if the aggregation produces a
  value that returns `(annihilate? true)` at any point, the reduction will
  return immediately."
  ([plus id]
   (monoid plus id nil))
  ([plus id annihilate?]
   (let [acc (combiner plus annihilate?)]
     (fn
       ([] id)
       ([x] x)
       ([x y] (plus x y))
       ([x y & more]
        (reduce acc x (cons y more)))))))

(defn group
  "Similar to [[monoid]] for types with invertible elements. Accepts:

  - binary `minus` and (associative) `plus` functions
  - a unary `negate` function
  - an element `id` that obeys `(plus id other) == (plus other id) == other`
  - optionally, an `annihilate?` function that should return true for any `x`
    such that `(plus x <any>) == x`.

  ;; TODO fill in actual docs about how this returns a function that will do
  subtraction.

  If the `annihilate?` function is supplied, then if the aggregation produces a
  value that returns `(annihilate? true)` at any point, the reduction will
  return immediately."
  ([minus plus invert id]
   (group minus plus invert id nil))
  ([minus plus invert id annihilate?]
   (let [acc (combiner plus annihilate?)]
     (fn
       ([] id)
       ([x] (invert x))
       ([x y] (minus x y))
       ([x y & more]
        (minus x (reduce acc y more)))))))

;; ## TODO Describe

(defn merge-fn
  "NOTE that the returned function recurs on increasing indices internally instead
  of walking through the lists directly. This method of traversing vectors is
  more efficient, and this function is called so often that the performance gain
  is worth it, and reads almost like the explicit sequence traversal."
  [compare add zero? make]
  (fn
    ([] [])
    ([xs] xs)
    ([xs ys]
     (loop [i (long 0)
            j (long 0)
            result (transient [])]
       (let [x (nth xs i nil)
             y (nth ys j nil)]
         (cond (not x) (into (persistent! result) (subvec ys j))
               (not y) (into (persistent! result) (subvec xs i))
               :else (let [[x-tags x-coef] x
                           [y-tags y-coef] y
                           compare-flag (compare x-tags y-tags)]
                       (cond
                         ;; If the terms have the same tag set, add the coefficients
                         ;; together. Include the term in the result only if the new
                         ;; coefficient is non-zero.
                         (zero? compare-flag)
                         (let [sum (add x-coef y-coef)]
                           (recur (inc i)
                                  (inc j)
                                  (if (zero? sum)
                                    result
                                    (conj! result (make x-tags sum)))))

                         ;; Else, pass the smaller term on unchanged and proceed.
                         (neg? compare-flag)
                         (recur (inc i) j (conj! result x))

                         :else
                         (recur i (inc j) (conj! result y))))))))))
