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
  "Utilities for aggregating sequences."
  (:require [sicmutils.generic :as g]))

;; I learned about "Kahan's summation trick" from `rational.scm` in the
;; `scmutils` package, where it shows up in the `sigma` function.

(defn naive-fold
  ([] 0.0)
  ([x] x)
  ([acc x]
   (+ acc x)))

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
  ([[acc c]] (+ acc c))
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
  ([[acc cs ccs]]  (+ acc cs ccs))
  ([[acc cs ccs] x]
   ;; TODO see if I can reuse the fold above! TWO calls to fold above.
   (let [acc+x (+ acc x)
         ;; called `c` in the algo...
         delta (if (>= (Math/abs ^double acc)
                       (Math/abs ^double x))
                 (+ (- acc acc+x) x)
                 (+ (- x acc+x) acc))
         sum' acc+x ;; recur with this?
         cs+delta (+ cs delta)
         cc (if (>= (Math/abs ^double cs)
                    (Math/abs ^double delta))
              (+ (- cs cs+delta) delta)
              (+ (- delta cs+delta) cs))]
     [acc+x cs+delta (+ ccs cc)])))

(def ^{:dynamic true
       :doc "boom, defaults to [[kahan-fold]]."}
  *fold*
  kahan-babushka-neumaier-fold)

;; TODO note that any monoid works!

(defn sum
  "Sums either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

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
   (scanning-sum
    (map f (range low high)))))

(defn generic-sum
  "Sums either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  Using the generic [[sicmutils.generic/+]] function."
  ([xs]
   (apply g/+ xs))
  ([f low high]
   (transduce (map f) g/+ (range low high))))

(defn halt-at
  "Returns a transducer that ends transduction when `pred` (applied to the
  aggregation in progress) returns true for an aggregation step.

  NOTE: This transducer should come first in a chain of transducers; it only
  inspects the aggregate, never the value, so putting it first will prevent
  unnecessary transformations of values if the aggregate signals completion."
  [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result]
       (rf result))
      ([result input]
       (if (pred result)
         (reduced result)
         (rf result input))))))

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

  Accepts a binary aggregation function `plus` and an identity element `id` and
  returns a multi-arity function that will reduce its arguments via `plus`. A
  0-arity call returns `id`.

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
