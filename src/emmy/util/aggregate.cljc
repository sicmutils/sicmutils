#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.aggregate
  "Namespace with algorithms for aggregating sequences in various ways."
  (:require [emmy.algebra.fold :as af]
            [emmy.generic :as g]))

(def ^{:doc "Sums either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  Using the generic [[emmy.generic/+]] function."
       :arglists '([xs], [f low high])}
  generic-sum
  (af/fold->sum-fn g/+))

;; ## Dynamically Bindable Summation
;;
;; The following functions allow for fold-agnostic summation. If a user
;; calls [[sum]] or [[scan]] in some function, they can later tune the way
;; summations happen deep in their code by binding [[*fold*]].
;;
;; For summation functions with explicit folds baked in,
;; see [[emmy.algebra.fold/fold->sum-fn]]
;; and [[emmy.algebra.fold/fold->scan-fn]].

(def
  ^{:dynamic true
    :doc "Fold used to aggregate values encountered by [[sum]] or [[fold]].

  Rebind this value to change the behavior of [[sum]] or [[fold]].

  Defaults to [[emmy.algebra.fold/kahan-babushka-neumaier-fold]]."}
  *fold*
  af/kahan-babushka-neumaier)

;; The following two functions are identical
;; to [[emmy.algebra.fold/fold->sum-fn]]
;; and [[emmy.algebra.fold/fold->scan-fn]] with [[*fold*]] baked in. I did
;; this for performance reasons; to use those functions and keep [[*fold*]]
;; dynamically rebindable I would have to pass the var `#'*fold*`.
;; De-referencing this var has a performance penalty that is not acceptable,
;; given that [[sum]] and [[scan]] are used in the inner loops of numerical
;; integration routines.

(defn sum
  "Takes either:

  - a series `xs` of numbers
  - A transformation function `f`, an inclusive-lower bound `low` and
    exclusive-upper bound `upper`

  And returns the result of aggregating either `xs` or `(map f (range low
  high))` using the fold dynamically bound to [[*fold*]].

  Use `binding` to substitute in a different fold:

  ```clj
  (require '[emmy.algebra.fold :as af])

  (binding [*fold* (af/join af/kahan af/min af/max)]
    (sum inc 0 10))
  ;;=> [55.0 1 10]
  ```"
  ([xs]
   (*fold*
    (reduce *fold* (*fold*) xs)))
  ([f low high]
   (let [xs (range low high)]
     (transduce (map f) *fold* xs))))

(defn scan
  "Takes either:

  - a series `xs` of numbers
  - A transformation function `f`, an inclusive-lower bound `low` and
    exclusive-upper bound `upper`

  And returns a lazy sequence of all intermediate values seen while aggregating
  either `xs` or `(map f (range low high))` using the fold dynamically bound
  to [[*fold*]].

  Use `binding` to substitute in a different fold:

  ```clj
  (require '[emmy.algebra.fold :as af])

  (binding [*fold* (af/join af/kahan af/min af/max)]
    (scan inc 0 3))
  ;;=> ([1.0 1 1] [3.0 1 2] [6.0 1 3])
  ```"
  ([xs]
   (->> (reductions *fold* (*fold*) xs)
        (rest)
        (map *fold*)))
  ([f low high]
   (scan
    (map f (range low high)))))

;; ## Pairwise Summation
;;
;; In contrast to [compensated summation
;; algorithms](https://en.wikipedia.org/wiki/Kahan_summation_algorithm), simply
;; adding up a vector of numbers recursively in pairs can offer great error
;; reduction with good performance.
;;
;; Adding up numbers from left to right usually means that as the accumulated
;; sum grows, you are adding the smaller numbers in the sequence into a
;; progressively larger total.
;;
;; By pairwise-adding up numbers recursively, you are more likely to be adding
;; numbers of similar magnitude together.
;;
;; [[*cutoff*]] lets you tune your "leaf size". A value of `1` would send you
;; all the way down to each single value; the default of 128, taken from Julia's
;; default, means that when you get to 128 elements or fewer you defer to naive
;; summation.

(def ^{:dynamic true
       :doc "Dynamically bindable size below which [[pairwise-sum]] will defer
  to [[sum]] to aggregate values."}
  *cutoff*
  128)

;; The [[pairwise-sum]] implementation below was inspired by the `pairwiseSum`
;; implementation in
;; the [`math-functions`](https://hackage.haskell.org/package/math-functions-0.3.4.2/docs/src/Numeric.Sum.html#pairwiseSum)
;; Haskell package. Those docs state:
;;
;; -- This approach is perhaps 10% faster than 'KBNSum', but has poorer
;; -- bounds on its error growth.  Instead of having roughly constant
;; -- error regardless of the size of the input vector, in the worst case
;; -- its accumulated error grows with /O(log n)/.
;;
;; NOTE that it might be interesting to combine [[pairwise-sum]] with the
;; compensated summation algorithms from [[emmy.algebra.fold]]. Page 7 of
;; this [paper by
;; Klein](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.582.288&rep=rep1&type=pdf)
;; seems to describe a way to aggregate the intermediate states of compensated
;; summation up through the binary addition tree (ie, implement a monoid on the
;; compensated summation state itself); but I couldn't figure out the indices!

(defn pairwise-sum
  "Given a vector of numbers, returns the [pairwise
  summation](https://en.wikipedia.org/wiki/Pairwise_summation) of the vector
  generated by arranging the vector into a binary tree and summing leaves
  together all the way up to the root.

  If `xs` is /not/ a vector, [[pairwise-sum]] will realize all elements into a
  vector before operating.

  If the initial vector, or some recursive slice, reaches a count
  <= [[*cutoff*]], [[pairwise-sum]] defers to `(reduce + xs)`.

  ### Performance Discussion

  [[pairwise-sum]] is perhaps 10% faster than [[sum]]
  with [[emmy.algebra.fold/kbn]] bound to [[*fold*]], but has poorer bounds
  on its error growth. Instead of having roughly constant error regardless of
  the size of the input, in the worst case its accumulated error grows with
  $O(\\log n)$.

  This improvement is due to the fact that [[pairwise-sum]] tends to add up
  numbers of similar magnitude, instead of adding deltas into a progressively
  larger sum.

  This implementation was inspired by the `pairwiseSum` implementation in
  the [`math-functions`](https://hackage.haskell.org/package/math-functions-0.3.4.2/docs/src/Numeric.Sum.html#pairwiseSum)
  Haskell package. The notes above were adapted from that function's docs."
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
;;
;; This section implements combinators that use various binary functions to
;; generate n-arity versions of those functions.

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

  And returns a function that will SUBTRACT elements. Given `x`, `y`, `z`, for
  example, the returned function will return `(- x y z)`, implemented as `(minus
  x (plus y z))`

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
