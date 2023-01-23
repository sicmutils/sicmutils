#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.algebra.fold
  "Namespace implementing various aggregation functions using the `fold`
  abstraction and combinators for generating new folds from fold primitives.

  Contains a number of algorithms for [compensated
  summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) of
  floating-point numbers."
  (:refer-clojure :exclude [min max count])
  (:require [clojure.core :as core]
            [emmy.generic :as g]
            [emmy.util.def :as ud])
  #?(:cljs
     (:require-macros [emmy.algebra.fold])))

;; ## Folds and Scans
;;
;; A [fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) is a
;; combination of:
;;
;; - some initial value into which you want to aggregate
;; - a combining function of type (accumulator, x) => accumulator, capable
;;   of "folding" each element `x` in some sequence of `xs` into the
;;   accumulating state
;; - a "present" function that converts the accumulator into a final value.
;;
;; NOTE: This also happens to be Clojure's required interface for the reducing
;; function you pass to [[clojure.core/transduce]]. Any of the folds implemented
;; in this namespace work well with `transduce` out of the box.
;;
;; Here is a simple example of a fold:

(defn generic-sum-fold
  "Fold-style function. The 2-arity merge operation adds the value `x` into the
  accumulating stating using [[emmy.generic/+]].

  - given 0 arguments, returns an accumulator of 0.0
  - given a single argument `acc`, acts as identity."
  ([] 0.0)
  ([acc] acc)
  ([acc x]
   (g/+ acc x)))

;; The accumulator is the floating point 0.0. The `present` function is just...
;; identity. This might seem pedantic to include, but many interesting folds
;; need to keep intermediate state around, so please indulge me for now.
;;
;; To "fold" a new number into the running total, simply add them together.
;;
;; Here is how to use this function to add up the integers from 0 to 9:

#_
(let [xs (range 10)]
  (= 45 (generic-sum-fold
         (reduce generic-sum-fold (generic-sum-fold) xs))))

;; To see how this abstraction is useful, let's first capture this ability to
;; make "summation" functions out of folds. (Note the docstring's description of
;; the other arities of `fold->sum-fn`... these allow you to define each of the
;; arities of a fold in separate functions if you like.)

(defn fold->sum-fn
  "Given

  - a 0-argument fn `init` that returns some \"empty\" accumulating value

  - a 2-argument fn `fold` of `(accumulator, x) => accumulator` responsible for
    merging some value `x` into the ongoing accumulation

  - a 1-argument fn `present` from `accumulator => final-result`

  Returns a function with two arities. The first arity takes a sequence `xs` and
  returns the result of accumulating all elements in `xs` using the functions
  above, then `present`ing the result.

  The second arity takes a transformation function `f`, an inclusive lower bound
  `low` and an exclusive upper bound `high` and returns the result of
  accumulating `(map f (range low high))`.

  ## Other Arities

  Given a single argument `fold`, `fold` is passed as each of the 0, 1 and 2
  arity arguments.

  Given `fold` and `present`, `fold` is used for the 0 and 2 arity arguments,
  `present` for the 1-arity argument."
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

;; Our example again:
#_
(let [sum (fold->sum-fn generic-sum-fold)
      xs  (range 10)]
  (= 45 (sum xs)))

;; ### Useful Folds
;;
;; This pattern is quite general. Here is example of a fold that (inefficiently)
;; computes the average of a sequence of numbers:

#_
(defn average
  ([] [0.0 0])
  ([[sum n]] (/ sum n))
  ([[sum n] x]
   [(+ sum x) (inc n)]))

;; The average of [0,9] is 4.5:

#_
(let [sum (fold->sum-fn average)]
  (= 4.5 (sum (range 10))))

;; (I'm not committing this particular implementation because it can overflow
;; for large numbers. There is a better implementation in Algebird, used
;; in [`AveragedValue`](https://github.com/twitter/algebird/blob/develop/algebird-core/src/main/scala/com/twitter/algebird/AveragedValue.scala)
;; that you should port when this becomes important.)
;;
;; Here are some more building blocks:

(defn constant
  "Given some value `const`, returns a fold that ignores all input and returns
  `const` for a call to any of its arities."
  [const]
  (fn [& _] const))

(defn count
  "Given some predicate `pred`, returns a fold that counts the number of items it
  encounters that return true when passed to `pred`, false otherwise."
  ([] (count (fn [_] true)))
  ([pred]
   (fn ([] 0)
     ([acc] acc)
     ([acc x]
      (if (pred x)
        (inc acc)
        acc)))))

(defn min
  "Fold that stores its minimum encountered value in its accumulator, and returns
  it when called on to present.

  Accumulation initializes with `nil`."
  ([] nil)
  ([acc] acc)
  ([acc x]
   (if acc
     (core/min acc x)
     x)))

(defn max
  "Fold that stores its maximum encountered value in its accumulator, and returns
  it when called on to present.

  Accumulation initializes with `nil`."
  ([] nil)
  ([acc] acc)
  ([acc x]
   (if acc
     (core/max acc x)
     x)))

;; NOTE also that any [[emmy.util.aggregate/monoid]] instance will work as
;; a fold out of the box. [[emmy.util.aggregate]] has utilities for
;; generating more explicit folds out of monoids.

;; ## Fold Combinators
;;
;; Folds can be "added" together in the following sense; if I have a sequence of
;; folds, I can run them in parallel across some sequence `xs` by combining them
;; into a single fold with these properties:
;;
;; - the accumulator is a vector of the accumulators of each input fold
;; - each `x` is merged into each accumulator using the appropriate fold
;; - `present` is called for every entry in the final vector
;;
;; This function is called `join`:

(defn join
  "Given some number of `folds`, returns a new fold with the following properties:

  - the accumulator is a vector of the accumulators of each input fold
  - each `x` is merged into each accumulator using the appropriate fold
  - `present` is called for every entry in the final vector

  Given a single `fold`, acts as identity.

  The no-argument call `(join)` is equivalent to `([[constant]] [])`."
  ([] (constant []))
  ([fold] fold)
  ([fold & folds]
   (let [folds (cons fold folds)]
     (fn
       ([]
        (mapv (fn [f] (f)) folds))
       ([accs]
        (mapv #(%1 %2) folds accs))
       ([accs x]
        (mapv (fn [f acc]
                (f acc x))
              folds accs))))))

;; For example, the following snippet computes the minimum, maximum and sum
;; of `(range 10)`:

#_
(let [fold (join min max generic-sum-fold)
      process (fold->sum-fn fold)]
  (= [0 9 45]
     (process (range 10))))

;; ### Scans
;;
;; Before moving on, let's pause and implement a similar transformation of a
;; fold, called `fold->scan-fn`. This is a generic form of Clojure's
;; `reductions` function; a "scan" takes a sequence of `xs` and returns a
;; sequence of all intermediate results seen by the accumulator, all passed
;; through `present`.

(defn fold->scan-fn
  "Given

  - a 0-argument fn `init` that returns some \"empty\" accumulating value

  - a 2-argument fn `fold` of `(accumulator, x) => accumulator` responsible for
    merging some value `x` into the ongoing accumulation

  - a 1-argument fn `present` from `accumulator => final-result`

  Returns a function with two arities. The first arity takes a sequence `xs` and
  returns a lazy sequence of all intermediate results of the summation. For
  example, given [0 1 2 3], the return sequence would be equivalent to:

  ```clj
  (def sum-fn (fold->sum-fn init fold present))

  [(sum-fn [0])
   (sum-fn [0 1])
   (sum-fn [0 1 2])
   (sum-fn [0 1 2 3])]
  ```

  The second arity takes a transformation function `f`, an inclusive lower bound
  `low` and an exclusive upper bound `high` and returns a lazy sequence of all
  intermediate results of accumulating `(map f (range low high))`.

  ## Other Arities

  Given a single argument `fold`, `fold` is passed as each of the 0, 1 and 2
  arity arguments.

  Given `fold` and `present`, `fold` is used for the 0 and 2 arity arguments,
  `present` for the 1-arity argument."
  ([fold]
   (fold->scan-fn fold fold fold))
  ([fold present]
   (fold->scan-fn fold fold present))
  ([init fold present]
   (fn scan
     ([xs]
      (->> (reductions fold (init) xs)
           (rest)
           (map present)))
     ([f low high]
      (scan
       (map f (range low high)))))))

;; Here is the previous example, using the fold to scan across `(range 4)`. Each
;; vector in the returned (lazy) sequence is the minimum, maximum and running
;; total seen up to that point.

#_
(let [fold (join min max generic-sum-fold)
      process (fold->scan-fn fold)]
  (i [[0 0 0]
      [0 1 1]
      [0 2 3]
      [0 3 6]]
     (process (range 4))))

;; ## Summing Sequences of Numbers
;;
;; This is a fascinating topic, and my explorations have not yet done it
;; justice. This section implements a number of folds designed to sum up
;; sequences of numbers in various error-limiting ways.
;;
;; Much of the numerical physics simulation code in the library (everything
;; in [[emmy.numerical.quadrature]], for example) depends on the ability to
;; sum sequences of floating point numbers without the accumulation of error due
;; to the machine representation of the number.
;;
;; Here is the naive way to add up a list of numbers:

(comment
  (defn naive-sum [xs]
    (apply g/+ xs)))

;; Simple! But watch it "break":

(comment
  ;; This should be 1.0...
  (= 0.9999999999999999
     (naive-sum [1.0 1e-8 -1e-8])))

;; Algorithms called ['compensated
;; summation'](https://en.wikipedia.org/wiki/Kahan_summation_algorithm)
;; algorithms are one way around this problem. Instead of simply accumulating
;; the numbers as they come, compensated summation keeps track of the piece of
;; the sum that would get erased from the sum due to lack of precision.
;;
;; Aggregation algorithms that require intermediate state as they traverse a
;; sequence are often excellent matches for the "fold" abstraction.
;;
;; The fold implementing Kahan summation tracks two pieces of state:
;;
;; - The running sum
;; - A running compensation term tracking lost low-order bits
;;
;; If you add a very small to a very large number, the small number will lose
;; bits. If you then SUBTRACT a large number and get back down to the original
;; small number's range, the compensation term can recover those lost bits for
;; you.
;;
;; See
;; the ['Accuracy'](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Accuracy)
;; section of the wiki article for a detailed discussion on the error bounds you
;; can expect with Kahan summation. I haven't grokked this yet, so please open a
;; PR with more exposition once you get it.

(defn kahan
  "Fold that tracks the summation of a sequence of floating point numbers, using
  the [Kahan summation
  algorithm](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) for
  maintaining stability in the face of accumulating floating point errors."
  ([] [0.0 0.0])
  ([[acc _]] acc)
  ([[acc c] x]
   (let [y (- x c)
         t (+ acc y)]
     [t (- (- t acc) y)])))

;; Voila, using [[kahan]], our example from before now correctly sums to 1.0:

#_
(= 1.0 ((fold->sum-fn kahan) [1.0 1e-8 -1e-8]))

;; From the [wiki
;; page](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements),
;; "Neumaier introduced an improved version of Kahan algorithm, which he calls
;; an 'improved Kahan–Babuška algorithm, which also covers the case when the
;; next term to be added is larger in absolute value than the running sum,
;; effectively swapping the role of what is large and what is small."
;;
;; Here is an example of where Kahan fails. The following should be 2.0, but
;; Kahan returns 0.0:

#_
(= 0.0 ((fold->sum-fn kahan) [1.0 1e100 1.0 -1e100]))

;; This improved fold is implemented here:

(defn kahan-babushka-neumaier
  "Implements a fold that tracks the summation of a sequence of floating point
  numbers, using Neumaier's improvement to [[kahan]].

  This algorithm is more efficient than [[kahan]], handles a wider range of
  cases (adding in numbers larger than the current running sum, for example) and
  should be preferred."
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

(def ^{:doc "Alias for [[kahan-babushka-neumaier]]."}
  kbn
  kahan-babushka-neumaier)

;; [[kbn]] returns the correct result for the example above:

#_
(= 2.0 ((fold->sum-fn kbn) [1.0 1e100 1.0 -1e100]))

;; The [wiki
;; page](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
;; mentions a "higher-order modification" of [[kahan-babushka-neumaier]], and I
;; couldn't help implementing the second-order version here:

(defn kahan-babushka-klein
  "Implements a fold that tracks the summation of a sequence of floating point
  numbers, using a second-order variation of [[kahan-babushka-neumaier]].

  See [this Wikipedia
  page](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
  for more information.

  This algorithm was proposed by Klein in ['A Generalized Kahan-Babushka
  Summation
  Algorithm'](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.582.288&rep=rep1&type=pdf),
  along with the higher-order versions implemented by [[kbk-n]]."
  ([] [0.0 0.0 0.0])
  ([acc] (reduce + acc))
  ([[acc cs ccs] x]
   (let [acc+x (+ acc x)
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

;; ### Higher-Order Kahan-Babushka-Klein

;; Now, the repetition above in the second-order version was too glaring to
;; ignore. Clearly it is possible to write efficient code for as high an order
;; as you'd like, as described in [Klein's
;; paper](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.582.288&rep=rep1&type=pdf).
;;
;; Because this code needs to be very efficient, I chose to implement the
;; higher-order fold generator using a macro.
;;
;; Each new order stacks three entries into the let-binding of the function
;; above, and adds a new term to the accumulator. Because all of these terms
;; live inside a single let-binding, we have to be careful with variable names.
;; It turns out we can get away with
;;
;; - one symbol for each term we're accumulating (`sum` and each compensation
;;   term)
;;
;; - a single symbol `delta` that we can reuse for all deltas generated.

(defn- klein-term
  "Takes symbolic variables for

  - `acc`, the accumulating term we're compensating for
  - `delta`, the shared symbol used for deltas

  and generates let-binding entries updating `acc` to `(+ acc delta)` and
  `delta` to the new compensation amount in `(+ acc delta)`."
  [acc delta]
  `[sum# (+ ~acc ~delta)
    ~delta (if (ud/fork
                :clj (>= (Math/abs ~(with-meta acc {:tag 'double}))
                         (Math/abs ~(with-meta delta {:tag 'double})))
                :cljs (>= (.abs js/Math ~acc)
                          (.abs js/Math ~delta)))
             (+ (- ~acc sum#) ~delta)
             (+ (- ~delta sum#) ~acc))
    ~acc sum#])

(defn ^:no-doc kbk-n-body
  "Given some order `n`, generates the function body of a fold implementing `n`-th
  order Kahan-Babushka-Klein summation.

  See [[kbk-n]] for more detail."
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

(defmacro kbk-n
  "Given some order `n`, returns a fold implementing `n`-th order
  Kahan-Babushka-Klein summation.

  Given `n` == 0, this is identical to a naive sum.
  Given `n` == 1, identical to [[kahan-babushka-neumaier]].
  Given `n` == 2, identical to [[kahan-babushka-klein]].

  `n` > 2 represent new compensated summation algorithms."
  [n]
  `(fn ~@(kbk-n-body n)))
