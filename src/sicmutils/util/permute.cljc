#_
"Copyright © 2020 Sam Ritchie.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

(ns sicmutils.util.permute
  "Utilities for generating permutations of sequences."
  (:require [sicmutils.special.factorial :as sf]
            #?(:cljs [sicmutils.generic :as g]))
  #?(:clj
     (:import (clojure.lang APersistentVector))))

(defn ^:no-doc delete-nth
  "returns the sequence `xs` with its `n`th element dropped."
  [xs n]
  (concat (take n xs)
          (drop (inc n) xs)))

(defn permutations
  "Returns a lazy sequence of every possible arrangement of the elements of `xs`."
  [xs]
  (if (empty? xs)
    '(())
    (letfn [(f [i item]
              (map (fn [perm]
                     (cons item perm))
                   (permutations
                    (delete-nth xs i))))]
      (sequence (comp (map-indexed f) cat)
                xs))))

;; Helper function for bounded-distributions
(defn- distribute [m index total distribution already-distributed]
  (loop [distribution distribution
         index index
         already-distributed already-distributed]
    (if (>= index (count m)) nil
        (let [quantity-to-distribute (- total already-distributed)
              mi (m index)]
          (if (<= quantity-to-distribute mi)
            (conj distribution [index quantity-to-distribute total])
            (recur (conj distribution [index mi (+ already-distributed mi)])
                   (inc index)
                   (+ already-distributed mi)))))))

;; Helper function for bounded-distributions
(defn- next-distribution [m total distribution]
  (let [[index this-bucket this-and-to-the-left] (peek distribution)]
    (cond
      (< index (dec (count m)))
      (if (= this-bucket 1)
        (conj (pop distribution) [(inc index) 1 this-and-to-the-left])
        (conj (pop distribution)
              [index (dec this-bucket) (dec this-and-to-the-left)]
              [(inc index) 1 this-and-to-the-left])),
                                        ; so we have stuff in the last bucket
      (= this-bucket total) nil
      :else
      (loop [distribution (pop distribution)],
        (let
            [[index this-bucket this-and-to-the-left] (peek distribution),
             distribution (if (= this-bucket 1)
                            (pop distribution)
                            (conj (pop distribution)
                                  [index (dec this-bucket) (dec this-and-to-the-left)]))],
          (cond
            (<= (- total (dec this-and-to-the-left)) (apply + (subvec m (inc index))))
            (distribute m (inc index) total distribution (dec this-and-to-the-left)),

            (seq distribution) (recur distribution)
            :else nil))))))

;; Helper function for multi-comb
(defn- bounded-distributions
  [m t]
  (let [step
        (fn step [distribution]
          (cons distribution
                (lazy-seq (when-let [next-step (next-distribution m t distribution)]
                            (step next-step)))))]
    (step (distribute m 0 t [] 0))))

(defn- join
  "Lazily concatenates a collection of collections into a flat sequence,
  because Clojure's `apply concat` is insufficiently lazy."
  [colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (concat (first s) (join (rest s))))))

(defn- multi-comb
  "Handles the case when you want the combinations of a list with duplicate items."
  [l t]
  (let [f (frequencies l),
        v (vec (distinct l)),
        domain (range (count v))
        m (vec (for [i domain] (f (v i))))
        qs (bounded-distributions m t)]
    (for [q qs]
      (join
       (for [[index this-bucket _] q]
         (repeat this-bucket (v index)))))))


(defn- all-different?
  "Annoyingly, the built-in distinct? doesn't handle 0 args, so we need
  to write our own version that considers the empty-list to be distinct"
  [s]
  (if (seq s)
    (apply distinct? s)
    true))


(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
         iter-comb
         (fn iter-comb [c j]
           (if (> j n) nil
               (let [c (assoc c j (dec (c j)))]
                 (if (< (c j) j) [c (inc j)]
                     (loop [c c, j j]
                       (if (= j 1) [c j]
                           (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
         step
         (fn step [c j]
           (cons (rseq (subvec c 1 (inc n)))
                 (lazy-seq (let [next-step (iter-comb c j)]
                             (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking t different elements from items"
  [items t]
  (let [v-items (vec (reverse items))]
    (if (zero? t) (list ())
        (let [cnt (count items)]
          (cond (> t cnt) nil
                (= t 1) (for [item (distinct items)] (list item))
                (all-different? items) (if (= t cnt)
                                         (list (seq items))
                                         (map #(map v-items %) (index-combinations t cnt))),
                :else (multi-comb items t))))))

#_(defn combinations
    "Returns a lazy sequence of every possible set of `p` elements chosen from
  `xs`."
    [xs p]
    (cond (zero? p) '(())
          (empty? xs) ()
          :else (let [[x & xs] xs]
                  (concat
                   (map (fn [more]
                          (cons x more))
                        (combinations xs (dec p)))
                   (combinations xs p)))))

(defn cartesian-product
  "Accepts a sequence of collections `colls` and returns a lazy sequence of the
  cartesian product of all collections.

  The cartesian product of N collections is a sequences of sequences, each `N`
  long, of every possible way of choosing `N` items where the first comes from
  the first entry in `colls`, the second from the second entry and so on.

  NOTE: This implementation comes from Alan Malloy at this [StackOverflow
  post](https://stackoverflow.com/a/18248031). Thanks, Alan!"
  [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian-product (rest colls))
          x (first colls)]
      (cons x more))))

(defn list-interchanges
  "Given a `permuted-list` and the `original-list`, returns the number of
  interchanges required to generate the permuted list from the original list."
  [permuted-list original-list]
  (letfn [(lp1 [plist n]
            (if (empty? plist)
              n
              (let [fp     (first plist)
                    bigger (rest (drop-while #(not= % fp) original-list))
                    more   (rest plist)]
                (lp2 n bigger more more 0))))
          (lp2 [n bigger more l increment]
            (if (empty? l)
              (lp1 more (+ n increment))
              (lp2 n bigger more
                   (rest l)
                   (if-not (some #{(first l)} bigger)
                     (inc increment)
                     increment))))]
    (lp1 permuted-list 0)))

(defn permutation-interchanges [permuted-list]
  (letfn [(lp1 [plist n]
            (if (empty? plist)
              n
              (let [[x & xs] plist]
                (lp2 n x xs xs 0))))
          (lp2 [n x xs l increment]
            (if (empty? l)
              (lp1 xs (+ n increment))
              (lp2 n x xs
                   (rest l)
                   (if (>= (first l) x)
                     increment
                     (inc increment)))))]
    (lp1 permuted-list 0)))

(defn- same-set?
  "Returns true if `x1` and `x2` contain the same elements, false otherwise."
  [x1 x2]
  (= (sort-by hash x1)
     (sort-by hash x2)))

(defn permutation-parity
  "If a single `permuted-list` is supplied, returns the parity of the number of
  interchanges required to sort the permutation.

  NOTE that the requirement that elements be sortable currently constrains
  `permuted-list`'s elements to be numbers that respond to `>=`.

  For two arguments, given a `permuted-list` and the `original-list`, returns
  the parity (1 for even, -1 for odd) of the number of the number of
  interchanges required to generate the permuted list from the original list.

  In the two-argument case, if the two lists aren't permutations of each other,
  returns 0."
  ([permuted-list]
   (let [swaps (permutation-interchanges permuted-list)]
     (if (even? swaps) 1 -1)))
  ([permuted-list original-list]
   (if (and (= (count permuted-list)
               (count original-list))
            (same-set? permuted-list original-list))
     (if (even? (list-interchanges permuted-list original-list))
       1
       -1)
     0)))

(defn permute
  "Given a `permutation` (represented as a list of numbers), and a sequence `xs`
  to be permuted, construct the list so permuted."
  [permutation xs]
  (let [xs (vec xs)]
    (map (fn [p] (get xs p))
         permutation)))

(defn- index-of [v x]
  #?(:clj (.indexOf ^APersistentVector v x)
     :cljs (#'-indexOf v x)))

(defn sort-and-permute
  "cont = (fn [ulist slist perm iperm] ...)

  Given a short list and a comparison function, to sort the list by the
  comparison, returning the original list, the sorted list, the permutation
  procedure and the inverse permutation procedure developed by the sort."
  [ulist <? cont]
  (let [n       (count ulist)
        lsource (map vector ulist (range n))
        ltarget (sort-by first (comparator <?) lsource)
        sorted  (mapv first ltarget)
        perm    (mapv second ltarget)
        iperm   (map (fn [i] (index-of perm i))
                     (range n))]
    (cont ulist
          sorted
          (fn [l] (permute perm l))
          (fn [l] (permute iperm l)))))

;; Sometimes we want to permute some of the elements of a list, as follows:

(defn subpermute
  "Given a sequence `xs` and a map `m` of replacement indices, returns a new
  version of `xs` with the element at the position marked by each key in `m`
  replaced by the element at each value in the original `xs`."
  [m xs]
  (reduce-kv (fn [acc k v]
               (assoc acc k (get xs v)))
             xs
             m))

(defn number-of-permutations
  "Returns the number of possible ways of permuting a collection of `n` distinct
  elements."
  [n]
  (sf/factorial n))

(defn number-of-combinations
  "Returns 'n choose k', the number of possible ways of choosing `k` distinct
  elements from a collection of `n` total items."
  [n k]
  {:pre [(>= n 0)]}
  (sf/binomial-coefficient n k))

(let [div #?(:clj / :cljs g//)]
  (defn multichoose
    "Returns the number of possible ways of choosing a multiset with cardinality `k`
  from a set of `n` items, where each item is allowed to be chosen multiple
  times."
    [n k]
    {:pre [(>= n 0) (>= k 0)]}
    (if (zero? k)
      1
      (div (sf/rising-factorial n k)
           (sf/factorial k)))))

(defn permutation-sequence
  "Produces an iterable sequence developing the permutations of the input sequence
  of objects (which are considered distinct) in church-bell-changes order - that
  is, each permutation differs from the previous by a transposition of adjacent
  elements (Algorithm P from §7.2.1.2 of Knuth).

  This is an unusual way to go about this in a functional language, but it's
  fun.

  This approach has the side-effect of arranging for the parity of the generated
  permutations to alternate; the first permutation yielded is the identity
  permutation (which of course is even).

  Inside, there is a great deal of mutable state, but this cannot be observed by
  the user."
  [as]
  (let [n (count as)
        a (object-array as)
        c (int-array n (repeat 0)) ;; P1. [Initialize.]
        o (int-array n (repeat 1))
        return #(into [] %)
        the-next (atom (return a))
        has-next (atom true)
        ;; step implements one-through of algorithm P up to step P2,
        ;; at which point we return false if we have terminated, true
        ;; if a has been set to a new permutation. Knuth's code is
        ;; one-based; this is zero-based.
        step (fn [j s]
               (let [q (int (+ (aget c j) (aget o j)))] ;; P4. [Ready to change?]
                 (cond (< q 0)
                       (do ;; P7. [Switch direction.]
                         (aset o j (int (- (aget o j))))
                         (recur (dec j) s))

                       (= q (inc j))
                       (if (zero? j)
                         false ;; All permutations have been delivered.
                         (do (aset o j (int (- (aget o j)))) ;; P6. [Increase s.]
                             (recur (dec j) (inc s)))) ;; P7. [Switch direction.]

                       :else ;; P5. [Change.]
                       (let [i1 (+ s (- j (aget c j)))
                             i2 (+ s (- j q))
                             t (aget a i1)
                             ]
                         (aset a i1 (aget a i2))
                         (aset a i2 t)
                         (aset c j q)
                         true ;; More permutations are forthcoming.
                         ))))]
    (#?(:clj iterator-seq :cljs #'cljs.core/chunkIteratorSeq)
     (reify #?(:clj java.util.Iterator :cljs Object)
       (hasNext [_] @has-next)
       (next [_]  ;; P2. [Visit.]
         (let [prev @the-next]
           (reset! has-next (step (dec n) 0))
           (reset! the-next (return a))
           prev))

       #?@(:cljs
           [IIterable
            (-iterator [this] this)])))))
