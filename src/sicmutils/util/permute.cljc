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

(ns sicmutils.util.permute
  "Utilities for generating permutations of sequences."
  #?(:clj
     (:import (clojure.lang APersistentVector))))

(defn permutations [xs]
  (if (empty? xs)
    #{xs}
    (mapcat (fn [item]
              (map (fn [perm] (conj perm item))
                   (permutations
                    (disj xs item))))
            xs)))

(defn combinations [xs p]
  (cond (zero? p)   #{()}
        (empty? xs) ()
        :else (concat
               (map (fn [more]
                      (conj more (first xs)))
                    (combinations (rest xs)
                                  (dec p)))
               (combinations (rest xs) p))))

(comment
  (is (= '[(a b c)
           (a b d)
           (a b e)
           (a c d)
           (a c e)
           (a d e)
           (b c d)
           (b c e)
           (b d e)
           (c d e)]
         (combinations '[a b c d e] 3))))

(declare same-set? member)

(defn list-interchanges
  " Returns the number of interchanges required to generate the permuted list from
  the original list."
  [permuted-list original-list]
  (letfn [(lp1 [plist n]
            (if (empty? plist)
              n
              (let [bigger (rest (member (first plist) original-list))
                    more   (rest plist)]
                (lp2 n bigger more more 0))))
          (lp2 [n bigger more l increment]
            (if (empty? l)
              (lp1 more (+ n increment))
              (lp2 (rest l)
                   (if (not (member (first l) bigger))
                     (inc increment)
                     increment))))]
    (lp1 permuted-list 0)))

(defn permutation-parity [permuted-list original-list]
  (if (same-set? permuted-list original-list)
    (if (even? (list-interchanges permuted-list original-list))
      1 -1)
    0))

(defn permutation-interchanges [permuted-list]
  (letfn [(lp1 [plist n]
            (if (empty? plist)
              n
              (let [[x & xs] plist]
                (lp2 n xs xs 0))))
          (lp2 [n x xs l increment]
            (if (empty? l)
              (lp1 xs (+ n increment))
              (lp2 (rest l)
                   (if (> (first l) x)
                     increment
                     (inc increment)))))]
    (lp1 permuted-list 0)))

(defn permute
  " Given a permutation (represented as a list of numbers), and a list to be
  permuted, construct the list so permuted."
  [permutation xs]
  (map (fn [p] (nth xs p))
       permutation))

(defn- index-of [v x]
  #?(:clj (.indexOf ^APersistentVector v x)
     :cljs (-indexOf v x)))

(defn sort-and-permute
  "cont = (lambda (ulist slist perm iperm) ...)

  Given a short list and a comparison function, to sort the list by the
  comparison, returning the original list, the sorted list, the permutation
  procedure and the inverse permutation procedure developed by the sort."
  [ulist <? cont]
  (let [n       (count ulist)
        lsource (map vector ulist (range n))
        ltarget (sort-by first (comparator <?) lsource)
        sorted  (map first ltarget)
        perm    (map second ltarget)
        iperm   (map (fn [i] (index-of perm i))
                     (range n))]
    (cont ulist
          sorted
          (fn [l] (permute perm l))
          (fn [l] (permute iperm l)))))

(comment
  ;; For example:
  (is (= [[0 2 0 0 1 2 0 0]
          [0 0 0 0 0 1 2 2]
          [0 0 0 0 0 1 2 2]
          [0 2 0 0 1 2 0 0]]
         (sort-and-permute [0 2 0 0 1 2 0 0]
                           <
                           (fn [unsorted sorted permuter unpermuter]
                             (list unsorted
                                   sorted
                                   (permuter unsorted)
                                   (unpermuter sorted)))))))

;; Sometimes we want to permute some of the elements of a list, as follows:

(defn subpermute [xs the-map]
  (let [n (count xs)]
    (loop [i 0
           source xs
           answer []]
      (if (= i n)
        answer
        (if-let [entry (the-map i)]
          (recur (inc i)
                 (rest source)
                 (conj answer (nth xs entry)))
          (recur (inc i)
                 (rest source)
                 (conj answer (first source))))))))

(comment
  (is (= '(a e d b c)
         (subpermute '[a b c d e]
                     {1 4
                      4 2
                      2 3
                      3 1}))))

(defn factorial
  "Returns the factorial of `n`, ie, the product of 1 to `n` (inclusive)."
  [n]
  (apply * (range 1 (inc n))))

(defn number-of-permutations [n]
  (factorial n))

(defn number-of-combinations [n k]
  (quot (factorial n)
        (* (factorial (- n k))
           (factorial k))))

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
