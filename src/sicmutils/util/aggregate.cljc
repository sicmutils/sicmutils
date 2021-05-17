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

(defn kahan-sum
  "Implements a fold that tracks the summation of a sequence of floating point
  numbers, using Kahan's trick for maintaining stability in the face of
  accumulating floating point errors."
  ([] [0.0 0.0])
  ([[sum c] x]
   (let [y (- x c)
         t (+ sum y)]
     [t (- (- t sum) y)])))

(defn sum
  "Sums either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  Using Kahan's summation trick behind the scenes to keep floating point errors
  under control."
  ([xs]
   (first
    (reduce kahan-sum [0.0 0.0] xs)))
  ([f low high]
   (sum (map f (range low high)))))

(defn scanning-sum
  "Returns every intermediate summation from summing either:

  - a series `xs` of numbers, or
  - the result of mapping function `f` to `(range low high)`

  Using Kahan's summation trick behind the scenes to keep floating point errors
  under control."
  ([xs]
   (->> (reductions kahan-sum [0.0 0.0] xs)
        (map first)
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

(defn- combiner [f stop?]
  (if stop?
    (fn [l r]
      (if (stop? l)
        (reduced l)
        (f l r)))
    f))

(defn accumulation
  "TODO document."
  ([plus id]
   (accumulation plus id nil))
  ([plus id annihilate?]
   (let [acc (combiner plus annihilate?)]
     (fn
       ([] id)
       ([x] x)
       ([x y] (plus x y))
       ([x y & more]
        (reduce acc (plus x y) more))))))

(defn inverse-accumulation
  "TODO document."
  ([minus plus invert id]
   (inverse-accumulation minus plus invert id nil))
  ([minus plus invert id annihilate?]
   (let [acc (combiner plus annihilate?)]
     (fn
       ([] id)
       ([x] (invert x))
       ([x y] (minus x y))
       ([x y & more]
        (minus x (reduce acc y more)))))))
