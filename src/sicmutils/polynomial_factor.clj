;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns sicmutils.polynomial-factor
  (:require [sicmutils
             [value :as v]
             [generic :as g]
             [expression :as x]
             [simplify :as s]
             [numsymb :refer [product? operands expt]]
             [polynomial :as p]
             [polynomial-gcd :refer [gcd gcd-seq]]]))

(defn split
  [p]
  (let [answer (fn [tracker const]
                 (into [const] (rest tracker)))]
    (loop [m (v/zero-like p)
           h p
           tracker []
           old-s p
           old-m (v/one-like p)]
      (if (v/unity? m)
        (answer tracker h)
        (let [gg (-> h p/partial-derivatives gcd-seq)
              new-s (g/exact-divide h (gcd h gg))
              new-m (gcd gg new-s)
              facts (g/exact-divide old-s new-s)
              doublefacts (gcd facts old-m)
              singlefacts (g/exact-divide new-s new-m)]
          (recur new-m
                 (g/exact-divide h (g/* new-m new-s))
                 (conj tracker doublefacts singlefacts)
                 new-s
                 new-m))))))

(defn actual-factors
  [factors]
  (filter #(not= % 1)
          (cons (first factors)
                (map-indexed #(expt %2 (+ %1 1)) (next factors)))))

(defn factor-polynomial-expression
  [p]
  (p/expression->
   (x/expression-of p)
   (fn [p v]
     (map (fn [factor] (g/simplify (p/->expression factor v)))
          (split p)))))

(defn ^:private flatten-product
  "Construct a list with all the top-level products in args spliced
  in; other items left wrapped."
  [factors]
  (mapcat #(if (product? %) (operands %) (list %)) factors))

(defn ->factors
  "Recursive generalization."
  [p v]
  (let [factors (map #(p/->expression % v) (split p))
        ff (actual-factors factors)]
    (condp = (count ff)
      0 1
      1 (first ff)
      (cons '* (flatten-product ff)))))

(def factor
  (s/analyzer (s/monotonic-symbol-generator "-f-")
              p/expression->
              ->factors
              p/operators-known))

(defn ^:private process-sqrt
  [expr]
  (let [fact-exp nil]))

(defn root-out-squares
  [expr]

  )
