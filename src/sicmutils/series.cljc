;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(ns sicmutils.series
  (:refer-clojure :rename {take core-take}
                  #?@(:cljs [:exclude [take]]))
  (:require #?(:cljs [goog.string :refer [format]])
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn Sequential Seqable))))

;; We would prefer to just use native Clojure lazy sequences to represent
;; series objects. But, they must be invokable as functions, so we must
;; wrap them in a defrecord.

(deftype Series [arity s]
  v/Value
  (nullity? [_] (empty? s))
  (unity? [_] false)
  (numerical? [_] false)
  (freeze [_] `[~'Series ~arity ~@(map g/simplify (core-take 4 s)) ~'...])
  (kind [_] ::series)

  Object
  (toString [S] (str (v/freeze S)))

  #?@
  (:clj
   [IFn
    (invoke [_ a]
            (Series. arity (map #(% a) s)))
    (invoke [_ a b]
            (Series. arity (map #(% a b) s)))
    (invoke [_ a b c]
            (Series. arity (map #(% a b c) s)))
    (invoke [_ a b c d]
            (Series. arity (map #(% a b c d) s)))
    (invoke [_ a b c d e]
            (Series. arity (map #(% a b c d e) s)))
    (invoke [_ a b c d e f]
            (Series. arity (map #(% a b c d e f) s)))
    (invoke [_ a b c d e f g]
            (Series. arity (map #(% a b c d e f g) s)))
    (invoke [_ a b c d e f g h]
            (Series. arity (map #(% a b c d e f g h) s)))
    (invoke [_ a b c d e f g h i]
            (Series. arity (map #(% a b c d e f g h i) s)))
    (invoke [_ a b c d e f g h i j]
            (Series. arity (map #(% a b c d e f g h i j) s)))
    (invoke [_ a b c d e f g h i j k]
            (Series. arity (map #(% a b c d e f g h i j k) s)))
    (invoke [_ a b c d e f g h i j k l]
            (Series. arity (map #(% a b c d e f g h i j k l) s)))
    (invoke [_ a b c d e f g h i j k l m]
            (Series. arity (map #(% a b c d e f g h i j k l m) s)))
    (invoke [_ a b c d e f g h i j k l m n]
            (Series. arity (map #(% a b c d e f g h i j k l m n) s)))
    (invoke [_ a b c d e f g h i j k l m n o]
            (Series. arity (map #(% a b c d e f g h i j k l m n o) s)))
    (invoke [_ a b c d e f g h i j k l m n o p]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q r) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r s]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s t) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
            (Series. arity (map #(apply % a b c d e f g h i j k l m n o p q r s t rest) s)))
    (applyTo [s xs] (AFn/applyToHelper s xs))

    Seqable
    (seq [_] s)]

   :cljs
   [IFn
    (-invoke [_ a]
             (Series. arity (map #(% a) s)))
    (-invoke [_ a b]
             (Series. arity (map #(% a b) s)))
    (-invoke [_ a b c]
             (Series. arity (map #(% a b c) s)))
    (-invoke [_ a b c d]
             (Series. arity (map #(% a b c d) s)))
    (-invoke [_ a b c d e]
             (Series. arity (map #(% a b c d e) s)))
    (-invoke [_ a b c d e f]
             (Series. arity (map #(% a b c d e f) s)))
    (-invoke [_ a b c d e f g]
             (Series. arity (map #(% a b c d e f g) s)))
    (-invoke [_ a b c d e f g h]
             (Series. arity (map #(% a b c d e f g h) s)))
    (-invoke [_ a b c d e f g h i]
             (Series. arity (map #(% a b c d e f g h i) s)))
    (-invoke [_ a b c d e f g h i j]
             (Series. arity (map #(% a b c d e f g h i j) s)))
    (-invoke [_ a b c d e f g h i j k]
             (Series. arity (map #(% a b c d e f g h i j k) s)))
    (-invoke [_ a b c d e f g h i j k l]
             (Series. arity (map #(% a b c d e f g h i j k l) s)))
    (-invoke [_ a b c d e f g h i j k l m]
             (Series. arity (map #(% a b c d e f g h i j k l m) s)))
    (-invoke [_ a b c d e f g h i j k l m n]
             (Series. arity (map #(% a b c d e f g h i j k l m n) s)))
    (-invoke [_ a b c d e f g h i j k l m n o]
             (Series. arity (map #(% a b c d e f g h i j k l m n o) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q r) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s t) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
             (Series. arity (map #(apply % a b c d e f g h i j k l m n o p q r s t rest) s)))

    IPrintWithWriter
    (-pr-writer [x writer _]
                (write-all writer
                           "#object[sicmutils.series.Series \""
                           (.toString x)
                           "\"]"))

    ISeqable
    (-seq [_] s)]))

(derive ::x/numerical-expression ::coseries)

(defn series? [s] (instance? Series s))

(defn starting-with
  "Form the infinite sequence starting with the supplied values. The
  remainder of the series will be filled with the zero-value
  corresponding to the first of the given values."
  [& xs]
  (->Series [:exactly 0] (concat xs (repeat (v/zero-like (first xs))))))

(defn partial-sums
  "Form the infinite sequence of partial sums of the given series"
  [s]
  (let [step (fn step [x xs]
               (lazy-seq (cons x
                               (step (g/+ x (first xs))
                                     (rest xs)))))]
    (->Series (.-arity s) (step (first (.-s s)) (rest (.-s s))))))

(defn take
  [n s]
  (->> s seq (core-take n)))

(defn fmap
  [f s]
  (->Series (.-arity s) (map f (.-s s))))

(defn sum
  [s n]
  (-> s partial-sums seq (nth n)))

(defn ^:private c*s [c s] (map #(g/* c %) s))

(defn ^:private s*c [s c] (map #(g/* % c) s))

(defn ^:private s+s [s t] (map g/+ s t))

(defn ^:private s*s
  "The Cauchy product of the two sequences"
  [s t]
  (let [step (fn step [s t]
               (lazy-seq (cons (g/mul (first s) (first t))
                               (s+s (c*s (first s) (rest t))
                                    (step (rest s) t)))))]
    (step s t)))

(defn value
  "Find the value of the series S applied to the argument x.
  This assumes that S is a series of applicables. If, in fact, S is a
  series of series-valued applicables, then the result will be a sort
  of layered sum of the values. Concretely, suppose that S has the
  form
    [[A1 A2 A3...] [B1 B2 B3...] [C1 C2 C3...]...]
  Then, this series applied to x will yield the series of values
    [(A1 x) (+ (A2 x) (B1 x)) (+ (A3 x) (B2 x) (C1 x)) ...]"
  [S x]
  (letfn [(collect [s]
            (let [first-result ((first s) x)]
              (if (series? first-result)
                (let [fr (.-s first-result)]
                  (lazy-seq (cons (first fr)
                                  (s+s (rest fr)
                                       (collect (rest s))))))
                ;; note that we have already realized first-result,
                ;; so it does not need to be behind lazy-seq.
                (cons first-result (lazy-seq (collect (rest s)))))))]
    (cond (= (.-arity S) [:exactly 0])
          (->Series (.-arity S) (collect (.-s S)))

          :else (u/unsupported (format "Cannot apply series of arity %s" (:arity S))))))

(defn generate
  "Produce the series generated by (f i) for i in 0, 1, ..."
  [f]
  (->Series [:exactly 0] (map f (range))))

(defmethod g/mul [::coseries ::series] [c s]
  (->Series (.-arity s) (c*s c (.-s s))))

(defmethod g/mul [::series ::coseries] [s c]
  (->Series (.-arity s) (s*c (.-s s) c)))

(defmethod g/mul [::series ::series] [s t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s*s (.-s s) (.-s t))))

(defmethod g/add [::series ::series] [s t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s+s (.-s s) (.-s t))))

(defmethod g/negate [::series] [s] (fmap g/negate s))

(defmethod g/sub [::series ::series] [s t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s+s (.-s s) (map g/negate (.-s t)))))

(defmethod g/square [::series] [s] (g/mul s s))

(defmethod g/partial-derivative
  [::series #?(:clj Sequential :cljs ISequential)]
  [s selectors]
  (let [a (.-arity s)]
    (cond (= a [:exactly 0])
          (->Series a (map #(g/partial-derivative % selectors) (.-s s)))

          :else
          (u/illegal (str "Can't differentiate series with arity " a)))))
