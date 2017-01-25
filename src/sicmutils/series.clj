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

(ns sicmutils.series
  (:refer-clojure :rename {take core-take map core-map})
  (:require [sicmutils
             [value :as v]
             [generic :as g]])
  (:import (clojure.lang IFn Sequential)))

;; We would prefer to just use native Clojure lazy sequences to represent
;; series objects. But, they must be invokable as functions, so we must
;; wrap them in a defrecord.

(defrecord Series [arity s]
  v/Value
  (nullity? [_] (empty? s))
  (unity? [_] false)
  (numerical? [_] false)
  (freeze [_] `[~'Series ~arity ~@(core-map g/simplify (core-take 4 s)) ~'...])
  (arity [_] arity)
  (kind [_] ::series)
  IFn
  (invoke [_ x] (->Series arity (core-map #(% x) s)))
  (invoke [_ x y] (->Series arity (core-map #(% x y) s)))
  (invoke [_ x y z] (->Series arity (core-map #(% x y z) s)))
  (applyTo [s xs] (->Series arity (core-map #(apply % xs) (:s s))))
  Object
  (toString [s] (str (v/freeze s))))

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
    (->Series (:arity s) (step (first (:s s)) (rest (:s s))))))

(defn ->seq
  "Convert the series to a (native, lazy) sequence of its values."
  [s]
  (when-not (instance? Series s)
    (throw (IllegalArgumentException. (str  "not a series:" (type s)))))
  (:s s))

(defn take
  [n s]
  (->> s ->seq (core-take n)))

(defn map
  [f s]
  (->Series (:arity s) (core-map f (:s s))))

(defn sum
  [s n]
  (-> s partial-sums ->seq (nth n)))

(defn ^:private c*s [c s] (core-map #(g/* c %) s))

(defn ^:private s*c [s c] (core-map #(g/* % c) s))

(defn ^:private s+s [s t] (core-map g/+ s t))

(defn ^:private s*s
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
    [A1 (+ A2 B1) (+ A3 B2 C1) ...]"
  [S x]
  (letfn [(collect [s]
            (let [first-result ((first s) x)]
              (if (series? first-result)
                (let [fr (:s first-result)]
                  (lazy-seq (cons (first fr)
                                  (s+s (rest fr)
                                       (collect (rest s))))))
                ;; note that we have already realized first-result,
                ;; so it does not need to be behind lazy-seq.
                (cons first-result (lazy-seq (collect (rest s)))))))]
    (cond (= (:arity S) [:exactly 0])
          (->Series (:arity S) (collect (:s S)))

          :else (throw (UnsupportedOperationException. (format "Cannot apply series of arity %s" (:arity S)))))))

(def generate #(->Series [:exactly 0] (core-map % (range))))

(defmethod g/mul
  [::coseries ::series]
  [c s]
  (->Series (:arity s) (c*s c (:s s))))

(defmethod g/mul
  [::series ::coseries]
  [s c]
  (->Series (:arity s) (s*c (:s s) c)))

(defmethod g/mul
  [::series ::series]
  [s t]
  {:pre [(= (:arity s) (:arity t))]}
  (->Series (:arity s) (s*s (:s s) (:s t))))

(defmethod g/add
  [::series ::series]
  [s t]
  {:pre [(= (:arity s) (:arity t))]}
  (->Series (:arity s) (s+s (:s s) (:s t))))

(defmethod g/negate [::series] [s] (map g/negate s))

(defmethod g/sub
  [::series ::series]
  [s t]
  {:pre [(= (:arity s) (:arity t))]}
  (->Series (:arity s) (s+s (:s s) (core-map g/negate (:s t)))))

(defmethod g/square [::series] [s] (g/mul s s))

(defmethod g/partial-derivative
  [::series Sequential]
  [{s :s arity :arity} selectors]
  (cond (= arity [:exactly 0])
        (->Series arity (core-map #(g/partial-derivative % selectors) s))

        :else
        (throw (IllegalArgumentException. (str "Can't differentiate series with arity " arity)))))

(derive ::sicmutils.expression/numerical-expression ::coseries)
(derive ::sicmutils.function/function ::coseries)
