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

(declare apply-to apply-to*)

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
  [& xs]
  (->Series [:exactly 0] (concat xs (repeat (v/zero-like (first xs))))))

(defn partial-sums
  [s]
  (let [step (fn step [x xs]
               (lazy-seq (cons x
                               (step (g/+ x (first xs))
                                     (rest xs)))))]
    (->Series (:arity s) (step (first (:s s)) (rest (:s s))))))

(defn ->seq
  [s]
  (when-not (instance? Series s)
    (throw (IllegalArgumentException. "not a series")))
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
