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
  (:require [sicmutils
             [value :as v]
             [expression :as x]
             [generic :as g]])
  (:import (clojure.lang IFn Seqable)))

;; We would prefer to just use native Clojure lazy sequences to represent
;; series objects. But, they must be invokable as functions, so we must
;; wrap them in a defrecord.

(declare apply-to apply-to*)

(defrecord Series [arity s]
  v/Value
  (nullity? [_] (empty? s))
  (unity? [_] false)
  (numerical? [_] false)
  (freeze [_] 'series)
  (arity [_] arity)
  (kind [_] ::series)
  IFn
  (invoke [_ x] (->Series arity (map #(% x) s)))
  (applyTo [s xs] (->Series arity (map #(apply % xs) s)))
  Object
  (toString [_] "a-series"))

(defn starting-with
  [& xs]
  (->Series [:exactly 0] (concat xs (repeat (v/zero-like (first xs))))))

(defn sum
  [{s :s} n]
  (reduce g/+ (take n s)))

(defn generate
  [f]
  (let [step (fn step [i] (lazy-seq (cons (f i) (step (inc i)))))]
    (->Series [:exactly 0] (step 0))))

(defn ->seq
  [s]
  (when-not (instance? Series s)
    (throw (IllegalArgumentException. "not a series")))
  (:s s))

(defmethod g/mul
  [::x/numerical-expression ::series]
  [n s]
  (->Series (:arity s) (map #(g/* n %) (:s s))))

(defmethod g/mul
  [::series ::x/numerical-expression]
  [s n]
  (->Series (:arity s) (map #(g/* % n) (:s s))))

(defmethod g/add
  [::series ::series]
  [s t]
  (->Series (:arity s) (map #(g/+ %1 %2) (:s s) (:s t))))

#_(defmethod g/mul
  [::series ::series]
  [s t]
  ;; todo: series multiplication; test with finite series
  (->Series (:arity s) ))
