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

(ns sicmutils.matrix
  (:import [clojure.lang PersistentVector IFn AFn ILookup]
           [sicmutils.structure Struct])
  (:require [sicmutils
             [value :as v]
             [structure :as s]
             [generic :as g]]))

(defrecord Matrix [r c ^PersistentVector v]
  v/Value
  (nullity? [_] (every? g/zero? v))
  (unity? [_] false)
  (zero-like [_] (Matrix. r c (vec (repeat r (vec (repeat c 0))))))
  (exact? [_] (every? v/exact? v))
  (freeze [_] `(~'matrix-by-rows ~@(map v/freeze v)))
  (arity [_] (v/joint-arity (map v/arity v)))
  (kind [_] ::matrix)
  IFn
  (invoke [_ x]
    (Matrix. r c (mapv (fn [e] (mapv #(% x) e)) v)))
  (invoke [_ x y]
    (Matrix. r c (mapv (fn [e] (mapv #(% x y) e)) v)))
  (invoke [_ x y z]
    (Matrix. r c (mapv (fn [e] (mapv #(% x y z) e)) v)))
  (invoke [_ w x y z]
    (Matrix. r c (mapv (fn [e] (mapv #(% w x y z) e)) v)))
  (applyTo [m xs]
    (AFn/applyToHelper m xs)))

(defn matrix-get-in
  "Like get-in for matrices, but obeying the scmutils convention: only one
  index is required to get an unboxed element from a column vector. This is
  perhaps an unprincipled exception..."
  [m is]
  (let [e (get-in (:v m) is)]
    (if (and (= 1 (count is))
             (= 1 (:c m))) (e 0) e)))

(defn by-rows
  [& rs]
  {:pre [(not-empty rs)
         (every? sequential? rs)]}
  (let [r (count rs)
        cs (map count rs)]
    (when-not (every? #(= % (first cs)) (next cs))
      (throw (IllegalArgumentException. "malformed matrix")))
    (Matrix. r (first cs) (mapv vec rs))))

(defn column-matrix
  [& es]
  {:pre [(not-empty es)]}
  (Matrix. (count es) 1 (vec (for [e es] [e]))))

(defn transpose
  "Transpose the matrix m."
  [{r :r c :c v :v}]
  (Matrix. c r
           (mapv (fn [i]
                   (mapv (fn [j]
                           (get-in v [j i]))
                         (range r)))
                 (range c))))

(defn ->structure
  "Convert m to a structure (a down tuple of up tuples)."
  [m]
  (let [mT (transpose m)]
    (apply s/down (map #(apply s/up %) (:v mT)))))

(defn seq->
  "Convert a sequence (typically, of function arguments) to an up-structure.
  GJS: Any matrix in the argument list wants to be converted to a row of
  columns"
  [s]
  (apply s/up (map #(if (instance? Matrix %) (->structure %) %) s)))

(defn ^:private mul
  "Multiplies the two matrices a and b"
  [{ra :r ca :c va :v :as a}
   {rb :r cb :c vb :v :as b}]
  (when (not= ca rb)
    (throw (IllegalArgumentException. "matrices incompatible for multiplication")))
  (Matrix. ra cb
           (mapv (fn [i]
                   (mapv (fn [j]
                           (reduce g/+ (for [k (range ca)]
                                         (g/* (get-in va [i k])
                                              (get-in vb [k j])))))
                         (range cb)))
                 (range ra))))

(defn ^:private elementwise
  "Computes the difference of two matrices."
  [f {ra :r ca :c va :v :as a} {rb :r cb :c vb :v :as b}]
  (when (or (not= ra rb) (not= ca cb))
    (throw (IllegalArgumentException. "matrices incompatible for subtraction")))
  (Matrix. ra ca
           (mapv (fn [i]
                   (mapv (fn [j]
                           (f (get-in va [i j]) (get-in vb [i j])))
                         (range rb)))
                 (range ra))))

(defn ^:private mul*up
  "Multiply a matrix by an up structure on the right. The return value is up."
  [{r :r c :c v :v :as m} u]
  (when (not= c (count u))
    (throw (IllegalArgumentException. "matrix and tuple incompatible for multiplication")))
  (apply s/up
         (map (fn [i]
                (reduce g/+ (for [k (range c)]
                              (g/* (get-in v [i k])
                                   (get u k)))))
              (range r))))

(defn ^:private mapr
  [f {r :r c :c v :v}]
  (Matrix. r c (mapv #(mapv f %) v)))

(defn s->m
  [ls ms rs]
  (let [ndowns (s/dimension ls)
        nups (s/dimension rs)]
    (Matrix. ndowns nups
             (mapv (fn [i]
                     (mapv (fn [j]
                             (g/* (s/unflatten (for [k (range ndowns)] (if (= i k) 1 0)) ls)
                                  (g/* ms
                                       (s/unflatten (for [k (range nups)] (if (= j k) 1 0)) rs))))
                           (range nups)))
                   (range ndowns)))))



(defmethod g/transpose [::matrix] [m] (transpose m))
(defmethod g/sub [::matrix ::matrix] [a b] (elementwise g/- a b))
(defmethod g/mul [::matrix ::matrix] [a b] (mul a b))
(defmethod g/mul [::matrix ::s/up] [m u] (mul*up m u))
(defmethod g/simplify [::matrix] [m] (->> m (mapr g/simplify) v/freeze))
