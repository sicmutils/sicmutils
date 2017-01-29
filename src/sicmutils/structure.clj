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

(ns sicmutils.structure
  (:import (clojure.lang Sequential Seqable IFn ILookup AFn Counted PersistentVector))
  (:require [clojure.string :refer [join]]
            [sicmutils
             [value :as v]
             [generic :as g]]))

(declare make)

(def ^:private orientation->symbol {::up 'up ::down 'down})

(deftype Struct [orientation ^PersistentVector v]
  v/Value
  (nullity? [_] (every? g/zero? v))
  (unity? [_] false)
  (zero-like [_] (Struct. orientation (mapv v/zero-like v)))
  (exact? [_] (every? v/exact? v))
  (numerical? [_] false)
  (freeze [_] `(~(orientation orientation->symbol) ~@(map v/freeze v)))
  (arity [_] (v/joint-arity (map v/arity v)))
  (kind [_] orientation)
  Object
  (equals [_ b]
    (and (instance? Struct b)
         (let [^Struct bs b]
           (= orientation (.orientation bs))
          (= v (.v bs)))))
  (toString [_] (str "(" (orientation orientation->symbol) " " (join " " (map str v)) ")"))
  Sequential
  Counted
  (count [_] (count v))
  Seqable
  (seq [_] (seq v))
  ILookup
  (valAt [_ key] (get v key))
  (valAt [_ key default] (get v key default))
  IFn
  (invoke [_ x]
    (Struct. orientation (mapv #(% x) v)))
  (invoke [_ x y]
    (Struct. orientation (mapv #(% x y) v)))
  (invoke [_ x y z]
    (Struct. orientation (mapv #(% x y z) v)))
  (invoke [_ w x y z]
    (Struct. orientation (mapv #(% w x y z) v)))
  (applyTo [s xs]
    (AFn/applyToHelper s xs)))

(defn structure->vector
  "Return the structure in unoriented vector form."
  [^Struct s]
  (.v s))

(defn vector->up
  "Form an up-tuple from a vector."
  [v]
  {:pre [(vector? v)]}
  (Struct. ::up v))

(defn vector->down
  "Form a down-tuple from a vector."
  [v]
  {:pre [(vector? v)]}
  (Struct. ::down v))

(defn ^:private make
  [orientation xs]
  (Struct. orientation (into [] xs)))

(defn up
  "Construct an up (contravariant) tuple from the arguments."
  [& xs]
  (make ::up xs))

(defn down
  "Construct a down (covariant) tuple from the arguments."
  [& xs]
  (make ::down xs))

(defn structure?
  "True if s is a structure (as far as we're concerned.)"
  [s]
  (or (instance? Struct s)
      (vector? s)))

(def ^:private opposite-orientation {::up ::down ::down ::up})

(defn orientation
  "Return the orientation of s, either ::up or ::down."
  [^Struct s]
  (if (instance? Struct s) (.orientation s)  ::up))

(defn opposite
  "Make a tuple containing xs with the orientation opposite to s."
  [s xs]
  (make (opposite-orientation (orientation s)) xs))

(defn same
  "Make a tuple containing xs with the same orientation as s."
  [s xs]
  (make (orientation s) xs))

(defn ^:private elementwise
  "Given a binary operator and two structures of the same size, return
  a structure with the same orientation as the first formed from the
  elementwise binary operation between corresponding elements of the
  structures."
  [op s t]
  (if (= (count s) (count t))
    (Struct. (orientation s) (mapv op s t))
    (throw (ArithmeticException.
            (str op " provided arguments of differing length")))))

(defn mapr
  "Return a structure with the same shape as s but with f applied to
  each primitive (that is, not structural) component."
  [f ^Struct s]
  (cond (instance? Struct s) (Struct. (.orientation s) (mapv #(mapr f %) (.v s)))
        (vector? s) (mapv #(mapr f %) s)
        :else (f s)))

(defn structure->access-chains
  "Return a structure of the same shape as s whose elements are access
  chains corresponding to position of each element (i.e., the sequence
  of indices needed to address that element)."
  [^Struct s]
  (if-not (instance? Struct s)
    nil
    (let [access (fn a [chain s]
                  (make (orientation s)
                        (map-indexed (fn [i elt]
                                       (if (structure? elt)
                                         (a (conj chain i) elt)
                                         ;; subtle (I'm afraid). Here is where we put
                                         ;; the access chain into the new structure.
                                         ;; But if we put it in as a vector, that would
                                         ;; introduce a new layer of structure since
                                         ;; vectors are considered up-tuples. So we
                                         ;; have to turn it into a seq, which will
                                         ;; forfeit structure-nature.
                                         (-> chain (conj i) seq)))
                                     s)))]
     (access [] s))))

(defn component
  "Given an access chain (a sequence of indices), return a function of
  structures that will retrieve that corresponding element."
  [& indices]
  #(get-in % indices))

(defn structure-assoc-in
  "Like assoc-in, but works for structures. At this writing we're not
  sure if we want to overwrite the stock definition of assoc-in to
  something that would fall through for standard clojure data types"
  [^Struct s [k & ks] value]
  (let [v (.v s)]
    (if ks
      (same s (assoc v k (structure-assoc-in (v k) ks value)))
      (same s (assoc v k value)))))

(defn ^:private compatible-for-contraction?
  "True if s and t are equal in length but opposite in orientation"
  [s t]
  (and (= (count s) (count t))
       (not= (orientation s) (orientation t))))

(defn ^:private inner-product
  "The inner produce of compatible structures (opposite orientation, same
  length)."
  [s t]
  (reduce g/+ (map g/* s t)))

(defn ^:private outer-product
  "The outer product of s and t is the structure s with each element at the
  first level post-multiplied by all of t, following the usual structure
  multiplication rules."
  [s t]
  (same t (map #(g/* s %) t)))

(defn ^:private cross-product
  "Cross product of structures of length 3. Input orientations are ignored;
  result is an up-tuple."
  [s t]
  (when (or (not= (count s) 3) (not= (count t) 3))
    (throw (IllegalArgumentException.
            "cross product only works on two elements of ^3")))
  (let [[s0 s1 s2] s [t0 t1 t2] t]
    (up (g/- (g/* s1 t2) (g/* t1 s2))
        (g/- (g/* s2 t0) (g/* s0 t2))
        (g/- (g/* s0 t1) (g/* t0 s1)))))

(defn ^:private mul
  "If s and t are compatible for contraction, returns their inner product,
  else their outer product."
  [s t]
  (if (compatible-for-contraction? s t)
    (inner-product s t)
    (outer-product s t)))

;; hmmm. why not do the repeated-squaring trick here?
;; perhaps structures are not typically raised to high
;; exponents.

(defn ^:private expt
  "Raise the structure s to the nth power."
  [s n]
  (cond (= n 1) s
        (> n 1) (g/* s (g/expt s (- n 1)))
        :else (throw (ArithmeticException. (str "Cannot: " `(expt ~s ~n))))))

(defn unflatten
  "Given a sequence of values and a model structure, unpack the values into
  a structure with the same shape as the model."
  ([values struct]
   (unflatten same values struct))
  ([constructor values struct]
   (letfn [(u [values struct]
             (if (structure? struct)
               (let [[values' struct']
                     (reduce (fn [[values struct] element]
                               (let [[values' struct'] (u values element)]
                                 [values' (conj struct struct')]))
                             [values []]
                             struct)]
                 [values' (constructor struct struct')])
               [(rest values) (first values)]))]
     (second (u values struct)))))

(defn compatible-shape
  "Return an object compatible for multiplication with the given one, with
  the slots filled with gensyms."
  [s]
  (unflatten opposite (repeatedly gensym) s))

(defn dimension
  [s]
  (-> s flatten count))

(defmethod g/add [::down ::down] [a b] (elementwise g/+ a b))
(defmethod g/add [::up ::up] [a b] (elementwise g/+ a b))
(defmethod g/sub [::down ::down] [a b] (elementwise g/- a b))
(defmethod g/sub [::up ::up] [a b] (elementwise g/- a b))
(defmethod g/cross-product [::up ::up] [a b] (cross-product a b))
(defmethod g/mul [::structure ::structure] [a b] (mul a b))

(derive ::up ::structure)
(derive ::down ::structure)
(derive PersistentVector ::up)

(defmethod g/mul
  [::structure :sicmutils.expression/numerical-expression]
  [a b]
  (outer-product b a))

(defmethod g/mul
  [:sicmutils.expression/numerical-expression ::structure]
  [a b]
  (outer-product a b))

(defmethod g/mul
  [::structure :sicmutils.calculus.derivative/differential]
  [a b]
  (outer-product b a))

(defmethod g/mul
  [:sicmutils.calculus.derivative/differential ::structure]
  [a b]
  (outer-product a b))

(defmethod g/div
  [::structure :sicmutils.expression/numerical-expression]
  [a b]
  (outer-product (g/invert b) a))

(defmethod g/div [::structure ::structure] [a b] (mul (g/invert b) a))
(defmethod g/expt [::structure Long] [a b] (expt a b))
(defmethod g/negate [::structure] [a] (same a (mapv g/negate a)))
(defmethod g/square [::structure] [a] (inner-product a a))
(defmethod g/cube [::structure] [a] (mul a (mul a a)))
(defmethod g/simplify [::structure] [a] (->> a (mapr g/simplify) v/freeze))
(defmethod g/transpose [::structure] [a] (opposite a (seq a)))

(defmethod g/magnitude [::structure] [^Struct a]
  (g/sqrt (reduce + (map g/square a))))
