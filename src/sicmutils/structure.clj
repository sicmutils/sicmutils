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
(declare make-identity-like)

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
    (AFn/applyToHelper s xs))
  )

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

(defn- make
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

(defn- elementwise
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
    (access [] s)))

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

(defn- compatible-for-contraction?
  "True if s and t are equal in length but opposite in orientation"
  [s t]
  (and (= (count s) (count t))
       (not= (orientation s) (orientation t))))

(defn- inner-product
  "The inner produce of compatible structures (opposite orientation, same
  length)."
  [s t]
  (reduce g/+ (map g/* s t)))

(defn- outer-product
  "The outer product of s and t is the structure s with each element at the
  first level post-multiplied by all of t, following the usual structure
  multiplication rules."
  [s t]
  (same t (map #(g/* s %) t)))

(defn square?
  "Returns [dimension major-orientation minor-orientation] if s is a
  square structure, else nil."
  [s]
  (let [major-size (count s)
        major-orientation (orientation s)
        minor-sizes (map #(if (structure? %) (count %) 1) s)
        minor-orientations (map orientation s)
        first-minor-orientation (first minor-orientations)]
    (if (and (every? #(= major-size %) minor-sizes)
             (every? #(= first-minor-orientation %) (rest minor-orientations)))
      [major-size major-orientation first-minor-orientation])))

(defn dot-product
  "Dot product of two structures of the same orientation and length."
  [v w]
  (when-not (and (= (orientation v) (orientation w))
                 (= (count v) (count w)))
    (throw (IllegalArgumentException. "arguments incompatible for dot product")))
  (g/* v (g/transpose w)))

(defn m:transpose
  "Transpose the structure s like a matrix. The result will have
  the same orientation at all levels."
  [^Struct s]
  (let [d1 (count s)
        d2s (map count (.v s))
        ragged (not (apply = d2s))
        o2s (map (fn [^Struct t] (.orientation t)) s)
        weird (not (apply = o2s))
        o2 (first o2s)]
    (when (or ragged weird)
      (prn "can't transpose" ragged weird d2s o2s)
      (throw (IllegalArgumentException.
              "a structure must be rectangular if it is to be transposed.")))
    (same s (for [i (range (first d2s))]
              (Struct. o2 (vec (for [j (range d1)]
                                 (get (get s j) i))))))))

(defn- without-index
  "The structure s with element index i removed"
  [^Struct s i]
  (same s (into
            (subvec (.v s) 0 i)
            (subvec (.v s) (inc i)))))

(defn substructure-without
  "The structure with the i'th component removed at the top level and the j'th
  component removed from each of the structures at the next level."
  [s i j]
  (let [a (map #(without-index % j) s)
        b (concat (take i a) (drop (inc i) a))]
    (same s b)))

(declare determinant)

(defn- make-square
  "Make a square structure of size n by n with outer and inner orientations as given,
  whose elements are (f i j), where i and j range from [0..n)"
  [n outer-orientation inner-orientation f]
  (make outer-orientation
        (for [i (range n)]
          (make inner-orientation
                (for [j (range n)]
                  (f i j))))))

(defn cofactors
  "Computes the matrix of cofactors of the given structure with the
  same shape, if s is square."
  [s]
  (let [[d outer-orientation inner-orientation] (square? s)
        checkerboard-negate (fn [s i j] (if (even? (+ i j)) s (g/negate s)))]
    (cond (< d 2) s
          (= d 2) (let [[[a b] [c d]] s]
                    (make outer-orientation
                          [(make inner-orientation [d (g/negate c)])
                           (make inner-orientation [(g/negate b) a])]))
          :else (make-square d outer-orientation inner-orientation
                             #(-> s (substructure-without %1 %2) determinant (checkerboard-negate %1 %2))))))

(defn determinant
  "Computes the determinant of s, which must have square shape. Generic
  operations are used, so this works on symbolic square structures."
  [s]
  (let [[d _ _] (square? s)]
    (when-not d (throw (IllegalArgumentException. "not square")))
    (condp = d
      0 (throw (IllegalArgumentException. "zero size matrix has no determinant"))
      1 (first (first s))
      2 (let [[[a b] [c d]] s]
          (g/- (g/* a d) (g/* b c)))
      (reduce g/+
              (map g/*
                   (cycle [1 -1])
                   (first s)
                   (for [i (range d)] (determinant (substructure-without s 0 i))))))))

(defn- invert
  "Computes the inverse of s viewed as a square matrix."
  [s]
  (let [[d o1 o2] (square? s)]
    (when-not d (throw (IllegalArgumentException. "not square")))
    (condp = d
      0 (throw (IllegalArgumentException. "zero size matrix has no inverse"))
      1 (make o1 [(make o2 [(g/invert (first (first s)))])])
      (let [C (cofactors s)
            Δ (reduce g/+ (map g/* (first s) (first C)))
            outer-orientation (if (= o1 o2) (opposite-orientation o1) o1)
            inner-orientation (if (= o1 o2) (opposite-orientation o2) o2)]
        (make-square d outer-orientation inner-orientation
                     #(-> C (nth %2) (nth %1) (g/divide Δ)))))))

(defn- cross-product
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

(defn- make-identity-like
  "Produce a multiplicative identity with the same shape as the square structure s."
  [s]
  (let [[d outer-orientation inner-orientation] (square? s)]
    (when-not d (throw (IllegalArgumentException. "cannot make non-square identity structure")))
    (make-square d outer-orientation inner-orientation #(if (= %1 %2) 1 0))))

(defn characteristic-polynomial
  "Compute the characteristic polynomial of the square structure s, evaluated
  at x. Typically x will be a dummy variable, but if you wanted to get the
  value of the characteristic polynomial at some particular point, you could
  supply a different expression."
  [s x]
  (determinant (g/- (g/* x (make-identity-like s)) s)))

(defn- mul
  "If s and t are compatible for contraction, returns their inner product,
  else their outer product."
  [s t]
  (if (compatible-for-contraction? s t)
    (inner-product s t)
    (outer-product s t)))

;; hmmm. why not do the repeated-squaring trick here?
;; perhaps structures are not typically raised to high
;; exponents.

(defn- expt
  "Raise the structure s to the nth power."
  [s n]
  (cond (= n 1) s
        (> n 1) (g/* s (g/expt s (- n 1)))
        :else (throw (ArithmeticException. (str "Cannot: " `(expt ~s ~n))))))

(defn- matrix->structure "TODO: implement" [m] m)

(defn seq->
  "Convert a sequence (typically, of function arguments) to an up-structure.
  GJS: Any matrix in the argument list wants to be converted to a row of
  columns (TODO: this is not implemented yet)"
  [s]
  (Struct. ::up (mapv matrix->structure s)))

(defn unflatten
  "Given a sequence of values and a model structure, unpack the values into
  a structure with the same shape as the model."
  [values struct]
  (letfn [(u [values struct]
            (if (structure? struct)
              (let [[values' struct']
                    (reduce (fn [[values struct] element]
                              (let [[values' struct'] (u values element)]
                                [values' (conj struct struct')]))
                            [values []]
                            struct)]
                [values' (same struct struct')])
              [(rest values) (first values)]))]
    (second (u values struct))))

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

(defmethod g/div [::structure ::structure] [a b] (mul (invert b) a))
(defmethod g/expt [::structure Long] [a b] (expt a b))
(defmethod g/negate [::structure] [a] (same a (mapv g/negate a)))
(defmethod g/invert [::structure] [a] (invert a))
(defmethod g/square [::structure] [a] (inner-product a a))
(defmethod g/cube [::structure] [a] (mul a (mul a a)))
(defmethod g/simplify [::structure] [a] (->> a (mapr g/simplify) v/freeze))
(defmethod g/transpose [::structure] [a] (opposite a (seq a)))
