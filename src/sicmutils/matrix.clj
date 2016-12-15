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
  (:refer-clojure :rename {map core-map get-in core-get-in})
  (:require [sicmutils
             [value :as v]
             [expression :as x]
             [structure :as s]
             [generic :as g]])
  (:import [clojure.lang PersistentVector IFn AFn ILookup]
           [sicmutils.structure Struct]))

(defrecord Matrix [r c ^PersistentVector v]
  v/Value
  (nullity? [_] (every? g/zero? v))
  (unity? [_] false)
  (zero-like [_] (Matrix. r c (vec (repeat r (vec (repeat c 0))))))
  (exact? [_] (every? v/exact? v))
  (freeze [_] (if (= c 1)
                `(~'column-matrix ~@(core-map (comp v/freeze first) v))
                `(~'matrix-by-rows ~@(core-map v/freeze v))))
  (arity [_] (v/joint-arity (core-map v/arity v)))
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

(defn generate
  "Create the r by c matrix whose entries are (f i j)"
  [r c f]
  (Matrix. r c
           (mapv (fn [i]
                   (mapv (fn [j]
                           (f i j))
                         (range c)))
                 (range r))))

(defn get-in
  "Like get-in for matrices, but obeying the scmutils convention: only one
  index is required to get an unboxed element from a column vector. This is
  perhaps an unprincipled exception..."
  [m is]
  (let [e (core-get-in (:v m) is)]
    (if (and (= 1 (count is))
             (= 1 (:c m))) (e 0) e)))

(defn matrix-some
  "True if f is true for some element of m."
  [f m]
  (some f (flatten (:v m))))

(defn map
  "Maps f over the elements of m."
  [f {r :r c :c v :v}]
  (Matrix. r c (mapv #(mapv f %) v)))

(defn matrix?
  [m]
  (instance? Matrix m))

(defn by-rows
  [& rs]
  {:pre [(not-empty rs)
         (every? sequential? rs)]}
  (let [r (count rs)
        cs (core-map count rs)]
    (when-not (every? #(= % (first cs)) (next cs))
      (throw (IllegalArgumentException. "malformed matrix")))
    (Matrix. r (first cs) (mapv vec rs))))

(defn column
  [& es]
  {:pre [(not-empty es)]}
  (Matrix. (count es) 1 (vec (for [e es] [e]))))

(defn transpose
  "Transpose the matrix m."
  [{r :r c :c v :v}]
  (generate c r #(core-get-in v [%2 %1])))

(defn ->structure
  "Convert m to a structure with given outer and inner orientations. Rows of
  M will become the inner tuples, unless t? is true, in which columns of m will
  form the inner tuples."
  [m outer-orientation inner-orientation t?]
  (Struct. outer-orientation (mapv #(Struct. inner-orientation %) (:v (if t? (transpose m) m)))))

(defn seq->
  "Convert a sequence (typically, of function arguments) to an up-structure.
  GJS: Any matrix in the argument list wants to be converted to a row of
  columns"
  [s]
  (apply s/up (core-map #(if (instance? Matrix %) (->structure % s/down s/up) %) s)))

(defn ^:private mul
  "Multiplies the two matrices a and b"
  [{ra :r ca :c va :v :as a}
   {rb :r cb :c vb :v :as b}]
  (when (not= ca rb)
    (throw (IllegalArgumentException. "matrices incompatible for multiplication")))
  (generate ra cb #(reduce g/+ (for [k (range ca)]
                                 (g/* (core-get-in va [%1 k])
                                      (core-get-in vb [k %2]))))))

(defn ^:private elementwise
  "Applies f elementwise between the matrices a and b."
  [f {ra :r ca :c va :v :as a} {rb :r cb :c vb :v :as b}]
  (when (or (not= ra rb) (not= ca cb))
    (throw (IllegalArgumentException. "matrices incompatible for operation")))
  (generate ra ca #(f (core-get-in va [%1 %2]) (core-get-in vb [%1 %2]))))

(defn square-structure->
  "Converts the square structure s into a matrix, and calls the
  continuation with that matrix and a function which will restore a
  matrix to a structure with the same inner and outer orientations as
  s. If invert is true, then up-up structures will become down-down
  and vice versa."
  [s k]
  (let [major-size (count s)
        major-orientation (s/orientation s)
        minor-sizes (core-map #(if (s/structure? %) (count %) 1) s)
        minor-orientations (core-map s/orientation s)
        minor-orientation (first minor-orientations)]
    (if (and (every? #(= major-size %) minor-sizes)
             (every? #(= minor-orientation %) (rest minor-orientations)))
      (let [need-transpose (= minor-orientation ::s/up)
            M (generate major-size major-size
                        #(core-get-in s (if need-transpose [%2 %1] [%1 %2])))]
        (k M #(->structure % major-orientation minor-orientation need-transpose)))
      (throw (IllegalArgumentException. "structure is not square")))))

(defn square-structure-operation
  "Applies matrix operation f to square structure s, returning a structure of the same
  type as that given."
  [s f]
  (square-structure-> s (fn [m ->s] (->s (f m)))))

(defn ^:private M*u
  "Multiply a matrix by an up structure on the right. The return value is up."
  [{r :r c :c v :v :as m} u]
  (when (not= c (count u))
    (throw (IllegalArgumentException. "matrix and tuple incompatible for multiplication")))
  (apply s/up
         (core-map (fn [i]
                (reduce g/+ (for [k (range c)]
                              (g/* (core-get-in v [i k])
                                   (get u k)))))
                   (range r))))

(defn ^:private d*M
  "Multiply a matrix by a down tuple on the left. The return value is down."
  [d {r :r c :c v :v :as m}]
  (when (not= r (count d))
    (throw (IllegalArgumentException. "matrix and tuple incompatible for multiplication")))
  (apply s/down
         (core-map (fn [i]
                     (reduce g/+ (for [k (range r)]
                                   (g/* (get d k)
                                        (core-get-in v [i k])
                                        ))))
                   (range c))))

(defn ^:private kronecker
  [i j]
  (if (= i j) 1 0))

(defn s->m
  "Convert the structure ms, which would be a scalar if the (compatible) multiplication
  (* ls ms rs) were performed, to a matrix."
  [ls ms rs]
  (let [ndowns (s/dimension ls)
        nups (s/dimension rs)]
    (generate ndowns nups
              #(g/* (s/unflatten (for [k (range ndowns)] (kronecker %1 k)) ls)
                    (g/* ms
                         (s/unflatten (for [k (range nups)] (kronecker %2 k)) rs))))))

(defn ^:private vector-disj
  "The vector formed by deleting the i'th element of the given vector."
  [v i]
  (vec (concat (take i v) (drop (inc i) v))))

(defn without
  "The matrix formed by deleting the i'th row and j'th column of the given matrix."
  [{r :r c :c v :v} i j]
  (Matrix. (dec r) (dec c)
           (mapv #(vector-disj % j)
                 (vector-disj v i))) )

(defn ^:private checkerboard-negate
  [s i j]
  (if (even? (+ i j)) s (g/negate s)))

(defn dimension
  [m]
  (when-not (and (instance? Matrix m)
                 (= (:c m) (:r m)))
    (throw (IllegalArgumentException. "not a square matrix")))
  (:r m))

(defn determinant
  "Computes the determinant of m, which must be square. Generic
  operations are used, so this works on symbolic square matrix."
  [{r :r c :c v :v :as m}]
  (when-not (= r c)
    (throw (IllegalArgumentException. "not square")))
  (condp = r
    0 m
    1 ((v 0) 0)
    2 (let [[[a b] [c d]] v]
        (g/- (g/* a d) (g/* b c)))
    (reduce g/+
            (core-map g/*
                      (cycle [1 -1])
                      (v 0)
                      (for [i (range r)] (determinant (without m 0 i)))))))

(defn cofactors
  "Computes the matrix of cofactors of the given structure with the
  same shape, if s is square."
  [{r :r c :c v :v :as m}]
  (when-not (= r c)
    (throw (IllegalArgumentException. "only square matrices have cofactors")))
  (cond (< r 2) m
        (= r 2) (let [[[a b] [c d]] v]
                  (Matrix. 2 2 [[d (g/negate c)]
                                [(g/negate b) a]]))
        :else (generate r r
                        #(-> m (without %1 %2) determinant (checkerboard-negate %1 %2)))))

(defn invert
  "Computes the inverse of a square matrix."
  [{r :r c :c v :v :as m}]
  (when-not (= r c) (throw (IllegalArgumentException. "not square")))
  (condp = r
    0 m
    1 (Matrix. 1 1 [[(g/invert ((v 0) 0))]])
    (let [C (cofactors m)
          Δ (reduce g/+ (core-map g/* (v 0) (-> C :v first)))]
      (map #(g/divide % Δ) (transpose C)))))

(defn I
  "Return the identity matrix of order n."
  [n]
  (generate n n #(kronecker %1 %2)))

(defn characteristic-polynomial
  "Compute the characteristic polynomial of the square matrix m, evaluated
  at x. Typically x will be a dummy variable, but if you wanted to get the
  value of the characteristic polynomial at some particular point, you could
  supply a different expression."
  [{r :r c :c v :v :as m} x]
  (when-not (= r c) (throw (IllegalArgumentException. "not square")))
  (determinant (g/- (g/* x (I r)) m)))

(defmethod g/transpose [::matrix] [m] (transpose m))
(defmethod g/invert [::matrix] [m] (invert m))
(defmethod g/negate [::matrix] [a] (map g/negate a))
(defmethod g/sub [::matrix ::matrix] [a b] (elementwise g/- a b))
(defmethod g/add [::matrix ::matrix] [a b] (elementwise g/+ a b))
(defmethod g/mul [::matrix ::matrix] [a b] (mul a b))
(defmethod g/mul [::x/numerical-expression ::matrix] [n a] (map #(g/* n %) a))
(defmethod g/mul [::matrix ::s/up] [m u] (M*u m u))
(defmethod g/mul [::s/down ::matrix] [d m] (d*M d m))
(defmethod g/simplify [::matrix] [m] (->> m (map g/simplify) v/freeze))
(defmethod g/determinant [::matrix] [m] (determinant m))

(defmethod g/determinant
  [::s/structure]
  [s]
  (square-structure-> s (fn [m _] (determinant m))))

(defmethod g/invert
  [::s/structure]
  [a]
  (let [a' (square-structure-operation a invert)]
    (if (= (s/orientation a') (s/orientation (first a')))
      (s/opposite a' (core-map #(s/opposite a' %) a'))
      a')))
