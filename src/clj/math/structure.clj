;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.structure
  (:import (clojure.lang Sequential Seqable IFn AFn Counted))
  (:require [math.value :as v]
            [math.expression :as x]
            [math.generic :as g]))

(declare make)
(declare make-identity-like)

(deftype Struct [orientation arity v]
  v/Value
  (nullity? [_] (every? g/zero? v))
  (unity? [_] false)
  (zero-like [_] (make orientation (-> v count (repeat 0))))
  (exact? [_] (every? g/exact? v))
  (numerical? [_] false)
  (compound? [_] true)
  (sort-key [_] 18)
  (freeze [_]
    `(~(orientation {:up 'up :down 'down}) ~@(map x/freeze-expression v)))
  (arity-of [_] arity)
  Object
  (equals [_ b]
    (and (instance? Struct b)
         (let [^Struct bs b]
           (= orientation (.orientation bs))
          (= v (.v bs)))))
  (toString [_] (str (cons orientation v)))
  Sequential
  Counted
  (count [_] (count v))
  Seqable
  (seq [_] (seq v))
  IFn
  (invoke [_ x]
    (make orientation (map #(% x) v)))
  (applyTo [s xs]
    (AFn/applyToHelper s xs))
  )

(defn- joint-arity
  [xs]
  (let [as (into #{} (map v/arity xs))]
    (cond (empty? as) 0
          (= (count as) 1) (first as)
          :else (throw (IllegalArgumentException.
                         (str "Cannot build structure of elements with differing arity " as))))))

(defn- make
  [orientation xs]
  (Struct. orientation (joint-arity xs) (vec xs)))

(defn up [& xs]
  (make :up xs))

(defn down [& xs]
  (make :down xs))

(defn structure? [s]
  (or (instance? Struct s)
      (vector? s)
      (list? s)))

(defn- down? [^Struct s]
  (and (instance? Struct s) (= (.orientation s) :down)))

(defn- up? [^Struct s]
  (or (vector? s)
      (list? s)
      (and (instance? Struct s) (= (.orientation s) :up))))

(def ^:private opposite-orientation {:up :down :down :up})

(defn opposite [s xs]
  (make (if (up? s) :down :up) xs))

(defn same [^Struct s xs]
  (make (.orientation s) xs))

(defn- orientation [^Struct s]
  (if (instance? Struct s) (.orientation s) :up))

(defn- elementwise [op s t]
  (if (= (count s) (count t))
    (make (orientation s) (map op s t))
    (throw (ArithmeticException.
            (str op " provided arguments of differing length")))))

(defn mapr
  "Return a structure with the same shape as s but with f applied to
  each primitive (that is, not structural) component."
  [f ^Struct s]
  (cond (instance? Struct s) (make (.orientation s) (map #(mapr f %) (.v s)))
        (sequential? s) (map f s)  ;; XXX what happens if we don't do this?
        :else (f s))
  )

(defn structure-assoc-in
  "Like assoc-in, but works for structures. At this writing we're not
  sure if we want to overwrite the stock definition of assoc-in to
  something that would fall through for standard clojure data types"
  [^Struct s keys value]
  (if (empty? keys) value
      (let [w (.v s)
            k0 (first keys)]
        (make (.orientation s)
              (assoc w k0 (structure-assoc-in (nth w k0) (next keys) value))))))

(defn structure-get-in
  "Like get-in, but for structures. See structure-assoc-in"
  [^Struct s keys]
  (if (empty? keys) s
      (recur (-> s .v (get (first keys))) (next keys))))

(defn- compatible-for-contraction?
  "True if s and t are equal in length but opposite in orientation"
  [s t]
  (and (= (count s) (count t))
       (not= (orientation s) (orientation t))))

(defn- inner-product
  [s t]
  (reduce g/+ 0 (map g/* s t)))

(defn- outer-product
  [a s]
  (make (orientation s) (map #(g/* a %) s)))

(defn square?
  "Returns [dimension major-orientation minor-orientation] if s is a square structure, else nil."
  [s]
  (let [major-size (count s)
        major-orientation (orientation s)
        minor-sizes (map count s)
        minor-orientations (map orientation s)
        first-minor-orientation (first minor-orientations)]
    (if (and (every? #(= major-size %) minor-sizes)
             (every? #(= first-minor-orientation %) (rest minor-orientations)))
      [major-size major-orientation first-minor-orientation])))

(defn transpose
  "The transpose of a structure s is just the same structure with the
  outermost orientation reversed."
  [s]
  (opposite s (seq s)))

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
  (let [[d outer-orientation inner-orientation] (square? s)]
    (cond (< d 2) s
          (= d 2) (let [[[a b] [c d]] s]
                    (make outer-orientation
                          [(make inner-orientation [d (g/negate c)])
                           (make inner-orientation [(g/negate b) a])]))
          :else (make-square d outer-orientation inner-orientation
                             #(-> s (substructure-without %1 %2) determinant (g/* (if (even? (+ %1 %2)) 1 -1)))))))

(defn determinant
  "Computes the determinant of s, which must have square shape. Generic
  operations are used, so this works on symbolic square structures."
  [s]
  (let [[d _ _] (square? s)]
    (when-not d (throw (IllegalArgumentException. "not square")))
    (cond (= d 0) (throw (IllegalArgumentException. "zero size matrix has no determinant"))
          (= d 1) (first (first s))
          (= d 2) (let [[[a b] [c d]] s]
                    (g/- (g/* a d) (g/* b c)))
          :else (reduce g/+
                        (map g/*
                             (cycle [1 -1])
                             (first s)
                             (for [i (range d)] (determinant (substructure-without s 0 i))))))))

(defn- invert
  "Computes the inverse of s viewed as a square matrix."
  [s]
  (let [[d o1 o2] (square? s)]
    (when-not d (throw (IllegalArgumentException. "not square")))
    (cond (= d 0) (throw (IllegalArgumentException. "zero size matrix has no inverse"))
          (= d 1) (make o1 [(make o2 [(g/invert (first (first s)))])])
          :else (let [C (cofactors s)
                      Δ (reduce g/+ (map g/* (first s) (first C)))
                      outer-orientation (if (= o1 o2) (opposite-orientation o1) o1)
                      inner-orientation (if (= o1 o2) (opposite-orientation o2) o2)]
                  (make-square d outer-orientation inner-orientation
                               #(-> C (nth %2) (nth %1) (g/divide Δ)))))))

(defn- make-identity-like
  [s]
  (let [[d outer-orientation inner-orientation] (square? s)]
    (when-not d (throw (IllegalArgumentException. "cannot make non-square identity structure")))
    (make-square d outer-orientation inner-orientation #(if (= %1 %2) 1 0))))

(defn characteristic-polynomial
  [s var]
  (determinant (g/- (g/* var (make-identity-like s)) s)))

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

(defn- expt [s n]
  (cond (= n 1) s
        (> n 1) (g/* s (g/expt s (- n 1)))
        :else (throw (ArithmeticException. (str "Cannot: " `(expt ~s ~n))))))

(defn- matrix->structure "TODO: implement" [m] m)

(defn seq->
  "Convert a sequence (typically, of function arguments) to an up-structure.
  GJS: Any matrix in the argument list wants to be converted to a row of
  columns (TODO: this is not implemented yet)"
  [s]
  (make :up (map matrix->structure s)))

(g/defhandler :+        [down? down?]           (partial elementwise g/+))
(g/defhandler :+        [up? up?]               (partial elementwise g/+))
(g/defhandler :-        [down? down?]           (partial elementwise g/-))
(g/defhandler :-        [up? up?]               (partial elementwise g/-))
(g/defhandler :*        [g/scalar? structure?]  outer-product)
(g/defhandler :*        [structure? g/scalar?]  #(outer-product %2 %1))
(g/defhandler :div      [structure? g/scalar?]  #(outer-product (g/invert %2) %1))
(g/defhandler :div      [structure? structure?] #(mul (invert %2) %1))
(g/defhandler :invert   [structure?]            invert)
(g/defhandler :*        [structure? structure?] mul)
(g/defhandler :**       [structure? integer?]   expt)
(g/defhandler :simplify [structure?]            #(mapr g/simplify %))
(g/defhandler :square   [structure?]            #(inner-product % %))
(g/defhandler :cube     [structure?]            #(g/* % % %))
(g/defhandler :negate   [structure?]            #(same % (map g/negate %)))

(println "struct initialized")
