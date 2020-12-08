;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.structure
  (:require [clojure.string :refer [join]]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.numsymb]
            [sicmutils.value :as v]
            #?(:cljs [cljs.reader]))
  #?(:clj
     (:import [clojure.lang AFn Counted IFn ILookup PersistentVector Seqable Sequential])))

(declare make)

;; Type Declarations

(def ^:private orientation->symbol
  {::up 'up ::down 'down})

(def ^:private orientation->separator
  {::up "↑" ::down "_"})

(def ^:private opposite-orientation
  {::up ::down ::down ::up})


(derive ::up ::structure)
(derive ::down ::structure)
(derive PersistentVector ::up)

;; Structures can interact with functions.
(derive ::structure ::f/cofunction)

(deftype Structure [orientation v]
  v/Value
  (zero? [_] (every? v/zero? v))
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] (Structure. orientation (v/zero-like v)))
  (one-like [o] (u/unsupported (str "one-like: " o)))
  (identity-like [o] (u/unsupported (str "identity-like: " o)))
  (exact? [_] (every? v/exact? v))
  (numerical? [_] false)
  (freeze [_] `(~(orientation orientation->symbol) ~@(map v/freeze v)))
  (kind [_] orientation)

  #?@(:clj
      [Object
       (equals [_ b]
               (and (instance? Structure b)
                    (= orientation (.-orientation b))
                    (= v (.-v b))))
       (toString [_] (str "("
                          (orientation orientation->symbol)
                          " "
                          (join " " (map str v))
                          ")"))

       Sequential

       Counted
       (count [_] (count v))

       Seqable
       (seq [_] (seq v))

       ILookup
       (valAt [_ key] (get v key))
       (valAt [_ key default] (get v key default))

       IFn
       (invoke [_ a]
               (Structure. orientation (mapv #(% a) v)))
       (invoke [_ a b]
               (Structure. orientation (mapv #(% a b) v)))
       (invoke [_ a b c]
               (Structure. orientation (mapv #(% a b c) v)))
       (invoke [_ a b c d]
               (Structure. orientation (mapv #(% a b c d) v)))
       (invoke [_ a b c d e]
               (Structure. orientation (mapv #(% a b c d e) v)))
       (invoke [_ a b c d e f]
               (Structure. orientation (mapv #(% a b c d e f) v)))
       (invoke [_ a b c d e f g]
               (Structure. orientation (mapv #(% a b c d e f g) v)))
       (invoke [_ a b c d e f g h]
               (Structure. orientation (mapv #(% a b c d e f g h) v)))
       (invoke [_ a b c d e f g h i]
               (Structure. orientation (mapv #(% a b c d e f g h i) v)))
       (invoke [_ a b c d e f g h i j]
               (Structure. orientation (mapv #(% a b c d e f g h i j) v)))
       (invoke [_ a b c d e f g h i j k]
               (Structure. orientation (mapv #(% a b c d e f g h i j k) v)))
       (invoke [_ a b c d e f g h i j k l]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l) v)))
       (invoke [_ a b c d e f g h i j k l m]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m) v)))
       (invoke [_ a b c d e f g h i j k l m n]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n) v)))
       (invoke [_ a b c d e f g h i j k l m n o]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o) v)))
       (invoke [_ a b c d e f g h i j k l m n o p]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p) v)))
       (invoke [_ a b c d e f g h i j k l m n o p q]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q) v)))
       (invoke [_ a b c d e f g h i j k l m n o p q r]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q r) v)))
       (invoke [_ a b c d e f g h i j k l m n o p q r s]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q r s) v)))
       (invoke [_ a b c d e f g h i j k l m n o p q r s t]
               (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q r s t) v)))
       (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
               (Structure. orientation (mapv #(apply % a b c d e f g h i j k l m n o p q r s t rest) v)))
       (applyTo [s xs] (AFn/applyToHelper s xs))]

      :cljs
      [IEquiv
       (-equiv [_ b]
               (if (instance? Structure b)
                 (and (= orientation (.-orientation b))
                      (= v (.-v b)))
                 (= v b)))

       Object
       (toString [_] (str "("
                          (orientation orientation->symbol) " " (join " " (map str v))
                          ")"))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer
                              "#object[sicmutils.structure.Structure \""
                              (.toString x)
                              "\"]"))

       ISequential

       ICounted
       (-count [_] (-count v))

       ISeqable
       (-seq [_] (-seq v))

       ILookup
       (-lookup [_ k] (-lookup v k))
       (-lookup [_ k default] (-lookup v k default))

       IFn
       (-invoke [_ a]
                (Structure. orientation (mapv #(% a) v)))
       (-invoke [_ a b]
                (Structure. orientation (mapv #(% a b) v)))
       (-invoke [_ a b c]
                (Structure. orientation (mapv #(% a b c) v)))
       (-invoke [_ a b c d]
                (Structure. orientation (mapv #(% a b c d) v)))
       (-invoke [_ a b c d e]
                (Structure. orientation (mapv #(% a b c d e) v)))
       (-invoke [_ a b c d e f]
                (Structure. orientation (mapv #(% a b c d e f) v)))
       (-invoke [_ a b c d e f g]
                (Structure. orientation (mapv #(% a b c d e f g) v)))
       (-invoke [_ a b c d e f g h]
                (Structure. orientation (mapv #(% a b c d e f g h) v)))
       (-invoke [_ a b c d e f g h i]
                (Structure. orientation (mapv #(% a b c d e f g h i) v)))
       (-invoke [_ a b c d e f g h i j]
                (Structure. orientation (mapv #(% a b c d e f g h i j) v)))
       (-invoke [_ a b c d e f g h i j k]
                (Structure. orientation (mapv #(% a b c d e f g h i j k) v)))
       (-invoke [_ a b c d e f g h i j k l]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l) v)))
       (-invoke [_ a b c d e f g h i j k l m]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m) v)))
       (-invoke [_ a b c d e f g h i j k l m n]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n) v)))
       (-invoke [_ a b c d e f g h i j k l m n o]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o) v)))
       (-invoke [_ a b c d e f g h i j k l m n o p]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p) v)))
       (-invoke [_ a b c d e f g h i j k l m n o p q]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q) v)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q r) v)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r s]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q r s) v)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
                (Structure. orientation (mapv #(% a b c d e f g h i j k l m n o p q r s t) v)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
                (Structure. orientation (mapv #(apply % a b c d e f g h i j k l m n o p q r s t rest) v)))
       ]))

(defn valid-orientation? [o]
  (contains? #{::up ::down} o))

(defn structure->vector
  "Return the structure in unoriented vector form."
  [^Structure s]
  (.-v s))

(defn vector->up
  "Form an up-tuple from a vector."
  [v]
  {:pre [(vector? v)]}
  (->Structure ::up v))

(defn vector->down
  "Form a down-tuple from a vector."
  [v]
  {:pre [(vector? v)]}
  (->Structure ::down v))

(defn ^:private make
  [orientation xs]
  (->Structure orientation (into [] xs)))

(defn up
  "Construct an up (contravariant) tuple from the arguments."
  [& xs]
  (make ::up xs))

(defn down
  "Construct a down (covariant) tuple from the arguments."
  [& xs]
  (make ::down xs))

(defn structure?
  "True if s is a structure."
  [s]
  (or (instance? Structure s)
      (vector? s)))

(defn up?
  "True if s in an up structure."
  [s]
  (or (vector? s)
      (and (instance? Structure s)
           (= (.-orientation s) ::up))))

(defn orientation
  "Return the orientation of s, either ::up or ::down."
  [^Structure s]
  (if (instance? Structure s) (.-orientation s) ::up))

(defn same-orientation? [l r]
  (= (orientation l)
     (orientation r)))

(defn opposite
  "Make a tuple containing xs with the orientation opposite to s."
  [s xs]
  (make (opposite-orientation (orientation s)) xs))

(defn same
  "Make a tuple containing xs with the same orientation as s."
  [s xs]
  (make (orientation s) xs))

(defn flip-indices
  "Make a tuple with the same shape as s but all orientations inverted."
  [s]
  (if (structure? s)
    (->Structure (opposite-orientation (orientation s))
                 (mapv flip-indices (seq s)))
    s))

(defn generate
  "Generate a structure with the given orientation whose elements are (f i)
  where i ranges from [0..dimension)"
  [dimension orientation f]
  {:pre [(valid-orientation? orientation)]}
  (->Structure orientation (mapv f (range dimension))))

(defn literal
  "Generates structure of the specified `orientation` and dimension `size`
  populated by symbolic entries, each prefixed by the supplied symbol `sym`.

  For example:

  (= (literal 'x 3 ::s/up)
     (up 'x↑0 'x↑1 'x↑2))

  See [[literal-up]] and [[literal-down]] for constructors with baked in
  orientations."

  [sym size orientation]
  {:pre [(valid-orientation? orientation)]}
  (let [separator (orientation->separator orientation)
        prefix    (str sym separator)]
    (generate size orientation
              (fn [i]
                (symbol (str prefix i))))))

(defn literal-up
  "Generates an `up` structure of dimension `size` populated by symbolic entries,
  each prefixed by the supplied symbol `sym`.

  For example:

  (= (literal-up 'x 3)
     (up 'x↑0 'x↑1 'x↑2))"
  [sym size]
  (literal sym size ::up))

(defn literal-down
  "Generates a `down` structure of dimension `size` populated by symbolic entries,
  each prefixed by the supplied symbol `sym`.

  For example:

  (= (literal-down 'x 3)
     (down 'x_0 'x_1 'x_2))"
  [sym size]
  (literal sym size ::down))

(defn- s:nth [s i]
  (cond (structure? s) (nth s i)
        (= i 0)        s
        :else (u/illegal
               (str "non-struct s:nth not supported on non-structure: "
                    s " with index: " i))))

;; The following mappers only make sense if, when there is more than one
;; structure they are all isomorphic.

(defn- map:l
  "map from the left across all structures. Works if you pass a scalar in too."
  [f [s :as structs]]
  (if (structure? s)
    (generate (count s)
              (orientation s)
              (fn [i]
                (let [xs (map #(s:nth % i) structs)]
                  (apply f xs))))
    (apply f structs)))

(defn- map:r:l
  [f structures]
  (map:l (fn [& elements]
           (if (structure? (first elements))
             (map:r:l f elements)
             (apply f elements)))
         structures))

(defn mapr
  "Return a structure with the same shape as s but with f applied to each
  primitive (that is, not structural) component."
  [f & structures]
  (map:r:l f structures))

(defn structure->access-chains
  "Return a structure of the same shape as s whose elements are access
  chains corresponding to position of each element (i.e., the sequence
  of indices needed to address that element)."
  [s]
  (when (structure? s)
    (letfn [(access [chain s]
              (make (orientation s)
                    (map-indexed
                     (fn [i elt]
                       (if (structure? elt)
                         (access (conj chain i) elt)
                         ;; subtle (I'm afraid). Here is where we put
                         ;; the access chain into the new structure.
                         ;; But if we put it in as a vector, that would
                         ;; introduce a new layer of structure since
                         ;; vectors are considered up-tuples. So we
                         ;; have to turn it into a seq, which will
                         ;; forfeit structure-nature.
                         (seq (conj chain i))))
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
  [s [k & ks] value]
  (let [v (structure->vector s)]
    (if ks
      (same s (assoc v k (structure-assoc-in (v k) ks value)))
      (same s (assoc v k value)))))

(defn- compatible-for-contraction?
  "True if s and t are equal in length but opposite in orientation"
  [s t]
  (and (not (same-orientation? s t))
       (= (count s) (count t))
       (every? (fn [[l r]]
                 (or (not (structure? l))
                     (not (structure? r))
                     (compatible-for-contraction? l r)))
               (map vector s t))))

(defn v:dot-product [v1 v2]
  (assert (= (count v1) (count v2))
          (str "Not same dimension -- v:dot-product"
               v1 ", " v2))
  (reduce g/+ (map g/* v1 v2)))

(defn- dot-product
  "Returns the dot product of the compatible structures `s` and `t` (opposite
  orientation, same )."
  [s t]
  (let [s-seq (flatten s)
        t-seq (flatten t)]
    (if (and (same-orientation? s t)
             (= (count s-seq) (count t-seq)))
      (reduce g/+ (map g/* s-seq t-seq))
      (u/illegal (str "incompatible structures: dot-product "
                      s ", " t)))))

(defn- inner-product
  "Returns the inner product of the compatible structures `s` and `t` (opposite
  orientation, same length)."
  [s t]
  (dot-product (g/conjugate s) t))

(defn- outer-product
  "The outer product of s and t is the structure s with each element at the
  first level post-multiplied by all of t, following the usual structure
  multiplication rules."
  [struct2 struct1]
  (letfn [(xform [s1]
            (mapr (fn [s2]
                    (g/* s1 s2))
                  struct2))]
    (mapr xform struct1)))

(defn- cross-product
  "Cross product of structures of length 3. Input orientations are ignored;
  result is an up-tuple."
  [s t]
  (when (or (not= (count s) 3)
            (not= (count t) 3))
    (u/illegal "cross product only works on two elements of ^3"))
  (let [[s0 s1 s2] s
        [t0 t1 t2] t]
    (up (g/- (g/* s1 t2) (g/* t1 s2))
        (g/- (g/* s2 t0) (g/* s0 t2))
        (g/- (g/* s0 t1) (g/* t0 s1)))))

(defn- s:*
  "If s and t are compatible for contraction, returns their dot product,
  else, not QUITE their outer product. Else, we multiply `s` by all rows of
  `t`."
  [s t]
  (if (compatible-for-contraction? s t)
    (v:dot-product s t)
    (same t (map #(g/* s %) t))))

(defn- structure*scalar [v s]
  (same v (map #(g/* % s) v)))

(defn- scalar*structure [s v]
  (same v (map #(g/* s %) v)))

;; hmmm. why not do the repeated-squaring trick here?
;; perhaps structures are not typically raised to high
;; exponents.

(defn ^:private expt
  "Raise the structure s to the nth power."
  [s n]
  (let [one (v/one-like n)]
    (cond (v/one? n) s
          (> n one) (g/* s (g/expt s (g/- n one)))
          :else (u/arithmetic-ex (str "Cannot: " `(expt ~s ~n))))))

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

(defn dimension [s]
  (if (sequential? s)
    (-> s flatten count)
    1))

(defn- elementwise
  "Given a binary operator and two structures of the same size, return
  a structure with the same orientation as the first formed from the
  elementwise binary operation between corresponding elements of the
  structures."
  [op s t]
  (if (= (count s) (count t))
    (->Structure (orientation s) (mapv op s t))
    (u/arithmetic-ex (str op " provided arguments of differing length"))))

(defmethod g/add [::down ::down] [a b] (elementwise g/+ a b))
(defmethod g/add [::up ::up] [a b] (elementwise g/+ a b))
(defmethod g/sub [::down ::down] [a b] (elementwise g/- a b))
(defmethod g/sub [::up ::up] [a b] (elementwise g/- a b))

(defmethod g/mul [::structure ::structure] [a b] (s:* a b))

(defmethod g/mul [::structure ::v/scalar] [a b]
  (structure*scalar a b))

(defmethod g/mul [::v/scalar ::structure] [a b]
  (scalar*structure a b))

(defmethod g/mul [::structure :sicmutils.operator/operator] [a b]
  (structure*scalar a b))

(defmethod g/mul [:sicmutils.operator/operator ::structure] [a b]
  (scalar*structure a b))

(defmethod g/mul [::structure :sicmutils.calculus.derivative/differential] [a b]
  (structure*scalar a b))

(defmethod g/mul [:sicmutils.calculus.derivative/differential ::structure] [a b]
  (scalar*structure a b))

(defmethod g/div [::structure ::v/scalar] [a b]
  (let [b' (g/invert b)]
    (same a (map #(g/* % b') a))))

(defmethod g/div [::structure ::structure] [a b] (s:* (g/invert b) a))
(defmethod g/expt [::structure ::v/integral] [a b] (expt a b))
(defmethod g/negate [::structure] [a] (mapr g/negate a))
(defmethod g/square [::structure] [a] (dot-product a a))
(defmethod g/cube [::structure] [a] (s:* a (s:* a a)))
(defmethod g/simplify [::structure] [a]
  (v/freeze (mapr g/simplify a)))

(defmethod g/magnitude [::structure] [a]
  (g/sqrt (inner-product a a)))

(defmethod g/abs [::structure] [a]
  (g/sqrt (dot-product a a)))

(defmethod g/conjugate [::structure] [a]
  (mapr g/conjugate a))

(defmethod g/transpose [::structure] [a] (opposite a (seq a)))
(defmethod g/dimension [::structure] [a] (dimension a))
(defmethod g/dot-product [::structure ::structure] [a b] (dot-product a b))
(defmethod g/inner-product [::structure ::structure] [a b] (inner-product a b))
(defmethod g/outer-product [::structure ::structure] [a b] (outer-product a b))

(defmethod g/cross-product [::up ::up] [a b] (cross-product a b))
