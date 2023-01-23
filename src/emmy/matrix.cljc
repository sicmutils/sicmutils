#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.matrix
  "This namespace contains an implementation of a [[Matrix]] datatype and various
  operations for creating and working with [[Matrix]] instances.

  [[emmy.matrix]] also extends many Emmy generic operations
  to the [[Matrix]] datatype."
  (:refer-clojure :exclude [get-in some])
  (:require [clojure.core :as core]
            [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.polynomial :as poly]
            [emmy.series :as series]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang Associative AFn IFn Sequential))))

(declare fmap identity-like identity? m:=)

(derive ::square-matrix ::matrix)
(derive ::column-matrix ::matrix)
(derive ::row-matrix ::matrix)
(derive ::matrix ::f/cofunction)

(deftype Matrix [r c v]
  v/Value
  (zero? [_] (every? #(every? v/zero? %) v))
  (one? [_] false)
  (identity? [m] (identity? m))
  (zero-like [this] (fmap v/zero-like this))
  (one-like [this] (identity-like this))
  (identity-like [this] (identity-like this))

  (freeze [_] (if (= c 1)
                `(~'column-matrix ~@(map (comp v/freeze first) v))
                `(~'matrix-by-rows ~@(map v/freeze v))))
  (exact? [_] (every? #(every? v/exact? %) v))
  (kind [_] (cond (= r c) ::square-matrix
                  (= r 1) ::row-matrix
                  (= c 1) ::column-matrix
                  :else ::matrix))

  d/IPerturbed
  (perturbed? [_] (boolean (core/some d/perturbed? v)))
  (replace-tag [M old new] (fmap #(d/replace-tag % old new) M))
  (extract-tangent [M tag] (fmap #(d/extract-tangent % tag) M))

  f/IArity
  (arity [_] (transduce (map f/seq-arity) f/combine-arities v))

  #?@(:clj
      [Object
       (equals [this that] (m:= this that))
       (toString [_] (pr-str v))

       Sequential

       Associative
       (assoc [_ k entry] (Matrix. r c (assoc v k entry)))
       (containsKey [_ k] (contains? v k))
       (entryAt [_ k] (.entryAt ^Associative v k))
       (count [_] (count v))
       (seq [_] (seq v))
       (valAt [_ key] (get v key))
       (valAt [_ key default] (get v key default))
       (empty [this] (fmap v/zero-like this))
       (equiv [this that] (m:= this that))

       IFn
       (invoke [_ a]
               (Matrix. r c (mapv (fn [row] (mapv #(% a) row)) v)))
       (invoke [_ a b]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b) row)) v)))
       (invoke [_ a b cx]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx) row)) v)))
       (invoke [_ a b cx d]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d) row)) v)))
       (invoke [_ a b cx d e]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e) row)) v)))
       (invoke [_ a b cx d e f]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f) row)) v)))
       (invoke [_ a b cx d e f g]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g) row)) v)))
       (invoke [_ a b cx d e f g h]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h) row)) v)))
       (invoke [_ a b cx d e f g h i]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i) row)) v)))
       (invoke [_ a b cx d e f g h i j]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j) row)) v)))
       (invoke [_ a b cx d e f g h i j k]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k) row)) v)))
       (invoke [_ a b cx d e f g h i j k l]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o p]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o p q]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o p q rx]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q rx) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o p q rx s]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q rx s) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o p q rx s t]
               (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q rx s t) row)) v)))
       (invoke [_ a b cx d e f g h i j k l m n o p q rx s t rest]
               (Matrix. r c (mapv (fn [row] (mapv #(apply % a b cx d e f g h i j k l m n o p q rx s t rest) row)) v)))
       (applyTo [m xs]
                (AFn/applyToHelper m xs))]

      :cljs
      [Object
       (toString [_] (pr-str v))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer
                              "#object[emmy.structure.Matrix \""
                              (.toString x)
                              "\"]"))

       IEmptyableCollection
       (-empty [this] (v/zero-like this))

       ISequential

       IEquiv
       (-equiv [this that] (m:= this that))

       ISeqable
       (-seq [_] (-seq v))

       ICounted
       (-count [_] (-count v))

       IIndexed
       (-nth [_ n] (-nth v n))
       (-nth [_ n not-found] (-nth v n not-found))

       ILookup
       (-lookup [_ k] (-lookup v k))
       (-lookup [_ k not-found] (-lookup v k not-found))

       IAssociative
       (-assoc [_ k entry] (Matrix. r c (-assoc v k entry)))
       (-contains-key? [_ k] (-contains-key? v k))

       IFind
       (-find [_ n] (-find v n))

       IFn
       (-invoke [_ a]
                (Matrix. r c (mapv (fn [row] (mapv #(% a) row)) v)))
       (-invoke [_ a b]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b) row)) v)))
       (-invoke [_ a b cx]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx) row)) v)))
       (-invoke [_ a b cx d]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d) row)) v)))
       (-invoke [_ a b cx d e]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e) row)) v)))
       (-invoke [_ a b cx d e f]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f) row)) v)))
       (-invoke [_ a b cx d e f g]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g) row)) v)))
       (-invoke [_ a b cx d e f g h]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h) row)) v)))
       (-invoke [_ a b cx d e f g h i]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i) row)) v)))
       (-invoke [_ a b cx d e f g h i j]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j) row)) v)))
       (-invoke [_ a b cx d e f g h i j k]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o p]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o p q]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o p q rx]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q rx) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o p q rx s]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q rx s) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o p q rx s t]
                (Matrix. r c (mapv (fn [row] (mapv #(% a b cx d e f g h i j k l m n o p q rx s t) row)) v)))
       (-invoke [_ a b cx d e f g h i j k l m n o p q rx s t rest]
                (Matrix. r c (mapv (fn [row] (mapv #(apply % a b cx d e f g h i j k l m n o p q rx s t rest) row)) v)))]))

(defn matrix?
  "Returns true if the supplied `m` is an instance of [[Matrix]], false
  otherwise."
  [m]
  (instance? Matrix m))

(defn num-rows
  "Returns the number of rows of the supplied matrix `m`. Throws if a
  non-matrix is supplied."
  [m]
  (if (matrix? m)
    (.-r ^Matrix m)
    (u/illegal (str "non-matrix supplied: " m))))

(defn num-cols
  "Returns the number of columns of the supplied matrix `m`. Throws if a
  non-matrix is supplied."
  [m]
  (if (matrix? m)
    (.-c ^Matrix m)
    (u/illegal (str "non-matrix supplied: " m))))

(defn matrix->vector
  "If `m` is already a vector, acts as identity. Else, returns the matrix as a
  vector of rows (or throws if neither of these types is passed)."
  [m]
  (cond (vector? m) m
        (matrix? m) (.-v ^Matrix m)
        :else
        (u/illegal (str "non-matrix supplied: " m))))

(defn square?
  "Returns true if `m` is a square matrix, false otherwise."
  [m]
  (and (matrix? m)
       (= (num-rows m)
          (num-cols m))))

(defn column?
  "Returns true if `m` is a matrix with a single column (a 'column matrix'),
  false otherwise."
  [m]
  (and (matrix? m)
       (= (num-cols m) 1)))

(defn row?
  "Returns true if `m` is a matrix with a single row (a 'row matrix'), false
  otherwise."
  [m]
  (and (matrix? m)
       (= (num-rows m) 1)))

(defn- m:=
  "Returns true if the matrices `this` and `that` are of identical shape and
  return `v/=` for all entries, false otherwise."
  [^Matrix this that]
  (and (instance? Matrix that)
       (let [^Matrix m that]
         (and (= (.-r this) (.-r m))
              (= (.-c this) (.-c m))
              (v/= (.-v this) (.-v m))))))

(declare make-diagonal)

(defn- matrix=scalar
  "Returns true if the matrix `m` is a diagonal matrix with all diagonal elements
  equal to `c`, false otherwise."
  [m c]
  (and (square? m)
       (m:= m (make-diagonal (num-rows m) c))))

(defn- scalar=matrix
  "Returns true if the matrix `m` is a diagonal matrix with all diagonal elements
  equal to `c`, false otherwise."
  [c m]
  (and (square? m)
       (m:= (make-diagonal (num-rows m) c) m)))

(defn generate
  "Returns a matrix with `r` rows and `c` columns, whose entries are generated by
  the supplied function `f`.

  If you only supply one dimension `n` the returned matrix will be square.

  The entry in the `i`th row and `j`-th column is `(f i j)`."
  ([n f]
   (generate n n f))
  ([r c f]
   (->Matrix r c
             (mapv (fn [i]
                     (mapv (fn [j]
                             (f i j))
                           (range c)))
                   (range r)))))

(defn literal-matrix
  "Generates a `nrows` x `ncols` matrix of symbolic entries, each prefixed by the
  supplied symbol `sym`.

  If `ncols` (the third argument) is not supplied, returns a square matrix of
  size `nrows` x `nrows`.

  NOTE: The symbols in the returned matrix record their Einstein-notation path
  into the structure that this matrix represents; a `down` of `up` columns. This
  means that the returned indices embedded in the symbols look flipped, `ji` vs
  `ij`.

  For example:

  ```clojure
  (= (literal-matrix 'x 2 2)
     (by-rows ['x_0↑0 'x_1↑0]
              ['x_0↑1 'x_1↑1]))
  ```"
  ([sym nrows]
   (literal-matrix sym nrows nrows))
  ([sym nrows ncols]
   (let [prefix (str sym "_")]
     (generate nrows ncols
               (fn [i j]
                 (symbol (str prefix j "↑" i)))))))

(defn literal-column-matrix
  "Returns a column matrix of `nrows` symbolic entries, each prefixed by the
  supplied symbol `sym`.

  For example:

  ```clojure
  (= (literal-column-matrix 'x 3)
     (by-cols ['x↑0 'x↑1 'x↑2]))
  ```"
  [sym nrows]
  (generate nrows 1
            (fn [i _]
              (symbol
               (str sym "↑" i)))))

(defn literal-row-matrix
  "Returns a row matrix of `ncols` symbolic entries, each prefixed by the
  supplied symbol `sym`.

  For example:

  ```clojure
  (= (literal-row-matrix 'x 3)
     (by-rows ['x_0 'x_1 'x_2]))
  ```"
  [sym ncols]
  (generate 1 ncols
            (fn [_ j]
              (symbol
               (str sym "_" j)))))

(defn get-in
  "Like [[clojure.core/get-in]] for matrices, but obeying the scmutils convention:
  only one index is required to get an unboxed element from a column vector.

  NOTE that this is perhaps an unprincipled exception..."
  [m is]
  (let [e (core/get-in m is)]
    (if (and (column? m)
             (= 1 (count is)))
      (e 0)
      e)))

(defn some
  "Returns true if `f` is true for some element of the matrix `m`, false
  otherwise. (Also works on arbitrary nested sequences.)"
  [f m]
  (core/some f (flatten m)))

(defn fmap
  "Maps `f` over the elements of the matrix `m` returning a new matrix of the same
  dimensions as `m`."
  [f m]
  (->Matrix (num-rows m)
            (num-cols m)
            (mapv #(mapv f %) m)))

(defn fmap-indexed
  "Maps `f` over three arguments:

  - each element of the matrix `m`
  - its row `i`
  - its column `j`

  and returns a new matrix of the same dimensions as `m`. "
  [f m]
  (letfn [(process-row [i row]
            (into [] (map-indexed
                      (fn [j elem] (f elem i j))
                      row)))]
    (let [new-rows (into [] (map-indexed
                             process-row m))]
      (->Matrix (num-rows m)
                (num-cols m)
                new-rows))))

(defn- well-formed?
  "Returns true if the supplied sequence contains only sequences of the same
  length (that could be transformed into columns of a matrix), false otherwise"
  [vs]
  {:pre [(seq vs) (every? seq vs)]}
  (let [counts (map count vs)]
    (every? #(= % (first counts))
            (next counts))))

(defn by-rows*
  "Returns a matrix whose rows consist of the supplied sequence of `rows`. These
  all must be the same length.

  for a variadic equivalent, see [[by-rows]]."
  [rows]
  (if (well-formed? rows)
    (->Matrix (count rows)
              (count (first rows))
              (mapv vec rows))
    (u/illegal "malformed matrix")))

(defn by-rows
  "Returns a matrix whose rows consist of the supplied sequence of `rows`. These
  all must be the same length.

  Variadic equivalent to [[by-rows*]]."
  [& rows]
  (by-rows* rows))

(defn by-cols*
  "Returns a matrix whose columns consist of the supplied sequence of `cols`.
  These all must be the same length.

  for a variadic equivalent, see [[by-cols]]."
  [cols]
  (if (well-formed? cols)
    (->Matrix (count (first cols))
              (count cols)
              (apply mapv vector cols))
    (u/illegal "malformed matrix")))

(defn by-cols
  "Returns a matrix whose columns consist of the supplied sequence of `cols`.
  These all must be the same length.

  Variadic equivalent to [[by-cols*]]."
  [& cols]
  (by-cols* cols))

(defn row*
  "Returns a row matrix populated by the supplied `xs`. For a variadic equivalent,
  see [[row]]."
  [xs]
  {:pre [(not-empty xs)]}
  (->Matrix 1 (count xs) [(vec xs)]))

(defn row
  "Returns a row matrix populated by the supplied `xs`. Variadic equivalent
  to [[row*]]."
  [& xs]
  (row* xs))

(defn column*
  "Returns a column matrix populated by the supplied `xs`. For a variadic equivalent,
  see [[column]]."
  [xs]
  {:pre [(not-empty xs)]}
  (->Matrix (count xs) 1 (mapv vector xs)))

(defn column
  "Returns a column matrix populated by the supplied `xs`. Variadic equivalent
  to [[column*]]."
  [& xs]
  (column* xs))

(defn transpose
  "Returns the transpose of the matrix `m`. The transpose is the original matrix,
  with rows and columns swapped."
  [m]
  (generate (num-cols m)
            (num-rows m)
            #(core/get-in m [%2 %1])))

(defn ->structure
  "Returns a structure generated by converting `m` into a nested structure with
  the supplied `outer-orientation` and `inner-orientation`.

  If `t?` is true, the columns of `m` will form the inner tuples. If `t?` is
  false, the rows of `m` will form the inner tuples.

  By default, if you supply a single argument (the matrix `m`), a matrix turns
  into a single outer `::s/down` of inner columns represented as `::up`
  structures."
  ([m] (->structure m ::s/down ::s/up true))
  ([m outer-orientation inner-orientation t?]
   {:pre [(s/valid-orientation? outer-orientation)
          (s/valid-orientation? inner-orientation)]}
   (let [m'   (if t? (transpose m) m)]
     (s/make outer-orientation
             (mapv #(s/make inner-orientation %)
                   m')))))

(defn seq->
  "Convert a sequence `xs` (typically, of function arguments) to an up-structure.

  Any matrix present in the argument list will be converted to row of columns
  via [[->structure]]."
  [xs]
  (s/up* (map (fn [m]
                (if (matrix? m)
                  (->structure m)
                  m))
              xs)))

(defn- mul
  "Returns the matrix product of `a` and `b`. Throws if `a` and `b` are
  incompatible for multiplication."
  [a b]
  (let [ra (num-rows a)
        rb (num-rows b)
        ca (num-cols a)
        cb (num-cols b)]
    (when-not (= ca rb)
      (u/illegal "matrices incompatible for multiplication"))
    (generate ra cb #(reduce
                      g/+ (for [k (range ca)]
                            (g/* (core/get-in a [%1 k])
                                 (core/get-in b [k %2])))))))

(defn- scalar*matrix
  "Returns a matrix of the same dimensions as matrix `m` with each entry
  multiplied (on the left) by the scalar quantity `c`."
  [c m]
  (fmap #(g/* c %) m))

(defn- matrix*scalar
  "Returns a matrix of the same dimensions as matrix `m` with each entry
  multiplied (on the right) by the scalar quantity `c`."
  [m c]
  (fmap #(g/* % c) m))

(defn ^:no-doc elementwise
  "Applies `f` elementwise between the matrices `a` and `b`. Throws if `a` and `b`
  don't have the same dimensions."
  [f a b]
  (let [ra (num-rows a)
        rb (num-rows b)
        ca (num-cols a)
        cb (num-cols b)]
    (when (or (not= ra rb)
              (not= ca cb))
      (u/illegal "matrices incompatible for operation"))
    (generate ra ca #(f (core/get-in a [%1 %2])
                        (core/get-in b [%1 %2])))))

(defn two-tensor->
  "Converts the square structure `s` into a matrix, and calls the supplied
  continuation `cont` with

  - the generated matrix
  - a function which will restore a matrix to a structure with the same inner
    and outer orientations as s

  Returns the result of the continuation call."
  [s cont]
  (if-let [{:keys [inner-size
                   outer-size
                   inner-orientation
                   outer-orientation]}
           (s/two-tensor-info s)]
    (let [transpose? (= inner-orientation ::s/up)
          [major-size minor-size]
          (if transpose?
            [inner-size outer-size]
            [outer-size inner-size])
          M (generate major-size minor-size
                      (fn [i j]
                        (let [path (if transpose? [j i] [i j])]
                          (core/get-in s path))))
          restore-fn (fn [m]
                       (->structure
                        m outer-orientation inner-orientation transpose?))]
      (cont M restore-fn))
    (u/illegal (str "structure " s " is not a 2-tensor"))))

(defn two-tensor-operation
  "Applies matrix operation `f` to square structure `s` and returns a structure of
  the same type as the supplied structure."
  [s f]
  (two-tensor-> s (fn [m ->s] (->s (f m)))))

(defn structure->matrix
  "Given some 2-tensor-shaped structure `s`, returns the corresponding matrix.

  The outer orientation is ignored; If the inner structures are `up`, they're
  treated as columns. Inner `down` structures are treated as rows."
  [s]
  (two-tensor-> s (fn [m _] m)))

(defn- M*u
  "Multiply a matrix by an up structure on the right. The return value is up."
  [m u]
  (when (not= (num-cols m) (count u))
    (u/illegal "matrix and tuple incompatible for multiplication"))
  (s/up*
   (map (fn [i]
          (let [row-i (nth m i)]
            (ua/generic-sum
             (fn [k]
               (g/* (nth row-i k)
                    (nth u k)))
             0 (num-cols m))))
        (range (num-rows m)))))

(defn- d*M
  "Multiply a matrix `m` by down tuple `d` on the left. The return value has
  orientation `down`."
  [d m]
  (when (not= (count d) (num-rows m))
    (u/illegal "matrix and tuple incompatible for multiplication"))
  (s/down*
   (map (fn [i]
          (ua/generic-sum
           (fn [k]
             (g/* (get d k)
                  (core/get-in m [k i])))
           0 (num-rows m)))
        (range (num-cols m)))))

(def ^{:dynamic true
       :doc "Set this dynamic variable to `false` to allow [[s->m]] to operate
  on structures for which `(* ls ms rs)` does NOT yield a numerical value."}
  *careful-conversion* true)

(defn s->m
  "Convert the structure `ms`, which would be a scalar if the (compatible)
  multiplication `(* ls ms rs)` were performed, to a matrix."
  ([ms rs]
   (let [ls (s/compatible-shape (g/* ms rs))]
     (s->m ls ms rs)))
  ([ls ms rs]
   (when *careful-conversion*
     (assert (v/numerical? (g/* ls (g/* ms rs)))))
   (let [ndowns (s/dimension ls)
         nups   (s/dimension rs)]
     (generate ndowns nups
               (fn [i j]
                 (g/* (s/unflatten
                       (s/basis-unit ndowns i) ls)
                      (g/* ms (s/unflatten
                               (s/basis-unit nups j) rs))))))))

(defn as-matrix
  "Any one argument function of a structure can be seen as a matrix. This is only
  useful if the function has a linear multiplier (e.g. derivative)"
  [F]
  (fn [s]
    (let [v (F s)]
      (s->m v s))))

(defn nth-row
  "Returns the `n`-th row of the supplied matrix `m` as a `down` structure."
  [m n]
  (s/down* (get m n)))

(defn nth-col
  "Returns the `n`-th column of the supplied matrix `m` as an `up` structure."
  [m n]
  (s/up* (map #(% n) m)))

(defn diagonal
  "Returns the diagonal of the supplied matrix `m` as an up structure. Errors if a
  type other than a diagonal matrix is supplied."
  [m]
  {:pre [(square? m)]}
  (let [rows  (num-rows m)]
    (s/up* (map #(core/get-in m [% %])
                (range 0 rows)))))

(defn up->column-matrix
  "Returns a column matrix with the contents of the supplied `up` structure.
  Errors if any other type is provided."
  [v]
  {:pre [(s/up? v)]}
  (column* v))

(defn column-matrix->up
  "Returns the single column from the supplied column matrix as an `up`. Errors if
  some other type is supplied."
  [m]
  {:pre [(column? m)]}
  (nth-col m 0))

(defn column-matrix->vector
  "Returns the single column from the supplied column matrix as a vector. Errors
  if some other type is supplied."
  [m]
  {:pre [(column? m)]}
  (mapv first m))

(defn down->row-matrix
  "Returns a row matrix with the contents of the supplied `down` structure.
  Errors if any other type is provided."
  [v]
  {:pre [(s/down? v)]}
  (by-rows (s/structure->vector v)))

(defn row-matrix->down
  "Returns the single row from the supplied row matrix as a `down`. Errors if some
  other type is supplied."
  [m]
  {:pre [(row? m)]}
  (nth-row m 0))

(defn row-matrix->vector
  "Returns the single row from the supplied row matrix as a vector. Errors if some
  other type is supplied."
  [m]
  {:pre [(row? m)]}
  (nth m 0))

(defn m->s
  "Convert the matrix `m` into a structure `S`, guided by the requirement that `(*
  ls S rs)` should be a scalar."
  [ls m rs]
  (let [ncols     (num-cols m)
        col-shape (s/compatible-shape ls)
        ms (s/unflatten (for [j (range ncols)]
                          (s/unflatten (nth-col m j) col-shape))
                        (s/compatible-shape rs))]
    (when *careful-conversion*
      (assert (v/numerical? (g/* ls (g/* ms rs)))
              (str "product is not numerical: " ls ms rs)))
    ms))

(defn s:transpose
  "Given structural inputs `ls` (optional), `ms` and `rs`, constrained such
  that `(* ls ms rs)` returns a numerical quantity, returns a result such that
  the following relationship remains true:

  ```clj
  (* <ls| (* ms |rs>)) = (* <rs| (* (s:transpose ms) |ls>))
  ```

  For example:

  ```clj
  (let [ls (s/up 1 2)
      ms (s/up (s/down 1 2) (s/down 3 4))
      rs (s/down 1 2)]
  (g/* ls (g/* ms rs))
  ;;=> 27

  (g/* rs (g/* (s:transpose ls ms rs) ls))
  ;;=> 27
  )
  ```

  `ls` is optional. If `ls` is not supplied, a compatible shape is generated
  internally."
  ([ms rs]
   (let [ls (s/compatible-shape (g/* ms rs))]
     (s:transpose ls ms rs)))
  ([ls ms rs]
   (m->s rs (transpose (s->m ls ms rs)) ls)))

(defn s:transpose-orientation
  "Given some 2 tensor `s`, returns a structure with elements 'transposed' by
  swapping the inner and outer orientations and dimensions, like a matrix
  transpose.

  Orientations are only flipped if they are different in the input. If the inner
  and outer orientations of `s` are the same, the returned structure has this
  identical orientation.

  For example:

  ```clj
  ;; opposite orientation gets flipped:
  (s:transpose-orientation (s/up (s/down 1 2 3) (s/down 4 5 6)))
  ;;=> (down (up 1 4) (up 2 5) (up 3 6))

  ;; same orientation stays the same:
  (s:transpose-orientation (s/down (s/down 1 2 3) (s/down 4 5 6)))
  ;;=> (down (down 1 4) (down 2 5) (down 3 6))
  ```

  See [[structure/two-tensor?]] for more detail on 2 tensors.

  NOTE: In scmutils, this function is called `s:transpose2`."
  [s]
  (let [ret (two-tensor-operation s transpose)]
    (if (= (s/orientation ret)
           (s/orientation (first ret)))
      ret
      (s/transpose ret))))

(declare invert)

(defn s:invert
  "Given some 2-tensor `s` (a 'square' nested structure), returns a structure
  that represents the multiplicative inverse of the supplied structure. The
  inner and outer structure orientations of `(s:invert s)` are the SAME as `s`.

  If `s` is an up-of-downs or down-of-ups, `(g/* s (s:invert s))`
  and `(g/* (s:invert s) s)` will evaluate to an identity-matrix-shaped
  up-of-downs or down-of-ups.

  If `s` is an up-of-ups or down-of-downs, multiplying `s` `(s:invert s)` will
  result in a scalar, as both structures collapse.

  NOTE: I DO NOT yet understand the meaning of this scalar! If you do, please
  open a pull request and explain it here."
  [s]
  (let [ret (two-tensor-operation s invert)]
    (if (= (s/orientation ret)
           (s/orientation (first ret)))
      (s/transpose ret)
      ret)))

(defn- delete
  "Returns the vector formed by deleting the `i`'th element of the given vector
  `v`."
  [v i]
  (if (vector? v)
    (into (subvec v 0 i)
          (subvec v (inc i)))
    (delete (into [] v) i)))

(defn with-substituted-row
  "Returns a new matrix of identical shape to `m`, with the vector `v` substituted
  for the `i`th row."
  [m i v]
  {:pre [(matrix? m)
         (<= 0 i)
         (< i (num-rows m))
         (= (num-cols m)
            (count v))]}
  (assoc m i v))

(defn submatrix
  "Returns the submatrix of the matrix (or matrix-like structure) `s` generated by
  taking

  - rows    from `lowrow` -> `hirow` (inclusive)
  - columns from `lowcol` -> `hicol` (inclusive)"
  [x lowrow hirow lowcol hicol]
  (let [m (if (s/structure? x)
            (two-tensor-> x (fn [m _] m))
            x)]
    (generate (inc (- hirow lowrow))
              (inc (- hicol lowcol))
              (fn [i j]
                (core/get-in m [(+ i lowrow)
                                (+ j lowcol)])))))

(defn without
  "Returns the matrix formed by deleting the `i`-th row and `j`-th column of the
  given matrix `m`.

  This is also called the 'minor' of m."
  [m i j]
  (->Matrix
   (dec (num-rows m))
   (dec (num-cols m))
   (mapv #(delete % j)
         (delete (matrix->vector m) i))) )

(defn- checkerboard-negate [s i j]
  (if (even? (+ i j))
    s
    (g/negate s)))

(defn dimension
  "Returns the 'dimension', ie, the number of rows & columns, of the supplied
  square matrix. Errors if some other type is supplied."
  [m]
  {:pre [(square? m)]}
  (num-rows m))

(defn trace
  "Returns the trace (the sum of diagonal elements) of the square matrix `m`.

  Generic operations are used, so this works on symbolic square matrices."
  [m]
  {:pre [(square? m)]}
  (let [rows  (num-rows m)]
    (transduce (map #(core/get-in m [% %]))
               g/+
               (range 0 rows))))

;; Note from GJS in scmutils: "Kleanthes Koniaris determinant routine, slightly
;; edited by GJS"

(defn general-determinant
  "Given coefficient procedures `add`, `sub`, `mul` and `zero?`, returns a
  procedure that efficiently computes the determinant of the supplied square
  matrix `m`.

  [[general-determinant]] is useful for generating fast type-specific
  determinant routines. See [[determinant]] for a default using generic
  arithmetic."
  [add sub mul zero?]
  (let [zero (add)]
    (fn [m]
      {:pre [(square? m)]}
      (let [c-det (atom nil)]
        (letfn [(c-det* [row [col & cols :as active-cols]]
                  (if-not cols
                    ;; one active column
                    (core/get-in m [row col])
                    (loop [idx 0
                           remaining-cols active-cols
                           answer zero]
                      (if-not (seq remaining-cols)
                        answer
                        (let [term (core/get-in m [row (first remaining-cols)])]
                          (if (zero? term)
                            (recur (inc idx)
                                   (rest remaining-cols)
                                   answer)
                            (let [without-i (delete active-cols idx)
                                  delta (mul term (@c-det (inc row) without-i))]
                              (recur (inc idx)
                                     (rest remaining-cols)
                                     (if (even? idx)
                                       (add answer delta)
                                       (sub answer delta))))))))))]
          (reset! c-det (memoize c-det*))
          (@c-det 0 (range (dimension m))))))))

(def ^{:doc "Returns the determinant of the supplied square matrix `m`.

  Generic operations are used, so this works on symbolic square matrices."
       :arglists '([m])}
  determinant
  (general-determinant g/+ g/- g/* v/numeric-zero?))

(defn cofactors
  "Returns the matrix of cofactors of the supplied square matrix `m`."
  [m]
  {:pre [(square? m)]}
  (let [r (num-rows m)]
    (cond (< r 2) m
          (= r 2) (let [[[a b] [c d]] m]
                    (->Matrix 2 2 [[d (g/negate c)]
                                   [(g/negate b) a]]))
          :else (generate r r
                          (fn [i j]
                            (-> (without m i j)
                                (determinant)
                                (checkerboard-negate i j)))))))

;;; The following implements the classical adjoint formula for the inverse of a
;;; matrix. This may be useful for symbolic applications.

(defn classical-adjoint-formula
  "Given coefficient procedures `add`, `sub`, `mul` and `zero?`, returns a
  procedure that efficiently computes the inverse of the supplied square
  matrix `m`.

  [[classical-adjoint-formula]] is useful for generating fast type-specific
  matrix inversion routines. See [[invert]] for a default using generic
  arithmetic."
  [add sub mul div zero?]
  (let [det (general-determinant add sub mul zero?)]
    (fn inv [A]
      (let [dim (dimension A)]
        (if (= dim 1)
          (->Matrix 1 1 [[(div (core/get-in A [0 0]))]])
          (let* [d  (det A)
                 -d (sub d)]
            (generate dim dim
                      (fn [i j]
                        (let [denom (if (even? (+ i j)) d -d)]
                          (div (det (without A j i)) denom))))))))))

(def ^{:doc "Returns the inverse of the supplied square matrix `m`."
       :arglists '([A])}
  invert
  (classical-adjoint-formula g/+ g/- g/* g// v/numeric-zero?))

(defn- m-div-m [m1 m2]
  (mul m1 (invert m2)))

(defn- m-div-c
  "Returns the result of multiplying (on the right) the scalar `c` by the inverse
  of matrix `m`."
  [m c]
  (matrix*scalar m (g/invert c)))

(defn- c-div-m
  "Returns the result of multiplying (on the left) the scalar `c` by the inverse
  of matrix `m`."
  [c m]
  (scalar*matrix c (invert m)))

(defn s:inverse
  ([ms rs]
   (let [ls (s/compatible-shape (g/* ms rs))]
     (s:inverse ls ms rs)))
  ([ls ms rs]
   (m->s (s/compatible-shape rs)
         (invert (s->m ls ms rs))
         (s/compatible-shape ls))))

(defn s:solve-linear-left [M product]
  (let [cp (s/compatible-shape product)
        cr (s/compatible-shape (s/s:* cp M))]
    (s/s:* (s:inverse cp M cr) product)))

(defn s:solve-linear-right [product M]
  (let [cp (s/compatible-shape product)
        cr (s/compatible-shape (s/s:* M cp))]
    (s/s:* product (s:inverse cr M cp))))

(defn s:divide-by-structure [rv s]
  (s:solve-linear-left s rv))

(defn make-zero
  "Return a zero-valued matrix of `m` rows and `n` columns (`nXn` if only `n` is
  supplied)."
  ([n] (make-zero n n))
  ([m n] (generate m n (constantly 0))))

(defn I
  "Return the identity matrix of order `n`."
  [n]
  (generate n n s/kronecker))

(defn identity-like
  "Return an identity matrix whose ones and zeros match the types of the supplied
  square matrix `M`. Errors if a non-square matrix `M` is supplied."
  [M]
  (if-not (square? M)
    (u/illegal "identity-like on non-square")
    (fmap-indexed (fn [elem i j]
                    (if (= i j)
                      (v/one-like elem)
                      (v/zero-like elem)))
                  M)))

(defn identity?
  "Returns true if the supplied matrix `m` is an identity matrix, false
  otherwise."
  [m]
  (and (square? m)
       (let [n (dimension m)]
         (every? true?
                 (for [i (range n)
                       j (range n)
                       :let [entry (core/get-in m [i j])]]
                   (if (= i j)
                     (v/one? entry)
                     (v/zero? entry)))))))

(defn make-diagonal
  "Given a single (sequential) argument `v`, returns the diagonal matrix of
  order `(count v)` with the elements of the sequence `v` along the diagonal.

  Given two arguments `n` and some constant `x`, returns a diagonal `n` by `n`
  matrix with `x` in every entry of the diagonal.

  `(make-diagonal <n> 1)` is equivalent to `(I n)`."
  ([v]
   (let [v (vec v)
         n (count v)]
     (generate n n (fn [i j]
                     (if (= i j) (v i) 0)))))
  ([n x]
   (generate n n #(if (= %1 %2) x 0))))

(defn diagonal?
  "Returns true if `m` is a diagonal matrix (ie, a square matrix where every
  non-diagonal element is zero), false otherwise."
  [m]
  (and (square? m)
       (let [n (dimension m)]
         (every? true?
                 (for [i (range n)
                       j (range n)
                       :when (not= i j)
                       :let [entry (core/get-in m [i j])]]
                   (v/zero? entry))))))

(defn symmetric?
  "Returns true if the supplied matrix `M` is equal to its own transpose (ie,
  symmetric), false otherwise."
  [M]
  (v/zero?
   (g/simplify
    (g/sub (transpose M) M))))

(defn antisymmetric?
  "Returns true if the supplied matrix `M` is equal to the negation of its own
  transpose (ie, antisymmetric), false otherwise."
  [M]
  (v/zero?
   (g/simplify
    (g/add (transpose M) M))))

(defn characteristic-polynomial
  "Returns the [characteristic
  polynomial](https://en.wikipedia.org/wiki/Characteristic_polynomial) of the
  square matrix `m`.

  If only `m` is supplied, returns a [[polynomial/Polynomial]] instance
  representing the matrix `m`'s characteristic polynomial.

  If `x` is supplied, returns the value of the characteristic polynomial of `m`
  evaluated at `x`.

  Typically `x` will be a symbolic variable, but if you wanted to get the value
  of the characteristic polynomial at some particular numerical point `x` you
  could pass that too."
  ([m]
   (characteristic-polynomial m (poly/identity)))
  ([m x]
   (let [r (num-rows m)
         c (num-cols m)]
     (when-not (= r c) (u/illegal "not square"))
     (let [Ix (make-diagonal r x)]
       (determinant
        (g/- Ix m))))))

;; ## Solving

(defn cramers-rule
  "Given coefficient procedures `add`, `sub`, `mul`, `div` and `zero?`, returns a
  procedure that efficiently computes the solution to an inhomogeneous system of
  linear equations, `A*x=b`, where the matrix `A` and the column matrix `b` are
  given. The returned procedure returns the column matrix `x`.

  Unlike LU decomposition, Cramer's rule generalizes to symbolic solutions.

  [[cramers-rule]] is useful for generating fast type-specific linear equation
  solvers. See [[solve]] for a default using generic arithmetic."
  [add sub mul div zero?]
  (let [det (general-determinant add sub mul zero?)]
    (fn solve [A b]
      {:pre [(square? A)
             (column? b)
             (= (dimension A)
                (num-rows b))]}
      (let [bv (nth-col b 0)
            bn (num-rows b)
            d  (det A)
            At (transpose A)]
        (column*
         (mapv (fn [i]
                 (div (det (with-substituted-row At i bv))
                      d))
               (range bn)))))))

(def ^{:doc "Given a matrix `A` and a column matrix `b`, computes the solution
  to an inhomogeneous system of linear equations, `A*x=b`, where the matrix `A`
  and the column matrix `b` are given.

 Returns the column matrix `x`.

 Unlike LU decomposition, Cramer's rule generalizes to symbolic solutions."
       :arglists '([A b])}
  solve
  (cramers-rule g/+ g/- g/* g// v/numeric-zero?))

(defn rsolve
  "Generalization of [[solve]] that can handle `up` and `down` structures, as well
  as `row` and `column` matrices.

  Given `row` or `down` values for `b`, `A` is appropriately transposed before
  solving."
  [b A]
  (cond (s/up? b)   (column-matrix->up
                     (solve A (up->column-matrix b)))
        (column? b) (solve A b)
        (s/down? b) (row-matrix->down
                     (transpose
                      (solve (transpose A)
                             (transpose
                              (down->row-matrix b)))))
        (row? b)
        (transpose
         (solve (transpose A)
                (transpose b)))
        :else (u/illegal (str "I don't know how to solve:" b A))))

;; ## Generic Operation Installation

(defmethod v/= [::matrix ::matrix] [a b] (m:= a b))
(defmethod v/= [::square-matrix ::v/scalar] [m c] (matrix=scalar m c))
(defmethod v/= [::v/scalar ::square-matrix] [c m] (scalar=matrix c m))

(defmethod g/negate [::matrix] [a] (fmap g/negate a))

(defmethod g/sub [::matrix ::matrix] [a b] (elementwise g/- a b))
(defmethod g/sub [::square-matrix ::v/scalar] [a b]
  (elementwise g/- a (make-diagonal (num-rows a) b)))

(defmethod g/sub [::v/scalar ::square-matrix] [a b]
  (elementwise g/- (make-diagonal (num-rows b) a) b))

(defmethod g/add [::matrix ::matrix] [a b] (elementwise g/+ a b))
(defmethod g/add [::square-matrix ::v/scalar] [a b]
  (elementwise g/+ a (make-diagonal (num-rows a) b)))

(defmethod g/add [::v/scalar ::square-matrix] [a b]
  (elementwise g/+ (make-diagonal (num-rows b) a) b))

(defmethod g/mul [::matrix ::matrix] [a b] (mul a b))
(defmethod g/mul [::v/scalar ::matrix] [n a] (scalar*matrix n a))
(defmethod g/mul [::matrix ::v/scalar] [a n] (matrix*scalar a n))

(defmethod g/mul [::matrix ::s/up] [m u] (M*u m u))
(defmethod g/mul [::s/down ::matrix] [d m] (d*M d m))

(defmethod g/div [::matrix ::v/scalar] [m c] (m-div-c m c))
(defmethod g/div [::v/scalar ::square-matrix] [c m] (c-div-m c m))
(defmethod g/div [::column-matrix ::square-matrix] [c m] (rsolve c m))
(defmethod g/div [::row-matrix ::square-matrix] [r m] (rsolve r m))
(defmethod g/div [::s/up ::square-matrix] [u m] (rsolve u m))
(defmethod g/div [::s/down ::square-matrix] [d m] (rsolve d m))
(defmethod g/div [::matrix ::square-matrix] [d m] (m-div-m d m))

(defmethod g/exp [::square-matrix] [m] (series/exp-series m))
(defmethod g/cos [::square-matrix] [m] (series/cos-series m))
(defmethod g/sin [::square-matrix] [m] (series/sin-series m))
(defmethod g/tan [::square-matrix] [m] (series/tan-series m))
(defmethod g/sec [::square-matrix] [m] (series/sec-series m))
(defmethod g/acos [::square-matrix] [m] (series/acos-series m))
(defmethod g/asin [::square-matrix] [m] (series/asin-series m))
(defmethod g/atan [::square-matrix] [m] (series/atan-series m))
(defmethod g/acot [::square-matrix] [m] (series/acot-series m))
(defmethod g/cosh [::square-matrix] [m] (series/cosh-series m))
(defmethod g/sinh [::square-matrix] [m] (series/sinh-series m))
(defmethod g/tanh [::square-matrix] [m] (series/tanh-series m))
(defmethod g/asinh [::square-matrix] [m] (series/asinh-series m))
(defmethod g/atanh [::square-matrix] [m] (series/atanh-series m))
(defmethod g/simplify [::matrix] [m] (fmap g/simplify m))

(defmethod g/invert [::matrix] [m] (invert m))

(defmethod g/make-rectangular [::matrix ::matrix] [a b]
  (elementwise g/make-rectangular a b))

(defmethod g/make-polar [::matrix ::matrix] [a b]
  (elementwise g/make-polar a b))

(defmethod g/real-part [::matrix] [m] (fmap g/real-part m))
(defmethod g/imag-part [::matrix] [m] (fmap g/imag-part m))
(defmethod g/conjugate [::matrix] [m]  (fmap g/conjugate m))

(defmethod g/transpose [::matrix] [m] (transpose m))

(defmethod g/determinant [::square-matrix] [m] (determinant m))
(defmethod g/determinant [::s/structure] [s]
  (two-tensor-> s (fn [m _] (determinant m))))

(defmethod g/trace [::square-matrix] [m] (trace m))
(defmethod g/trace [::s/structure] [s]
  (two-tensor-> s (fn [m _] (trace m))))

(defmethod g/invert [::s/structure] [a]
  (s:invert a))

(defmethod g/div [::s/structure ::s/structure] [rv s]
  (s:divide-by-structure rv s))

(defmethod g/solve-linear [::square-matrix ::s/up] [A b] (rsolve b A))
(defmethod g/solve-linear [::square-matrix ::s/down] [A b] (rsolve b A))
(defmethod g/solve-linear [::square-matrix ::column-matrix] [A b] (rsolve b A))
(defmethod g/solve-linear [::square-matrix ::row-matrix] [A b] (rsolve b A))
(defmethod g/solve-linear [::s/structure ::s/structure] [s product]
  (s:solve-linear-left s product))

(defmethod g/solve-linear [::s/structure ::v/scalar] [s c]
  (s/structure*scalar (s:invert s) c))

(defmethod g/solve-linear-right [::row-matrix ::square-matrix] [b A] (rsolve b A))
(defmethod g/solve-linear-right [::down ::square-matrix] [b A] (rsolve b A))
(defmethod g/solve-linear-right [::s/structure ::s/structure] [product s]
  (s:solve-linear-right product s))

(defmethod g/solve-linear-right [::v/scalar ::s/structure] [c s]
  (s/scalar*structure c (s:invert s)))

(defmethod g/dimension [::square-matrix] [m] (dimension m))
(defmethod g/dimension [::column-matrix] [m] (num-rows m))
(defmethod g/dimension [::row-matrix] [m] (num-cols m))

;; ## Column / Row Matrices only...

(defmethod g/dot-product [::row-matrix ::row-matrix] [a b]
  (g/dot-product (row-matrix->down a)
                 (row-matrix->down b)))

(defmethod g/dot-product [::column-matrix ::column-matrix] [a b]
  (g/dot-product (column-matrix->up a)
                 (column-matrix->up b)))

(defmethod g/inner-product [::row-matrix ::row-matrix] [a b]
  (g/inner-product (row-matrix->vector a)
                   (row-matrix->vector b)))

(defmethod g/inner-product [::column-matrix ::column-matrix] [a b]
  (g/inner-product (column-matrix->up a)
                   (column-matrix->up b)))

(defmethod g/cross-product [::row-matrix ::row-matrix] [a b]
  (by-rows
   (s/structure->vector
    (g/cross-product (row-matrix->vector a)
                     (row-matrix->vector b)))))

(defmethod g/cross-product [::column-matrix ::column-matrix] [a b]
  (up->column-matrix
   (g/cross-product (column-matrix->up a)
                    (column-matrix->up b))))

(defmethod g/outer-product [::column-matrix ::row-matrix] [a b] (mul a b))

(defmethod g/partial-derivative [::matrix v/seqtype] [M selectors]
  (fmap #(g/partial-derivative % selectors)
        M))
