#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.quaternion
  "This namespace provides a number of functions and constructors for working
  with [[Quaternion]] instances in Clojure and ClojureScript, and
  installs [[Quaternion]] into the Emmy generic arithmetic system.

  For other numeric extensions, see [[emmy.ratio]], [[emmy.complex]]
  and [[emmy.numbers]]."
  (:refer-clojure :exclude [zero?])
  (:require [emmy.complex :as sc]
            [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as m]
            [emmy.structure :as ss]
            [emmy.util :as u]
            [emmy.util.logic :as ul]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang AFn Associative
                            MapEntry
                            IObj IFn IReduce IKVReduce
                            Indexed Sequential
                            Reversible))))

;; # Quaternions
;;
;; The [quaternion number system](https://en.wikipedia.org/wiki/Quaternion) is
;; an extension of the complex number system. By introducing /three/ "imaginary"
;; numbers $i$, $j$ and $k$, William Hamilton was able to devise a number system
;; capable of representing rotations in 3-dimensional space, analogous to the
;; ability of complex numbers to represent rotation and scaling in the 2d plane.
;;
;; The book ["A History of Vector
;; Analysis"](https://www.amazon.com/History-Vector-Analysis-Evolution-Mathematics/dp/0486679101)
;; tells the full historical tale, from Hamilton's discovery of quaternions up
;; through modern vector analysis. This namespace is mostly concerned with the
;; implementation and manipulation of quaternions, but I expect more writing
;; will appear in the library on this fascinating topic.
;;
;; This namespace begins with an implementation of a [[Quaternion]] type along
;; with a number of accessors and constructors. Next comes a suite of arithmetic
;; functions, followed by transcendental functions
;; like [[exp]], [[log]], [[sin]] etc.
;;
;; Next comes an API for describing rotations in various ways, and converting
;; between quaternions and other representations of rotations, like 3D and 4D
;; matrices, Euler angles and more.
;;
;; Finally, the namespace ends by installing the [[Quaternion]] type into the
;; generic system. Quaternions are compatible with complex numbers and real
;; numbers, and interact with them by casting them up to [[Quaternion]]
;; instances.
;;
;; ### Sources
;;
;; This implementation was inspired by a number of excellent Quaternion
;; libraries:
;;
;; - The original scmutils implementation in Scheme, of course
;; - [Spire's implementation in Scala](https://github.com/typelevel/spire/blob/main/core/src/main/scala/spire/math/Quaternion.scala#L202)
;; - [weavejester's Euclidean library in
;;   Clojure](https://github.com/weavejester/euclidean) and his [associated
;;   talk](https://www.booleanknot.com/slides/functional-3d-game-design.pdf)
;; - [Boost's C++ Quaternion package](https://www.boost.org/doc/libs/1_78_0/libs/math/doc/html/quaternions.html)
;; - [quaternions in C++, by ferd36](https://github.com/ferd36/quaternions)
;; - [Quaternion.js](https://github.com/infusion/Quaternion.js/)
;;
;; For more reading, see:
;;
;; - ["Quaternion"](https://en.wikipedia.org/wiki/Quaternion) on Wikipedia
;; - ["Octonion" on Wikipedia](https://en.wikipedia.org/wiki/Octonion), for an
;;   extension of the quaternions to 8 dimensions
;; - ["Cayley-Dickson
;;   Construction"](https://en.wikipedia.org/wiki/Cayley%E2%80%93Dickson_construction)
;;   for a method that allows you to build successively higher-order extensions
;;   of the real, complex, quaternion, octonion, sedonion tower.

(declare arity eq evaluate zero? one?)

;; ## Quaternion Type Definition
;;
;; The [[Quaternion]] type is a container for the four coefficients $(a, b, c,
;; d)$ of a quaternion of the form $a + bi + cj + dk$, along with an optional
;; metadata entry `m`. (The coefficient fields are named `r`, `i`, `j` and `k`
;; after the associated imaginary.)
;;
;; The type implements a number of protocols, designed to achieve the following
;; goals:
;;
;; - Quaternions should be `seq`-able into a sequence of the coefficient
;;   entries, but also act like vectors with a fixed count of 4, capable of
;;   supporting `rseq`, indexed lookups and efficient reductions.
;;
;; - Unlike a vector, if a [[Quaternion]] has function entries, applying the
;;   quaternion as a function should apply each component to the arguments and
;;   return a new quaternion with the results as the new coefficients.
;;
;; - The `D` operator should work on quaternions, so derivatives of functions
;;   that return quaternions work well.
;;
;; - All [[emmy.value/Value]] functions should work well.

(deftype Quaternion [r i j k m]
  f/IArity
  (arity [this] (arity this))

  d/IPerturbed
  (perturbed? [_]
    (or (d/perturbed? r)
        (d/perturbed? i)
        (d/perturbed? j)
        (d/perturbed? k)))

  (replace-tag [_ old new]
    (Quaternion.
     (d/replace-tag r old new)
     (d/replace-tag i old new)
     (d/replace-tag j old new)
     (d/replace-tag k old new)
     m))

  (extract-tangent [_ tag]
    (Quaternion.
     (d/extract-tangent r tag)
     (d/extract-tangent i tag)
     (d/extract-tangent j tag)
     (d/extract-tangent k tag)
     m))

  v/Value
  (zero? [this] (zero? this))
  (one? [this] (one? this))
  (identity? [this] (one? this))

  (zero-like [_]
    (Quaternion. (v/zero-like r) 0 0 0 m))
  (one-like [_]
    (Quaternion. (v/one-like r) 0 0 0 m))
  (identity-like [_]
    (Quaternion. (v/one-like r) 0 0 0 m))

  (exact? [_]
    (and (v/exact? r)
         (v/exact? i)
         (v/exact? j)
         (v/exact? k)))
  (freeze [_]
    (list 'quaternion
          (v/freeze r)
          (v/freeze i)
          (v/freeze j)
          (v/freeze k)))
  (kind [_] ::quaternion)

  #?@(:clj
      [Object
       (equals [self q] (eq self q))
       (toString [_] (str "#sicm/quaternion [" r " " i  " " j " " k "]"))

       IObj
       (meta [_] m)
       (withMeta [_ m] (Quaternion. r i j k m))

       Sequential

       Associative
       (assoc [_ key v]
              (if (int? key)
                (case (int key)
                  0 (Quaternion. v i j k m)
                  1 (Quaternion. r v j k m)
                  2 (Quaternion. r i v k m)
                  3 (Quaternion. r i j v m)
                  (throw (IndexOutOfBoundsException.)))
                (throw (IllegalArgumentException.))))

       (containsKey [_ k] (boolean (#{0 1 2 3} k)))
       (entryAt [this k]
                (when-let [v (.valAt ^Associative this k nil)]
                  (MapEntry. k v)))
       (cons [_ _]
             (throw
              (UnsupportedOperationException.
               (str "cons not suported on Quaternion instances. convert to"
                    " vector first!"))))
       (count [_] 4)
       (seq [_] (list r i j k))
       (valAt [this key]
              (.valAt ^Associative this key nil))
       (valAt [_ key not-found]
              (if (int? key)
                (case (int key)
                  0 r
                  1 i
                  2 j
                  3 k
                  not-found)
                not-found))
       (empty [_] (Quaternion. 0 0 0 0 m))
       (equiv [this that] (.equals this that))

       Indexed
       (nth [this k] (.valAt ^Associative this k))
       (nth [this k default] (.valAt ^Associative this k default))

       IReduce
       (reduce [_ f] (f (f (f r i) j) k))
       (reduce [_ f start] (f (f (f (f start r) i) j) k))

       IKVReduce
       (kvreduce [_ f init]
                 (-> (f init r 0)
                     (f i 1)
                     (f j 2)
                     (f k 3)))

       Reversible
       (rseq [_] (list k j i r))

       IFn
       (invoke [this]
               (evaluate this []))
       (invoke [this a]
               (evaluate this [a]))
       (invoke [this a b]
               (evaluate this [a b]))
       (invoke [this a b c]
               (evaluate this [a b c]))
       (invoke [this a b c d]
               (evaluate this [a b c d]))
       (invoke [this a b c d e]
               (evaluate this [a b c d e]))
       (invoke [this a b c d e f]
               (evaluate this [a b c d e f]))
       (invoke [this a b c d e f g]
               (evaluate this [a b c d e f g]))
       (invoke [this a b c d e f g h]
               (evaluate this [a b c d e f g h]))
       (invoke [this a b c d e f g h i]
               (evaluate this [a b c d e f g h i]))
       (invoke [this a b c d e f g h i j]
               (evaluate this [a b c d e f g h i j]))
       (invoke [this a b c d e f g h i j k]
               (evaluate this [a b c d e f g h i j k]))
       (invoke [this a b c d e f g h i j k l]
               (evaluate this [a b c d e f g h i j k l]))
       (invoke [this a b c d e f g h i j k l m]
               (evaluate this [a b c d e f g h i j k l m]))
       (invoke [this a b c d e f g h i j k l m n]
               (evaluate this [a b c d e f g h i j k l m n]))
       (invoke [this a b c d e f g h i j k l m n o]
               (evaluate this [a b c d e f g h i j k l m n o]))
       (invoke [this a b c d e f g h i j k l m n o p]
               (evaluate this [a b c d e f g h i j k l m n o p]))
       (invoke [this a b c d e f g h i j k l m n o p q]
               (evaluate this [a b c d e f g h i j k l m n o p q]))
       (invoke [this a b c d e f g h i j k l m n o p q r]
               (evaluate this [a b c d e f g h i j k l m n o p q r]))
       (invoke [this a b c d e f g h i j k l m n o p q r s]
               (evaluate this [a b c d e f g h i j k l m n o p q r s]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t]
               (evaluate this [a b c d e f g h i j k l m n o p q r s t]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
               (evaluate this (into [a b c d e f g h i j k l m n o p q r s t] rest)))
       (applyTo [s xs] (AFn/applyToHelper s xs))]

      :cljs
      [Object
       (toString [_]
                 (str "#sicm/quaternion [" r " " i  " " j " " k "]"))

       IEquiv
       (-equiv [this that] (eq this that))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ meta] (Quaternion. r i j k meta))

       ISequential

       IEmptyableCollection
       (-empty [_] (Quaternion. 0 0 0 0 m))

       ICollection
       (-conj [_ _]
              (throw
               (js/Error.
                (str "conj not suported on Quaternion instances. convert to"
                     " vector first!"))))

       ISeqable
       (-seq [_] (list r i j k))

       ICounted
       (-count [_] 4)

       IIndexed
       (-nth [this n] (-nth this n nil))
       (-nth [_ n default]
             (case n
               0 r
               1 i
               2 j
               3 k
               default))

       ILookup
       (-lookup [this key] (-lookup this key nil))
       (-lookup [this key not-found]
                (if (number? key)
                  (-nth this key not-found)
                  not-found))

       IAssociative
       (-assoc [_ key v]
               (case key
                 0 (Quaternion. v i j k m)
                 1 (Quaternion. r v j k m)
                 2 (Quaternion. r i v k m)
                 3 (Quaternion. r i j v m)
                 (throw
                  (js/Error.
                   "Quaternion's key for assoc must be 0, 1, 2 or 3."))))
       (-contains-key? [_ k]
                       (boolean
                        (#{0 1 2 3} k)))

       IFind
       (-find [this n]
              (when (-contains-key? this n)
                (MapEntry. n (-nth this n nil) nil)))

       IReduce
       (-reduce [_ f] (f (f (f r i) j) k))
       (-reduce [_ f start] (f (f (f (f start r) i) j) k))

       IKVReduce
       (-kv-reduce [_ f init]
                   (-> (f init r 0)
                       (f i 1)
                       (f j 2)
                       (f k 3)))

       IReversible
       (-rseq [_] (list k j i r))

       IIterable
       (-iterator [this] (ranged-iterator (vec this) 0 4))

       IFn
       (-invoke [this]
                (evaluate this []))
       (-invoke [this a]
                (evaluate this [a]))
       (-invoke [this a b]
                (evaluate this [a b]))
       (-invoke [this a b c]
                (evaluate this [a b c]))
       (-invoke [this a b c d]
                (evaluate this [a b c d]))
       (-invoke [this a b c d e]
                (evaluate this [a b c d e]))
       (-invoke [this a b c d e f]
                (evaluate this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (evaluate this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (evaluate this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (evaluate this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (evaluate this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (evaluate this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (evaluate this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (evaluate this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (evaluate this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (evaluate this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (evaluate this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (evaluate this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (evaluate this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (evaluate this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (evaluate this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (evaluate this (into [a b c d e f g h i j k l m n o p q r s t] rest)))]))

#_{:clj-kondo/ignore [:redefined-var]}
(do (ns-unmap 'emmy.quaternion '->Quaternion)
    (defn ->Quaternion
      "Positional factory function for [[Quaternion]].

  The final metadata argument `m` defaults to nil if not supplied."
      ([r i j k]
       (Quaternion. r i j k nil))
      ([r i j k m]
       (Quaternion. r i j k m))))

#?(:clj
   (defmethod print-method Quaternion
     [^Quaternion q ^java.io.Writer w]
     (.write w (.toString q))))

(defn quaternion?
  "Returns `true` if `q` is an instance of [[Quaternion]], false otherwise."
  [q]
  (instance? Quaternion q))

;; ## Component Accessors
;;
;; The following functions provide access to specific components, or
;; coefficients, of a quaternion `q`, as well as other named things you might
;; like to extract, like the quaternion's vector or real components.

(defn get-r
  "Returns the `r` component of the supplied quaternion `q`.

  Identical to [[real-part]]."
  [^Quaternion q]
  (.-r q))

(defn real-part
  "Returns the `r` component of the supplied quaternion `q`.

  Identical to [[get-r]]."
  [^Quaternion q]
  (.-r q))

(defn get-i
  "Returns the `i` component of the supplied quaternion `q`."
  [^Quaternion q]
  (.-i q))

(defn get-j
  "Returns the `j` component of the supplied quaternion `q`."
  [^Quaternion q]
  (.-j q))

(defn get-k
  "Returns the `k` component of the supplied quaternion `q`."
  [^Quaternion q]
  (.-k q))

(defn complex-1
  "Returns the `r` and `i` components of the quaternion `q` as a `Complex` number
  instance."
  [q]
  (sc/complex (get-r q) (get-i q)))

(defn complex-2
  "Returns the `j` and `k` components of the quaternion `q` as a `Complex` number
  instance."
  [q]
  (sc/complex (get-j q) (get-k q)))

(defn ->complex-pair
  "Returns a pair of complex number created respectively from the `(r,i)`
  and `(j,k)` components of the supplied quaternion `q`.

  NOTE that this only works if the coefficients of `q` are real numbers, due to
  restrictions on the current complex number implementation. "
  [q]
  {:pre [(quaternion? q)]}
  [(complex-1 q) (complex-2 q)])

(defn ->vector
  "Returns a 4-vector of the coefficients of quaternion `q`.

  Works identically to `(vec q)`, but more efficient as we are able to create
  the new vector in one shot."
  [q]
  {:pre [(quaternion? q)]}
  [(get-r q) (get-i q) (get-j q) (get-k q)])

(defn three-vector
  "Returns a 3-vector holding the coefficients of the non-real (imaginary)
  components of the quaternion `q`."
  [q]
  {:pre [(quaternion? q)]}
  [(get-i q) (get-j q) (get-k q)])

;; ## Quaternion Predicates

(defn real?
  "Returns true if the quaternion `q` has zero entries for all non-real fields,
  false otherwise."
  [q]
  (and (v/zero? (get-i q))
       (v/zero? (get-j q))
       (v/zero? (get-k q))))

(defn zero?
  "Returns true if `q` is a quaternion with zeros in all coefficient positions,
  false otherwise."
  [q]
  (and (real? q) (v/zero? (get-r q))))

(defn one?
  "Returns true if `q` is a [[real?]] quaternion with a one-like coefficient in
  the real position, false otherwise."
  [q]
  (and (real? q) (v/one? (get-r q))))

(defn pure?
  "Returns true if the quaternion `q` has a zero real entry, false otherwise.

  A 'pure' quaternion is sometimes called an 'imaginary' quaternion."
  [q]
  (v/zero? (get-r q)))

(declare magnitude-sq)

(defn unit?
  "Returns true if `q` is a unit quaternion (ie, a 'versor', a quaternion
  with [[magnitude]] equal to one), false otherwise.

  To check if the [[magnitude]] of `q` is /approximately/ equal to one, pass a
  tolerance via the `:epsilon` keyword argument.

  For more control, use [[magnitude]] to compute the magnitude directly."
  ([q & {:keys [epsilon]}]
   (let [mag-sq (magnitude-sq q)]
     (if epsilon
       ((v/within epsilon) 1 (g/sqrt mag-sq))
       (v/one? mag-sq)))))

(defn eq
  "Returns true if the supplied quaternion `q1` is equal to the value `q2`. The
  rules for [[eq]] are as follows:

  - If `q2` is a quaternion, returns true if all coefficients match, false
    otherwise

  - If `q2` is complex, returns true if the real and `i` coefficients are equal,
    with `j` and `k` coefficients of `q1` equal to zero, false otherwise

  - If `q2` is sequential with a count of 4, it's interpreted as a vector of
    quaternion coefficients.

  Else, if `q1` is a [[real?]] quaternion, returns true if the real component of
  `q1` is [[emmy.value/=]] to `q2`, false otherwise."
  [q1 q2]
  (or (identical? q1 q2)
      (let [r (get-r q1)
            i (get-i q1)
            j (get-j q1)
            k (get-k q1)]
        (cond
          (quaternion? q2)
          (let [q2 ^Quaternion q2]
            (and (v/= r (.-r q2))
                 (v/= i (.-i q2))
                 (v/= j (.-j q2))
                 (v/= k (.-k q2))))

          (sc/complex? q2)
          (and (v/= r (sc/real q2))
               (v/= i (sc/imaginary q2))
               (v/zero? j)
               (v/zero? k))

          (sequential? q2)
          (and (= (count q2) 4)
               (= r (nth q2 0))
               (= i (nth q2 1))
               (= j (nth q2 2))
               (= k (nth q2 3)))

          :else (and (real? q1)
                     (v/= r q2))))))

;; ## Constructors

(def ^{:doc "The zero quaternion. All coefficients are equal to 0."}
  ZERO (->Quaternion 0 0 0 0))

(def ^{:doc "The identity quaternion. The real coefficient is equal to 1, and
  all coefficients are equal to 0."}
  ONE (->Quaternion 1 0 0 0))

(def ^{:doc "Unit quaternion with `i` coefficient equal to 1, all other
  coefficients equal to 0."}
  I (->Quaternion 0 1 0 0))

(def ^{:doc "Unit quaternion with `j` coefficient equal to 1, all other
  coefficients equal to 0."}
  J (->Quaternion 0 0 1 0))

(def ^{:doc "Unit quaternion with `k` coefficient equal to 1, all other
  coefficients equal to 0."}
  K (->Quaternion 0 0 0 1))

(defn make
  "Constructor that builds [[Quaternion]] instances out of a variety of types.
  Given:

  - a quaternion `x`, acts as identity.

  - a sequential `x`, returns a quaternion with coefficients built from the
    first four entries.

  - a complex number `x`, returns a quaternion built from the real and imaginary
    components of `x` with `j` and `k` components equal to zero.

  - a real number `x` and 3-vector, returns a quaternion with real coefficient
    equal to `x` and imaginary components equal to the elements of the vector

  - 4 distinct arguments `r`, `i`, `j` and `k`, returns a quaternion with these
    as the coefficients."
  ([x]
   (cond (quaternion? x) x
         (sequential? x) (apply ->Quaternion (take 4 x))
         (sc/complex? x) (->Quaternion
                          (sc/real x)(sc/imaginary x) 0 0)
         :else (->Quaternion x 0 0 0)))
  ([r [i j k]]
   (->Quaternion r i j k))
  ([r i j k]
   (->Quaternion r i j k)))

(defn ^:no-doc parse-quaternion
  "Implementation of a reader literal that turns literal 4-vectors into calls
  to [[make]]. For all other input, call [[make]] directly.

  Installed by default under #sicm/quaternion."
  [x]
  (if (vector? x)
    (if (= (count x) 4)
      (let [[r i j k] x]
        `(make ~r ~i ~j ~k))
      (u/illegal
       (str "Quaternion literal vectors require 4 elements. Received: " x)))
    `(make ~x)))

(defn from-complex
  "Given two complex numbers `a` and `b`, returns a quaternion instance with

  - `r` and `i` components set to the real and imaginary components of `a`
  - `j` and `k` components set to the real and imaginary components of `b`"
  [a b]
  (make (g/real-part a)
        (g/imag-part a)
        (g/real-part b)
        (g/imag-part b)))

(declare scale-l)

(defn spherical
  "Generates a [[Quaternion]] instance, given:

  - a magnitude `r`
  - a rotation angle `theta`, with a natural range of `-2*pi` to `2*pi`
  - `colat`, the [colatitude](https://mathworld.wolfram.com/Colatitude.html) of
    the (non-real) vectorial part of the quaternion
  - `lon`, the longitude of the vectorial part of the quaternion"
  [r theta colat lon]
  (let [half-t (g// theta 2)
        cos-ht (g/cos half-t)
        sin-ht (g/sin half-t)
        sin-co (g/sin colat)
        r*sin-ht (g/* r sin-ht)
        r*sin-ht*sin-co (g/* r*sin-ht sin-co)]
    (make (g/* r cos-ht)
          (g/* r*sin-ht*sin-co (g/cos lon))
          (g/* r*sin-ht*sin-co (g/sin lon))
          (g/* r*sin-ht (g/cos colat)))))

(defn semipolar
  "Returns a [[Quaternion]] `q` with magnitude `rho`, built such that:

  - the magnitude of `q` equals `rho`
  - the magnitude `([[complex-1]] q)` equals `(* rho (cos alpha))`
  - the angle of `([[complex-1]] q)` equals `theta1`
  - The magnitude `([[complex-2]] q)` equals `(* rho (cos alpha))`
  - the angle of `([[complex-2]] q)` equals `theta12`

  This strange, possibly unnecessary constructor taken from the [Boost
  quaternion
  implementation](https://www.boost.org/doc/libs/1_78_0/libs/math/doc/html/math_toolkit/create.html)."
  [r alpha theta1 theta2]
  (let [r*cos-a (g/* r (g/cos alpha))
        r*sin-a (g/* r (g/sin alpha))]
    (make (g/* r*cos-a (g/cos theta1))
          (g/* r*cos-a (g/sin theta1))
          (g/* r*sin-a (g/cos theta2))
          (g/* r*sin-a (g/sin theta2)))))

(defn multipolar
  "Returns a [[Quaternion]] instance with [[complex-1]] part built from the polar
  coordinates `r1` and `theta1` and [[complex-2]] part built from `r2` and
  `theta2`"
  [r1 theta1 r2 theta2]
  (make (g/* r1 (g/cos theta1))
        (g/* r1 (g/sin theta1))
        (g/* r2 (g/cos theta2))
        (g/* r2 (g/sin theta2))))

(defn cylindrospherical
  "Returns a [[Quaternion]] `q` with [[real-part]] equal to `t` and
  the [[three-vector]] part built from the spherical coordinates `r`, `colat`
  and `lon`."
  [t r theta phi]
  (let [r*sin-theta (g/* r (g/sin theta))]
    (make t
          (g/* r*sin-theta (g/cos phi))
          (g/* r*sin-theta (g/sin phi))
          (g/* r (g/cos theta)))))

(defn cylindrical
  "Returns a [[Quaternion]] `q` with [[complex-1]] built from the polar
  coordinates `mag` and `angle`, and `j` and `k` components equal to the
  supplied `j` and `k`."
  [mag angle j k]
  (make (g/* mag (g/cos angle))
        (g/* mag (g/sin angle))
        j
        k))

;; ## Quaternions with Function Coefficients
;;
;; These functions apply specifically to quaternions with function coefficients.

(defn arity
  "Given a quaternion `q` with function coefficients, returns an arity compatible
  with all function coefficient entries.

  NOTE that by default, if any arities are incompatible, the function will
  return `[:at-least 0]`. To force strict arity checks,
  bind [[emmy.function/*strict-arity-checks*]] to `true`."
  [q]
  (f/seq-arity
   (->vector q)))

(defn evaluate
  "Given a quaternion `q` with function coefficients and a sequence `args` of
  arguments, and returns a new [[Quaternion]] generated by replacing each
  coefficient with the result of applying the (functional) coefficient to
  `args`."
  [q args]
  (->Quaternion
   (apply (get-r q) args)
   (apply (get-i q) args)
   (apply (get-j q) args)
   (apply (get-k q) args)))

;; NOTE: the definition for [[partial-derivative]] comes from `quaternion.scm`
;; in the original scmutils library. [This
;; pdf](http://home.ewha.ac.kr/~bulee/quaternion.pdf)
;; and [Wikipedia](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation#Differentiation_with_respect_to_the_rotation_quaternion)
;; seem to state that the derivative of, say, a quaternion-valued function with
;; respect to a quaternion should /not/ be defined this way. If you run into
;; trouble, study these sources and please correct this issue! Or if this
;; implementation is correct, please add some exposition and delete the comment.

(defn partial-derivative
  "Given a quaternion `q` with function coefficients and a possibly-empty sequence
  of partial derivative `selectors`, returns a new [[Quaternion]] generated by
  replacing each (functional) coefficient with its derivative with respect to
  `selectors`."
  [q selectors]
  (->Quaternion
   (g/partial-derivative (get-r q) selectors)
   (g/partial-derivative (get-i q) selectors)
   (g/partial-derivative (get-j q) selectors)
   (g/partial-derivative (get-k q) selectors)))

;; ## Algebra
;;
;; The next section implements quaternion arithmetic. These implementations are
;; installed into the generic arithmetic system at the bottom of the namespace.

(defn add
  "Variadic function that returns the sum of all supplied quaternions.

  Given 1 argument `q`, acts as identity. Given no arguments, returns [[ZERO]],
  the additive identity.

  The sum of two or more quaternions is a new quaternion with coefficients equal
  to the elementwise sum of the coefficients of all supplied quaternions."
  ([] ZERO)
  ([q] q)
  ([q1 q2]
   (->Quaternion
    (g/add (get-r q1) (get-r q2))
    (g/add (get-i q1) (get-i q2))
    (g/add (get-j q1) (get-j q2))
    (g/add (get-k q1) (get-k q2))))
  ([q1 q2 & more]
   (reduce add (add q1 q2) more)))

(defn ^:no-doc scalar+quaternion
  "Returns the quaternion result of adding scalar `s` to the real part of
  quaternion `q`. Addition occurs with scalar `s` on the left.

  See [[quaternion+scalar]] for right addition."
  [s q]
  (->Quaternion
   (g/add s (get-r q))
   (get-i q)
   (get-j q)
   (get-k q)))

(defn ^:no-doc quaternion+scalar
  "Returns the quaternion result of adding scalar `s` to the real part of
  quaternion `q`. Addition occurs with scalar `s` on the right.

  See [[scalar+quaternion]] for left addition."
  [q s]
  (->Quaternion
   (g/add (get-r q) s)
   (get-i q)
   (get-j q)
   (get-k q)))

(defn negate
  "Returns the negation (additive inverse) of the supplied quaternion `q`.

  The additive inverse of a quaternion is a new quaternion that, when [[add]]ed
  to `q`, will produce the [[ZERO]] quaternion (the additive identity)."
  [q]
  (->Quaternion
   (g/negate (get-r q))
   (g/negate (get-i q))
   (g/negate (get-j q))
   (g/negate (get-k q))))

(defn sub
  "Variadic function for subtracting quaternion arguments.

  - Given no arguments, returns [[ZERO]], the additive identity.
  - Given 1 argument `q`, acts as identity.
  - Given 2 arguments, returns the difference of quaternions `q1` and `q2`.
  - Given more than 2 arguments, returns the difference of the first quaternion
    `q1` with the sum of all remaining arguments.

  The difference of two quaternions is a new quaternion with coefficients equal
  to the pairwise difference of the coefficients of `q1` and `q2`."
  ([] ZERO)
  ([q] (negate q))
  ([q1 q2]
   (->Quaternion
    (g/sub (get-r q1) (get-r q2))
    (g/sub (get-i q1) (get-i q2))
    (g/sub (get-j q1) (get-j q2))
    (g/sub (get-k q1) (get-k q2))))
  ([q1 q2 & more]
   (sub q1 (apply add q2 more))))

(defn ^:no-doc scalar-quaternion
  "Returns the quaternion result of subtracting `s` from the real part of
  quaternion `q` and negating all imaginary entries.

  See [[quaternion-scalar]] for a similar function with arguments reversed."
  [s q]
  (->Quaternion
   (g/sub s (get-r q))
   (g/negate (get-i q))
   (g/negate (get-j q))
   (g/negate (get-k q))))

(defn ^:no-doc quaternion-scalar
  "Returns the quaternion result of subtracting `s` from the real part of
  quaternion `q` and negating all imaginary entries.

  See [[quaternion-scalar]] for a similar function with arguments reversed."
  [q s]
  (->Quaternion
   (g/sub (get-r q) s)
   (get-i q)
   (get-j q)
   (get-k q)))

(defn mul
  "Variadic function that returns the product of all supplied quaternions.

  Given 1 argument `q`, acts as identity. Given no arguments, returns [[ONE]],
  the multiplicative identity.

  The product of two or more quaternions is a new quaternion generated by
  multiplying together each quaternion of the form `(r+ai+bj+ck)`, respecting
  the quaternion rules:

  i^2 == j^2 == k^2 == -1
  ijk == -1,
  ij  == k,  jk == i,  ki == j
  ji  == -k, kj == -i, ik == -j"
  ([] ONE)
  ([q] q)
  ([q1 q2]
   (let [r1 (get-r q1) i1 (get-i q1) j1 (get-j q1) k1 (get-k q1)
         r2 (get-r q2) i2 (get-i q2) j2 (get-j q2) k2 (get-k q2)]
     (->Quaternion
      (g/- (g/* r1 r2) (g/+ (g/* i1 i2) (g/* j1 j2) (g/* k1 k2)))
      (g/+ (g/* r1 i2) (g/* i1 r2) (g/* j1 k2) (g/* -1 k1 j2))
      (g/+ (g/* r1 j2) (g/* -1 i1 k2) (g/* j1 r2) (g/* k1 i2))
      (g/+ (g/* r1 k2) (g/* i1 j2) (g/* -1 j1 i2) (g/* k1 r2)))))
  ([q1 q2 & more]
   (reduce mul (mul q1 q2) more)))

(defn scale-l
  "Returns a new quaternion generated by multiplying each coefficient of the
  supplied quaternion `q` by the supplied scalar `s` on the left."
  [s q]
  (->Quaternion
   (g/* s (get-r q))
   (g/* s (get-i q))
   (g/* s (get-j q))
   (g/* s (get-k q))))

(defn scale
  "Returns a new quaternion generated by multiplying each coefficient of the
  supplied quaternion `q` by the supplied scalar `s` on the right."
  [q s]
  (->Quaternion
   (g/* (get-r q) s)
   (g/* (get-i q) s)
   (g/* (get-j q) s)
   (g/* (get-k q) s)))

(defn conjugate
  "Returns the conjugate of the supplied quaternion `q`.

  The conjugate of a quaternion is a new quaternion with real coefficient equal
  to that of `q` and each imaginary coefficient negated. `(mul q (conjugate q))`
  will return a [[real?]] quaternion."
  [q]
  (->Quaternion
   (get-r q)
   (g/negate (get-i q))
   (g/negate (get-j q))
   (g/negate (get-k q))))

(defn q-div-scalar
  "Returns a new quaternion generated by dividing each coefficient of the supplied
  quaternion `q` by the supplied scalar `s`."
  [q s]
  (->Quaternion
   (g// (get-r q) s)
   (g// (get-i q) s)
   (g// (get-j q) s)
   (g// (get-k q) s)))

(defn dot-product
  "Returns the quaternion dot product of the supplied quaternions `l` and `r`.

  The quaternion dot product is the sum of the products of the corresponding
  coefficients of each quaternion, equal to

  $$r_l * r_r + i_l * i_r + j_l * j_r + k_l * k_r$$"
  [l r]
  (let [[lr li lj lk] l
        [rr ri rj rk] r]
    (g/+ (g/* lr rr)
         (g/* li ri)
         (g/* lj rj)
         (g/* lk rk))))

(defn cross-product
  "Returns a quaternion representing the (vector) cross product of the two pure
  sides (retrieved via [[three-vector]]) of the supplied quaternions `l` and
  `r`.

  NOTE that the suggestion for this function comes from this [C++ quaternion
  library](https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1109).
  Strictly, this is not the 'cross product of two quaternions'."
  [l r]
  (make 0 (ss/cross-product
           (three-vector l)
           (three-vector r))))

(defn invert
  "Returns the multiplicative inverse of the supplied quaternion `q`.

  The inverse of a quaternion is a new quaternion that, when [[mul]]tiplied by
  `q`, will produce the [[ONE]] quaternion (the multiplicative identity)."
  [q]
  (q-div-scalar
   (conjugate q)
   (magnitude-sq q)))

(defn div
  "Variadic function for dividing quaternion arguments.

  - Given no arguments, returns [[ONE]], the multiplicative identity.
  - Given 1 argument `q`, acts as identity.
  - Given 2 arguments, returns the quotient of quaternions `q1` and `q2`.
  - Given more than 2 arguments, returns the quotient of the first quaternion
    `q1` with the product of all remaining arguments.

  The quotient of two quaternions is a new quaternion equal to the product of
  `q1` and the multiplicative inverse of `q2`"
  ([] ONE)
  ([q] (invert q))
  ([q1 q2]
   (mul q1 (invert q2)))
  ([q1 q2 & more]
   (div q1 (apply mul q2 more))))

(defn magnitude-sq
  "Returns the square of the [[magnitude]] of the supplied quaternion `q`,
  equivalent to taking the [[dot-product]] of `q` with itself."
  [q]
  (g/+ (g/square (get-r q))
       (g/square (get-i q))
       (g/square (get-j q))
       (g/square (get-k q))))

(defn magnitude
  "Returns the norm of the supplied quaternion `q`.

  The norm of a quaternion is the square root of the sum of the squares of the
  quaternion's coefficients."
  [q]
  (g/sqrt
   (magnitude-sq q)))

(defn normalize
  "Returns a new quaternion generated by dividing each coefficient of the supplied
  quaternion `q` by the [[magnitude]] of `q`. (If the [[magnitude]]
  is [[zero?]], returns the zero quaternion `q`.)

  The returned quaternion will have [[magnitude]] (approximately) equal to
  1. [[unit?]] will return true for a [[normalize]]d quaternion, though you may
  need to supply an `:epsilon`."
  [q]
  (if (zero? q)
    q
    (q-div-scalar q (magnitude q))))

(defn commutator
  "Returns the commutator of the supplied quaternions `l` and `r`.

  The commutator of two quaternions is equal to

  ```clj
  (- (* l r) (* r l))
  ```"
  [l r]
  (sub
   (mul l r)
   (mul r l)))

;; ## Transcendental Functions

(defn log
  "Returns the logarithm $\\ln q$ of the supplied quaternion `q`.

  Given a quaternion $q$ with real part $r$ and non-real vector $\\vec{v}$, the
  logarithm [is computed
  as](https://en.wikipedia.org/wiki/Quaternion#Exponential,_logarithm,_and_power_functions)

  $$
  \\ln(q) = \\ln \\|q\\| + \\frac{\\mathbf{v}}{\\|\\mathbf{v}\\|} \\
  \\arccos \\frac{r}{\\|\\q\\|}
  $$"
  [q]
  (let [[r i j k] q]
    (if (and (v/zero? j)
             (v/zero? k))
      (if (v/zero? i)
        (make (g/log r))
        (make (g/log (g/abs [r i]))
              (g/atan i r)
              0 0))
      (let [q-mag (magnitude q)
            v     (three-vector q)
            v-mag (g/abs v)]
        (make (g/log q-mag)
              (g/mul (g/acos (g/div r q-mag))
                     (g/div v v-mag)))))))

(defn exp
  "Returns the exponential $e^q$ of the supplied quaternion `q`.

  Given a quaternion $q$ with real part $r$ and non-real vector $\\vec{v}$, the
  exponential [is computed
  as](https://en.wikipedia.org/wiki/Quaternion#Exponential,_logarithm,_and_power_functions)

  $$
  \\exp(q) = e^r \\left(\\cos \\|\\mathbf{v}\\| \\
  + \\frac{\\mathbf{v}}{\\|\\mathbf{v}\\|} \\sin\\|\\mathbf{v}\\| \\right)
  $$"
  [q]
  (let [r     (real-part q)
        exp-r (g/exp r)
        v     (three-vector q)
        v-mag (g/abs v)]
    (if (v/zero? v-mag)
      (make exp-r 0 0 0)
      (make (g/* exp-r (g/cos v-mag))
            (g/* exp-r (g/sinc v-mag) v)))))

(defn cos
  "Returns the cosine of the supplied quaternion `q`.

  See the [Boost
  documentation](https://www.boost.org/doc/libs/1_78_0/libs/math/doc/html/math_toolkit/trans.html)
  and [source](https://www.boost.org/doc/libs/1_78_0/boost/math/quaternion.hpp)
  for a reference implementation."
  [q]
  (let [r     (real-part q)
        v     (three-vector q)
        v-mag (g/abs v)]
    (make (g/* (g/cos r) (g/cosh v-mag))
          (g/* (g/* (g/- (g/sin r))
                    (g/sinhc v-mag))
               v))))

(defn sin
  "Returns the sine of the supplied quaternion `q`.

  See the [Boost
  documentation](https://www.boost.org/doc/libs/1_78_0/libs/math/doc/html/math_toolkit/trans.html)
  and [source](https://www.boost.org/doc/libs/1_78_0/boost/math/quaternion.hpp)
  for a reference implementation."
  [q]
  (let [r     (real-part q)
        v     (three-vector q)
        v-mag (g/abs v)]
    (make (g/* (g/sin r) (g/cosh v-mag))
          (g/* (g/* (g/cos r)
                    (g/sinhc v-mag))
               v))))

(defn tan
  "Returns the tangent of the supplied quaternion `q`.

  [[tan]] is defined as `(/ (sin q) (cos q))`."
  [q]
  (div (sin q) (cos q)))

(defn cosh
  "Returns the hyperbolic cosine of the supplied quaternion `q`.

  [[cosh]] is defined in terms of the [[exp]] function as `(e^q + e^{-q}) / 2`."
  [q]
  (-> (add (exp q) (exp (negate q)))
      (q-div-scalar 2)))

(defn sinh
  "Returns the hyperbolic sine of the supplied quaternion `q`.

  [[sinh]] is defined in terms of the [[exp]] function as `(e^q - e^{-q}) / 2`."
  [q]
  (-> (sub (exp q) (exp (negate q)))
      (q-div-scalar 2)))

(defn tanh
  "Returns the hyperbolic tangent of the supplied quaternion `q`.

  [[tan]] is defined as `(/ (sinh q) (cosh q))`."
  [q]
  (div (sinh q) (cosh q)))

(defn expt
  "Returns the result of raising quaternion `q` to the real, complex or quaternion
  power `p`."
  [q p]
  (if (v/native-integral? p)
    (g/default-expt q p)
    (exp
     (if (quaternion? p)
       (mul (log q) p)
       (scale (log q) p)))))

(defn sqrt
  "Returns the square root of the supplied quaternion `q`.

  `([[sqrt]] q)` is identical to, but more efficient than, raising `q` to the
  1/2 power.

  Thanks to the [Spire
  library](https://github.com/typelevel/spire/blob/82f607714f94ba1c70b13fd4751063dfdcd155f5/core/src/main/scala/spire/math/Quaternion.scala#L217)
  for the correct implementation used here."
  [q]
  (let [r (get-r q)]
    (if (real? q)
      (if (g/negative? r)
        (make 0 (g/sqrt (g/abs r)) 0 0)
        (make (g/sqrt r) 0 0 0))
      (let [n   (g/sqrt (g/+ r (magnitude q)))
            rt2 (g/sqrt 2)]
        (make (g// n rt2)
              (g// (three-vector q)
                   (g/* n rt2)))))))

;; ## Quaternions and 3D rotations
;;
;; The story of quaternions, from their discovery to their modern day usage, is
;; tightly connected to their [utility in describing rotations in 3-dimensional
;; space](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation).
;;
;; The following section provides a number of utilities for building quaternions
;; that represent rotations, and moving quaternions and other representations of
;; rotations in 3d and 4d space.
;;
;;
;; ### Conversion to/from Angle-Axis

(defn from-angle-normal-axis
  "Returns a [[Quaternion]] that represents a rotation of `angle` radians around
  the unit (normalized) vector described by the second argument, a 3-vector with
  components `x`, `y` and `z`.

  The second argument represents an axis of rotation.

  NOTE: If you have an UN-normalized axis, prefer [[from-angle-axis]]."
  [angle [x y z]]
  (let [half-angle (g/div angle 2)
        half-sine  (g/sin half-angle)]
    (->Quaternion (g/cos half-angle)
                  (g/* half-sine x)
                  (g/* half-sine y)
                  (g/* half-sine z))))

(defn from-angle-axis
  "Returns a [[Quaternion]] that represents a rotation of `angle` radians around a
  normalized version of the vector described by `axis`. `axis` must be a
  3-vector with components `x`, `y` and `z`.

  Given an `axis` with numeric entries, [[from-angle-axis]] will explicitly
  normalize `axis` before calling [[from-angle-normal-axis]]. If any entries are
  non-numerical (ie, symbolic), [[from-angle-axis]] will instead log an
  assumption that the magnitude of `axis` == 1 and proceed.

  NOTE: If you have an already-normalized axis,
  prefer [[from-angle-normal-axis]]."
  [angle axis]
  (if (every? v/number? axis)
    (let [v-mag  (g/abs axis)
          normal (g/div axis v-mag)]
      (from-angle-normal-axis angle normal))
    (let [vv (g/simplify
              (ss/vector-dot-product axis axis))]
      (and (ul/assume! (list '= vv 1) 'from-angle-axis)
           (from-angle-normal-axis angle axis)))))

(defn pitch
  "Create a quaternion representing a pitch rotation by the supplied
  `angle` (specified in radians)."
  [angle]
  (from-angle-normal-axis angle [1 0 0]))

(defn yaw
  "Create a quaternion representing a yaw rotation by the supplied
  `angle` (specified in radians)."
  [angle]
  (from-angle-normal-axis angle [0 1 0]))

(defn roll
  "Create a quaternion representing a roll rotation by the supplied
  `angle` (specified in radians)."
  [angle]
  (from-angle-normal-axis angle [0 0 1]))

(def ^{:dynamic true
       :doc "Tolerance setting for [[->angle-axis]]."}
  *angle-axis-tolerance*
  1e-8)

(defn ->angle-axis
  "Given a unit quaternion `q` [representing a spatial
  rotation](https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation) (sometimes
  called a 'versor'), returns a pair of

  - `theta`, the rotation in radians about the rotation axis
  - `axis`, a 3-element unit vector with elements `x`, `y` and `z` representing
    an axis of rotation in 3d Euclidean space.

  If the unit quaternion `q` represents NO rotation, the axis is undefined; this
  manifests as the squared norm of the non-real vector part of `q` sitting
  within [[*angle-axis-tolerance*]] of 0.

  In this case, the conversion is degenerate and [[->angle-axis]] returns the
  pair [0 [1 0 0]] as a default. (This check only occurs with a quaternion with
  all numeric elements in the non-real positions.)"
  [q]
  {:pre [(quaternion? q)]}
  (let [v  (three-vector q)
        vv (ss/vector-dot-product v v)]
    (if (and (every? v/number? v)
             ((v/within *angle-axis-tolerance*) 0.0 vv))
      [0 [1 0 0]]
      (let [v-mag (g/sqrt vv)
            theta (g/mul 2 (g/atan v-mag (real-part q)))
            axis (g/div v v-mag)]
        [theta axis]))))

;; ## Quaternion / Matrix Relationships
;;
;; ### Complex 2x2

(defn from-complex-matrix
  "Given a 2x2 complex matrix `M` of the form

  ```
  [ a + b i,  c + d i]
  [ -c + d i, a - b i]
  ```

  Returns a [[Quaternion]] instance with coefficients `[a b c d]`."
  [M]
  (let [[[a+bi c+di]] (m/matrix->vector M)]
    (->Quaternion
     (g/real-part a+bi)
     (g/imag-part a+bi)
     (g/real-part c+di)
     (g/imag-part c+di))))

(defn ->complex-matrix
  "Returns a 2x2 complex matrix representation of the supplied Quaternion `q`.

  For a quaternion with coefficients `[a b c d]`, the returned matrix will have
  the following form:

  ```
  [ a + b i,  c + d i]
  [ -c + d i, a - b i]
  ```

  NOTE that this currently only works for quaternions `q` with real or symbolic
  entries."
  [q]
  (let [[r i j k] q]
    (m/by-rows
     [(g/make-rectangular r i)
      (g/make-rectangular j k)]
     [(g/make-rectangular (g/negate j) k)
      (g/make-rectangular r (g/negate i))])))

;; ### Real 4x4 matrices

(def ^{:doc "4x4 matrix representation of the quaternion [[ONE]]."}
  ONE-matrix
  (m/by-rows
   [1 0 0 0]
   [0 1 0 0]
   [0 0 1 0]
   [0 0 0 1]))

(def ^{:doc "4x4 matrix representation of the quaternion [[I]]."}
  I-matrix
  (m/by-rows
   [0 1 0 0]
   [-1 0 0 0]
   [0 0 0 -1]
   [0 0 1 0]))

(def ^{:doc "4x4 matrix representation of the quaternion [[J]]."}
  J-matrix
  (m/by-rows
   [0 0 1 0]
   [0 0 0 1]
   [-1 0 0 0]
   [0 -1 0 0]))

(def ^{:doc "4x4 matrix representation of the quaternion [[K]]."}
  K-matrix
  (m/by-rows
   [0 0 0 1]
   [0 0 -1 0]
   [0 1 0 0]
   [-1 0 0 0]))

(defn from-4x4-matrix
  "Given a 4x4 matrix representation of a quaternion, returns the associated
  quaternion by extracting the first row."
  [four-matrix]
  (make
   (nth four-matrix 0)))

(defn ->4x4-matrix
  "Returns the 4x4 matrix representation of the supplied [[Quaternion]] `q`."
  [q]
  (g/+ (g/* (get-r q) ONE-matrix)
       (g/* (get-i q) I-matrix)
       (g/* (get-j q) J-matrix)
       (g/* (get-k q) K-matrix)))

;; ### Tensor Representations of Quaternions

(def ^{:doc "4x4 down-up tensor representation of the quaternion [[ONE]]."}
  ONE-tensor
  (m/->structure ONE-matrix))

(def ^{:doc "4x4 down-up tensor representation of the quaternion [[I]]."}
  I-tensor
  (m/->structure I-matrix))

(def ^{:doc "4x4 down-up tensor representation of the quaternion [[J]]."}
  J-tensor
  (m/->structure J-matrix))

(def ^{:doc "4x4 down-up tensor representation of the quaternion [[K]]."}
  K-tensor
  (m/->structure K-matrix))

;; ### 3x3 Rotation Matrix Representations

(def ^:private quarter (g// 1 4))

(defn from-rotation-matrix
  "Given an orthogonal 3x3 matrix M representing a rotation in 3-space, returns
  the unit quaternion that corresponds to the same transformation.

  GJS notes in scmutils that this algorithm is the 'expanded Matt Mason method'.

  NOTE Orthogonal means, no stretching allowed, only rotation!

  NOTE this routine uses non-generic [[clojure.core/>=]]
  and [[clojure.core/max]] internally, so if you use numeric entries (or if your
  entries simplify down to numbers), make sure that they work with these native
  operations. No `BigInt` in ClojureScript for now, for example."
  [M]
  (let [[[r11 r12 r13] [r21 r22 r23] [r31 r32 r33]] M
        q0-2 (g/* quarter (g/+ 1 r11 r22 r33))
        q1-2 (g/* quarter (g/+ 1 r11 (g/- r22) (g/- r33)))
        q2-2 (g/* quarter (g/+ 1 (g/- r11) r22 (g/- r33)))
        q3-2 (g/* quarter (g/+ 1 (g/- r11) (g/- r22) r33))

        q0q1 (g/* quarter (g/- r32 r23))
        q0q2 (g/* quarter (g/- r13 r31))
        q0q3 (g/* quarter (g/- r21 r12))
        q1q2 (g/* quarter (g/+ r12 r21))
        q1q3 (g/* quarter (g/+ r13 r31))
        q2q3 (g/* quarter (g/+ r23 r32))

        q0-2s (g/simplify q0-2)
        q1-2s (g/simplify q1-2)
        q2-2s (g/simplify q2-2)
        q3-2s (g/simplify q3-2)]
    (cond (and (v/number? q0-2s) (v/number? q1-2s)
               (v/number? q2-2s) (v/number? q3-2s))
          (cond (>= q0-2s (max q1-2s q2-2s q3-2s))
                (let [q0 (g/sqrt q0-2s)
                      q1 (g// q0q1 q0)
                      q2 (g// q0q2 q0)
                      q3 (g// q0q3 q0)]
                  (make q0 q1 q2 q3))

                (>= q1-2s (max q0-2s q2-2s q3-2s))
                (let [q1 (g/sqrt q1-2s)
                      q0 (g// q0q1 q1)
                      q2 (g// q1q2 q1)
                      q3 (g// q1q3 q1)]
                  (make q0 q1 q2 q3))

                (>= q2-2s (max q0-2s q1-2s q3-2s))
                (let [q2 (g/sqrt q2-2s)
                      q0 (g// q0q2 q2)
                      q1 (g// q1q2 q2)
                      q3 (g// q2q3 q2)]
                  (make q0 q1 q2 q3))

                :else
                (let [q3 (g/sqrt q3-2s)
                      q0 (g// q0q3 q3)
                      q1 (g// q1q3 q3)
                      q2 (g// q2q3 q3)]
                  (make q0 q1 q2 q3)))

          (not (v/numeric-zero? q0-2s))
          (let [q0 (g/sqrt q0-2)
                q1 (g// q0q1 q0)
                q2 (g// q0q2 q0)
                q3 (g// q0q3 q0)]
            (make q0 q1 q2 q3))

          (not (v/numeric-zero? q1-2s))
          (let [q1 (g/sqrt q1-2)
                q0 0
                q2 (g// q1q2 q1)
                q3 (g// q1q3 q1)]
            (make q0 q1 q2 q3))

          (not (v/numeric-zero? q2-2s))
          (let [q2 (g/sqrt q2-2)
                q0 0
                q1 0
                q3 (g// q2q3 q2)]
            (make q0 q1 q2 q3))

          :else (make 0 0 0 0))))

(defn ->rotation-matrix
  "Given a normalized [[Quaternion]] `q`, returns the corresponding orthogonal 3x3
  rotation matrix representing a rotation in 3d-space.

  The implementation here will first normalize `q` for you and then generate a
  rotation matrix from that new quaternion `q-normal`.

  [[->rotation-matrix]] will still work if `q` isn't normalized; but if
  a [[Quaternion]] isn't normalized it doesn't make sense to interpret it as a
  rotation.

  See https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation#Quaternion-derived_rotation_matrix"
  [q]
  {:pre [(quaternion? q)]}
  (let [q0 (get-r q) q1 (get-i q) q2 (get-j q) q3 (get-k q)
        q0**2 (g/square q0)
        q1**2 (g/square q1)
        q2**2 (g/square q2)
        q3**2 (g/square q3)
        mag-sq (g/simplify
                (g/+ q0**2 q1**2 q2**2 q3**2))]
    ;; NOTE that this assertion is not quite right; yes, we are 'assuming' that
    ;; the quaternion is normalized, since it doesn't make sense as a rotation
    ;; otherwise.
    ;;
    ;; But we are also deliberately normalizing as we go. So all might still
    ;; 'work' if we fail this check. Note this for yourself!
    (ul/assume! (list '= mag-sq 1) '->rotation-matrix)
    (m/by-rows [(-> (g/+ q0**2 q1**2 (g/negate q2**2) (g/negate q3**2))
                    (g// mag-sq))
                (-> (g/* 2 (g/- (g/* q1 q2) (g/* q0 q3)))
                    (g// mag-sq))
                (-> (g/* 2 (g/+ (g/* q1 q3) (g/* q0 q2)))
                    (g// mag-sq))]
               [(-> (g/* 2 (g/+ (g/* q1 q2) (g/* q0 q3)))
                    (g// mag-sq))
                (-> (g/+ q0**2 (g/negate q1**2) q2**2 (g/negate q3**2))
                    (g// mag-sq))
                (-> (g/* 2 (g/- (g/* q2 q3) (g/* q0 q1)))
                    (g// mag-sq))]
               [(-> (g/* 2 (g/- (g/* q1 q3) (g/* q0 q2)))
                    (g// mag-sq))
                (-> (g/* 2 (g/+ (g/* q2 q3) (g/* q0 q1)))
                    (g// mag-sq))
                (-> (g/+ q0**2 (g/negate q1**2) (g/negate q2**2) q3**2)
                    (g// mag-sq))])))

;; ## Generic Method Installation
;;
;; ### Equality
;;
;; Because equality is a symmetric relation, these methods arrange their
;; arguments so that the quaternion is passed into [[eq]] first.

(defmethod v/= [::quaternion ::quaternion] [a b] (eq a b))
(defmethod v/= [v/seqtype ::quaternion] [a b] (eq b a))
(defmethod v/= [::quaternion v/seqtype] [a b] (eq a b))
(defmethod v/= [::sc/complex ::quaternion] [a b] (eq b a))
(defmethod v/= [::quaternion ::sc/complex] [a b] (eq a b))
(defmethod v/= [::v/real ::quaternion] [a b] (eq b a))
(defmethod v/= [::quaternion ::v/real] [a b] (eq a b))

(defmethod g/simplify [::quaternion] [q]
  (->Quaternion
   (g/simplify (get-r q))
   (g/simplify (get-i q))
   (g/simplify (get-j q))
   (g/simplify (get-k q))
   (meta q)))

;; ### Arithmetic

(defmethod g/add [::quaternion ::quaternion] [a b] (add a b))
(defmethod g/add [::v/scalar ::quaternion] [a b] (scalar+quaternion a b))
(defmethod g/add [::quaternion ::v/scalar] [a b] (quaternion+scalar a b))
(defmethod g/add [::sc/complex ::quaternion] [a b] (add (make a) b))
(defmethod g/add [::quaternion ::sc/complex] [a b] (add a (make b)))

(defmethod g/negate [::quaternion] [q] (negate q))

(defmethod g/sub [::quaternion ::quaternion] [a b] (sub a b))
(defmethod g/sub [::v/scalar ::quaternion] [a b] (scalar-quaternion a b))
(defmethod g/sub [::quaternion ::v/scalar] [a b] (quaternion-scalar a b))
(defmethod g/sub [::sc/complex ::quaternion] [a b] (sub (make a) b))
(defmethod g/sub [::quaternion ::sc/complex] [a b] (sub a (make b)))

(defmethod g/mul [::quaternion ::quaternion] [a b] (mul a b))
(defmethod g/mul [::v/scalar ::quaternion] [s q] (scale-l s q))
(defmethod g/mul [::quaternion ::v/scalar] [q s] (scale q s))
(defmethod g/mul [::sc/complex ::quaternion] [a b] (mul (make a) b))
(defmethod g/mul [::quaternion ::sc/complex] [a b] (mul a (make b)))

(defmethod g/expt [::quaternion ::quaternion] [a b] (expt a b))
(defmethod g/expt [::quaternion ::sc/complex] [a b] (expt a (make b)))
(defmethod g/expt [::quaternion ::v/real] [a b] (expt a b))

(defmethod g/invert [::quaternion] [q] (invert q))

(defmethod g/div [::quaternion ::quaternion] [a b] (div a b))
(defmethod g/div [::v/scalar ::quaternion] [s q] (scale-l s (g/invert q)))
(defmethod g/div [::quaternion ::v/scalar] [q s] (q-div-scalar q s))
(defmethod g/div [::sc/complex ::quaternion] [a b] (div (make a) b))
(defmethod g/div [::quaternion ::sc/complex] [a b] (div a (make b)))

(defmethod g/sqrt [::quaternion] [q] (sqrt q))
(defmethod g/log [::quaternion] [q] (log q))

;; ### Transcendental Functions

(defmethod g/exp [::quaternion] [q] (exp q))
(defmethod g/sin [::quaternion] [q] (sin q))
(defmethod g/cos [::quaternion] [q] (cos q))
(defmethod g/tan [::quaternion] [q] (tan q))
(defmethod g/cosh [::quaternion] [q] (cosh q))
(defmethod g/sinh [::quaternion] [q] (sinh q))
(defmethod g/tanh [::quaternion] [q] (tanh q))

(defmethod g/infinite? [::quaternion] [q]
  (or (g/infinite? (get-r q))
      (g/infinite? (get-i q))
      (g/infinite? (get-j q))
      (g/infinite? (get-k q))))

(defmethod g/abs [::quaternion] [q] (magnitude q))
(defmethod g/magnitude [::quaternion] [q] (magnitude q))
(defmethod g/conjugate [::quaternion] [q] (conjugate q))
(defmethod g/real-part [::quaternion] [q] (real-part q))

(defmethod g/partial-derivative [::quaternion v/seqtype] [q selectors]
  (partial-derivative q selectors))

(defmethod g/cross-product [::quaternion ::quaternion] [a b] (cross-product a b))
(defmethod g/cross-product [::quaternion ::v/scalar] [_ _] ZERO)
(defmethod g/cross-product [::v/scalar ::quaternion] [_ _] ZERO)
(defmethod g/cross-product [::quaternion ::sc/complex] [a b]
  (let [i2 (sc/imaginary b)]
    (make 0 0
          (g/* (get-k a) i2)
          (g/- (g/* (get-j a) i2)))))

(defmethod g/cross-product [::sc/complex ::quaternion] [a b]
  (let [i1 (sc/imaginary a)]
    (make 0 0
          (g/- (g/* i1 (get-k b)))
          (g/* i1 (get-j b)))))

(defmethod g/dot-product [::quaternion ::quaternion] [a b] (dot-product a b))
(defmethod g/dot-product [::v/scalar ::quaternion] [a b] (g/* a (get-r b)))
(defmethod g/dot-product [::quaternion ::v/scalar] [a b] (g/* (get-r a) b))
(defmethod g/dot-product [::sc/complex ::quaternion] [a b]
  (g/+ (g/* (sc/real a) (get-r b))
       (g/* (sc/imaginary a) (get-i b))))

(defmethod g/dot-product [::quaternion ::sc/complex] [a b]
  (g/+ (g/* (get-r a) (sc/real b))
       (g/* (get-i a) (sc/imaginary b))))

(defmethod g/solve-linear-right [::quaternion ::v/scalar] [q s] (q-div-scalar q s))
(defmethod g/solve-linear-right [::quaternion ::quaternion] [a b] (div a b))
(defmethod g/solve-linear-right [::sc/complex ::quaternion] [a b] (div (make a) b))
(defmethod g/solve-linear-right [::quaternion ::sc/complex] [a b] (div a (make b)))

(defmethod g/solve-linear [::v/scalar ::quaternion] [s q] (q-div-scalar q s))
(defmethod g/solve-linear [::quaternion ::quaternion] [a b] (div b a))
(defmethod g/solve-linear [::sc/complex ::quaternion] [a b] (div b (make a)))
(defmethod g/solve-linear [::quaternion ::sc/complex] [a b] (div (make b) a))
