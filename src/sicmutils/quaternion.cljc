;;
;; Copyright © 2021 Sam Ritchie.
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

(ns sicmutils.quaternion
  "This namespace provides a number of functions and constructors for working
  with [[Quaternion]] instances in Clojure and Clojurescript, and
  installs [[Quaternion]] into the SICMUtils generic arithmetic system.

  For other numeric extensions, see [[sicmutils.ratio]], [[sicmutils.complex]]
  and [[sicmutils.numbers]]."
  (:refer-clojure :exclude [zero?])
  (:require [sicmutils.complex :as sc]
            [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util.logic :as ul]
            [sicmutils.matrix :as m]
            [sicmutils.structure :as ss]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn Associative Counted
                            MapEntry
                            IObj IFn IReduce IKVReduce
                            Indexed Sequential
                            Reversible))))

;; # Quaternions
;;
;; A "quaternion" is an extension of a complex number...
;;
;; note how this idea came up, link to the history of vector analysis book.

;; - this namespace implements them, provides an API and installs them into the
;;   generic arithmetic system, making them COMPATIBLE with complex, real
;;   numbers etc where possible.
;;
;; - also note rotation matrix API etc
;;
;; - thank weavejester for the impl
;; - thank spire https://github.com/typelevel/spire/blob/main/core/src/main/scala/spire/math/Quaternion.scala#L202
;;
;; - also thank you for
;; - https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1138

(declare arity eq evaluate zero? one?)

;; ## Quaternion Type Definition
;;
;; TODO note what we are trying to support here with these various protocol and
;; interface implementations.
;;
;; - quaternions can hold functions in each position, so we want to support
;;   `f/Arity` and `IFn`. These are basically vectors that can be applied in the
;;   function position as IFn instances.
;;
;; - we also want to be able to take derivatives of Quaternion-valued functions.
;;
;; - A quaternion can also act as
;;
;;   - a sequence of its coefficients
;;   - a vector, and all this implies... it's an associative thing, etc etc,
;;     reducible, reversible.

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

  (zero-like [this]
    (Quaternion. (v/zero-like r) 0 0 0 m))
  (one-like [o]
    (Quaternion. (v/one-like r) 0 0 0 m))
  (identity-like [o]
    (Quaternion. (v/one-like r) 0 0 0 m))

  (exact? [this]
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
       (cons [_ o]
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
       (empty [this] (Quaternion. 0 0 0 0 m))
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
       (-conj [coll o]
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
       (-contains-key? [coll k]
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
       (-rseq [coll] (list k j i r))

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

(do (ns-unmap 'sicmutils.quaternion '->Quaternion)
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

;; TODO write ->polar rep
;; https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L552

(defn ->polar [q])

(defn ->complex
  "Returns a complex number created from the real and imaginary
  components (dropping `j`, `k`).

  NOTE that this only works if the coefficients of `q` are real numbers, due to
  restrictions on the current complex number implementation. "
  [q]
  {:pre [(quaternion? q)]}
  (sc/complex (get-r q)
              (get-i q)))

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
  `q1` is [[sicmutils.value/=]] to `q2`, false otherwise."
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
                          (sc/real x) (sc/imaginary x) 0 0)
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

;; TODO fill these in from
;; https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L482

(declare scale-l)

(defn spherical
  "TODO docs"
  [rho theta phi1 phi2]
  (let [s1    (g/sin phi1)
        s2    (g/sin phi2)
        c1    (g/cos phi2)
        c2    (g/cos phi2)
        c1*c2 (g/* c1 c2)]
    (make (g/* rho (g/cos theta) c1*c2)
          (g/* rho (g/sin theta) c1*c2)
          (g/* rho s1 c2)
          (g/* rho s2))))

(defn semipolar [rho alpha theta1 theta2]
  (let [cos-a (g/cos alpha)
        sin-a (g/sin alpha)]
    (make (g/* rho cos-a (g/cos theta1))
          (g/* rho cos-a (g/sin theta1))
          (g/* rho sin-a (g/cos theta2))
          (g/* rho sin-a (g/sin theta2)))))

(defn multipolar [rho1 theta1 rho2 theta2]
  (make (g/* rho1 (g/cos theta1))
        (g/* rho1 (g/sin theta1))
        (g/* rho2 (g/cos theta2))
        (g/* rho2 (g/sin theta2))))

(defn cylindrospherical [t r lon lat]
  (let [cos-lat (g/cos lat)]
    (make t
          (g/* r (g/cos lon) cos-lat)
          (g/* r (g/sin lon) cos-lat)
          (g/* r (g/sin lat)))))

(defn cylindrical [r angle h1 h2]
  (make (g/* r (g/cos angle))
        (g/* r (g/sin angle))
        h1
        h2))

;; ## Quaternions with Function Coefficients
;;
;; These functions apply specifically to quaternions with function coefficients.

(defn arity
  "Given a quaternion `q` with function coefficients, returns an arity compatible
  with all function coefficient entries.

  NOTE that by default, if any arities are incompatible, the function will
  return `[:at-least 0]`. To force strict arity checks,
  bind [[sicmutils.function/*strict-arity-checks*]] to `true`."
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
;; Okay, boom, quaternion algebra!

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

;; TODO document that the suggestion here came from
;; https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1109

(defn cross-product
  "Returns a quaternion representing the (vector) cross product of the two pure
  sides (retrieved via [[three-vector]]) of the supplied quaternions `l` and
  `r`."
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

(defn magnitude
  "Returns the norm of the supplied quaternion `q`.

  The norm of a quaternion is the square root of the sum of the squares of the
  quaternion's coefficients."
  [q]
  (g/sqrt
   (dot-product q q)))

(defn magnitude-sq
  "Returns the square of the [[magnitude]] of the supplied quaternion `q`,
  equivalent to taking the [[dot-product]] of `q` with itself."
  [q]
  (dot-product q q))

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


;; TODO note the idea from https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1117

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

;; TODO documented etc up to here.

(defn log
  ";; NOTE that this is good, ported from quaternion.js... not bad, handles zero
  cases well.

  check: https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1157"
  [q]
  (let [[r i j k] q]
    (if (and (v/zero? j)
             (v/zero? k))
      (make (g/log (g/abs [r i]))
            (g/atan i r)
            0 0)
      (let [q-mag (magnitude q)
            v     (three-vector q)
            v-mag (g/abs v)]
        (make (g/log q-mag)
              (g/mul (g/acos (g/div r q-mag))
                     (g/div v v-mag)))))))

;; TODO verify these with
;; https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1138

(defn exp
  "TODO this is good as well, lovely! I think this works now."
  [q]
  (let [r     (real-part q)
        exp-r (g/exp r)
        v     (three-vector q)
        v-mag (g/abs v)]
    (if (v/zero? v-mag)
      (make exp-r 0 0 0)
      (make (g/* exp-r
                 (g/cos v-mag))
            (g/* exp-r
                 (g/sin v-mag)
                 (g/div v v-mag))))))

;; TODO add more transcendental functions to be totally complete
;; https://www.boost.org/doc/libs/1_78_0/libs/math/doc/html/math_toolkit/trans.html

(defn cos [q]
  (let [r     (real-part q)
        v     (three-vector q)
        v-mag (g/abs v)]
    (make (g/* (g/cos r) (g/cosh v-mag))
          (g/* (g/* (g/- (g/sin r))
                    (g/sinhc v-mag))
               v))))

(defn sin [q]
  (let [r     (real-part q)
        v     (three-vector q)
        v-mag (g/abs v)]
    (make (g/* (g/sin r) (g/cosh v-mag))
          (g/* (g/* (g/cos r)
                    (g/sinhc v-mag))
               v))))

(defn tan [q]
  (div (sin q) (cos q)))

(defn cosh [q]
  (-> (add (exp q) (exp (negate q)))
      (q-div-scalar 2)))

(defn sinh [q]
  (-> (sub (exp q) (exp (negate q)))
      (q-div-scalar 2)))

(defn tanh [q]
  (div (sinh q) (cosh q)))

;; TODO do a better job here based on this quaternion.js impl, so we match when
;; complex numbers are at play...:
;; https://github.com/infusion/Quaternion.js/blob/master/quaternion.js#L577-L645

;; TODO NOTE quaternion power here theorem 1.3
;; https://web.archive.org/web/20170705123142/http://www.lce.hut.fi/~ssarkka/pub/quat.pdf

(defn expt
  "TODO check if it's a native integer and go here:
  https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L1268"
  [q p]
  (if (v/native-integral? p)
    (g/default-expt q p)
    (exp
     (if (quaternion? p)
       (mul (log q) p)
       (scale (log q) p)))))

(defn sqrt
  "Thanks to Spire for the implementation:
  https://github.com/typelevel/spire/blob/82f607714f94ba1c70b13fd4751063dfdcd155f5/core/src/main/scala/spire/math/Quaternion.scala#L217

  NOTE that if we have a real number we "
  [q]
  (let [r (get-r q)]
    (if (real? q)
      (if (g/negative? r)
        (make 0 (g/sqrt (g/abs r)) 0 0)
        (make (g/sqrt r)))
      (let [n   (g/sqrt (g/+ r (magnitude q)))
            rt2 (g/sqrt 2)]
        (make (g// n rt2)
              (g// (three-vector q)
                   (g/* n rt2)))))))

;; ## Quaternions and 3D rotations

;; ### Conversion to/from Angle-Axis

(defn from-angle-normal-axis
  "Create a quaternion from an angle in radians and a normalized axis vector.

  Call this if you've ALREADY normalized the vector!"
  [angle [x y z]]
  (let [half-angle (g/div angle 2)
        half-sine  (g/sin half-angle)]
    (->Quaternion (g/cos half-angle)
                  (g/* half-sine x)
                  (g/* half-sine y)
                  (g/* half-sine z))))

;; TODO read this and the one above and figure out the weavejester code this
;; came from. Then combine wih the GJS style.

(defn from-angle-axis
  "Create a quaternion from an angle in radians and an arbitrary axis vector."
  [angle axis]
  (let [vv     (g/abs axis)
        normal (g/div axis vv)]
    (from-angle-normal-axis angle normal)))

;; NOTE above is the weavejester version. Next, the scmutils way of doing it.
;; assumption is good... otherwise the same. It ASSUMES instead of normalizing.

;; NOTE: this should almost certainly normalize the axis? well, we'll see. If it
;; is symbolic, we want to just LOG that we are normalizing.

(defn angle-axis->
  "NOTE from gjs: Given a axis (a unit 3-vector) and an angle...

  TODO change name?"
  [theta axis]
  (let [v (g/simplify
           (ss/vector-dot-product axis axis))]
    ;; TODO SO the way this works is if it can trivially evaluate to false, THEN
    ;; we throw an error.
    (ul/assume! (list '= v 1) 'angle-axis->))
  (make (g/cos (g// theta 2))
        (g/* (g/sin (g// theta 2))
             axis)))

;; What to do if the vector part is 0? Here is one style:
;; https://math.stackexchange.com/questions/291110/numerically-stable-extraction-of-axis-angle-from-unit-quaternion
;;
;; that is basically what we have here. Except that solution bails with
;; numerically tiny vector and returns a default.
;;
;; TODO re-read what GJS has going on here.
;;
;;   TODO note that is undefined if you are not provided with a unit quaternion,
;;   so we can make that assumption, no problem... log it!

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

(defn ->angle-axis
  "TODO complete! this is old, from GJS.

  Problem: this is singular if the vector part is zero."
  ([q] (->angle-axis q vector))
  ([q continue]
   {:pre [(quaternion? q)]}
   (let [v (three-vector q)
         theta (g/mul 2 (g/atan
                         (g/abs v)
                         (real-part q)))
         axis (g/div v (g/abs v))]
     (continue theta axis))))

;; ### to/from 3D axes
;;
;; NOTE this one is weird. I think this is, give me the new orthogonal set of
;; axes you want to point at, and I will generate a rotation to get to those.

(defn axes
  "Return the three axes of the quaternion."
  [q]
  ;; NOTE that this norm was actually the dot-product.
  (let [n (magnitude-sq q)]
    (if (and (v/number? n)
             (or (v/zero? n)
                 (g/negative? n)))
      (m/I 3)
      (let [;; TODO check assumption here! for symbolic we can log an assumption.
            ;; Can we hardcode the zero case more easily, since so much disappears?
            s (g// 2 n)
            [w x y z] q
            xs (g/* x s)  ys (g/* y s)  zs (g/* z s)  ws (g/* w s)
            xx (g/* x xs) xy (g/* x ys) xz (g/* x zs) xw (g/* x ws)
            yy (g/* y ys) yz (g/* y zs) yw (g/* y ws)
            zz (g/* z zs) zw (g/* z ws)]
        [[(g/- 1 (g/+ yy zz)) (g/+ xy zw) (g/- xz yw)]
         [(g/- xy zw) (g/- 1 (g/+ xx zz)) (g/+ yz xw)]
         [(g/+ xz yw) (g/- yz xw) (g/- 1 (g/+ xx yy))]]))))

(defn from-axes
  "Create a quaternion from three axis vectors."
  [x-axis y-axis z-axis]
  (let [[m00 m10 m20] x-axis
        [m01 m11 m21] y-axis
        [m02 m12 m22] z-axis
        trace (g/+ m00 m11 m22)]
    (cond
      (>= trace 0)
      (let [s (g/sqrt (inc trace))
            r (/ 0.5 s)]
        (make (* r (- m21 m12))
              (* r (- m02 m20))
              (* r (- m10 m01))
              (* 0.5 s)))

      (and (> m00 m11) (> m00 m22))
      (let [s (Math/sqrt (- (inc m00) m11 m22))
            r (/ 0.5 s)]
        (make (* 0.5 s)
              (* r (+ m10 m01))
              (* r (+ m02 m20))
              (* r (- m21 m12))))

      (> m11 m22)
      (let [s (Math/sqrt (- (inc m11) m00 m22))
            r (/ 0.5 s)]
        (make (* r (+ m10 m01))
              (* 0.5 s)
              (* r (+ m21 m12))
              (* r (- m02 m20))))

      :else
      (let [s (Math/sqrt (- (inc m22) m00 m11))
            r (/ 0.5 s)]
        (make (* r (+ m02 m20))
              (* r (+ m21 m12))
              (* 0.5 s)
              (* r (- m10 m01)))))))

(comment
  (defn look-at
    "Create a quaternion that is directed at a point specified by a vector."
    [direction up]
    (let [z-axis (v/normalize direction)
          x-axis (v/normalize (v/cross up direction))
          y-axis (v/normalize (v/cross direction x-axis))]
      (from-axes x-axis y-axis z-axis))))

;; ## TODO add quaternions as 2x2 complex matrices
;; ## https://github.com/ferd36/quaternions/blob/master/include/quaternion.h#L578


;; ## Quaternions as 4x4 matrices

(def ONE-matrix
  (m/by-rows
   [1 0 0 0]
   [0 1 0 0]
   [0 0 1 0]
   [0 0 0 1]))

(def I-matrix
  (m/by-rows
   [0 1 0 0]
   [-1 0 0 0]
   [0 0 0 -1]
   [0 0 1 0]))

(def J-matrix
  (m/by-rows
   [0 0 1 0]
   [0 0 0 1]
   [-1 0 0 0]
   [0 -1 0 0]))

(def K-matrix
  (m/by-rows
   [0 0 0 1]
   [0 0 -1 0]
   [0 1 0 0]
   [-1 0 0 0]))

(def ONE-tensor (m/->structure ONE-matrix))
(def I-tensor (m/->structure I-matrix))
(def J-tensor (m/->structure J-matrix))
(def K-tensor (m/->structure K-matrix))

(defn ->4x4
  "Returns a 4x4 matrix that represents the supplied [[Quaternion]] `q`."
  [q]
  (g/+ (g/* (get-r q) ONE-matrix)
       (g/* (get-i q) I-matrix)
       (g/* (get-j q) J-matrix)
       (g/* (get-k q) K-matrix)))

(defn q:4x4->
  "TODO what is the deal with this first row thing??"
  [four-matrix]
  (make
   (nth four-matrix 0)))

;; To rotate a 3-vector by the angle prescribed by a unit quaternion.

;; TODO see if it's more efficient to do this, AND if so move the next one to
;; the tests.

(defn rotate
  "Rotate a vector with a quaternion."
  [q v]
  {:pre [(quaternion? q)
         (vector? v)
         (= 3 (count v))]}
  (let [[vx vy vz] v
        [qw qx qy qz] q]
    [(g/+ (g/* qw qw vx)       (g/* 2 qy qw vz) (g/* -2 qz qw vy)    (g/* qx qx vx)
          (g/* 2 qy qx vy)     (g/* 2 qz qx vz) (g/- (g/* qz qz vx)) (g/- (g/* qy qy vx)))
     (g/+ (g/* 2 qx qy vx)     (g/* qy qy vy)   (g/* 2 qz qy vz)     (g/* 2 qw qz vx)
          (g/- (g/* qz qz vy)) (g/* qw qw vy)   (g/* -2 qx qw vz)    (g/- (g/* qx qx vy)))
     (g/+ (g/* 2 qx qz vx)     (g/* 2 qy qz vy) (g/* qz qz vz)       (g/* -2 qw qy vx)
          (g/- (g/* qy qy vz)) (g/* 2 qw qx vy) (g/- (g/* qx qx vz)) (g/* qw qw vz))]))

(defn rotate [q]
  {:pre [(quaternion? q)]}
  ;;(assert (q:unit? q)) This assertion is really:

  ;; TODO log this `assume-unit!` as a new function, have it throw on false.
  (let [vv (->vector q)
        v  (g/simplify (g/dot-product vv vv))]
    (ul/assume! (list '= v 1) 'rotate))
  (let [q* (conjugate q)]
    (fn the-rotation [three-v]
      (three-vector
       (mul q (make 0 three-v) q*)))))

;; ## Relation to 3x3 Rotation Matrices
;;
;; Expanded Matt Mason method.

;; TODO get the simplify stuff going, maybe in a separate PR... AND remove this
;; `careful-simplify` stuff.

(def careful-simplify g/simplify)

(defn rotation-matrix->
  "TODO change >= etc to using compare... OR just go ahead and add those to v/
  finally!"
  [M]
  ;; (assert (orthogonal-matrix? M))
  ;; returns a unit quaternion
  (let [r11 (get-in M [0 0]) r12 (get-in M [0 1]) r13 (get-in M [0 2])
        r21 (get-in M [1 0]) r22 (get-in M [1 1]) r23 (get-in M [1 2])
        r31 (get-in M [2 0]) r32 (get-in M [2 1]) r33 (get-in M [2 2])
        quarter (g// 1 4)

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

        q0-2s (careful-simplify q0-2)
        q1-2s (careful-simplify q1-2)
        q2-2s (careful-simplify q2-2)
        q3-2s (careful-simplify q3-2)]
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

(defn ->rotation-matrix [q]
  {:pre [(quaternion? q)]}
  ;;(assert (q:unit? q))
  ;; This assertion is really:
  ;; TODO same thing, pull out assumption!!
  (let [vv (->vector q)
        v  (g/simplify (g/dot-product vv vv))]
    (ul/assume! (list '= v 1) 'quaternion->rotation-matrix))
  (let [q0 (get-r q) q1 (get-i q)
        q2 (get-j q) q3 (get-k q)
        m-2 (g/+ (g/square q0) (g/square q1)
                 (g/square q2) (g/square q3))]
    (m/by-rows [(g// (g/+ (g/expt q0 2)
                          (g/expt q1 2)
                          (g/* -1 (g/expt q2 2))
                          (g/* -1 (g/expt q3 2)))
                     m-2)
                (g// (g/* 2 (g/- (g/* q1 q2) (g/* q0 q3)))
                     m-2)
                (g// (g/* 2 (g/+ (g/* q1 q3) (g/* q0 q2)))
                     m-2)]
               [(g// (g/* 2 (g/+ (g/* q1 q2) (g/* q0 q3)))
                     m-2)
                (g// (g/+ (g/expt q0 2)
                          (g/* -1 (g/expt q1 2))
                          (g/expt q2 2)
                          (g/* -1 (g/expt q3 2)))
                     m-2)
                (g// (g/* 2 (g/- (g/* q2 q3) (g/* q0 q1)))
                     m-2)]
               [(g// (g/* 2 (g/- (g/* q1 q3) (g/* q0 q2)))
                     m-2)
                (g// (g/* 2 (g/+ (g/* q2 q3) (g/* q0 q1)))
                     m-2)
                (g// (g/+ (g/expt q0 2)
                          (g/* -1 (g/expt q1 2))
                          (g/* -1 (g/expt q2 2))
                          (g/expt q3 2))
                     m-2)])))

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

;; TODO add tests for scalar / quat
(defmethod g/div [::quaternion ::quaternion] [a b] (div a b))
(defmethod g/div [::v/scalar ::quaternion] [q s] (scale-l s (g/invert q)))
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

;; TODO test!
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

(defmethod g/dot-product [::quaternion ::quaternion] [a b] (dot-product a b))
(defmethod g/dot-product [::v/scalar ::quaternion] [a b] (g/* a (get-r b)))
(defmethod g/dot-product [::quaternion ::v/scalar] [a b] (g/* (get-r a) b))
(defmethod g/dot-product [::sc/complex ::quaternion] [a b]
  (g/+ (g/* (sc/real a) (get-r b))
       (g/* (sc/imaginary a) (get-i b))))

(defmethod g/dot-product [::quaternion ::sc/complex] [a b]
  (g/+ (g/* (get-r a) (sc/real b))
       (g/* (get-i a) (sc/imaginary b))))

(defmethod g/solve-linear-right [::quaternion ::scalar] [q s] (q-div-scalar q s))
(defmethod g/solve-linear-right [::quaternion ::quaternion] [a b] (div a b))
(defmethod g/solve-linear-right [::sc/complex ::quaternion] [a b] (div (make a) b))
(defmethod g/solve-linear-right [::quaternion ::sc/complex] [a b] (div a (make b)))

(defmethod g/solve-linear [::v/scalar ::quaternion] [s q] (q-div-scalar q s))
(defmethod g/solve-linear [::quaternion ::quaternion] [a b] (div b a))
(defmethod g/solve-linear [::sc/complex ::quaternion] [a b] (div b (make a)))
(defmethod g/solve-linear [::quaternion ::sc/complex] [a b] (div (make b) a))

;; ## Implementation Notes
;;
;; TODO: look at `assume!` in scmutils. What if you can show that the thing IS
;; indeed true? Does the logging just happen as the default failure?
;;
;; NOTE I think I know what this does now, and it lives in
;; `sicmutils.util.logic`. We can augment what that does - but it tries to
;; execute whatever statement it can,and fails if that thing can provably go
;; false.
;;
;; NOTE AND you might not want to normalize every damned time, if you have some
;; symbolic thing. Do it twice and it's going to get crazy. So why not just
;; assume that it's normalized?

;; TODO slerp, fromAxisAngle (confirm), fromEuler (confirm), fromBetweenVectors https://github.com/infusion/Quaternion.js/

;; slerp notes: https://math.stackexchange.com/questions/93605/understanding-the-value-of-inner-product-of-two-quaternions-in-slerp
;;
;; TODO confirm we have ->3x3 matrix and ->4x4 matrix https://github.com/infusion/Quaternion.js/
;;
;; TODO quaternion sinum  https://github.com/typelevel/spire/blob/master/core/src/main/scala/spire/math/Quaternion.scala#L187
