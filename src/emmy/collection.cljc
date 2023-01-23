#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.collection
  "This namespace contains implementations of various Emmy protocols for
  native Clojure collections."
  (:require [clojure.set :as cs]
            [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang PersistentVector
                            IPersistentVector
                            IPersistentMap
                            IPersistentSet
                            ISeq))))

;; ## Vector Implementations
;;
;; Vectors are implicitly treated as [[emmy.structure/Structure]] instances
;; with an `up` orientation, and implement [[v/freeze]] identically. They can
;; act as `zero?`, but they can't act as `one?` or `identity?`; those are
;; reserved for instances that have no effect on multiplication.

(defmethod g/simplify [PersistentVector] [v]
  (mapv g/simplify v))

(extend-type #?(:clj IPersistentVector :cljs PersistentVector)
  v/Value
  (zero? [v] (every? v/zero? v))
  (one? [_] false)
  (identity? [_] false)
  (zero-like [v] (mapv v/zero-like v))
  (one-like [_] 1)
  (identity-like [_] 1)
  (exact? [v] (every? v/exact? v))
  (freeze [v] `(~'up ~@(map v/freeze v)))
  (kind [v] (type v))

  ;; Another difference from [[emmy.structure/Structure]] is that a
  ;; structure of functions acts as a function itself that applies its entries
  ;; to its arguments. Vectors already implement IFn (they take an index and
  ;; look it up), so they can't respond the same way as a structure via
  ;; arity. (the `2` arity takes an additional default value.)
  f/IArity
  (arity [_] [:between 1 2])

  ;; Vectors are functors, so they can be perturbed if any of their elements are
  ;; perturbed. [[d/replace-tag]] and [[d/extract-tangent]] pass the buck down
  ;; the vector's elements.
  d/IPerturbed
  (perturbed? [v] (boolean (some d/perturbed? v)))
  (replace-tag [v old new] (mapv #(d/replace-tag % old new) v))
  (extract-tangent [v tag] (mapv #(d/extract-tangent % tag) v)))

;; ## Sequences
;;
;; Sequences can't act as functions or respond to any of
;; the [[v/zero?]]-and-friends predicates. They pass along the operations that
;; they can implement to their elements via [[map]].

(defmethod g/simplify [v/seqtype] [a]
  (map g/simplify a))

#_{:clj-kondo/ignore [:redundant-do]}
(#?@(:clj [do]
     :cljs [doseq [klass [Cons IndexedSeq LazySeq List Range IntegerRange]]])
 (extend-type #?(:clj ISeq :cljs klass)
   v/Value
   (zero? [_] false)
   (one? [_] false)
   (identity? [_] false)
   (zero-like [xs] (map v/zero-like xs))
   (one-like [xs] (u/unsupported (str "one-like: " xs)))
   (identity-like [xs] (u/unsupported (str "identity-like: " xs)))
   (exact? [_] false)
   (freeze [xs] (map v/freeze xs))
   (kind [xs] (type xs))

   d/IPerturbed
   (perturbed? [_] false)
   (replace-tag [xs old new] (map #(d/replace-tag % old new) xs))
   (extract-tangent [xs tag] (map #(d/extract-tangent % tag) xs))))

;; ## Maps
;;
;; Maps acts as functors that can be perturbed and zeroed out (and pass along
;; calls to [[g/partial-derivative]] to their elements!), but not much else.
;;
;; NOTE: There is probably a case for making something
;; like [[emmy.structure/Structure]] backed by a map, for a sort of sparse
;; structure, or a dataframe-like structure with named fields instead of
;; positional fields. Nothing like this exists yet!

#?(:clj
   (derive IPersistentMap ::map)

   :cljs
   (do
     (derive PersistentHashMap ::map)
     (derive PersistentArrayMap ::map)
     (derive PersistentTreeMap ::map)))

(defmethod g/negate [::map] [m]
  (u/map-vals g/negate m))

(defmethod g/add [::map ::map] [a b]
  (merge-with g/add a b))

(defmethod g/sub [::map ::map] [a b]
  (merge-with g/add a (u/map-vals g/negate b)))

(defmethod g/mul [::map ::v/scalar] [m x]
  (u/map-vals #(g/mul % x) m))

(defmethod g/mul [::v/scalar ::map] [x m]
  (u/map-vals #(g/mul x %) m))

(defmethod g/div [::map ::v/scalar] [m x]
  (u/map-vals #(g/div % x) m))

(defn- combine [f m1 m2 l-default]
  (letfn [(merge-entry [m e]
			      (let [k (key e)
                  v (val e)]
			        (assoc m k (f (get m k l-default) v))))]
    (reduce merge-entry m1 (seq m2))))

(defmethod g/make-rectangular [::map ::map] [m1 m2]
  (combine g/make-rectangular m1 m2 0))

(defmethod g/make-polar [::map ::map] [m1 m2]
  (combine g/make-polar m1 m2 0))

(defmethod g/real-part [::map] [m]
  (u/map-vals g/real-part m))

(defmethod g/imag-part [::map] [m]
  (u/map-vals g/imag-part m))

(defmethod g/simplify [::map] [m]
  (u/map-vals g/simplify m))

(let [sentinel #?(:cljs (NeverEquiv.)
                  :clj (Object.))]
  (defmethod v/= [::map ::map] [x y]
    (boolean
     (when (== (count x) (count y))
       (reduce-kv
        (fn [_ k v]
          (if (v/= (get y k sentinel) v)
            true
            (reduced false)))
        true
        x)))))

(defmethod g/partial-derivative [::map v/seqtype] [m selectors]
  (u/map-vals #(g/partial-derivative % selectors)
              m))

#_{:clj-kondo/ignore [:redundant-do]}
(#?@(:clj [do] :cljs [doseq [klass [PersistentHashMap PersistentArrayMap PersistentTreeMap]]])
 (extend-type #?(:clj IPersistentMap :cljs klass)
   v/Value
   (zero? [m] (every? v/zero? (vals m)))
   (one? [_] false)
   (identity? [_] false)
   (zero-like [m] (u/map-vals v/zero-like m))
   (one-like [m] (u/unsupported (str "one-like: " m)))
   (identity-like [m] (u/unsupported (str "identity-like: " m)))
   (exact? [m] (every? v/exact? (vals m)))
   (freeze [m] (u/map-vals v/freeze m))
   (kind [m] (if (sorted? m)
               (type m)
               (:type m (type m))))

   f/IArity
   (arity [_] [:between 1 2])

   d/IPerturbed
   (perturbed? [m] (boolean (some d/perturbed? (vals m))))
   (replace-tag [m old new] (u/map-vals #(d/replace-tag % old new) m))
   (extract-tangent [m tag]
     (if-let [t (:type m)]
       ;; Do NOT attempt to recurse into the values if this map is being used as a
       ;; simple representation for some other type, like a manifold point.
       (u/unsupported (str "`extract-tangent` not supported for type " t "."))
       (u/map-vals #(d/extract-tangent % tag) m)))))


;; ## Sets
;;
;; Emmy treats Clojure's set data structure as a monoid, with set union as
;; the addition operation and the empty set as the zero element.

#?(:clj
   (derive IPersistentSet ::set)

   :cljs
   (do
     (derive PersistentHashSet ::set)
     (derive PersistentTreeSet ::set)))

(defmethod g/add [::set ::set] [a b]
  (cs/union a b))

#_{:clj-kondo/ignore [:redundant-do]}
(#?@(:clj [do] :cljs [doseq [klass [PersistentHashSet PersistentTreeSet]]])
 (extend-type #?(:clj IPersistentSet :cljs klass)
   v/Value
   (zero? [s] (empty? s))
   (one? [_] false)
   (identity? [_] false)
   (zero-like [_] #{})
   (one-like [s] (u/unsupported (str "one-like: " s)))
   (identity-like [s] (u/unsupported (str "identity-like: " s)))
   (exact? [_] false)
   (freeze [s] (u/unsupported (str "freeze: " s)))
   (kind [s] (type s))

   f/IArity
   (arity [_] [:between 1 2])))
