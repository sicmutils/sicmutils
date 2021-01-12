
;; Copyright © 2020 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology

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
;;

(ns sicmutils.collection
  "This namespace contains implementations of various SICMUtils protocols for
  native Clojure collections."
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang IPersistentVector
                            IPersistentMap
                            ISeq
                            LazySeq))))

;; ## Vector Implementations
;;
;; Vectors are implicitly treated as [[sicmutils.structure/Structure]] instances
;; with an `up` orientation. They can act as `zero?`, but they can't act as
;; `one?` or `identity?`; those are reserved for instances that have no effect
;; on multiplication.
;;
(extend-type #?(:clj IPersistentVector :cljs PersistentVector)
  v/Value
  (zero? [v] (every? v/zero? v))
  (one? [_] false)
  (identity? [_] false)
  (zero-like [v] (mapv v/zero-like v))
  (one-like [v] (u/unsupported (str "one-like: " v)))
  (identity-like [v] (u/unsupported (str "identity-like: " v)))
  (exact? [v] (every? v/exact? v))
  (freeze [v] (mapv v/freeze v))
  (kind [v] (type v))

  ;; Another difference from [[sicmutils.structure/Structure]] is that a structure
  ;; of functions acts as a function itself that applies its entries to its
  ;; arguments. Vectors already implement IFn (they take an index and look it up),
  ;; so they can't respond the same way as a structure via arity.
  f/IArity
  (arity [v] [:exactly 1])

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

(#?@(:clj [do] :cljs [doseq [klass [Cons IndexedSeq LazySeq List]]])
 (extend-type #?(:clj ISeq :cljs klass)
   v/Value
   (zero? [_] false)
   (one? [_] false)
   (identity? [_] false)
   (zero-like [xs] (map v/zero-like xs))
   (one-like [xs] (u/unsupported (str "one-like: " xs)))
   (identity-like [xs] (u/unsupported (str "identity-like: " xs)))
   (exact? [xs] false)
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
;; like [[sicmutils.structure/Structure]] backed by a map, for a sort of sparse
;; structure, or a dataframe-like structure with named fields instead of
;; positional fields. Nothing like this exists yet!

#?(:clj
   (derive IPersistentMap ::map)

   :cljs
   (do
     (derive PersistentHashMap ::map)
     (derive PersistentArrayMap ::map)
     (derive PersistentTreeMap ::map)))

(letfn [(map-vals [f m]
          (into (empty m)
                (map (fn [[k v]] [k (f v)]))
                m))]

  (defmethod g/partial-derivative [::map v/seqtype] [m selectors]
    (map-vals #(g/partial-derivative % selectors)
              m))

  (#?@(:clj [do] :cljs [doseq [klass [PersistentHashMap PersistentArrayMap PersistentTreeMap]]])
   (extend-type #?(:clj IPersistentMap :cljs klass)
     v/Value
     (zero? [m] (every? v/zero? (vals m)))
     (one? [_] false)
     (identity? [_] false)
     (zero-like [m] (map-vals v/zero-like m))
     (one-like [m] (u/unsupported (str "one-like: " m)))
     (identity-like [m] (u/unsupported (str "identity-like: " m)))
     (exact? [m] (every? v/exact? (vals m)))
     (freeze [m] (map-vals v/freeze m))
     (kind [m] (:type m (type m)))

     f/IArity
     (arity [m] [:exactly 1])

     d/IPerturbed
     (perturbed? [m] (boolean (some d/perturbed? (vals m))))
     (replace-tag [m old new] (map-vals #(d/replace-tag % old new) m))
     (extract-tangent [m tag] (map-vals #(d/extract-tangent % tag) m)))))
