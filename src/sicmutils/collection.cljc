
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
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang IPersistentVector
                            IPersistentMap
                            ISeq
                            LazySeq))))

;; Vector Implementations

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

  f/IArity
  (arity [v] [:exactly 1])

  d/IPerturbed
  (perturbed? [v] (boolean (some d/perturbed? v)))
  (replace-tag [v old new] (mapv #(d/replace-tag % old new) v))
  (extract-tangent [v tag] (mapv #(d/extract-tangent % tag) v)))

;; ## Sequences

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

(letfn [(map-vals [f m]
          (into (empty m)
                (map (fn [[k v]] [k (f v)]))
                m))]
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
     (arity [_] [:exactly 1])

     d/IPerturbed
     (perturbed? [m] (boolean (some d/perturbed? (vals m))))
     (replace-tag [m old new] (map-vals #(d/replace-tag % old new) m))
     (extract-tangent [m tag] (map-vals #(d/extract-tangent % tag) m)))))
