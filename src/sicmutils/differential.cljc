;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.differential
  (:refer-clojure :rename {compare core-compare}
                  #?@(:cljs [:exclude [compare]]))
  (:require [clojure.string :refer [join]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.stream :as us]
            [sicmutils.util.vector-set :as uv]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn))))

;; ## Calculus of Infinitesimals
;;
;; The idea is that we compute derivatives by passing special
;; "differential objects" [x,dx] through functions. A first approximation to the
;; idea is as follows:
;;
;; f[x,dx] |---> [f(x), Df(x)*dx]
;;
;; Note that the derivative of f at the point x, DF(x), is the coefficient of dx
;; in the result. If we then pass this result through another function, we
;; obtain the chain-rule answer we would hope for.
;;
;; g[f(x), Df(x)*dx] |---> [g(f(x)), DG(f(x))*DF(x)*dx]
;;
;; Thus, we can find the derivative of a composition by this process. We need
;; only define how each of the primitives act on these "differentials" and then
;; we can use ordinary Scheme compositions of these to do the job. See the
;; procedure diff:derivative near the bottom to understand how derivatives are
;; computed given this differential algebra. This idea was discovered by Dan
;; Zuras and Gerald Jay Sussman in 1992. DZ and GJS made the first version of
;; this code during an all nighter in 1992.
;;
;; To expand this idea to work for multiple derivatives of functions of several
;; variables we define an algebra in "infinitesimal space". The objects are
;; multivariate power series in which no incremental has exponent greater than
;; 1. This was worked out in detail by Hal Abelson around 1994, and painfully
;; redone in 1997 by Sussman with the help of Hardy Mayer and Jack Wisdom.
;;
;; A rare and surprising bug was discovered by Alexey Radul in 2011. This was
;; fixed by remapping the infinitesimals for derivatives of functions that
;; returned functions. This was done kludgerously, but it works.
;;
;; ### Data Structures
;;
;; A differential quantity is a typed list of differential terms, representing
;; the power series alluded to earlier. The terms are kept in a sorted order, in
;; ascending order. (Order is the number of incrementals. So dx*dy is higher
;; order than dx or dy.)

;; A differential term is implemented as a pair whose first element is a set of
;; tags and whose second is the coefficient. Let's do the tag set implementation
;; first.

(def ^:private tags first)
(def ^:private coefficient second)

(defn- make-term
  ([coef] [uv/empty-set coef])
  ([tags coef] [tags coef]))

;; Each differential term has a list of tags. The tags represent the
;; incrementals. Roughly, "dx" and "dy" are tags in the terms: 3*dx, 4*dy,
;; 2*dx*dy. There is a tag created for each derivative that is in progress.
;; Since the only use of a tag is to distinguish unnamed incrementals we use
;; positive integers for the tags.

(let [next-tag (atom 0)]
  (defn fresh-tag []
    (swap! next-tag inc)))

;; Tags are small integers. Tag sets are typically of small cardinality. So we
;; experiment with implementing them as small vectors, instead of sorted sets.
;; This implementation lives in [[sicmutils.util.vector-set]], and we use it
;; here.

(defn- tag-in-term?
  "Return true if `t` is in the tag-set of the supplied `term`, false otherwise."
  [term t]
  (uv/contains? (tags term) t))

;; ## Term List Algebra
;;
;; Now, we get to 'terms'. TODO note what is going on... we've multiplied out a
;; bunch of these things, but we can always turn them back into a dual number.
;;
;; From scmutils: "Differential term lists represent a kind of power series, so
;; they can be added and multiplied. It is important to note that when terms are
;; multiplied, no contribution is made if the terms being multiplied have a
;; differential tag in common. Thus dx^2 = zero."
;;
;; TODO also note that Clojure vectors already compare properly, so term lists
;; can be compared nicely now if we compare by just the first terms, not the
;; coefs.... maybe this doesn't matter.

(defn- terms:+
  "Iterate and build up the result while preserving order and dropping zero sums."
  [xs ys]
  (loop [xs xs
         ys ys
         result []]
    (cond (empty? xs) (into result ys)
          (empty? ys) (into result xs)
          :else (let [[a-tags a-coef :as a] (first xs)
                      [b-tags b-coef :as b] (first ys)
                      c (core-compare a-tags b-tags)]
                  (cond
                    (= c 0)
                    (let [r-coef (g/+ a-coef b-coef)]
                      (recur (rest xs)
                             (rest ys)
                             (if (v/zero? r-coef)
                               result
                               (conj result (make-term a-tags r-coef)))))

                    (< c 0)
                    (recur (rest xs) ys (conj result a))

                    :else
                    (recur xs (rest ys) (conj result b)))))))

(defn- terms:*
  "Eagerly multiply the two term lists."
  [xs ys]
  (for [[x-tags x-coef] xs
        [y-tags y-coef] ys
        :when (empty? (uv/intersection x-tags y-tags))]
    (make-term (uv/union x-tags y-tags)
               (g/* x-coef y-coef))))

(defn- collect-terms
  "Build a term list up of pairs of tags => coefficients by grouping together and
  summing coefficients paired with the same term list.

  The result will be sorted by term list, and contain no duplicate term lists."
  [tags->coefs]
  (let [terms (for [[tags tags-coefs] (group-by tags tags->coefs)
                    :let [c (transduce (map coefficient) g/+ tags-coefs)]
                    :when (not (v/zero? c))]
                [tags c])]
    (into [] (sort-by tags terms))))

;; ## Differential
;;
;; Now that we have term lists, we can define the real deal, the full thing. As
;; with other types, the step here is to wrap up this thing in a `deftype` so we
;; can customize it.

;; TODO note that we are extending scalar here for blah reasons.

(derive ::differential ::v/scalar)

(declare d:apply compare equiv finite-term one?)

;; Contract is that terms is sorted, and never empty.

(deftype Differential [terms]
  f/IArity
  (arity [_]
    (f/arity (coefficient (first terms))))

  v/Numerical
  (numerical? [_]
    (v/numerical? (coefficient (first terms))))

  v/Value
  (zero? [this]
    (every? (comp v/zero? coefficient) terms))

  (one? [this] (one? this))
  (identity? [this] (one? this))

  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [_] `[~'Differential ~@terms])
  (exact? [_] false)

  (kind [_] ::differential)

  Object
  #?(:clj (equals [a b] (equiv a b)))
  (toString [_] (str "D[" (join " " (map #(join " → " %) terms)) "]"))

  #?@(:clj
      [Comparable
       (compareTo [a b] (compare a b))

       IFn
       (invoke [this]
               (d:apply this []))
       (invoke [this a]
               (d:apply this [a]))
       (invoke [this a b]
               (d:apply this [a b]))
       (invoke [this a b c]
               (d:apply this [a b c]))
       (invoke [this a b c d]
               (d:apply this [a b c d]))
       (invoke [this a b c d e]
               (d:apply this [a b c d e]))
       (invoke [this a b c d e f]
               (d:apply this [a b c d e f]))
       (invoke [this a b c d e f g]
               (d:apply this [a b c d e f g]))
       (invoke [this a b c d e f g h]
               (d:apply this [a b c d e f g h]))
       (invoke [this a b c d e f g h i]
               (d:apply this [a b c d e f g h i]))
       (invoke [this a b c d e f g h i j]
               (d:apply this [a b c d e f g h i j]))
       (invoke [this a b c d e f g h i j k]
               (d:apply this [a b c d e f g h i j k]))
       (invoke [this a b c d e f g h i j k l]
               (d:apply this [a b c d e f g h i j k l]))
       (invoke [this a b c d e f g h i j k l m]
               (d:apply this [a b c d e f g h i j k l m]))
       (invoke [this a b c d e f g h i j k l m n]
               (d:apply this [a b c d e f g h i j k l m n]))
       (invoke [this a b c d e f g h i j k l m n o]
               (d:apply this [a b c d e f g h i j k l m n o]))
       (invoke [this a b c d e f g h i j k l m n o p]
               (d:apply this [a b c d e f g h i j k l m n o p]))
       (invoke [this a b c d e f g h i j k l m n o p q]
               (d:apply this [a b c d e f g h i j k l m n o p q]))
       (invoke [this a b c d e f g h i j k l m n o p q r]
               (d:apply this [a b c d e f g h i j k l m n o p q r]))
       (invoke [this a b c d e f g h i j k l m n o p q r s]
               (d:apply this [a b c d e f g h i j k l m n o p q r s]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t]
               (d:apply this [a b c d e f g h i j k l m n o p q r s t]))
       (applyTo [this xs] (AFn/applyToHelper this xs))]

      :cljs
      [IEquiv
       (-equiv [a b] (equiv a b))

       IComparable
       (-compare [a b]  (compare a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))

       IFn
       (-invoke [this]
                (d:apply this []))
       (-invoke [this a]
                (d:apply this [a]))
       (-invoke [this a b]
                (d:apply this [a b]))
       (-invoke [this a b c]
                (d:apply this [a b c]))
       (-invoke [this a b c d]
                (d:apply this [a b c d]))
       (-invoke [this a b c d e]
                (d:apply this [a b c d e]))
       (-invoke [this a b c d e f]
                (d:apply this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (d:apply this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (d:apply this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (d:apply this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (d:apply this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (d:apply this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (d:apply this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (d:apply this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (d:apply this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (d:apply this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (d:apply this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (d:apply this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (d:apply this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (d:apply this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (d:apply this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (d:apply this (concat [a b c d e f g h i j k l m n o p q r s t] rest)))]))

#?(:clj
   (defmethod print-method Differential
     [^Differential s ^java.io.Writer w]
     (.write w (.toString s))))

;; ## Accessor Methods

(defn differential?
  "Returns true if the supplied object is an instance of `Differential`, false
  otherwise."
  [dx]
  (instance? Differential dx))

(defn- bare-terms
  "Returns the `-terms` field of the supplied `Differential` object. Errors if any
  other type is supplied."
  [dx]
  {:pre [(differential? dx)]}
  (.-terms ^Differential dx))

(defn- ->terms
  "Returns a vector of terms that represent the supplied [[Differential]]; any term
  with a coefficient of zero will be filtered out before return.

  If you pass a non-[[Differential]], [[->terms]] will return a singleton term
  list (or `[]` if the argument was zero)."
  [dx]
  (cond (differential? dx)
        (filterv (fn [term]
                   (not (v/zero? (coefficient term))))
                 (bare-terms dx))

        (v/zero? dx) []
        :else        [(make-term dx)]))

(defn- terms->differential
  "Returns a differential instance generated from a vector of terms. This method
  will do some mild cleanup, or canonicalization:

  - any empty term list will return 0
  - a singleton term list with no tags will return its coefficient

  NOTE this method assumes that the input is properly sorted, and contains no
  zero coefficients."
  [terms]
  {:pre [(vector? terms)]}
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (empty? (tags (first terms))))
        (coefficient (first terms))

        :else (->Differential terms)))

(defn from-terms
  "Accepts a sequence of tags => coefficient (also called a \"term\") and returns:

  - a [[Differential]] instance, if the terms resolve to a differential with
    non-zero infinitesimal terms
  - the original input otherwise.

  The (unenforced) contract is that if you call this method, all tags should be
  sorted and contain duplicates."
  [tags->coefs]
  (terms->differential
   (collect-terms tags->coefs)))

(defn max-order-tag
  "Given one or more well-formed differential objects, returns the
  maximum ('highest order') tag found in the highest-order term of the any of
  the [[Differential]] instances.

  If there is NO maximal tag (ie, if you provide differentials with no non-zero
  tangent parts, or non-differentials), returns nil."
  ([dx]
   (when (differential? dx)
     (let [last-term   (peek (->terms dx))
           highest-tag (peek (tags last-term))]
       highest-tag)))
  ([dx & dxs]
   (letfn [(max-termv [dx]
             (if-let [max-order (max-order-tag dx)]
               [max-order]
               []))]
     (when-let [orders (seq (mapcat max-termv (cons dx dxs)))]
       (apply max orders)))))

(defn- d:apply
  "If the coefficients are themselves functions, apply them to the args for ALL
  coefficients."
  [diff args]
  (terms->differential
   (into [] (mapcat (fn [term]
                      (let [result (apply (coefficient term) args)]
                        (if (v/zero? result)
                          []
                          [(make-term (tags term) result)]))))
         (->terms diff))))

(defn d:+
  "Returns an object representing the sum of the two differential objects `dx` and
  `dy`. This works by summing the coefficients of all terms with the same list
  of tags.

  Works with non-differentials on either or both sides."
  [dx dy]
  (terms->differential
   (terms:+ (->terms dx)
            (->terms dy))))

(defn d:*
  "Returns an object representing the product of the two differential objects `dx`
  and `dy`.

  This works by multiplying out all terms:

  $$(dx1 + dx2 + dx3 + ...)(dy1 + dy2 + dy3...)$$

  and then collecting any duplicate terms by summing their coefficients.

  Works with non-differentials on either or both sides."
  [dx dy]
  (from-terms
   (terms:* (->terms dx)
            (->terms dy))))

(defn bundle
  "Generate a new differential object with the supplied `primal` and `tangent`
  components, and the supplied internal `tag` that this differential will carry
  around to prevent perturbation confusion.

  If the `tangent` component is `0`, acts as identity on `primal`.

  `tangent` defaults to 1."
  ([primal tag]
   (bundle primal 1 tag))
  ([primal tangent tag]
   (let [term (make-term (uv/make [tag]) tangent)]
     (d:+ primal (->Differential [term])))))

;; ## IPerturbed Protocol, Containers
;;
;; The [[IPerturbed]] protocol allows the user to treat other objects that
;; aren't specifically [[Differential]] instances as if they were
;; 'differential'. The docstrings tell some of the story:

(defprotocol IPerturbed
  (perturbed? [this]
    "Returns true if the supplied object has some non-zero tangent to be
    extracted via [[extract-tangent]], false otherwise. (Return `false` by
    default if you can't detect a perturbation.)")

  (replace-tag [this old-tag new-tag]
    "If the supplied object `this` is perturbed, Returns a similar object with
    the the perturbation modified by replacing any perturbation tagged with
    `old-tag` with the `new-tag`. Else, acts as identity.")

  (extract-tangent [this tag]
    "If the supplied object `this` is perturbed, return the tangent component
    paired with the supplied tag. Else, returns `([[sicmutils.value/zero-like]]
    this)`."))

;; The [[IPerturbed]] protocol exists for two reasons:
;;
;; 1. Any function that takes a [[Differential]] can stuff that differential
;;    into a container, or wrap it inside of a function (any 'functor',
;;    technically); we have to be able to `fmap` into the structure and extract
;;    the tangent components from the [[Differential]] instances.

;; 2. The way that [[Differential]] instances interact with container types in
;;    sicmutils makes it easy for these captures to occur all over. Whenever we
;;    multiply a [[Differential]] by a structure, a function, a vector, any of
;;    those things, our implementation of the sicmutils generics pushes
;;    the [[Differential]] inside those objects, rather than forming
;;    a [[Differential]] with, for example, some structure in the primal and
;;    tangent parts.
;;
;; NOTE: [[perturbed?]] is (as of version 0.14.0) only used
;; by [[sicmutils.abstract.function]] to detect if the caller is attempting to
;; take its derivative. In the original scmutils, this check is
;; actually [[differential?]], vs checking for containers. I think that our
;; implementation is more correct.
;;
;; Here are the default implementations, and the implementations
;; for [[Differential:]]

(extend-protocol IPerturbed
  #?(:clj Object :cljs default)
  (perturbed? [_] false)
  (replace-tag [this _ _] this)
  (extract-tangent [_ _] 0)

  Differential
  (perturbed? [_] true)
  (replace-tag [dx oldtag newtag]
    (terms->differential
     (mapv (fn [term]
             (let [tagv (tags term)]
               (if (uv/contains? tagv oldtag)
                 (make-term (-> (tags term)
                                (uv/disj oldtag)
                                (uv/conj newtag))
                            (coefficient term))
                 term)))
           (bare-terms dx))))
  (extract-tangent [dx tag]
    (from-terms
     (mapcat (fn [term]
               (let [tagv (tags term)]
                 (if (uv/contains? tagv tag)
                   [(make-term (uv/disj tagv tag)
                               (coefficient term))]
                   [])))
             (bare-terms dx)))))

;; ## Differential Parts API
;;
;; These functions give higher-level access to the components of
;; a [[Differential]] you're typically interested in.

(defn finite-term
  "Returns the term of the supplied [[Differential]] `dx` that has no tags
  attached to it, `0` otherwise.

  NOTE that this will only work with a well-formed [[Differential]], ie, an
  instance with all terms sorted by their list of tags."
  [dx]
  (if (differential? dx)
    (let [[head] (bare-terms dx)
          ts     (tags head)]
      (if (= [] ts)
        (coefficient head)
        0))
    dx))

;; The [[primal-part]] is all terms except for terms
;; containing [[max-order-tag]], and [[tangent-part]] is the remaining terms,
;; all of which contain that tag. So:

;;                        f
;; x + dx + dy + dx*dy |----> f(x+dx) + Df(x+dx)*(dy+dx*dy)
;;
;; Alternatively, we might have factored out the LOWEST order tag by default and
;; computed the following:
;;
;;                        f
;; x + dx + dy + dx*dy |----> f(x+dy) + Df(x+dy)*(dx+dx*dy)
;;
;; The ultimate values of the derivatives don't care, because mixed partials of
;; R^2 --> R commute.

(defn primal-part
  "Returns a differential containing only the terms of `dx` that do NOT contain
  the supplied `tag`, ie, the primal component of `dx` with respect to the `tag`
  infinitesimal. If no tag is supplied, defaults to the tag of maximum order
  across all terms in the supplied `dx`.

  Every [[Differential]] can be factored into a dual number of the form

      primal + (tangent * tag)

  NOTE: For each tag in any of its terms. [[primal-part]] returns this first
  piece, potentially simplified into a non-[[Differential]] if the primal part
  contains no other tags."
  ([dx] (primal-part dx (max-order-tag dx)))
  ([dx tag]
   (if (differential? dx)
     (let [sans-tag? #(not (tag-in-term? % tag))]
       (->> (->terms dx)
            (filterv sans-tag?)
            (from-terms)))
     dx)))

(defn tangent-part
  "Returns a differential containing only the terms of `dx` that contain the
  supplied `tag`, ie, the tangent component of `dx` with respect to the `tag`
  infinitesimal. If no tag is supplied, defaults to the tag of maximum order
  across all terms in the supplied `dx`.

  NOTE: Every [[Differential]] can be factored into a dual number of the form

      primal + (tangent * tag)

  For each tag in any of its terms. [[tangent-part]] returns a [[Differential]]
  representing `(tangent * tag)`, or 0 if `dx` contains no terms with the
  supplied `tag`.

  NOTE: the 2-arity case is similar to `([[extract-tangent]] dx tag)`; the only
  difference is that [[extract-tangent]] drops the `dx` tag from all terms in
  the returned value."
  ([dx] (tangent-part dx (max-order-tag dx)))
  ([dx tag]
   (if (differential? dx)
     (->> (->terms dx)
          (filterv #(tag-in-term? % tag))
          (from-terms))
     0)))

(defn primal-tangent-pair
  "Returns a pair of the primal and tangent components of the supplied `dx`, with
  respect to the supplied `tag`. See the docs for [[primal-part]]
  and [[tangent-part]] for more details.

  Equivalent to `[([[primal-part]] dx tag) ([[tangent-part]] dx tag)]` but
  slightly more efficient if you want both pieces."
  ([dx] (primal-tangent-pair dx (max-order-tag dx)))
  ([dx tag]
   (if-not (differential? dx)
     [dx 0]
     (let [[tangent-terms primal-terms]
           (us/separatev #(tag-in-term? % tag)
                         (->terms dx))]
       [(from-terms primal-terms)
        (from-terms tangent-terms)]))))

(defn one?
  "Returns true if the supplied instance has a [[finite-part]] that responds true
  to [[sicmutils.value/one?]], and zero coefficients on any of its tangent
  components; false otherwise.

  NOTE: This means that [[one?]] will not do what you expect as a conditional
  inside some function. If you want to branch inside some function you're taking
  the derivative of, prefer `(= 1 dx)`. This will only look at
  the [[finite-part]] and ignore the values of the tangent parts."
  [dx]
  (let [[p t] (primal-tangent-pair dx)]
    (and (v/one? p)
         (v/zero? t))))

(defn equiv
  "Returns true if all of the supplied objects have equal finite parts, false
  otherwise.

  Use [[equiv]] if you want to compare non-differentials with
  [[Differential]]s and ignore all tangent components. If you _do_ want to take
  the tangent components into account, prefer [[eq]]."
  ([_] true)
  ([a b]
   (= (finite-term a)
      (finite-term b)))
  ([a b & more]
   (reduce equiv (equiv a b) more)))

(defn eq
  "For non-differentials, this is identical to [[clojure.core/=]].
  For [[Differential]] instances, equality acts on tangent components too.

  If you want to ignore the tangent components, use [[equiv]]."
  ([_] true)
  ([a b]
   (= (->terms a)
      (->terms b)))
  ([a b & more]
   (reduce eq (eq a b) more)))

(defn compare
  "Comparator that compares differential instances with each other or
  non-differentials using only the [[finite-part]] of each instance. Matches the
  response of [[equiv]].

  Acts as [[clojure.core/compare]] for non-differentials."
  [a b]
  (core-compare
   (finite-term a)
   (finite-term b)))

(defn compare-full
  "Comparator that compares differential instances with each other or
  non-differentials using all tangent terms each instance. Matches the response
  of [[eq]].

  Acts as [[clojure.core/compare]] for non-differentials."
  [a b]
  (core-compare
   (->terms a)
   (->terms b)))

;; ## Chain Rule
;;
;; Finally we come to the heart of it - the chain rule. The following functions
;; apply the chain rule to unary functions, binary functions and "n-ary"
;; functions.

(defn- lift-1
  "Given:

  - some unary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument

  Returns a new unary function that operates on both the original type of `f`
  and [[Differential]] instances.

  NOTE: With the current implementation, `f` has to ALREADY be able to
  handle [[Differential]] instances. This is accomplished today because the
  function returned by `lift-1` is installed into the generic arithmetic system
  as, say, `g/exp`, and `g/exp` is passed as the argument.

  As an example, if the [[primal-part]] of `x` is still a [[Differential]], `f`
  is going to receive a [[Differential]], and must already be lifted too."
  [f df:dx]
  (fn [x]
    (let [[px tx] (primal-tangent-pair x)]
      (if (and (v/number? tx) (v/zero? tx))
        (f px)
        (d:+ (f px) (d:* (df:dx px) tx))))))

(defn- lift-2
  "Given:

  - some binary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument
  - a function `df:dy`, similar to `df:dx` for the second arg

  Returns a new binary function that operates on both the original type of `f`
  and [[Differential]] instances.

  NOTE: You'll encounter subtle problems if you try to pass some function `f`
  that has not _already_ been lifted via `lift-2`. See the docs for [[lift-1]]
  for more detail."
  [f df:dx df:dy]
  (fn [x y]
    (let [tag     (max-order-tag x y)
          [xe dx] (primal-tangent-pair x tag)
          [ye dy] (primal-tangent-pair y tag)
          a (f xe ye)
          b (if (and (v/number? dx) (v/zero? dx))
              a
              (d:+ a (d:* dx (df:dx xe ye))))]
      (if (and (v/number? dy) (v/zero? dy))
        b
        (d:+ b (d:* (df:dy xe ye) dy))))))

(defn- lift-n
  "Given:

  - some function `f` that can handle 0 => any # of args
  - `df:dx`, a fn that returns the derivative wrt the single arg in the unary case
  - `df:dx1` and `df:dx2`, fns that return the derivative with respect to the
    first and second args in the binary case

  Returns a new binary function that operates on both the original type of `f`
  and [[Differential]] instances.

  NOTE: The n-ary case of `f` is populated by nested calls to the binary case.
  That means that this is NOT an appropriate lifting method for an n-ary
  function that isn't built out of associative binary calls.

  NOTE: You'll encounter subtle problems if you try to pass some function `f`
  that has not _already_ been lifted via `lift-2`. See the docs for [[lift-1]]
  for more detail."
  [f df:dx df:dx1 df:dx2]
  (let [f1 (lift-1 f df:dx)
        f2 (lift-2 f df:dx1 df:dx2)]
    (fn call
      ([] (f))
      ([x] (f1 x))
      ([x y] (f2 x y))
      ([x y & more]
       (reduce call (call x y) more)))))

;; ## Generic Method Installation

(defn- defunary [generic-op differential-op]
  (defmethod generic-op [::differential] [a] (differential-op a)))

(defn- defbinary [generic-op differential-op]
  (doseq [signature [[::differential ::differential]
                     [::v/scalar ::differential]
                     [::differential ::v/scalar]]]
    (defmethod generic-op signature [a b] (differential-op a b))))

(defbinary g/add
  (lift-2 g/add
          (fn [_ _] 1)
          (fn [_ _] 1)))

(defunary g/negate
  (lift-1 g/negate (fn [_] -1)))

(defunary g/negative?
  (fn [x] (g/negative? (finite-term x))))

(defbinary g/sub
  (lift-2 g/sub
          (fn [_ _] 1)
          (fn [_ _] -1)))

(let [mul (lift-2
           g/mul
           (fn [_ y] y)
           (fn [x _] x))]
  (defbinary g/mul mul)
  (defunary g/square (fn [x] (mul x x)))
  (defunary g/cube (fn [x] (mul x (mul x x))))
  (defbinary g/dot-product mul))

(defunary g/invert
  (lift-1 g/invert
          (fn [x] (g/div -1 (g/square x)))))

(defbinary g/div
  (lift-2 g/div
          (fn [_ y] (g/div 1 y))
          (fn [x y] (g/div (g/negate x)
                          (g/square y)))))

(defunary g/abs
  (fn [x]
    (let [f (primal-part x)
          func (cond (< f 0) (lift-1 (fn [x] x) (fn [_] -1))
                     (> f 0) (lift-1 (fn [x] x) (fn [_] 1))
                     (= f 0) (u/illegal "Derivative of g/abs undefined at zero")
                     :else (u/illegal (str "error! derivative of g/abs at" x)))]
      (func x))))

(defunary g/sqrt
  (lift-1 g/sqrt
          (fn [x]
            (g/invert
             (g/mul (g/sqrt x) 2)))))

(let [expt (lift-2
            g/expt
            (fn [x y]
              (g/mul y (g/expt x (g/sub y 1))))
            (fn [x y]
              (if (and (v/number? x) (v/zero? y))
                (if (v/number? y)
                  (if (not (g/negative? y))
                    0
                    (u/illegal "Derivative undefined: expt"))
                  0)
                (g/* (g/log x) (g/expt x y)))))]
  (defmethod g/expt [::differential ::differential] [d n] (expt d n))
  (defmethod g/expt [::v/scalar ::differential] [d n] (expt d n)))

;; TODO note the simpler case here.
(let [power (lift-2
             g/expt
             (fn [x y]
               (g/mul y (g/expt x (g/sub y 1))))
             (fn [_ _]
               (u/illegal "can't get there from here")))]
  (defmethod g/expt [::differential ::v/scalar] [d n] (power d n)))

(defunary g/log
  (lift-1 g/log g/invert))

(defunary g/exp
  (lift-1 g/exp g/exp))

(defunary g/sin
  (lift-1 g/sin g/cos))

(defunary g/cos
  (lift-1 g/cos
          (fn [x] (g/negate (g/sin x)))))

(defunary g/tan
  (lift-1 g/tan
          (fn [x]
            (g/invert
             (g/square (g/cos x))))))

(defunary g/asin
  (lift-1 g/asin
          (fn [x]
            (g/invert
             (g/sqrt (g/sub 1 (g/square x)))))))

(defunary g/acos
  (lift-1 g/acos
          (fn [x]
            (g/negate
             (g/invert
              (g/sqrt (g/sub 1 (g/square x))))))))

(defunary g/atan
  (lift-1 g/atan (fn [x]
                   (g/invert
                    (g/add 1 (g/square x))))))

(defbinary g/atan
  (lift-2 g/atan
          (fn [y x]
            (g/div x (g/add (g/square x)
                            (g/square y))))
          (fn [y x]
            (g/div (g/negate y)
                   (g/add (g/square x)
                          (g/square y))))))

(defunary g/sinh
  (lift-1 g/sinh g/cosh))

(defunary g/cosh
  (lift-1 g/cosh g/sinh))

(defunary g/tanh
  (lift-1 g/tanh
          (fn [x]
            (g/sub 1 (g/square (g/tanh x))))))

(defmethod g/partial-derivative [::differential v/seqtype] [a selectors]
  (let [tag (max-order-tag a)
        px  (primal-part a tag)
        tx  (extract-tangent a tag)]
    (d:+ (g/partial-derivative px selectors)
         (d:* (g/partial-derivative tx selectors)
              (bundle 0 1 tag)))))
