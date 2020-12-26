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

(ns sicmutils.differential
  (:require [clojure.string :refer [join]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.stream :as us]
            [sicmutils.value :as v]))

(defprotocol IPerturbed
  (perturbed? [this])
  (replace-tag [this old new])
  (extract-tangent [this tag]))

(extend-protocol IPerturbed
  #?(:clj Object :cljs default)
  (perturbed? [_] false)
  (replace-tag [this _ _] this)
  (extract-tangent [_ _] 0))

(derive ::differential ::v/scalar)

;; A differential term is implemented as a pair whose first element is a set of
;; tags and whose second is the coefficient. Let's do the tag set implementation
;; first.

(def ^:private empty-tags [])

(let [next-tag (atom 0)]
  (defn fresh-tag []
    (swap! next-tag inc)))

(defn tag-in?
  "Return true if `t` is in the tag-set `ts`, false otherwise."
  [ts t]
  (some #(= % t) ts))

(defn drop-tag
  "Return the tag set formed by dropping t from ts."
  [ts t]
  (filterv #(not= % t) ts))

(defn insert-tag
  "Inserts tag into its appropriate sorted space in `tags`."
  [tags tag]
  (loop [tags tags
         ret  []]
    (cond (empty? tags) (conj ret tag)
          (< tag (first tags)) (into (conj ret tag) tags)
          (= tag (first tags))
          (u/illegal (str "elem " tag "already present in " tags))
          :else (recur (rest tags)
                       (conj ret (first tags))))))

(defn- tag-union
  "Returns a vector that contains the union of the sorted vectors `x` and `y`."
  [x y]
  (let [xs (sort (into x y))]
    (into [] (dedupe) xs)))

(defn- tag-intersection
  "Returns a vector that contains the intersection of the two sorted vectors `x`
  and `y`."
  [x y]
  (loop [i (long 0)
         j (long 0)
         r (transient [])]
    (let [xi (nth x i nil)
          yj (nth y j nil)]
      (cond (not (and xi yj)) (persistent! r)
            (< xi yj) (recur (inc i) j r)
            (> xi yj) (recur i (inc j) r)
            :else     (recur (inc i) (inc j) (conj! r xi))))))

;; ## Term List Algebra
;;
;; Now, we get to 'terms'.
;;
;; From scmutils: "Differential term lists represent a kind of power series, so
;; they can be added and multiplied. It is important to note that when terms are
;; multiplied, no contribution is made if the terms being multiplied have a
;; differential tag in common. Thus dx^2 = zero."

(def tags first)
(def coefficient second)

(defn make-term
  ([coef] [empty-tags coef])
  ([tags coef] [tags coef]))

;; The data structure of a tag set. Tags are small integers. Tag sets are
;; typically of small cardinality. So we experiment with implementing them as
;; small vectors, instead of sorted sets.

(defn- tag-in-term?
  "Return true if `t` is in the tag-set of the supplied `term`, false otherwise."
  [term t]
  (tag-in? (tags term) t))

(defn terms:+
  "Iterate and build up the result while preserving order and dropping zero sums."
  [xs ys]
  (loop [xs xs
         ys ys
         result []]
    (cond (empty? xs) (into result ys)
          (empty? ys) (into result xs)
          :else (let [[a-tags a-coef :as a] (first xs)
                      [b-tags b-coef :as b] (first ys)
                      c (compare a-tags b-tags)]
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

(defn terms:* [xs ys]
  (for [[x-tags x-coef] xs
        [y-tags y-coef] ys
        :when (empty? (tag-intersection x-tags y-tags))]
    (make-term (tag-union x-tags y-tags)
               (g/* x-coef y-coef))))

(defn- collect-terms [tags->coefs]
  (into []
        (sort-by first
                 (for [[tags tags-coefs] (group-by tags tags->coefs)
                       :let [c (transduce (map coefficient) g/+ tags-coefs)]
                       :when (not (v/zero? c))]
                   [tags c]))))

;; ## Differential

(declare d:= d:one? d:zero? bare-coefficient)

;; A differential is a sequence of differential terms, ordered by the
;; tag set.
;;
;; TODO implement ifn, compare, equals for clj, cljs

(deftype Differential [terms]
  f/IArity
  (arity [_]
    (f/arity (coefficient (first terms))))

  v/Numerical
  (numerical? [d] (v/numerical? (bare-coefficient d)))

  v/Value
  (zero? [this] (d:zero? this))
  (one? [this] (d:one? this))
  (identity? [this] (d:one? this))
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [_] `[~'Differential ~@terms])
  (exact? [_] false)

  (kind [_] ::differential)

  Object
  (toString [_] (str "D[" (join " " (map #(join " → " %) terms)) "]"))

  #?(:clj
     (equals [a b] (d:= a b)))

  #?@(:cljs
      [IEquiv
       (-equiv [a b] (d:= a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

#?(:clj
   (defmethod print-method Differential
     [^Differential s ^java.io.Writer w]
     (.write w (.toString s))))

(defn differential?
  "Returns true if the supplied object is an instance of `Differential`, false
  otherwise."
  [dx]
  (instance? Differential dx))

(defn terms
  "For the supplied `Differential` object, returns its vector of terms."
  [dx]
  {:pre [(differential? dx)]}
  (.-terms ^Differential dx))

(defn- differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list (unless d is zero, in
  which case we return the empty term list)."
  [dx]
  (cond (differential? dx)
        (filterv (fn [term]
                   (not (v/zero?
                         (coefficient term))))
                 (terms dx))

        (v/zero? dx) []
        :else        [(make-term empty-tags dx)]))

(defn terms->differential
  "Returns a differential instance generated from a vector of terms."
  [terms]
  {:pre [(vector? terms)]}
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (empty? (tags (first terms))))
        (coefficient (first terms))

        :else (->Differential terms)))

(defn bare-coefficient
  "Returns:
  - if we're a differential, the differential of the coefficient of the first
  term
  - or else the input itself.

  TODO note what is happening here."
  [dx]
  (if (differential? dx)
    (recur (coefficient
            (first (terms dx))))
    dx))

(defn diff:apply
  "If the coefficients are themselves functions, apply them to the args for ALL
  coefficients."
  [diff args]
  (terms->differential
   (map (fn [term]
          (make-term (tags term)
                     (apply (coefficient term) args)))
        (differential->terms diff))))

(defn sum->differential
  "The input here is a mapping (loosely defined) between sets of
  differential tags and coefficients.

  The mapping can be an actual map, or just a sequence of pairs. The
  differential tag sets are sequences of integer tags, which should be sorted.

  TODO note that this groups and sums; maybe less efficient than doing it the
  power series way. Check!"
  [tags->coefs]
  (terms->differential
   (collect-terms tags->coefs)))

;; TODO sort this? Make things private again?

(extend-protocol IPerturbed
  Differential
  (perturbed? [_] true)
  (replace-tag [dx oldtag newtag]
    (terms->differential
     (mapv (fn [term]
             (let [tagv (tags term)]
               (if (tag-in? tagv oldtag)
                 (make-term (-> (tags term)
                                (drop-tag oldtag)
                                (insert-tag newtag))
                            (coefficient term))
                 term)))
           (terms dx))))
  (extract-tangent [dx tag]
    (sum->differential
     (mapcat (fn [term]
               (let [tagv (tags term)]
                 (if (tag-in? tagv tag)
                   [(make-term (drop-tag tagv tag)
                               (coefficient term))]
                   [])))
             (terms dx)))))

(defn d:+
  "Adds two objects differentially. (One of the objects might not be
  differential; in which case we lift it into a trivial differential
  before the addition.)"
  [dx dy]
  (terms->differential
   (terms:+ (differential->terms dx)
            (differential->terms dy))))

(defn d:*
  "Form the product of the differentials dx and dy."
  [dx dy]
  (sum->differential
   (terms:* (differential->terms dx)
            (differential->terms dy))))

(defn bundle
  ([primal tag]
   (bundle primal 1 tag))
  ([primal tangent tag]
   (d:+ primal (->Differential [[[tag] tangent]]))))

(defn max-order-tag
  "From each of the differentials in the sequence ds, find the highest
  order term; then return the greatest tag found in any of these
  terms; i.e., the highest-numbered tag of the highest-order term."
  ([dx]
   (when (differential? dx)
     (let [last-term   (peek (differential->terms dx))
           highest-tag (peek (tags last-term))]
       highest-tag)))
  ([d & ds]
   (letfn [(max-termv [d]
             (if-let [max-order (max-order-tag d)]
               [max-order]
               []))]
     (->> (mapcat max-termv (cons d ds))
          (apply max)))))

(defn primal-part
  "The differential containing only those terms _without_ the given tag"
  ([dx] (primal-part dx (max-order-tag dx)))
  ([dx tag]
   (if (differential? dx)
     (let [sans-tag? (complement #(tag-in-term? % tag))]
       (->> (differential->terms dx)
            (filterv sans-tag?)
            (sum->differential)))
     dx)))

(defn tangent-part
  "The differential containing only those terms _with_ the given tag"
  ([dx] (tangent-part dx (max-order-tag dx)))
  ([dx tag]
   (if (differential? dx)
     (->> (differential->terms dx)
          (filterv #(tag-in-term? % tag))
          (sum->differential))
     0)))

(defn primal-tangent-pair
  "Split the differential into the parts with and without tag and return the
  pair."
  ([dx] (primal-tangent-pair dx (max-order-tag dx)))
  ([dx tag]
   (if-not (differential? dx)
     [dx 0]
     (let [[tangent-terms primal-terms]
           (us/separatev #(tag-in-term? % tag)
                         (differential->terms dx))]
       [(sum->differential primal-terms)
        (sum->differential tangent-terms)]))))

(defn d:zero? [dx]
  (every? (comp v/zero? coefficient)
          (terms dx)))

(defn d:one? [dx]
  (let [[p t] (primal-tangent-pair dx)]
    (and (v/one? p)
         (v/zero? t))))

(defn d:=
  "Returns true if the [[Differential]] instance `a` equals `b`, false otherwise."
  [a b]
  {:pre [(differential? a)]}
  (if (differential? b)
    (= (terms a)
       (terms b))
    (= (primal-part a) b)))

(defn diff:compare
  "Comparator that can compare differentials with non-differentials."
  [a b]
  (compare (primal-part a)
           (primal-part b)))

(defn- unary-op [f df:dx]
  (fn [x]
    (let [[p t] (primal-tangent-pair x)]
      (d:+ (f p) (d:* (df:dx p) t)))))

(defn- binary-op [f df:dx df:dy]
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

(def ^:private diff-+ (binary-op g/+ (fn [_ _] 1) (fn [_ _] 1)))
(def ^:private diff-- (binary-op g/- (fn [_ _] 1) (fn [_ _] -1)))
(def ^:private diff-* (binary-op g/* (fn [_ y] y) (fn [x _] x)))
(def ^:private diff-div
  (binary-op g/div
             (fn [_ y] (g/invert y))
             (fn [x y] (g/negate (g/divide x (g/square y))))))

(def ^:private sin (unary-op g/sin g/cos))
(def ^:private cos (unary-op g/cos #(g/negate (g/sin %))))
(def ^:private tan (unary-op g/tan #(g/invert (g/square (g/cos %)))))

(def ^:private asin
  (unary-op g/asin #(g/invert (g/sqrt (g/sub 1 (g/square %))))))

(def ^:private acos
  (unary-op g/acos #(g/negate (g/invert (g/sqrt (g/sub 1 (g/square %)))))))

(def ^:private atan
  (unary-op g/atan #(g/invert (g/add 1 (g/square %)))))

(def ^:private atan2
  (binary-op g/atan
             (fn [y x]
               (g/divide x
                         (g/add (g/square x)
                                (g/square y))))
             (fn [y x]
               (g/divide (g/negate y)
                         (g/add (g/square x)
                                (g/square y))))))

(defn- abs [x]
  (let [f (primal-part x)
        func (cond (< f 0) (unary-op (fn [x] x) (fn [_] -1))
                   (> f 0) (unary-op (fn [x] x) (fn [_] 1))
                   (= f 0) (u/illegal "Derivative of g/abs undefined at zero")
                   :else (u/illegal (str "error! derivative of g/abs at" x)))]
    (func x)))

(def ^:private sinh (unary-op g/sinh g/cosh))
(def ^:private cosh (unary-op g/cosh g/sinh))
(def ^:private tanh (unary-op g/tanh #(g/sub 1 (g/square (g/tanh %)))))

(def ^:private sqrt (unary-op g/sqrt #(-> % g/sqrt (g/mul 2) g/invert)))
(def ^:private exp (unary-op g/exp g/exp))
(def ^:private negate (unary-op g/negate (fn [_] -1)))

(def ^:private power
  (binary-op g/expt
             (fn [x y]
               (g/mul y (g/expt x (g/sub y 1))))
             (fn [_ _]
               (u/illegal "can't get there from here"))))

(def ^:private expt
  (binary-op g/expt
             (fn [x y]
               (g/mul y (g/expt x (g/sub y 1))))
             (fn [x y]
               (if (and (v/number? x) (v/zero? y))
                 (if (v/number? y)
                   (if (not (g/negative? y))
                     0
                     (u/illegal "Derivative undefined: expt"))
                   0)
                 (g/* (g/log x) (g/expt x y))))))
(def ^:private log (unary-op g/log g/invert))

(defn- defbinary [generic-op differential-op]
  (doseq [signature [[::differential ::differential]
                     [::v/scalar ::differential]
                     [::differential ::v/scalar]]]
    (defmethod generic-op signature [a b] (differential-op a b))))

(defn- defunary [generic-op differential-op]
  (defmethod generic-op [::differential] [a] (differential-op a)))

(defmethod g/expt [::differential ::v/number] [d n] (power d n))
(defbinary g/expt expt)

(defbinary g/add diff-+)
(defbinary g/sub diff--)
(defbinary g/mul diff-*)
(defbinary g/div diff-div)

(defunary g/log log)
(defunary g/exp exp)
(defunary g/abs abs)
(defunary g/sqrt sqrt)

(defunary g/sin sin)
(defunary g/cos cos)
(defunary g/tan tan)

(defunary g/asin asin)
(defunary g/acos acos)
(defunary g/atan atan)
(defbinary g/atan atan2)

(defunary g/sinh sinh)
(defunary g/cosh cosh)
(defunary g/tanh tanh)

(defunary g/negate negate)
(defunary g/invert #(diff-div 1 %))
(defunary g/square #(diff-* % %))
(defunary g/cube #(diff-* % (diff-* % %)))

(defbinary g/dot-product diff-*)
