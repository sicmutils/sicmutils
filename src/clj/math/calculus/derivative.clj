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

(ns math.calculus.derivative
  (:require [math.value :as v]
            [math.generic :as g]
            [math.operator :as o]
            [math.structure :as struct]
            [clojure.set :as set]
            ))

;; A differential term is implemented as a map entry
;; whose key is a set of tags and whose second is the coefficient.
(def ^:private tags first)
(def ^:private coefficient second)

(declare differential-of)

;; A differential is a map from (sorted) differential
;; tag-sets to coefficients.
(defrecord Differential [terms]
  v/Value
  (nullity? [d]
    (every? g/zero? (map coefficient d)))
  (unity? [_]
    false) ;; XXX! this needs to be fixed
  (zero-like [_] 0)
  (exact? [_] false)
  (compound? [_] false)
  (numerical? [d]
    (g/numerical-quantity? (differential-of d)))
  (sort-key [_] 80)
  )

(def differential? (partial instance? Differential))

(defn differential-of
  "The differential of a quantity is, if we're a differential, the differential
  of the coefficient of the highest-order term part, or else the input itself."
  [dx]
  (loop [dx dx]
    (if (instance? Differential dx)
      (recur (coefficient (last (:terms dx))))
      dx)))

;; Kind of sad to call vec here. Why aren't sorted-sets comparable?
;; Monitor this as Clojure evolves. Or submit a patch yourself!
(def ^:private empty-differential (sorted-map-by #(compare (vec %1) (vec %2))))
(def ^:private empty-tags (sorted-set))

(defn- canonicalize-differential
  [tags->coefs]
  (if (empty? tags->coefs)
    0
    (let [tags->coef (first tags->coefs)]
      (if (and (= (count tags->coefs) 1)
               (empty? (tags tags->coef)))
        (coefficient tags->coef)
        (Differential. (into empty-differential tags->coefs))))))

(defn make-differential
  "The input here is a mapping (loosely defined) between sets of differential
  tags and coefficients. The mapping can be an actual map, or just a sequence
  of pairs. The differential tag sets can be actual sets of tags, or in fact
  any kind of sequence of tags. This form of differential creation takes care
  of everything, and canonicalizes the results. This process involves converting
  the tag sets into sorted-sets, grouping the terms by tag-set value, summing
  the corresponding entries, dropping zero-coefficient entries, and seeing
  whether the resulting map is equivalent to a scalar; after all that either
  a scalar or a Differential object is returned."
  [tags->coefs]
  (->> tags->coefs
       ; force tag sequences into sorted set form
       (map (fn [[tag-sequence coefficient]] [(apply sorted-set tag-sequence) coefficient]))
       ; group by canonicalized tag-set
       (group-by tags)
       ; tag sets now map to [tag-set coefficient-list]. Sum the coefficients
       ; and produce the map of canonicalized tag-set to coefficient-sum
       (map (fn [[tag-set coefficients]] [tag-set (apply g/+ (map coefficient coefficients))]))
       ; drop tag-set:coefficient pairs where the coefficient is a zero object
       (filter (fn [[_ coefficient]] (not (g/zero? coefficient))))
       ; potentially demote the resulting diffferential object to a constant
       canonicalize-differential))

(defn- differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list (unless d is zero, in
  which case we return the empty term list)."
  [dx]
  (cond (instance? Differential dx) (:terms dx)
        (g/zero? dx) empty-differential
        :else (conj empty-differential [empty-tags dx])))

(defn- dxs+dys
  "Inputs are sequences of differential terms; returns the sequence of differential
  terms representing the sum."
  [dxs dys]
  (loop [dxs dxs
         dys dys
         result empty-differential]
    (cond
      (empty? dxs) (into result dys)
      (empty? dys) (into result dxs)
      :else (let [[a-tags a-coef :as a] (first dxs)
                  [b-tags b-coef :as b] (first dys)]
              (cond
                (= a-tags b-tags) (let [r-coef (g/+ a-coef b-coef)]
                                    (recur (rest dxs) (rest dys)
                                           (if (not (g/zero? r-coef))
                                             (conj result [a-tags r-coef])
                                             result)))
                ;; kind of sad to call vec here.
                (< (compare (vec a-tags) (vec b-tags)) 0) (recur (rest dxs) dys (conj result a))
                :else (recur dxs (rest dys) (conj result b)))))))

(defn- dx*dys
  "Multiplies a Differential by a sequence of differential terms (differential-set,
  coefficient) pairs. The result is a map of differential tag-sets to coefficients."
  [[x-tags x-coef] dys]
  (loop [dys dys
         result empty-differential]
    (if (nil? dys) result
                   (let [y1 (first dys) y-tags (tags y1)]
                     (recur (next dys)
                            (if (empty? (set/intersection x-tags y-tags))
                              (conj result [(set/union x-tags y-tags)
                                            (g/* x-coef (second y1))])
                              result))))))

(defn- dxs*dys
  [as bs]
  (if (or (empty? as)
          (empty? bs)) {}
                       (dxs+dys
                         (dx*dys (first as) bs)
                         (dxs*dys (next as) bs))))

(defn dx+dy
  "Adds two objects differentially. (One of the objects might not be
  differential; in which case we lift it into a trivial differential
  before the addition.)"
  [a b]
  (make-differential
    (dxs+dys (differential->terms a) (differential->terms b))))

(defn dx*dy [a b]
  (make-differential
    (dxs*dys (differential->terms a) (differential->terms b))))

(def ^:private next-differential-tag (atom 0))

(defn- make-differential-tag []
  (swap! next-differential-tag inc))

(defn- make-x+dx [x dx]
  ;; warning: this is not quite what GJS dopes ...
  (Differential. {(sorted-set) x
                  (sorted-set dx) 1}))

;(defn- hide-tag-in-procedure [& args] false) ; XXX

(defn- extract-dx-part [dx obj]
  (letfn [(extract
            ;Collect all the terms of the differential in which
            ;dx is a member of the term's tag set; drop that
            ;tag from each set and return the differential formed
            ;from what remains.
            [obj]
            (if (differential? obj)

              (make-differential
                (filter identity (map
                                   (fn [[ts coef]]
                                     (if (ts dx)
                                       [(disj ts dx) coef]))
                                   (:terms obj))))
              0))
          (dist
            [obj]
            (cond (struct/structure? obj) (struct/mapr dist obj)
                  (o/operator? obj) (extract obj)           ;; TODO: tag-hiding
                  ;; (matrix? obj) (m:elementwise dist obj) XXX
                  ;; (quaternion? obj) XXX
                  ;; (series? obj) XXX
                  ;;
                  ;; ((operator? obj)
                  ;;  (hide-tag-in-procedure dx
                  ;;                         (g:* (make-operator dist 'extract (operator-subtype obj))
                  ;;                              obj)))
                  (ifn? obj) (comp dist obj)             ;; TODO: innocent of the tag-hiding business
                  :else (extract obj)))]
    (dist obj)))

(defn derivative
  [f]
  (fn [x]
    (let [dx (make-differential-tag)]
      (extract-dx-part dx (f (make-x+dx x dx))))))

(defn- finite-and-infinitesimal-parts
  "Partition the terms of the given differential into the finite and
  infinite parts."
  [x]
  (if (differential? x)
    (let [dts (differential->terms x)
          keytag (-> dts last first last)
          {finite-part false
           infinitesimal-part true} (group-by #(-> % first (contains? keytag)) dts)]
      ;; if the input differential was well-formed, then it is safe to
      ;; construct differential objects on the split parts.
      [(make-differential finite-part)
       (make-differential infinitesimal-part)])
    [x 0]))

(defn- unary-op
  [f df:dx]
  (fn [x]
    (let [[finite-part infinitesimal-part] (finite-and-infinitesimal-parts x)]
      (dx+dy (f finite-part)
             (dx*dy (df:dx finite-part)
                    infinitesimal-part)))))

;; (defn max-order-tag [& ds]
;;   (last (apply set/union (map #(-> % differential->terms last :tags) ds))))

(defn max-order-tag
  "From each of the differentials in the sequence ds, find the highest
  order term; then return the greatest tag found in any of these
  terms; i.e., the highest-numbered tag of the highest-order term."
  [ds]
  ;; might consider a mapcat through reduce max here instead of
  ;; building a new set just to find the max
  (->> ds (map #(-> % differential->terms last first)) (apply set/union) last))

(defn with-tag
  "XXX doc and decide if we need the two infra"
  [tag dx]
  (->> dx :terms (filter #(-> % first (contains? tag))) make-differential))

(defn without-tag
  "A real differential is expected here. document this and the above and below,
  if we turn out to keep all three of them. It seems there must be a better way
  to do this..."
  [tag dx]
  (->> dx :terms (filter #(-> % first (contains? tag) not)) make-differential))

(defn with-and-without-tag
  "XXX doc and decide if we need above two"
  [tag dx]
  (let [{finite-terms false infinitesimal-terms true}
        (group-by #(-> % tags (contains? tag)) (differential->terms dx))]
    [(make-differential infinitesimal-terms)
     (make-differential finite-terms)]))

(defn- binary-op
  [f ∂f:∂x ∂f:∂y]
  (fn [x y]
    (let [mt (max-order-tag [x y])
          [dx xe] (with-and-without-tag mt x)
          [dy ye] (with-and-without-tag mt y)
          a (f xe ye)
          b (if (and (number? dx) (zero? dx))
              a
              (dx+dy a (dx*dy dx (∂f:∂x xe ye))))
          c (if (and (number? dy) (zero? dy))
              b
              (dx+dy b (dx*dy (∂f:∂y xe ye) dy)))]
      c)))


(def ^:private diff-+   (binary-op g/+ (constantly 1) (constantly 1)))
(def ^:private diff--   (binary-op g/- (constantly 1) (constantly -1)))
(def ^:private diff-*   (binary-op g/* (fn [_ y] y)   (fn [x _] x)))
(def ^:private diff-div (binary-op g/divide
                                   (fn [_ y] (g/divide 1 y))
                                   (fn [x y] (g/negate (g/divide x (g/square y))))))
(def ^:private sine     (unary-op g/sin g/cos))
(def ^:private cosine   (unary-op g/cos #(-> % g/sin g/negate)))
(def ^:private tangent  (unary-op g/tan #(-> % g/cos g/square g/invert)))
(def ^:private sqrt     (unary-op g/sqrt #(-> % g/sqrt (g/* 2) g/invert)))
(def ^:private negate   (unary-op #(g/* -1 %) (constantly -1)))
(def ^:private power
  (binary-op g/expt
             (fn [x y]
               (g/* y (g/expt x (g/- y 1))))
             (fn [_ _]
               (throw (IllegalArgumentException. "can't get there from here")))))

;; XXX unary-op is memoized in scmutils. But rather than memoizing that,
;; it might be better just to memoize entire simplications.

(defn- euclidean-structure
  [selectors f]
  (letfn [(structural-derivative
            [g v]
            (cond (struct/structure? v)
                  (struct/opposite v
                                   (map-indexed
                                     (fn [i v_i]
                                       (structural-derivative
                                         (fn [w]
                                           (g (struct/structure-assoc-in v [i] w)))
                                         v_i))
                                     v))
                  (or (g/numerical-quantity? v) (g/abstract-quantity? v))
                  ((derivative g) v)
                  :else
                  (throw (IllegalArgumentException. (str "bad structure " g v)))))
          (a-euclidean-derivative
            [v]
            (cond (struct/structure? v)
                  (structural-derivative
                    (fn [w]
                      (f (if (empty? selectors) w (struct/structure-assoc-in v selectors w))))
                    (struct/structure-get-in v selectors))
                  (empty? selectors)
                  ((derivative f) v)
                  :else
                  (throw (IllegalArgumentException. (str "Bad selectors " f selectors v)))))]
    a-euclidean-derivative))

(defn- multivariate-derivative
  [f selectors]
  (let [a (v/arity f)
        d (partial euclidean-structure selectors)]
    (cond (= a 0) (constantly 0)
          (= a 1) (d f)
          (= a 2) (fn [x y]
                    ((d (fn [s] (apply f (seq s))))
                      (struct/seq-> [x y])))
          (= a 3) (fn [x y z]
                    ((d (fn [s] (apply f (seq s))))
                      (struct/seq-> [x y z])))
          (= a 4) (fn [w x y z]
                    ((d (fn [s] (apply f (seq s))))
                      (struct/seq-> [w x y z])))

          :else (throw (IllegalArgumentException. (str "Haven't implemented this yet: arity " a))))))

(defn- not-compound?
  [x]
  (if (satisfies? v/Value x)
    (not (v/compound? x))
    true))

;; we note that: (D f) where f is a literal function returns
;; 'a-euclidean-derivative', which when applied to 'x gives
;; ((D f) x). So, we have to define D.

(g/defhandler :+      [differential? not-compound?] diff-+)
(g/defhandler :+      [not-compound? differential?] diff-+)
(g/defhandler :-      [differential? not-compound?] diff--)
(g/defhandler :-      [not-compound? differential?] diff--)
(g/defhandler :*      [differential? not-compound?] diff-*)
(g/defhandler :*      [not-compound? differential?] diff-*)
(g/defhandler :div    [differential? not-compound?] diff-div)
(g/defhandler :div    [not-compound? differential?] diff-div)
(g/defhandler :invert [differential?] #(diff-div 1 %))
(g/defhandler :square [differential?] #(g/* % %))
(g/defhandler :negate [differential?] negate)
(g/defhandler :sin    [differential?] sine)
(g/defhandler :cos    [differential?] cosine)
(g/defhandler :tan    [differential?] tangent)
(g/defhandler :sqrt   [differential?] sqrt)
(g/defhandler :**     [differential? (complement differential?)] power)
(g/defhandler :∂      [#(or (ifn? %) (struct/structure? %))
                       (constantly true)] multivariate-derivative)

(def D
  (o/make-operator #(g/partial-derivative % []) :derivative))

(defn pd
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors) :partial-derivative))

(println "derivative initialized")
