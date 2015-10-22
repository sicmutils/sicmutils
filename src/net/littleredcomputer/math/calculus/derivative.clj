;
; Copyright (C) 2015 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns net.littleredcomputer.math.calculus.derivative
  (:require [net.littleredcomputer.math
             [value :as v]
             [generic :as g]
             [operator :as o]
             [structure :as struct]]
            [clojure.set :as set])
  (:import (clojure.lang Sequential)))

;; A differential term is implemented as a map entry
;; whose key is a set of tags and whose second is the coefficient.
(def ^:private tags first)
(def ^:private coefficient second)

(declare differential-of)

;; A differential is a map from (sorted) differential
;; tag-sets to coefficients.
(defrecord Differential [terms]
  v/Value
  (nullity? [_] (every? g/zero? (map coefficient terms)))
  (unity? [_] false)                                        ;; XXX! this needs to be fixed
  (zero-like [_] 0)
  (exact? [_] false)
  (compound? [_] false)
  (numerical? [d] (g/numerical-quantity? (differential-of d)))
  (kind [_] ::differential))

(defn differential?
  [x]
  (cond (instance? Differential x) true
        (struct/structure? x) (some differential? x)
        :else false))

(defn differential-of
  "The differential of a quantity is, if we're a differential, the differential
  of the coefficient of the highest-order term part, or else the input itself."
  [dx]
  (loop [dx dx]
    (if (instance? Differential dx)
      (recur (coefficient (last (:terms dx))))
      dx)))

(defn- sorted-set-compare
  [xs ys]
  (let [cc (compare (count xs) (count ys))]
    (if (not= 0 cc) cc
                    (loop [xs xs ys ys]
                      (if (nil? xs) 0
                                    (let [c (compare (first xs) (first ys))]
                                      (if (zero? c)
                                        (recur (next xs) (next ys))
                                        c)))))))

(def ^:private empty-differential (sorted-map-by sorted-set-compare))
(def ^:private empty-tags (sorted-set))

(defn canonicalize-differential
  [{terms :terms}]
  (if (empty? terms)
    0
    (let [tags->coef (first terms)]
      (if (and (= (count terms) 1)
               (empty? (tags tags->coef)))
        (coefficient tags->coef)
        (Differential. (into empty-differential terms))))))


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
  (if (and (sorted? tags->coefs) (map? tags->coefs))
    (Differential. tags->coefs)
    (->> tags->coefs
         ; force tag sequences into sorted set form
         (map (fn [[tag-sequence coefficient]] [(into (sorted-set) tag-sequence) coefficient]))
         ; group by canonicalized tag-set
         (group-by tags)
         ; tag sets now map to [tag-set coefficient-list]. Sum the coefficients
         ; and produce the map of canonicalized tag-set to coefficient-sum
         (map (fn [[tag-set coefficients]] [tag-set (reduce g/+ 0 (map coefficient coefficients))]))
         ; drop tag-set:coefficient pairs where the coefficient is a zero object
         (remove (fn [[_ coefficient]] (g/zero? coefficient)))
         ; build the differential object
         (into empty-differential)
         Differential.)))

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
                                           (if-not (g/zero? r-coef)
                                             (assoc result a-tags r-coef)
                                             result)))
                (< (sorted-set-compare a-tags b-tags) 0) (recur (rest dxs) dys (conj result a))
                :else (recur dxs (rest dys) (conj result b)))))))

(defn- dx*dys
  "Multiplies a differential term by a sequence of differential terms (differential-set,
  coefficient) pairs. The result is a map of differential tag-sets to coefficients."
  [[x-tags x-coef] dys]
  (loop [dys dys
         result empty-differential]
    (if (nil? dys) result
                   (let [y1 (first dys) y-tags (tags y1)]
                     (recur (next dys)
                            (if (empty? (set/intersection x-tags y-tags))
                              (assoc result
                                (set/union x-tags y-tags)
                                (g/* x-coef (coefficient y1)))
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

(defonce ^:private next-differential-tag (atom 0))

(defn- make-differential-tag []
  (swap! next-differential-tag inc))

(defn- make-x+dx [x dx]
  ;; warning: this is not quite what GJS dopes ...
  (Differential. (into empty-differential {(sorted-set) x (sorted-set dx) 1})))

;(defn- hide-tag-in-procedure [& args] false) ; XXX

(defn- extract-dx-part [dx obj]
  (letfn [(extract
            ;; Collect all the terms of the differential in which
            ;; dx is a member of the term's tag set; drop that
            ;; tag from each set and return the differential formed
            ;; from what remains.
            [obj]
            (if (differential? obj)
              (->> obj :terms
                   (map (fn [[ts coef]] (if (ts dx) [(disj ts dx) coef])))
                   (filter some?)
                   make-differential
                   canonicalize-differential)
              0))
          (dist
            [obj]
            (cond (struct/structure? obj) (struct/mapr dist obj)
                  (o/operator? obj) (do (throw (IllegalArgumentException. "can't differentiate an operator yet"))
                                        (extract obj))      ;; TODO: tag-hiding
                  ;; (matrix? obj) (m:elementwise dist obj) XXX
                  ;; (quaternion? obj) XXX
                  ;; (series? obj) XXX
                  ;;
                  ;; ((operator? obj)
                  ;;  (hide-tag-in-procedure dx
                  ;;                         (g:* (make-operator dist 'extract (operator-subtype obj))
                  ;;                              obj)))
                  ;; note: we had ifn? here, but this is bad, since symbols and
                  ;; keywords implement IFn.
                  (fn? obj) #(-> % obj dist)                ;; TODO: innocent of the tag-hiding business
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
          {finite-part        false
           infinitesimal-part true} (group-by #(-> % tags (contains? keytag)) dts)]
      ;; if the input differential was well-formed, then it is safe to
      ;; construct differential objects on the split parts.
      [(-> finite-part make-differential)
       (-> infinitesimal-part make-differential)])
    [(into empty-differential [[] x]) empty-differential]))

(defn- unary-op
  [f df:dx]
  (fn [x]
    (let [[finite-part infinitesimal-part] (finite-and-infinitesimal-parts x)
          canonicalized-finite-part (canonicalize-differential finite-part)]
      (canonicalize-differential
        (dx+dy (f canonicalized-finite-part)
               (dx*dy (df:dx canonicalized-finite-part)
                      infinitesimal-part))))))

;; (defn max-order-tag [& ds]
;;   (last (apply set/union (map #(-> % differential->terms last :tags) ds))))

(defn max-order-tag
  "From each of the differentials in the sequence ds, find the highest
  order term; then return the greatest tag found in any of these
  terms; i.e., the highest-numbered tag of the highest-order term."
  [ds]
  (->> ds (mapcat #(-> % differential->terms last tags)) (reduce max)))

(defn with-tag
  "XXX doc and decide if we need the two infra"
  [tag dx]
  (->> dx :terms
       (filter #(-> % tags (contains? tag)))
       make-differential
       canonicalize-differential))

(defn without-tag
  "Document this and the above and below, if we turn out to keep all
  three of them. It seems there must be a better way to do this..."
  [tag dx]
  (->> dx
       differential->terms
       (remove #(-> % tags (contains? tag)))
       make-differential
       canonicalize-differential))

(defn with-and-without-tag
  "XXX doc and decide if we need above two"
  [tag dx]
  (let [{finite-terms false infinitesimal-terms true}
        (group-by #(-> % tags (contains? tag)) (differential->terms dx))]
    [(-> infinitesimal-terms make-differential canonicalize-differential)
     (-> finite-terms make-differential canonicalize-differential)]))

(defn- binary-op
  [f ∂f:∂x ∂f:∂y _kw]
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
      (canonicalize-differential c))))


(def ^:private diff-+ (binary-op g/+ (constantly 1) (constantly 1) :plus))
(def ^:private diff-- (binary-op g/- (constantly 1) (constantly -1) :minus))
(def ^:private diff-* (binary-op g/* (fn [_ y] y) (fn [x _] x) :times))
(def ^:private diff-div (binary-op g/divide
                                   (fn [_ y] (g/divide 1 y))
                                   (fn [x y] (g/negate (g/divide x (g/square y)))) :divide))
(def ^:private sine (unary-op g/sin g/cos))
(def ^:private arcsine (unary-op g/asin #(->> % g/square (g/- 1) g/sqrt (g/divide 1))))
(def ^:private cosine (unary-op g/cos #(-> % g/sin g/negate)))
(def ^:private arccosine (unary-op g/acos #(->> % g/square (g/- 1) g/sqrt (g/divide 1) (g/* -1))))
(def ^:private tangent (unary-op g/tan #(-> % g/cos g/square g/invert)))
(def ^:private sqrt (unary-op g/sqrt #(-> % g/sqrt (g/* 2) g/invert)))
(def ^:private exp (unary-op g/exp g/exp))
(def ^:private negate (unary-op g/negate (constantly -1)))
(def ^:private power
  (binary-op g/expt
             (fn [x y]
               (g/* y (g/expt x (g/- y 1))))
             (fn [_ _]
               (throw (IllegalArgumentException. "can't get there from here"))) :expt))

;; XXX unary-op is memoized in scmutils. But rather than memoizing that,
;; it might be better just to memoize entire simplications.

(defn- euclidean-structure
  [selectors f]
  (letfn [(structural-derivative [g v]
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
          (a-euclidean-derivative [v]
                                  (cond (struct/structure? v)
                                        (structural-derivative
                                          (fn [w]
                                            (f (if (empty? selectors) w (struct/structure-assoc-in v selectors w))))
                                          (get-in v selectors))
                                        (empty? selectors)
                                        ((derivative f) v)
                                        :else
                                        (throw (IllegalArgumentException. (str "Bad selectors " f selectors v)))))]
    a-euclidean-derivative))

(defn- multivariate-derivative
  [f selectors]
  (let [a (v/arity f)
        d (partial euclidean-structure selectors)]
    (cond (= a [:exactly 0]) (constantly 0)
          (= a [:exactly 1]) (with-meta (d f) {:arity a})
          (= a [:exactly 2]) (with-meta (fn [x y]
                                          ((d (fn [[x y]] (f x y)))
                                            (struct/seq-> [x y]))) {:arity a})
          (= a [:exactly 3]) (with-meta (fn [x y z]
                                          ((d (fn [[x y z]] (f x y z)))
                                            (struct/seq-> [x y z]))) {:arity a})
          (= a [:exactly 4]) (with-meta (fn [w x y z]
                                          ((d (fn [[w x y z]] (f w x y z)))
                                            (struct/seq-> [w x y z]))) {:arity a})
          :else (throw (IllegalArgumentException. (str "Haven't implemented this yet: arity " a))))))

;; we note that: (D f) where f is a literal function returns
;; 'a-euclidean-derivative', which when applied to 'x gives
;; ((D f) x). So, we have to define D.

(defn- define-binary-operation
  [generic-operation differential-operation]
  (doseq [signature [[::differential ::differential]
                     [:net.littleredcomputer.math.expression/numerical-expression ::differential]
                     [::differential :net.littleredcomputer.math.expression/numerical-expression]]]
    (defmethod generic-operation signature [a b] (differential-operation a b))))

(defn- define-unary-operation
  [generic-operation differential-operation]
  (defmethod generic-operation ::differential [a] (differential-operation a)))

(defmethod g/expt [::differential Number] [d n] (power d n))

(define-binary-operation g/add diff-+)
(define-binary-operation g/sub diff--)
(define-binary-operation g/mul diff-*)
(define-binary-operation g/div diff-div)
(define-unary-operation g/exp exp)
(define-unary-operation g/sqrt sqrt)
(define-unary-operation g/sin sine)
(define-unary-operation g/asin arcsine)
(define-unary-operation g/cos cosine)
(define-unary-operation g/acos arccosine)
(define-unary-operation g/tan tangent)
(define-unary-operation g/negate negate)
(define-unary-operation g/invert #(diff-div 1 %))
(define-unary-operation g/square #(diff-* % %))
(define-unary-operation g/cube #(diff-* % (diff-* % %)))
(derive ::differential :net.littleredcomputer.math.function/cofunction)
(defmethod g/partial-derivative
  [:net.littleredcomputer.math.function/function Sequential]
  [f selectors]
  (multivariate-derivative f selectors))
(defmethod g/partial-derivative
  [:net.littleredcomputer.math.structure/structure Sequential]
  [f selectors]
  (multivariate-derivative f selectors))

(def D
  (o/make-operator #(g/partial-derivative % []) :derivative))

(defn pd
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors) :partial-derivative))
