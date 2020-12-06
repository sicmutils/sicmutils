;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(ns sicmutils.calculus.derivative
  (:refer-clojure :rename {partial core-partial}
                  #?@(:cljs [:exclude [partial]]))
  (:require [clojure.string :refer [join]]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :as o]
            [sicmutils.series :as series]
            [sicmutils.structure :as struct]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; A differential term is implemented as a pair whose first element is
;; a set of tags and whose second is the coefficient.
(def ^:private tags first)
(def ^:private coefficient second)

(declare differential-of)

(comment
  ;; TODO fix this for differential one and identity?
  (define (diff:one? x)
    (assert (differential? x))
    (and (g:one? (finite-part x))
         (diff:zero? (infinitesimal-part x))))
  )
;; A differential is a sequence of differential terms, ordered by the
;; tag set.
(deftype Differential [terms]
  v/Value
  (zero? [_] (every? v/zero? (map coefficient terms)))
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [_] `[~'Differential ~@terms])
  (exact? [_] false)
  (numerical? [d] (v/numerical? (differential-of d)))
  (kind [_] ::differential)

  Object
  (toString [_] (str "D[" (join " " (map #(join " → " %) terms)) "]"))

  #?(:clj
     (equals [_ b]
             (and (instance? Differential b)
                  (= terms (.-terms b)))))

  #?@(:cljs
      [IEquiv
       (-equiv [_ b]
               (and (instance? Differential b)
                    (= terms (.-terms b))))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

#?(:clj
   (defmethod print-method Differential [^Differential s ^java.io.Writer w]
     (.write w (.toString s))))

(defn differential?
  [x]
  (cond (instance? Differential x) true
        (struct/structure? x) (some differential? x)
        (matrix/matrix? x) (matrix/matrix-some differential? x)
        :else false))

(defn differential-of
  "The differential of a quantity is, if we're a differential, the differential
  of the coefficient of the highest-order term part, or else the input itself."
  [dx]
  (loop [dx dx]
    (if (instance? Differential dx)
      (recur (coefficient (last (.-terms dx))))
      dx)))

(def ^:private empty-differential [])
(def ^:private empty-tags [])

(defn canonicalize-differential [^Differential dx]
  (let [ts (.-terms dx)]
    (cond (empty? ts) 0
          (and (= (count ts) 1)
               (empty? (tags (first ts)))) (coefficient (first ts))
          :else dx)))

(defn make-differential
  "The input here is a mapping (loosely defined) between sets of
  differential tags and coefficients. The mapping can be an actual
  map, or just a sequence of pairs. The differential tag sets are
  sequences of integer tags, which should be sorted."
  [tags->coefs]
  (->Differential
   (into empty-differential
         (sort-by first
                  (for [[tags tags-coefs] (group-by tags tags->coefs)
                        :let [c (reduce #(g/+ %1 (coefficient %2)) 0 tags-coefs)]
                        :when (not (v/zero? c))]
                    [tags c])))))

(defn ^:private differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list (unless d is zero, in
  which case we return the empty term list)."
  [dx]
  (cond (instance? Differential dx) (let [^Differential d dx] (.-terms d))
        (v/zero? dx) []
        :else [[empty-tags dx]]))

;; The data structure of a tag set. Tags are small integers. Tag sets are
;; typically of small cardinality. So we experiment with implementing them
;; as small vectors, instead of sorted sets.

(defn ^:private tag-union
  "The union of the sorted vectors ts and us."
  [ts us]
  (-> ts (concat us) sort dedupe vec))

(defn ^:private tag-intersection
  "Intersection of sorted vectors ts and us."
  [ts us]
  (loop [ts ts
         us us
         result []]
    (cond (empty? ts) result
          (empty? us) result
          :else (let [t (first ts)
                      u (first us)
                      c (compare t u)]
                  (cond (= c 0) (recur (rest ts) (rest us) (conj result t))
                        (< c 0) (recur (rest ts) us result)
                        :else (recur ts (rest us) result))))))

(defn ^:private tag-in?
  "Return true if t is in the tag-set ts"
  [ts t]
  (some #(= % t) ts))

(defn ^:private tag-without
  "Return the tag set formed by dropping t from ts"
  [ts t]
  (filterv #(not= % t) ts))

(defn dx+dy
  "Adds two objects differentially. (One of the objects might not be
  differential; in which case we lift it into a trivial differential
  before the addition.)"
  [dx dy]
  (->Differential
   ;; Iterate and build up the result while preserving order and dropping zero sums.
   (loop [dxs (differential->terms dx)
          dys (differential->terms dy)
          result []]
     (cond
       (empty? dxs) (into result dys)
       (empty? dys) (into result dxs)
       :else (let [[a-tags a-coef :as a] (first dxs)
                   [b-tags b-coef :as b] (first dys)
                   c (compare a-tags b-tags)]
               (cond (= c 0) (let [r-coef (g/+ a-coef b-coef)]
                               (recur (rest dxs) (rest dys)
                                      (if-not (v/zero? r-coef)
                                        (conj result [a-tags r-coef])
                                        result)))
                     (< c 0) (recur (rest dxs) dys (conj result a))
                     :else (recur dxs (rest dys) (conj result b))))))))

(defn dx*dy
  "Form the product of the differentials dx and dy."
  [dx dy]
  (make-differential
   (for [[x-tags x-coef] (differential->terms dx)
         [y-tags y-coef] (differential->terms dy)
         :when (empty? (tag-intersection x-tags y-tags))]
     [(tag-union x-tags y-tags) (g/* x-coef y-coef)])))

(def ^:private make-differential-tag
  (let [next-differential-tag (atom 0)]
    #(swap! next-differential-tag inc)))

(defn ^:private make-x+dx [x dx]
  (dx+dy x (->Differential [[[dx] 1]])))

;(defn ^:private hide-tag-in-procedure [& args] false) ; XXX

(defn ^:private extract-dx-part [tag obj]
  (letfn [(extract
            ;; Collect all the terms of the differential in which
            ;; dx is a member of the term's tag set; drop that
            ;; tag from each set and return the differential formed
            ;; from what remains.
            [obj]
            (if (differential? obj)
              (let [^Differential d obj]
                (canonicalize-differential
                 (make-differential
                  (for [[tags coef] (.-terms d)
                        :when (tag-in? tags tag)]
                    [(tag-without tags tag) coef]))))
              0))
          (dist
            [obj]
            (cond (struct/structure? obj) (struct/mapr dist obj)
                  (matrix/matrix? obj) (matrix/fmap dist obj)
                  (series/series? obj) (series/fmap dist obj)
                  (o/operator? obj) (do (u/illegal "can't differentiate an operator yet")
                                        (extract obj))      ;; TODO: tag-hiding
                  ;; (quaternion? obj) XXX
                  ;; ((operator? obj)
                  ;;  (hide-tag-in-procedure dx
                  ;;                         (g:* (make-operator dist 'extract (operator-subtype obj))
                  ;;                              obj)))
                  ;; note: we had ifn? here, but this is bad, since symbols and
                  ;; keywords implement IFn.
                  (fn? obj) #(dist (obj %))                ;; TODO: innocent of the tag-hiding business
                  :else (extract obj)))]
    (dist obj)))

(defn derivative [f]
  (fn [x]
    (let [dx (make-differential-tag)]
      (extract-dx-part dx (f (make-x+dx x dx))))))

(defn ^:private with-finite-and-infinitesimal-parts
  "Partition the terms of the given differential into the finite and
  infinite parts. The continuation is called with these two parts."
  [x continue]
  {:pre [(differential? x)]}
  (let [dts (differential->terms x)
        keytag (-> dts last first last)
        {finite-part nil
         infinitesimal-part true} (group-by #(tag-in? (tags %) keytag) dts)]
    ;; since input differential is well-formed, it is safe to
    ;; construct differential objects on the split parts.
    (continue (->Differential finite-part) (->Differential infinitesimal-part))))

(defn ^:private unary-op
  [f df:dx]
  (fn [x]
    (with-finite-and-infinitesimal-parts x
      (fn [finite-part infinitesimal-part]
        (let [canonicalized-finite-part (canonicalize-differential finite-part)]
          (canonicalize-differential
           (dx+dy (f canonicalized-finite-part)
                  (dx*dy (df:dx canonicalized-finite-part)
                         infinitesimal-part))))))))

(defn max-order-tag
  "From each of the differentials in the sequence ds, find the highest
  order term; then return the greatest tag found in any of these
  terms; i.e., the highest-numbered tag of the highest-order term."
  [ds]
  (->> ds (mapcat #(-> % differential->terms last tags)) (apply max)))

(defn with-tag
  "The differential containing only those terms with the given tag"
  [tag dx]
  (->> dx
       differential->terms
       (filter #(-> % tags (tag-in? tag)))
       make-differential
       canonicalize-differential))

(defn without-tag
  "The differential containing only those terms without the given tag"
  [tag dx]
  (->> dx
       differential->terms
       (remove #(tag-in? (tags %) tag))
       make-differential
       canonicalize-differential))

(defn with-and-without-tag
  "Split the differential into the parts with and without tag and return the pair"
  [tag dx]
  (let [{finite-terms nil infinitesimal-terms true}
        (group-by #(tag-in? (tags %) tag) (differential->terms dx))]
    [(-> infinitesimal-terms make-differential canonicalize-differential)
     (-> finite-terms make-differential canonicalize-differential)]))

(defn ^:private binary-op
  [f df:dx df:dy _kw]
  (fn [x y]
    (let [mt (max-order-tag [x y])
          [dx xe] (with-and-without-tag mt x)
          [dy ye] (with-and-without-tag mt y)
          a (f xe ye)
          b (if (and (v/number? dx) (v/zero? dx))
              a
              (dx+dy a (dx*dy dx (df:dx xe ye))))
          c (if (and (v/number? dy) (v/zero? dy))
              b
              (dx+dy b (dx*dy (df:dy xe ye) dy)))]
      (canonicalize-differential c))))

(def ^:private diff-+ (binary-op g/+ (constantly 1) (constantly 1) :plus))
(def ^:private diff-- (binary-op g/- (constantly 1) (constantly -1) :minus))
(def ^:private diff-* (binary-op g/* (fn [_ y] y) (fn [x _] x) :times))
(def ^:private diff-div
  (binary-op g/divide
             (fn [_ y] (g/divide 1 y))
             (fn [x y] (g/negate (g/divide x (g/square y)))) :divide))

(def ^:private sin (unary-op g/sin g/cos))
(def ^:private cos (unary-op g/cos #(g/negate (g/sin %))))
(def ^:private tan (unary-op g/tan #(g/invert (g/square (g/cos %)))))

(def ^:private asin
  (unary-op g/asin #(g/invert (g/sqrt (g/- 1 (g/square %))))))
(def ^:private acos
  (unary-op g/acos #(g/* -1 (g/invert (g/sqrt (g/- 1 (g/square %)))))))
(def ^:private atan
  (unary-op g/atan #(g/invert (g/+ 1 (g/square %)))))
(def ^:private atan2
  (binary-op g/atan
             (fn [y x]
               (g/divide x
                         (g/+ (g/square x)
                              (g/square y))))
             (fn [y x]
               (g/divide (g/negate y)
                         (g/+ (g/square x)
                              (g/square y))))
             :atan2))

(def ^:private sinh (unary-op g/sinh g/cosh))
(def ^:private cosh (unary-op g/cosh g/sinh))
(def ^:private tanh (unary-op g/tanh #(g/- 1 (g/square (g/tanh %)))))

(def ^:private sqrt (unary-op g/sqrt #(-> % g/sqrt (g/* 2) g/invert)))
(def ^:private exp (unary-op g/exp g/exp))
(def ^:private negate (unary-op g/negate (constantly -1)))
(def ^:private power
  (binary-op g/expt
             (fn [x y]
               (g/* y (g/expt x (g/- y 1))))
             (fn [_ _]
               (u/illegal "can't get there from here"))
             :expt))
(def ^:private expt
  (binary-op g/expt
             (fn [x y]
               (g/* y (g/expt x (g/- y 1))))
             (fn [x y]
               (if (and (v/number? x) (v/zero? y))
                 (if (v/number? y)
                   (if (not (g/negative? y))
                     0
                     (u/illegal "Derivative undefined: expt"))
                   0)
                 (g/* (g/log x) (g/expt x y))))
             :expt))
(def ^:private log (unary-op g/log g/invert))

;; XXX unary-op is memoized in scmutils. But rather than memoizing that,
;; it might be better just to memoize entire simplifications.

(defn ^:private euclidean-structure
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


                  (or (v/numerical? v) (x/abstract? v))
                  ((derivative g) v)

                  :else
                  (u/illegal (str "bad structure " g ", " v))))
          (a-euclidean-derivative [v]
            (cond (struct/structure? v)
                  (structural-derivative
                   (fn [w]
                     (f (if (empty? selectors) w (struct/structure-assoc-in v selectors w))))
                   (get-in v selectors))

                  (empty? selectors)
                  ((derivative f) v)

                  :else
                  (u/illegal (str "Bad selectors " f selectors v))))]
    a-euclidean-derivative))

(defn ^:private multivariate-derivative
  [f selectors]
  (let [a (v/arity f)
        d (core-partial euclidean-structure selectors)
        make-df #(with-meta % {:arity a :from :multivariate-derivative})]
    (condp = a
      [:exactly 0] (make-df (constantly 0))
      [:exactly 1] (make-df (d f))
      [:exactly 2] (make-df (fn [x y]
                              ((d (fn [[x y]] (f x y)))
                               (matrix/seq-> [x y]))))
      [:exactly 3] (make-df (fn [x y z]
                              ((d (fn [[x y z]] (f x y z)))
                               (matrix/seq-> [x y z]))))
      [:exactly 4] (make-df (fn [w x y z]
                              ((d (fn [[w x y z]] (f w x y z)))
                               (matrix/seq-> [w x y z]))))
      [:between 1 2] (make-df (fn [x & y]
                                (if (nil? y)
                                  ((d f) x)
                                  ((d (fn [[x y]] (f x y)))
                                   (matrix/seq-> (cons x y))))))
      (fn [& xs]
        (when (empty? xs) (u/illegal "No args passed to derivative?"))
        (if (= (count xs) 1)
          ((d f) (first xs))
          ((d #(apply f %)) (matrix/seq-> xs)))))))

(defn ^:private define-binary-operation
  [generic-operation differential-operation]
  (doseq [signature [[::differential ::differential]
                     [::v/scalar ::differential]
                     [::differential ::v/scalar]]]
    (defmethod generic-operation signature [a b] (differential-operation a b))))

(defn ^:private define-unary-operation
  [generic-operation differential-operation]
  (defmethod generic-operation [::differential] [a] (differential-operation a)))

(defmethod g/expt [::differential ::v/number] [d n] (power d n))
(define-binary-operation g/expt expt)

(define-binary-operation g/add diff-+)
(define-binary-operation g/sub diff--)
(define-binary-operation g/mul diff-*)
(define-binary-operation g/div diff-div)

(define-unary-operation g/log log)
(define-unary-operation g/exp exp)
(define-unary-operation g/sqrt sqrt)

(define-unary-operation g/sin sin)
(define-unary-operation g/cos cos)
(define-unary-operation g/tan tan)

(define-unary-operation g/asin asin)
(define-unary-operation g/acos acos)
(define-unary-operation g/atan atan)
(define-binary-operation g/atan atan2)

(define-unary-operation g/sinh sinh)
(define-unary-operation g/cosh cosh)
(define-unary-operation g/tanh tanh)

(define-unary-operation g/negate negate)
(define-unary-operation g/invert #(diff-div 1 %))
(define-unary-operation g/square #(diff-* % %))
(define-unary-operation g/cube #(diff-* % (diff-* % %)))

(derive ::differential ::o/co-operator)
(derive ::differential ::series/coseries)
(derive ::differential ::f/cofunction)

(defmethod g/partial-derivative
  [::v/function v/seqtype]
  [f selectors]
  (multivariate-derivative f selectors))

(defmethod g/partial-derivative [::v/function nil] [f _]
  (multivariate-derivative f []))

(defmethod g/partial-derivative [::struct/structure v/seqtype] [f selectors]
  (multivariate-derivative f selectors))

(defmethod g/partial-derivative [::matrix/matrix v/seqtype] [f selectors]
  (multivariate-derivative f selectors))

(def derivative-symbol 'D)

(def D
  "Derivative operator. Produces a function whose value at some point can
  multiply an increment in the arguments, to produce the best linear estimate
  of the increment in the function value."
  (o/make-operator #(g/partial-derivative % [])
                   derivative-symbol))

(defn partial
  "Partial differentiation of a function at the (zero-based) slot index
  provided."
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors)
                   :partial-derivative))

(defn taylor-series
  "Returns a `Series` of the coefficients of the taylor series of the function `f`
  evaluated at `x`, with incremental quantity `dx`.

  NOTE: The `(constantly dx)` term is what allows this to work with arbitrary
  structures of `x` and `dx`. Without this wrapper, `((g/* dx D) f)` with `dx`
  == `(up 'dx 'dy)` would expand to this:

  `(fn [x] (* (s/up ('dx x) ('dy x))
              ((D f) x)))`

  `constantly` delays the interpretation of `dx` one step:

  `(fn [x] (* (s/up 'dx 'dy)
              ((D f) x)))`
  "
  [f x dx]
  (((g/exp (g/* (constantly dx) D)) f) x))
