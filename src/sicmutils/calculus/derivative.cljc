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
            [sicmutils.util.stream :as us]
            [sicmutils.value :as v]))

;; A differential term is implemented as a pair whose first element is
;; a set of tags and whose second is the coefficient.


(def ^:private empty-differential [])
(def ^:private empty-tags [])

(defn- make-term
  ([coef] [empty-tags coef])
  ([tags coef] [tags coef]))

(def ^:private tags first)
(def ^:private coefficient second)

(declare canonicalize
         differential-of
         with-finite-and-infinitesimal-parts)

;; A differential is a sequence of differential terms, ordered by the
;; tag set.
(deftype Differential [terms]
  v/Value
  (zero? [_] (every? (comp v/zero? coefficient) terms))
  (one? [this]
    (with-finite-and-infinitesimal-parts this
      (fn [finite inft]
        (let [cf (canonicalize finite)]
          (and (v/one? cf)
               (v/zero? inft))))))
  (identity? [this] (v/one? this))
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
                  (= terms (.-terms ^Differential b)))))

  #?@(:cljs
      [IEquiv
       (-equiv [_ b]
               (and (instance? Differential b)
                    (= terms (.-terms ^Differential b))))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

#?(:clj
   (defmethod print-method Differential
     [^Differential s ^java.io.Writer w]
     (.write w (.toString s))))

(defn differential-type?
  "Returns true if the supplied object is an instance of `Differential`, false
  otherwise."
  [dx]
  (instance? Differential dx))

(defn differential?
  "Returns true if the supplied `x` is a 'differential' quantity, false
  otherwise."
  [x]
  (cond (differential-type? x) true
        (struct/structure? x)  (some differential? x)
        (matrix/matrix? x)     (matrix/some differential? x)
        :else false))

(defn terms
  "For the supplied `Differential` object, returns its vector of terms."
  [dx]
  {:pre [(differential-type? dx)]}
  (.-terms ^Differential dx))

(defn- differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list (unless d is zero, in
  which case we return the empty term list)."
  [dx]
  (cond (differential-type? dx)
        (filterv (fn [term]
                   (not (v/zero?
                         (coefficient term))))
                 (terms dx))

        (v/zero? dx) empty-differential
        :else        [(make-term empty-tags dx)]))

(defn terms->differential
  "Returns a differential instance generated from a vector of terms.

  Similar to [[canonicalize]] if you only have a term list.

  TODO maybe consolidate if `canonicalize` is the only place we use this."
  [terms]
  {:pre [(vector? terms)]}
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (empty? (tags (first terms))))
        (coefficient (first terms))

        :else (->Differential terms)))

(defn canonicalize
  "Returns the term list of the supplied differential, _unless_ that term list is
  empty (returns 0) or contains a singleton term (returns its coefficient)"
  [dx]
  (let [ts (terms dx)]
    (cond (empty? ts) 0
          (and (= (count ts) 1)
               (empty? (tags (first ts))))
          (coefficient (first ts))
          :else dx)))

(defn differential-of
  "The differential of a quantity is:

  - if we're a differential, the differential of the coefficient of the
    highest-order term part
  - or else the input itself.

  TODO why is it the FIRST element, the car, in the scheme edition? Maybe it
  just doesn't matter and we assume that they're all the same."
  [dx]
  (if (differential-type? dx)
    (recur (coefficient
            (peek (terms dx))))
    dx))

(defn arity [x]
  (if (differential-type? x)
    (recur (coefficient
            (peek (terms x))))
    (f/arity x)))

(defn diff:apply
  "If the coefficients are themselves functions, apply them to the args for ALL
  coefficients."
  [diff args]
  (terms->differential
   (map (fn [term]
          (make-term (tags term)
                     (apply (coefficient term) args)))
        (differential->terms diff))))

;; The data structure of a tag set. Tags are small integers. Tag sets are
;; typically of small cardinality. So we experiment with implementing them as
;; small vectors, instead of sorted sets.

(let [next-differential-tag (atom 0)]
  (defn- make-differential-tag []
    (swap! next-differential-tag inc)))

(defn- tag-in?
  "Return true if `t` is in the tag-set `ts`, false otherwise."
  [ts t]
  (some #(= % t) ts))

(defn- tag-in-term?
  "Return true if `t` is in the tag-set of the supplied `term`, false otherwise."
  [term t]
  (tag-in? (tags term) t))

(defn- drop-tag
  "Return the tag set formed by dropping t from ts"
  [ts t]
  (filterv #(not= % t) ts))

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
;; From scmutils: "Differential term lists represent a kind of power series, so
;; they can be added and multiplied. It is important to note that when terms are
;; multiplied, no contribution is made if the terms being multiplied have a
;; differential tag in common. Thus dx^2 = zero."

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
  (into empty-differential
        (sort-by first
                 (for [[tags tags-coefs] (group-by tags tags->coefs)
                       :let [c (transduce (map coefficient) g/+ tags-coefs)]
                       :when (not (v/zero? c))]
                   [tags c]))))

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

(defn d:+
  "Adds two objects differentially. (One of the objects might not be
  differential; in which case we lift it into a trivial differential
  before the addition.)"
  [dx dy]
  (->Differential
   (terms:+ (differential->terms dx)
            (differential->terms dy))))

(defn d:*
  "Form the product of the differentials dx and dy."
  [dx dy]
  (->Differential
   (collect-terms
    (terms:* (differential->terms dx)
             (differential->terms dy)))))

(defn- make-x+dx [x dx]
  (d:+ x (->Differential [[[dx] 1]])))

(defn ^:private extract-dx-part [tag obj]
  (letfn [(extract
            ;; Collect all the terms of the differential in which
            ;; dx is a member of the term's tag set; drop that
            ;; tag from each set and return the differential formed
            ;; from what remains.
            [obj]
            (if (differential? obj)
              (sum->differential
               (for [[tags coef] (terms obj)
                     :when (tag-in? tags tag)]
                 [(drop-tag tags tag) coef]))
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
                  (fn? obj) #(dist (obj %)) ;; TODO: innocent of the tag-hiding business
                  :else (extract obj)))]
    (dist obj)))

(defn derivative [f]
  (fn [x]
    (let [dx (make-differential-tag)]
      (extract-dx-part dx (f (make-x+dx x dx))))))

(defn- with-finite-and-infinitesimal-parts
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
    (continue (->Differential finite-part)
              (->Differential infinitesimal-part))))

(defn- unary-op
  [f df:dx]
  (fn [x]
    (with-finite-and-infinitesimal-parts x
      (fn [finite-part infinitesimal-part]
        (let [canonicalized-finite-part (canonicalize finite-part)]
          (canonicalize
           (d:+ (f canonicalized-finite-part)
                (d:* (df:dx canonicalized-finite-part)
                     infinitesimal-part))))))))


(defn max-order-tag
  "From each of the differentials in the sequence ds, find the highest
  order term; then return the greatest tag found in any of these
  terms; i.e., the highest-numbered tag of the highest-order term."
  [ds]
  (letfn [(max-termv [d]
            (if-let [max-order (-> (differential->terms d)
                                   (peek)
                                   (tags)
                                   (peek))]
              [max-order]
              []))]
    (->> (mapcat max-termv ds)
         (apply max))))

(defn with-tag
  "The differential containing only those terms _with_ the given tag"
  [tag dx]
  (if (differential-type? dx)
    (->> (differential->terms dx)
         (filterv #(tag-in-term? % tag))
         (sum->differential))
    0))

(defn without-tag
  "The differential containing only those terms _without_ the given tag"
  [tag dx]
  (if (differential-type? dx)
    (let [sans-tag? (complement #(tag-in-term? % tag))]
      (->> (differential->terms dx)
           (filterv sans-tag?)
           (sum->differential)))
    dx))

(defn with-and-without-tag
  "Split the differential into the parts with and without tag and return the
  pair."
  [tag dx]
  (let [[infinitesimal-terms finite-terms]
        (us/separatev #(tag-in-term? % tag)
                      (differential->terms dx))]
    [(sum->differential infinitesimal-terms)
     (sum->differential finite-terms)]))

(defn ^:private binary-op
  [f df:dx df:dy _kw]
  (fn [x y]
    (let [mt (max-order-tag [x y])
          [dx xe] (with-and-without-tag mt x)
          [dy ye] (with-and-without-tag mt y)
          a (f xe ye)
          b (if (and (v/number? dx) (v/zero? dx))
              a
              (d:+ a (d:* dx (df:dx xe ye))))
          c (if (and (v/number? dy) (v/zero? dy))
              b
              (d:+ b (d:* (df:dy xe ye) dy)))]
      (canonicalize c))))

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
                                         (g (assoc-in v [i] w)))
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
                     (f (if (empty? selectors)
                          w
                          (assoc-in v selectors w))))
                   (get-in v selectors))

                  (empty? selectors)
                  ((derivative f) v)

                  :else
                  (u/illegal (str "Bad selectors " f selectors v))))]
    a-euclidean-derivative))

(defn ^:private multivariate-derivative
  [f selectors]
  (let [a (f/arity f)
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

(define-binary-operation g/dot-product diff-*)

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
