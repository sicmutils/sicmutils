(ns math.calculus.derivative
  (:require [math.value :as v]
            [math.generic :as g]
            [math.operator :as o]
            [math.structure :as struct]
            [clojure.set :as set]
            ))
;;
;; some general thoughts on this module: we have observed,
;; at various debugging times, intermediate values of
;; differential containing coefficient-zero items. Where
;; do these come from? We are also skeptical of the implementation
;; of differential->terms-collapse. Maybe better done with a
;; group-by, filter than a reduce over the addition.
;;

;; If you construct one of these directly, make sure tags
;; is sorted correctly; if in doubt, use make-differential-term
(defrecord DifferentialTerm [tags coefficient]
  )

(declare differential-of)

;; If you construct one of these directly, make sure terms
;; is appropriately sorted. If in doubt, use make-differential
(defrecord Differential [terms]
  v/Value
  (nullity? [x]
    (every? g/zero? (map #(:coefficient %) (:terms x))))
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

(defn make-differential-term [dxs coefficient]
  (DifferentialTerm. (apply sorted-set dxs) coefficient))

(defn differential-of [dx]
  (loop [dx dx]
    (if (instance? Differential dx)
      (recur (:coefficient (first (:terms dx))))
      dx)))

(defn- differential-term-list?
  [as]
  (or (empty? as)
      (and (sequential? as)
           (every? #(instance? DifferentialTerm %) as))))

(defn make-differential
  "Constructs a differential from a list of terms. If the list is empty,
  you get plain old 0. If there is just one term and it does not
  contain any differential tags, then you get the coefficient of the
  term (i.e., things that aren't differential anymore as a result of
  derivative computation get folded town to their plain
  values). Otherwise a Differential object containing the terms is
  returned."
  [terms]
  {:pre [(differential-term-list? terms)]}
  (cond (empty? terms) 0
        (and (= (count terms) 1) (-> terms first :tags empty?)) (-> terms first :coefficient)
        ;; kind of sad to call vec here. Why aren't the seqs comparable?
        :else (Differential. (sort-by #(-> % :tags vec) terms))))

(defn- dxs+dys
  ; TODO: solve the mystery of the zero coefficients.
  [as bs]
  {:pre [(differential-term-list? as)
         (differential-term-list? bs)]}
  (loop [as as bs bs rs []]
    (cond
     (empty? as) (into rs bs)
     (empty? bs) (into rs as)
     :else (let [{a-tags :tags a-coef :coefficient :as a} (first as)
                 {b-tags :tags b-coef :coefficient :as b} (first bs)]
             (cond
              (= a-tags b-tags) (let [r-coef (g/+ a-coef b-coef)]
                                  (recur (rest as) (rest bs)
                                         (if (not (g/zero? r-coef))
                                           (conj rs (DifferentialTerm. a-tags r-coef))
                                           rs)))
              ;; kind of sad to call vec here.
              (< (compare (vec a-tags) (vec b-tags)) 0) (recur (rest as) bs (conj rs a))
              :else (recur as (rest bs) (conj rs b)))))))

(defn dx*dys
  [{:keys [tags coefficient] :as dx} dys]
  {:pre [(instance? DifferentialTerm dx)
         (differential-term-list? dys)]}
  (loop [dys dys result []]
    (if (nil? dys) result
        (let [y1 (first dys) y-tags (:tags y1)]
          (recur (next dys)
                 (if (empty? (set/intersection tags y-tags))
                   (conj result (DifferentialTerm.
                                 (set/union tags y-tags)
                                 (g/* coefficient (:coefficient y1))))
                   result))))))

(defn dxs*dys
  [as bs]
  {:pre [(differential-term-list? as)
         (differential-term-list? bs)]}
  (if (empty? as) []
      (dxs+dys
       (dx*dys (first as) bs)
       (dxs*dys (next as) bs))))

(defn- differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list"
  [d]
  (if (instance? Differential d) (:terms d)
      [(DifferentialTerm. (sorted-set) d)]))

;; XXX: perhaps instead of differential->terms we should
;; have a function which lifts a non-differential object into
;; a trivial differential, or throws if this cannot be done.
;; this might eliminate the need for the not-compound? tests
;; in the GJS code. Might also simplify the adders!

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
  (Differential. [(DifferentialTerm. (sorted-set) x)
                  (DifferentialTerm. (sorted-set dx) 1)]))

(defn- terms->differential-collapse
  [terms]
  {:pre [(differential-term-list? terms)]}
  (make-differential (reduce dxs+dys [] (map vector terms))))

;(defn- hide-tag-in-procedure [& args] false) ; XXX

(defn- extract-dx-part [dx obj]
  (letfn [(extract [obj]
            (if (differential? obj)
              (terms->differential-collapse
               (mapcat
                (fn [term]
                  (let [tags (:tags term)]
                    (if (tags dx)
                      [(DifferentialTerm. (disj tags dx) (:coefficient term))])))
                (:terms obj)))
              0))
          (dist [obj]
            (cond (struct/structure? obj) (struct/mapr dist obj)
                  (o/operator? obj) (do (prn "DIFF AN OPERATOR" obj) (extract obj))
                  ;; (matrix? obj) (m:elementwise dist obj) XXX
                  ;; (quaternion? obj) XXX
                  ;; (operator? obj) XXX
                  ;;
                  ;; Is this latter one causing trouble with invokable derivatives?
                  ;; yes it is. A function? is not something that simply implements IFn;
                  ;; we will have to be more delicate. Er, or else we will have to
                  ;; implement hide-tag-in-procedure. Until we do this, we may be
                  ;; susceptible to the "amazing bug."
                  ;;
                  ;; (instance? clojure.lang.IFn obj) (hide-tag-in-procedure dx (comp dist obj))
                  ;;
                  ;; (series? obj) XXX

                  :else (extract obj)))]
    (dist obj)))

(defn derivative
  [f]
  (fn [x]
    (let [dx (make-differential-tag)]
      (extract-dx-part dx (f (make-x+dx x dx))))))

;; (define compound-type-tags
;;   (list vector-type-tag
;;      ;;column-type-tag
;;      quaternion-type-tag
;;      row-type-tag
;;      matrix-type-tag
;;      series-type-tag
;;      abstract-matrix-type-tag))

;;; To turn a unary function into one that operates on differentials
;;; we must supply the derivative.  This is the essential chain rule.



;;; The finite-part is all terms except for terms containing the
;;; highest numbered differential tag in a term of highest order, and
;;; infinitesimal-part is the remaining terms, all of which contain
;;; that differential tag.  So:

;;;                           f
;;;    x + dx + dy + dx*dy |----> f(x+dx) + Df(x+dx)*(dy+dx*dy)
;;; Alternatively, we might have computed the following, but we think
;;; that the ultimate values of derivatives don't care, because mixed
;;; partials of R^2 --> R commute.

;;;    x + dx + dy + dx*dy |----> f(x+dy) + Df(x+dy)*(dx+dx*dy)

(defn- finite-and-infinitesimal-parts
  "Partition the terms of the given differential into the finite and
  infinite parts. XXX we aren't using terms->differential-collapse
  because it doesn't seem like we need to. Alert."
  [x]
  (if (differential? x)
    (let [dts (differential->terms x)
          keytag (-> dts last :tags last)
          {finite-part false
           infinitesimal-part true} (group-by #(-> % :tags (contains? keytag)) dts)]
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
  (->> ds (map #(-> % differential->terms last :tags)) (apply set/union) last))

(defn with-tag
  "XXX doc and decide if we need the two infra"
  [tag dx]
  (->> dx :terms (filter #(-> % :tags (contains? tag))) terms->differential-collapse))

(defn without-tag
  "A real differential is expected here. document this and the above and below,
  if we turn out to keep all three of them. It seems there must be a better way
  to do this..."
  [tag dx]
  (->> dx :terms (filter #(-> % :tags (contains? tag) not)) terms->differential-collapse))

(defn with-and-without-tag
  "XXX doc and decide if we need above two"
  [tag dx]
  (let [{finite-terms false infinitesimal-terms true}
        (group-by #(-> % :tags (contains? tag)) (differential->terms dx))]
    [(terms->differential-collapse infinitesimal-terms)
     (terms->differential-collapse finite-terms)]))

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


(def ^:private diff-+ (binary-op g/+ (constantly 1) (constantly 1)))
(def ^:private diff-- (binary-op g/- (constantly 1) (constantly -1)))
(def ^:private diff-* (binary-op g/* (fn [_ y] y)   (fn [x _] x)))
(def ^:private sin    (unary-op g/sin g/cos))
(def ^:private cos    (unary-op g/cos #(g/* -1 (g/sin %))))
(def ^:private power
  (binary-op g/expt
             (fn [x y]
               (g/* y (g/expt x (g/- y 1))))
             (fn [_ _]
               (throw (IllegalArgumentException. "can't get there from here")))))
(def ^:private sqrt (unary-op g/sqrt #(g/invert (g/* 2 (g/sqrt %)))))

;; XXX unary-op is memoized in scmutils. But rather than memoizing that,
;; it might be better just to memoize entire simplications.

(defn- euclidean-structure
  [selectors f]
  (letfn [(sd [g v]
            (cond (struct/structure? v) (throw (IllegalArgumentException. "oops"))
                  (or (g/numerical-quantity? v) (g/abstract-quantity? v)) ((derivative g) v)
                  :else (throw (IllegalArgumentException. (str "bad structure " g v)))))
          (a-euclidean-derivative [v]
            (cond (struct/structure? v)
                  (sd (fn [w]
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
        d (fn [f] (euclidean-structure selectors f))] ;; partial application opportunity
    (cond (= a 0) (constantly 0)
          (= a 1) (d f)
          (= a 2) (fn [x y]
                    ((d (fn [s] (apply f (seq s))))
                     (struct/seq-> [x y])))
          :else (throw (IllegalArgumentException. "Haven't implemented this yet!")))))

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
(g/defhandler :square [differential?] #(g/* % %))
(g/defhandler :sin    [differential?] sin)
(g/defhandler :cos    [differential?] cos)
(g/defhandler :sqrt   [differential?] sqrt)
(g/defhandler :**     [differential? (complement differential?)] power)
(g/defhandler :∂      [#(or (ifn? %) (struct/structure? %))
                       (constantly true)] multivariate-derivative)
(println "derivative initialized")

;;; SIMPLE-DERIVATIVE-INTERNAL represents the essential computation.
;;; To compute the derivative of function f at point x, make a new
;;; differential object with incremental part 1 (the coefficient of
;;; the new differential tag, dx) and with constant part x.  We pass
;;; this through the function f, and then extract the terms which
;;; contain the the differential tag dx, removing that tag.  This
;;; leaves the derivative.
;;;
;;;                           f
;;;                 x + dx |----> f(x) + Df(x)*dx
;;;                  \                  /
;;;                   \                /
;;;                    \     Df       /
;;;                      x |----> Df(x)
