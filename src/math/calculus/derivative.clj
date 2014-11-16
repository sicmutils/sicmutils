(ns math.calculus.derivative
  (:require [clojure.set :as set]
            [math.generic :as g]
            [math.struct :as struct]
            [math.function :as f]
            ))

;; If you construct one of these directly, make sure tags
;; is sorted correctly; if in doubt, use make-differential-term
(defrecord DifferentialTerm [tags coefficient]
  )

(declare make-differential)
(declare differential->terms)

;; If you construct one of these directly, make sure terms
;; is appropriately sorted. If in doubt, use make-differential
(defrecord Differential [terms]
  g/Value
  (zero? [x]
    (every? g/zero? (map #(.coefficient %) (.terms x))))
  (one? [x]
    false) ;; XXX! this needs to be fixed
  (zero-like [d] 0)
  (exact? [d] false)
  (compound? [d] false)
  (sort-key [d] 80)
  ;; this isn't quite right: it "applies" things which aren't functions
  ;; clojure.lang.IFn
  ;; (invoke [d xs]
  ;;   (prn "INVOKING" d "ON" xs)
  ;;   (make-differential
  ;;    (map #(DifferentialTerm. (.tags %) ((.coefficient %) xs))
  ;;         (differential->terms d))))
  )

(def ^:private differential? (partial instance? Differential))

(defn make-differential
  "Constructs a differential from a list of terms. If the list is empty,
  you get plain old 0. If there is just one term and it does not
  contain any differential tags, then you get the coefficient of the
  term (i.e., things that aren't differential anymore as a result of
  derivative computation get folded town to their plain
  values). Otherwise a Differential object containing the terms is
  returned."
  [terms]
  (cond (empty? terms) 0
        (and (= (count terms) 1) (-> terms first .tags empty?)) (-> terms first .coefficient)
        ;; kind of sad to call vec here. Why aren't the seqs comparable?
        :else (Differential. (sort-by #(-> % .tags vec) terms))))

(defn make-differential-term [dxs coefficient]
  (DifferentialTerm. (apply sorted-set dxs) coefficient))

(defn- dxs+dys
  [as bs]
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
  (loop [dys dys result []]
    (if (nil? dys) result
        (let [y1 (first dys) y-tags (.tags y1)]
          (recur (next dys)
                 (if (empty? (set/intersection tags y-tags))
                   (conj result (DifferentialTerm.
                                 (set/union tags y-tags)
                                 (g/* coefficient (.coefficient y1))))
                   result))))))

(defn dxs*dys
  [as bs]
  (if (empty? as) []
      (dxs+dys
       (dx*dys (first as) bs)
       (dxs*dys (next as) bs))))

(defn- differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list"
  [d]
  (if (instance? Differential d) (.terms d)
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
  (make-differential (reduce dxs+dys [] (map vector terms))))

(defn- hide-tag-in-procedure [& args] false) ; XXX

(defn- extract-dx-part [dx obj]
  (letfn [(extract [obj]
            (if (differential? obj)
              (terms->differential-collapse
               (mapcat
                (fn [term]
                  (let [tags (.tags term)]
                    (if (tags dx)
                      [(DifferentialTerm. (disj tags dx) (.coefficient term))])))
                (.terms obj)))
              0))
          (dist [obj]
            (cond (struct/structure? obj) (struct/mapr dist obj)
                  ;; (matrix? obj) (m:elementwise dist obj) XXX
                  ;; (quaternion? obj) XXX
                  ;;
                  ;; Is this latter one causing trouble with invokable derivatives?
                  ;; yes it is. A function? is not something that simply implements IFn;
                  ;; we will have to be more delicate. Er, or else we will have to
                  ;; implement hide-tag-in-procedure. Until we do this, we may be
                  ;; susceptible to the "amazing bug."
                  ;;
                  ;; (instance? clojure.lang.IFn obj) (hide-tag-in-procedure dx (comp dist obj))
                  ;;
                  ;; (operator? obj) XXX
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
          keytag (-> dts last .tags last)
          {finite-part false
           infinitesimal-part true} (group-by #(-> % .tags (contains? keytag)) dts)]
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

(defn- max-order-tag [& ds]
  (last (apply set/union (map #(-> % differential->terms last .tags) ds))))

(defn- binary-op
  [f ∂f:∂x ∂f:∂y]
  (fn [x y]
    (let [mt (max-order-tag x y)
          {xe-terms false dx-terms true} (group-by #(-> % .tags (contains? mt)) (differential->terms x))
          {ye-terms false dy-terms true} (group-by #(-> % .tags (contains? mt)) (differential->terms y))
          xe (terms->differential-collapse xe-terms)
          dx (terms->differential-collapse dx-terms)
          ye (terms->differential-collapse ye-terms)
          dy (terms->differential-collapse dy-terms)
          a (f xe ye)
          b (if (and (number? dx) (zero? dx))
              a
              (dx+dy a (dx*dy dx (∂f:∂x xe ye))))
          c (if (and (number? dy) (zero? dy))
              b
              (dx+dy b (dx*dy (∂f:∂y xe ye) dy)))]
      c)))


(def ^:private diff-+
  (binary-op g/+
             (fn [x y] 1)
             (fn [x y] 1)))

(def ^:private diff--
  (binary-op g/-
             (fn [x y] 1)
             (fn [x y] -1)))

(def ^:private diff-*
  (binary-op g/*
             (fn [x y] y)
             (fn [x y] x)))


;; XXX unary-op is memoized in scmutils. But rather than memoizing that,
;; it might be better just to memoize entire simplications.

(def ^:private sin
  (unary-op g/sin g/cos))

(def ^:private cos
  (unary-op g/cos #(g/* -1 (g/sin %))))

(def ^:private power
  (binary-op g/expt
             (fn [x y]
               (g/* y (g/expt x (g/- y 1))))
             (fn [x y]
               (throw (java.lang.IllegalArgumentException. "can't get there from here")))))


(defn- not-compound?
  [x]
  (if (satisfies? g/Value x)
    (not (g/compound? x))
    true))

(g/defhandler :+ [differential? not-compound?] diff-+)
(g/defhandler :+ [not-compound? differential?] diff-+)
(g/defhandler :- [differential? not-compound?] diff--)
(g/defhandler :- [not-compound? differential?] diff--)
(g/defhandler :* [differential? not-compound?] diff-*)
(g/defhandler :* [not-compound? differential?] diff-*)
(g/defhandler :**  [differential? (complement differential?)] power)
(g/defhandler :sin [differential?] sin)
(g/defhandler :cos [differential?] cos)
(println "diff initialized")

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
