(ns math.diff
  (:require [math.generic :as g]
            [math.struct :as s]
            [math.function :as f]
            ))

;; If you construct one of these directly, make sure terms
;; is appropriately sorted. If in doubt, use make-differential

(defrecord Differential [terms]
  g/Value
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] 0)
  (exact? [x] false)
  (sort-key [x] 35)
  )

(def differential? (partial instance? Differential))

;; If you construct one of these directly, make sure tags
;; is sorted correctly; if in doubt, use make-differential-term

(defrecord DifferentialTerm [tags coefficient]
  )

(defn- make-differential [terms]
  (Differential. (sort-by #(-> % .tags seq) terms)))


(defn add [u v]
  )

(defn add-differential-term-lists
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
              (< (seq a-tags) (seq b-tags)) (recur (rest as) bs (conj rs a))
              :else (recur as (rest bs) (conj rs b)))))))

;; the basic arithmetic of differentials

;; (define (d:+ u v)
;;   (terms->differential
;;    (dtl:+ (differential->terms u)
;;        (differential->terms v))))

(defn- terms->differential [terms]
  (cond (empty? terms) 0
        (and (= (count terms) 1) (-> terms first .tags empty?)) (-> terms first .coefficient)
        :else (make-differential terms)))

(defn- differential->terms
  "Given a differential, returns the vector of DifferentialTerms
  within; otherwise, returns a singleton differential term
  representing d with an empty tag list"
  [d]
  (if (instance? Differential d) (.terms d)
      [(DifferentialTerm. (sorted-set) d)]))

(defn add-differential [a b]
  (terms->differential
   (add-differential-term-lists (differential->terms a) (differential->terms b))))

;; (define (differential->terms diff)
;;   (if (differential? diff)
;;     (differential-term-list diff)
;;     (list
;;      (make-differential-term '() diff))))

;; (define (terms->differential terms)
;;   (cond ((null? terms) :zero)
;;      ((and (null? (cdr terms))
;;            (null? (differential-tags (car terms))))
;;       (differential-coefficient (car terms)))
;;      (else
;;       (make-differential terms))))


(def ^:private next-differential-tag (atom 0))

(defn- make-differential-tag []
  (swap! next-differential-tag inc))


(defn- make-x+dx [x dx]
  (add x (Differential. [(DifferentialTerm. (sorted-set dx) 1)])))

(defn- terms->differential-collapse [& args] false) ; XXX
(defn- hide-tag-in-procedure [& args] false) ; XXX

(defn- extract-dx-part [dx obj]
  (letfn [(extract [obj]
            (if (differential? obj)
              (terms->differential-collapse
               (mapcat
                (fn [term]
                  (let [tags (.tags term)]; XXX convert to if-let
                    (if (tags dx)
                      [(DifferentialTerm. (tags disj dx) (.coefficient term))])))
                (.terms obj)))
              0))
          (dist [obj]
            (cond (s/structure? obj) (s/mapr dist obj)
                  ;(matrix? obj) (m:elementwise dist obj) XXX
                  ;(quaternion? obj) XXX
                  (instance? clojure.lang.IFn obj) (hide-tag-in-procedure dx (comp dist obj))
                  ;(operator? obj) XXX
                  ;(series? obj) XXX
                  :else (extract obj)))]
    (dist obj)))

(defn- simple-derivative-internal [f x]
  (let [dx (make-differential-tag)]
    (extract-dx-part dx (f (make-x+dx x dx)))))

(defn derivative [f]
  (partial simple-derivative-internal f))
;; why doesn't simple-derivative-internal just return a
;; function?

;; (defn terms->differential [terms]
;;   (cond (empty? terms) 0
;;         ))

;; (define (terms->differential terms)
;;   (cond ((null? terms) :zero)
;;      ((and (null? (cdr terms))
;;            (null? (differential-tags (car terms))))
;;       (differential-coefficient (car terms)))
;;      (else
;;       (make-differential terms))))


;;; SIMPLE-DERIVATIVE-INTERNAL represents the essential computation.
;;; To compute the derivative of function f at point x, make a new
;;; differential object with incremental part 1 (the coefficient of
;;; the new differential tag, dx) and with constant part x.  We pass
;;; this through the function f, and then extract the terms which
;;; contain the the differential tag dx, removing that tag.  This
;;; leaves the derivative.

;;;                           f
;;;                 x + dx |----> f(x) + Df(x)*dx
;;;                  \                  /
;;;                   \                /
;;;                    \     Df       /
;;;                      x |----> Df(x)
