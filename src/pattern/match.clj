(ns pattern.match
  (:require [clojure.walk :refer [postwalk-replace]]))

(def ^:private zero [{} nil])

(defn match-one [thing]
  (fn [frame [x & xs] succeed]
    (and (= x thing)
         (succeed frame xs))))

(defn match-var [var]
  (fn [frame [x & xs] succeed]
    (if x
      (if-let [binding (frame var)]
       (and (= binding x) (succeed frame xs))
       (succeed (assoc frame var x) xs)))))

(defn match-segment [var]
  (fn [frame xs succeed]
    (if-let [binding (frame var)]
      ;; the segment value is bound.
      (let [binding-count (count binding)]
        (if (= (take binding-count xs) binding)
          (succeed frame (drop binding-count xs))))
      ;; the segment value is unbound.
      (loop [before [] after xs]
        (or (succeed (assoc frame var before) after)
            (if-not (empty? after)
              (recur (conj before (first after)) (rest after))))))))

(defn match-list [& matchers]
  (fn [frame xs succeed]
    (if (seq? xs)
      (letfn [(step [as matchers frame]
                (cond (not (empty? matchers))
                      ((first matchers) frame as
                       (fn [new-frame new-as]
                         (step new-as (rest matchers) new-frame)))
                      (not (empty? as)) false ;; XXX test comment delete me
                      (empty? as) (succeed frame (rest xs))
                      :else false))]
        (step (first xs) matchers frame)))))

(defn- variable-reference? [x]
  (and (sequential? x)
       (= (first x) :?)))

(defn- segment-reference? [x]
  (and (sequential? x)
       (= (first x) :??)))

(defn- variable [x]
  (second x))

(defn pattern->matcher [pattern]
  (if (sequential? pattern)
    (cond (variable-reference? pattern) (match-var (variable pattern))
          (segment-reference? pattern) (match-segment (variable pattern))
          :else (apply match-list (map pattern->matcher pattern)))
    (match-one pattern)))

(defn match [matcher data]
  (let [receive (fn [frame data] (if (empty? data) frame))]
    (matcher {} (list data) receive)))

(defn substitute [frame form]
  (if (sequential? form)
    (loop [result []
           [x & xs] form]
      (if x
        (cond (variable-reference? x) (recur (conj result (frame (variable x))) xs)
              (segment-reference? x) (recur (into result (frame (variable x))) xs)
              :else (recur (conj result (substitute frame x)) xs)
              )
        result))
    form
    ))

(defn match-and-substitute [pattern consequence]
  (let [matcher (pattern->matcher pattern)]
    (fn [data]
      (if-let [frame (match matcher data)]
        (substitute frame consequence)
        data))))

(defn compile-consequence [dict-symbol consequence]
  (cond (variable-reference? consequence)
        `(list (~dict-symbol '~(variable consequence)))
        (segment-reference? consequence)
        (list dict-symbol (variable consequence))
        (seq? consequence)
        `(concat ~@(map (partial compile-consequence dict-symbol) consequence))
        :else `(list '~consequence)
        ))

(defmacro rule [pattern consequence]
  (let [dict-symbol (gensym)
        compiled-consequence (compile-consequence dict-symbol consequence)]
    `(let [matcher# (pattern->matcher '~pattern)]
       (fn [data# continue#]
         (if-let [~dict-symbol (match matcher# data#)]
           (continue# ~compiled-consequence))))))
