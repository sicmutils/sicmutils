(ns pattern.match)

(def ^:private zero [{} nil])

;; monadic style:
;; a "parse" is a pair [frame tail]; each combinator is of the form
;; p -> [p], and combinators combine via mapcat

(defn match-one-monadic [thing]
  (fn [[frame [x & xs]]]
    (if (= x thing) [[frame xs]])))

(defn match-var-monadic [var]
  (fn [[frame [x & xs]]]
    (if-let [binding (frame var)]
      (if (= binding x) [[frame xs]])
      [[(assoc frame var x) xs]])))

(defn segments [xs]
  (defn step [l r]
    (lazy-seq
     (cons [l r]
           (if (empty? r) nil
               (step (conj l (first r)) (rest r))))))
  (step [] xs))

(defn match-segment-monadic [var]
  (fn [[frame xs]]
    (if-let [binding (frame var)]
      ;; segement value is bound; succeed if it recurs here
      (loop [xs xs
             binding binding]
        (if (empty? binding)
          ;; xxx redo this with and 
          [[frame xs]]
          (if (some? xs)
            (if (= (first xs) (first binding))
              (recur (next xs) (next binding))
              ))
          ))
      ;; segment value is unbound. generate a lazy sequence
      ;; of possible parses
      (map (fn [[before after]]
             [(assoc frame var before) after])
           (segments xs))
      )
    )
  )

;; TODO: partial applications of map & filter! these should be
;; reconsidered when we get to Clojure 1.7.
(defn match-list-monadic [& matchers]
  (comp
   (partial filter (fn [[frame xs]] (empty? xs)))
   (reduce (fn [c m] (comp (partial mapcat m) c)) list matchers)))

(defn match-one [thing]
  (fn [frame [x & xs] succeed]
    (and (= x thing)
         (succeed frame xs))))

(defn match-var [var]
  (fn [frame [x & xs] succeed]
    (if-let [binding (frame var)]
      (and (= binding x) (succeed frame xs))
      (succeed (assoc frame var x) xs))))

(defn match-segment [var]
  (fn [frame xs succeed]
    (let [binding (frame var)]
      (if binding
        ;; the segment value is bound. Ensure that what follows in the
        ;; list corresponds to what is bound.
        (loop [xs xs
               binding binding]
          (if (empty? binding)
            (succeed frame xs)
            (and (not (empty? xs))
                 (= (first xs) (first binding))
                 (recur (rest xs) (rest binding)))))
        ;; the segment value is unbound. Try continuing after binding
        ;; it to all the possible initial sequences of values here.

        (loop [before []
               after xs]
          (or (succeed (assoc frame var before) after)
              (if-not (empty? after)
                (recur (conj before (first after)) (rest after)))))
        ))))

(defn match-list [& matchers]
  (fn [frame xs succeed]
    (letfn [(step [xs matchers frame]
              (cond (not (empty? matchers))
                    ((first matchers) frame xs
                     (fn [new-frame new-xs]
                       (step new-xs (rest matchers) new-frame)))
                    (not (empty? xs)) false
                    (empty? xs) (succeed frame xs)
                    :else false))]
      (step xs matchers frame))))







