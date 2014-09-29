(ns pattern.match)

(defn match-one [thing]
  (fn [[x & xs] frame succeed]
    (and (= x thing)
         (succeed frame xs))))

(defn match-var [var]
  (fn [[x & xs] frame succeed]
    (let [binding (frame var)]
      (if binding
        (and (= binding x) (succeed frame xs))
        (succeed (assoc frame var x) xs)))))

(defn match-segment [var]
  (fn [x frame succeed]
    (let [binding (frame var)]
      (if binding
        ;; the segment value is bound. Ensure that what follows in the
        ;; list corresponds to what is bound.
        (loop [xs x
               binding binding]
          (if (empty? binding)
            (succeed frame xs)
            (and (not (empty? xs))
                 (= (first xs) (first binding))
                 (recur (rest xs) (rest binding)))))
        ;; the segment value is unbound. Try continuing after binding
        ;; it to all the possible initial sequences of values here.
        (loop [before []
               after x]
          (or (succeed (assoc frame var before) after)
              (if-not (empty? after)
                (recur (conj before (first after)) (rest after)))))
        ))))

(defn match-list [& matchers]
  (fn [x frame succeed]
    (letfn [(step [x matchers frame]
              (cond (not (empty? matchers))
                    ((first matchers) x frame
                     (fn [new-frame new-xs]
                       (step new-xs (rest matchers) new-frame)))
                    (not (empty? x)) false
                    (empty? x) (succeed frame x)
                    :else false))]
      (step x matchers frame))))

