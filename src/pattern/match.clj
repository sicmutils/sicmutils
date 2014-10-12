(ns pattern.match)

(def ^:private zero [{} nil])

(defn match-one
  "Combinator which succeeds iff the head of the data is equal to
  thing. The frame is not modified."
  [thing]
  (fn [frame xs succeed]
    (and (sequential? xs)
         (= (first xs) thing)
         (succeed frame (next xs)))))

(defn match-var
  "If the variable is not bound in the frame, and there is more
  matchable data, this combinator will succeed by consuming the next
  item in the data and produces a frame in which the data seen is
  bound to the pattern variable. If the variable is bound, then the
  value seen must match the binding to succeed (the frame is not
  modified in this case)."
  [var]
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

(defn variable-reference?
  "True if x is a variable reference (i.e., it looks like (:? ...))"
  [x]
  (and (sequential? x)
       (= (first x) :?)))

(defn segment-reference?
  "True if x is a segment reference (i.e., it looks like (:?? ...))"
  [x]
  (and (sequential? x)
       (= (first x) :??)))

(defn variable
  "Return the variable contained in a variable or segment reference
  form"
  [x]
  (second x))

(defn pattern->matcher
  "Given a pattern (which is essentially a form consisting of
  constants mixed with pattern variables) returns a match combinator
  for the pattern."
  [pattern]
  (if (sequential? pattern)
    (cond (variable-reference? pattern) (match-var (variable pattern))
          (segment-reference? pattern) (match-segment (variable pattern))
          :else (apply match-list (map pattern->matcher pattern)))
    (match-one pattern)))

(defn match
  "Convenience function for applying a match combinator to some data.
  Primes the process with an empty frame and supplies a continuation
  which will return the pattern bindings if the match is successful,
  nil otherwise."
  [matcher data]
  (let [receive (fn [frame data] (if (empty? data) frame))]
    (matcher {} (list data) receive)))
