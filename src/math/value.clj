(ns math.value
  (:refer-clojure :rename {zero? core-zero?}))

(defprotocol Value
  (numerical? [this])
  (abstract? [this])
  (zero? [this])
  (one? [this])
  (zero-like [this])
  (one-like [this])
  (exact? [this])
  (compound? [this])
  (sort-key [this])
  (freeze [this])
  )

(defn arity
  [f]
  {:pre [(ifn? f)]}
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(def machine-epsilon
  (loop [e 1.0]
    (if (not= 1.0 (+ 1.0 (/ e 2.0)))
      (recur (/ e 2.0))
      e)))

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [ε]
  (fn [x y] (< (Math/abs (- x y)) ε)))

(println "value initialized")
