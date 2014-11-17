(ns math.value
  (:refer-clojure :rename {zero? core-zero?}))

(defprotocol Value
  (zero? [this])
  (one? [this])
  (zero-like [this])
  (one-like [this])
  (exact? [this])
  (compound? [this])
  (sort-key [this])
  ;; should we do this or have applicables extend IFn?
  ;; (apply-to [this these])
  )
