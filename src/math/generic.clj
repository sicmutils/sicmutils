(ns math.generic
  (:refer-clojure :rename {map core-type})
  (:gen-class))

;; belongs to generic
(defn type [a]
  (or (:type (meta a))
      (core-type a)))

