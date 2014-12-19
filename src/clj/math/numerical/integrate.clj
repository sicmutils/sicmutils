(ns math.numerical.integrate
  (:import [math.numerical Simpson]))

;; simple Simpson's rule to get things off the ground

(defn integrate [f a b]
  (Simpson/integrate 1e-8 f a b))
