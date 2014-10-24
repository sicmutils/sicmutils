(ns math.integral)

;; simple Simpson's rule to get things off the ground

(defn- simpson-rule [n a b f]
  (let [h (/ (- b a) n)
        s (+ (f a) (f b))
        fi (fn [i] (f (+ a (* i h))))
        t (reduce + (for [i (range 1 n 2)] (* 4 (fi i))))
        u (reduce + (for [i (range 2 (dec n) 2)] (* 2 (fi i))))]
    (/ (* h (+ s t u)) 3)
   ))

(defn integrate [a b f]
  (simpson-rule 1000 a b f))
