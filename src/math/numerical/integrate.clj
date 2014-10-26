(ns math.numerical.integrate)

;; simple Simpson's rule to get things off the ground

(defn- simpson-rule
  "Break the interval of integration [a,b] into n intervals, then
  apply Simpsons's rule to integrate the function f"
  [n f a b]
  (let [h (/ (- b a) n)
        s (+ (f a) (f b))
        fi (fn [i] (f (+ a (* i h))))
        t (reduce + (for [i (range 1 n 2)] (* 4 (fi i))))
        u (reduce + (for [i (range 2 (dec n) 2)] (* 2 (fi i))))]
    (/ (* h (+ s t u)) 3)
   ))

(defn integrate [f a b]
  (simpson-rule 1000 f a b))
