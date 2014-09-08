(ns math.poly
  (:refer-clojure :exclude [merge])
  (:gen-class))

(def make sorted-map)

(defn map-poly [f p]
  (into (empty p) (map #(vector (first %) (f (second %))) p)))

(def neg (partial map-poly -))

;;
;; this is ok so far as it doesn't generate an intermediate,
;; but yucky in that it will freeze and reconstitute the sorted
;; map on each step. would be nicer to accumulate a list of pairs
;; and then return (into (sorted-map) R).
;; that should be easy to arrange.
;;
;; or maybe could use merge-with from core. But that doesn't take
;; advantage of sorted-ness, which I think we still want.
;; But its implementation does point to ways this could be simpler.
;;

(defn- merge [f p q]
  (loop [P (seq p)
         Q (seq q)
         R (make)]
    (cond
     (empty? P) (into R Q)
     (empty? Q) (into R P)
     :else (let [[op cp] (first P)
                 [oq cq] (first Q)]
             (cond
              (= op oq) (let [v (f cp cq)]
                          (recur (rest P) (rest Q)
                                 (if (not= v 0)
                                   (assoc R op v)
                                   R)))
              (< op oq) (recur (rest P) Q (assoc R op (f cp)))
              :else (recur P (rest Q) (assoc R oq (f cq)))))
     )))

(def add (partial merge +))
(def sub (partial merge -))

