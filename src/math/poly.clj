(ns math.poly
  (:refer-clojure :exclude [merge] :rename {map core-map})
  (:gen-class))

(defn make [& oc-pairs]
  (with-meta 
    (into (sorted-map) (filter (fn [[o c]] (not= c 0)) oc-pairs))
    {:type :poly}))

;; should we rely on the constructors and manipulators never to allow
;; a zero coefficient into the list, or should we change degree to
;; scan for nonzero coefficients? In the normal case, there would be
;; none, but in corner cases it would still be robust.

(defn degree [p]
  (or (first (first (rseq p))) 0))

(defn- map [f p]
  (into (empty p) (core-map #(vector (first %) (f (second %))) p)))

(def neg (partial map -))

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
  (loop [P p
         Q q
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
              :else (recur P (rest Q) (assoc R oq (f cq))))))))


(def add (partial merge +))
(def sub (partial merge -))

(defn mul [p q]
  (reduce add (make) (for [[op cp] p [oq cq] q] [[(+ op oq) (* cp cq)]])))

