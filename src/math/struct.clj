(ns math.struct
  (:require [math.generic :as g]))

(defn up [& xs] (with-meta (apply vector xs) {:orientation :up}))
(defn down [& xs] (with-meta (apply vector xs) {:orientation :down}))

(defn- orientation [s]
  (or (:orientation (meta s)) :up))

(def ^:private structure? vector?)

(defn- row? [s]
  (and (structure? s) (= (orientation s) :down)))

(defn- column? [s]
  (and (structure? s) (= (orientation s) :up)))

(defn- with-orientation-of [s t]
  (with-meta t {:orientation (orientation s)}))

(defn- elementwise [op s t]
  (if (= (count s) (count t))
    (with-orientation-of s (vec (map op s t)))
    (throw (IllegalArgumentException.
            (str op " provided arguments of differing length")))))

(defn- scalar-multiply [a s]
  (with-orientation-of s (vec (map #(g/* a %) s))))

(defn- compatible-for-contraction? [s t]
  (and (= (count s) (count t))
       (not= (orientation s) (orientation t))))

(defn inner-product [s t]
  (apply g/+ (map g/* s t)))

(defn outer-product [s t]
  (with-orientation-of t (vec (map #(g/* s %) t))))

(defn- mul [s t]
  (if (compatible-for-contraction? s t)
    (inner-product s t)
    (outer-product s t)))

(g/defhandler :+   [row? row?]          (partial elementwise g/+))
(g/defhandler :+   [column? column?]    (partial elementwise g/+)) 
(g/defhandler :-   [row? row?]          (partial elementwise g/-))
(g/defhandler :-   [column? column?]    (partial elementwise g/-))
(g/defhandler :*   [number? structure?] scalar-multiply)
(g/defhandler :*   [structure? number?] (g/flip scalar-multiply))
(g/defhandler :/   [structure? number?] (fn [s n] (scalar-multiply (/ n) s)))
(g/defhandler :*   [structure? structure?] mul)

(g/defhandler :negate [structure?]
  (fn [s] (with-orientation-of s (vec (map g/negate s)))))

(println "struct initialized")


