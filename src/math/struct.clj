(ns math.struct
  (:require [math.generic :as g]))

(defn up [& xs] (with-meta (apply vector xs) {:type :up}))
(defn down [& xs] (with-meta (apply vector xs) {:type :down}))

(defn- row? [s] (= (g/typeof s) :down))
(defn- column? [s] (= (g/typeof s) :up))
(defn- structure? [s]  (or (row? s) (column? s)))

(defn- elementwise [op s t]
  (if (= (count s) (count t))
    (with-meta (vec (map op s t)) {:type (:type (meta s))} )
    (throw (IllegalArgumentException.
            (str op " provided arguments of differing length")))))

(defn- scalar-multiply [a s]
  (with-meta (vec (map #(g/mul a %) s)) {:type (:type (meta s))}))

(g/defhandler :+   [row? row?]          (partial elementwise g/add))
(g/defhandler :+   [column? column?]    (partial elementwise g/add)) 
(g/defhandler :-   [row? row?]          (partial elementwise g/sub))
(g/defhandler :-   [column? column?]    (partial elementwise g/sub))
(g/defhandler :*   [number? structure?] scalar-multiply)
(g/defhandler :*   [structure? number?] (g/flip scalar-multiply))
(g/defhandler :/   [structure? number?] (fn [s n] (scalar-multiply (/ n) s)))
(g/defhandler :-   [structure?]         g/neg)
(g/defhandler :neg [structure?]
  (fn [s] (with-meta (vec (map g/neg s)) {:type (:type (meta s))})))

(println "struct initialized")


