(ns math.core
  (:require [math.poly :as poly])
  (:gen-class))

(def p0 (poly/make))
(def p1 (poly/make [2 1] [0 1]))
(def p2 (poly/make [2 2] [1 3]))

(defn modular
  "Arithmetic with op (mod n)"
  [n op a b]
  (mod (op a b) n))

(def +m12 (partial modular 12 +))
(def -m12 (partial modular 12 -))
(def *m12 (partial modular 12 *))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println p2)
  (println "boo")
  ;(println (map #(vector (first %) (- (second %)))  p2))   
  ;(println (map (fn [[k v]] [k (- 0 v)])  p2))   
  (println (poly/neg p2))
  (println (+m12 13 4))
  (println (-m12 (*m12 (+m12 5 8) 3) 7))
  (println ((partial modular 17 +) 13 11))
  (println ((partial modular 17 -) 13 11))
  (println ((partial modular 17 *) 13 11))
  (println (poly/add p0 p2))
  (println (poly/add p2 p0))
  (println (poly/add p0 p0))
  (println "hello")
  (println (poly/add p1 p1))
  (println (poly/add p1 p2))
  (println (poly/sub p1 p1))
  (println (poly/sub p1 p2))
  (println (poly/sub p2 p1)))
