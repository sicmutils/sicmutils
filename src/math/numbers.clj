(ns math.numbers
  (:require [math.generic :as g]))

;; still to be done: constant folding across expressions

(extend-protocol g/Value
  java.lang.Long
  (id+? [x] (zero? x))
  (id*? [x] (= x 1))
  java.lang.Double
  (id+? [x] (zero? x))
  (id*? [x] (= x 1.0))
  )

(defn- numeric-add [n x]
  (if (g/id+? n) x `(g/add ~n ~x)))

(defn- numeric-sub1 [n x]
  (if (g/id+? n) (g/sub x) `(g/sub ~n ~x)))

(defn- numeric-sub2 [x n]
  (if (g/id+? n) x `(g/sub ~x ~n)))

(defn- numeric-mul [n x]
  (if (g/id*? n) x `(g/mul ~n ~x)))

(g/defhandler :+   [number? number?] +)
(g/defhandler :+   [symbol? number?] (g/flip numeric-add))
(g/defhandler :+   [number? symbol?] numeric-add)
(g/defhandler :-   [number? number?] -)
(g/defhandler :-   [symbol? number?] numeric-sub2)
(g/defhandler :-   [number? symbol?] numeric-sub1)
(g/defhandler :-   [number?]         -)
(g/defhandler :neg [number?]         -)
(g/defhandler :-   [symbol?]         (fn [x] `(g/neg ~x)))
(g/defhandler :*   [number? number?] *)
(g/defhandler :*   [symbol? number?] (g/flip numeric-mul))
(g/defhandler :*   [number? symbol?] numeric-mul)
(g/defhandler :/   [number? number?] /)
(g/defhandler :/   [symbol? number?] (fn [a b] `(g/div ~a ~b)))
(g/defhandler :/   [number? symbol?] (fn [a b] `(g/div ~a ~b)))
(g/defhandler :/   [number?]         /)
(g/defhandler :/   [symbol?]         (fn [x] `(g/div ~x)))
