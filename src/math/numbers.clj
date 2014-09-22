(ns math.numbers
  (:require [math.generic :as g]))

;; still to be done: constant folding across expressions

(extend-protocol g/Value
  Long
  (id+? [x] (zero? x))
  (id*? [x] (= x 1))
  Double
  (id+? [x] (zero? x))
  (id*? [x] (= x 1.0))
  )

(defn- n-plus-x [n x]
  (if (g/id+? n) x `(g/add ~n ~x)))

(defn- n-minus-x [n x]
  (if (g/id+? n) (g/sub x) `(g/sub ~n ~x)))

(defn- x-minus-n [x n]
  (if (g/id+? n) x `(g/sub ~x ~n)))

(defn- n-times-x [n x]
  (if (g/id*? n) x `(g/mul ~n ~x)))

(defn- x-div-n [x n]
  (cond (g/id*? n) x
        (g/id+? n) (throw (IllegalArgumentException. "division by zero"))
        :else `(g/div ~x ~n)))

(defn- n-div-x [n x]
  (if (g/id+? n) 0 `(g/div ~n ~x)))

(g/defhandler :+   [number? number?] +)
(g/defhandler :+   [symbol? number?] (g/flip n-plus-x))
(g/defhandler :+   [number? symbol?] n-plus-x)
(g/defhandler :+   [symbol? symbol?] n-plus-x)
(g/defhandler :-   [number? number?] -)
(g/defhandler :-   [symbol? number?] x-minus-n)
(g/defhandler :-   [number? symbol?] n-minus-x)
(g/defhandler :-   [symbol? symbol?] n-minus-x)
(g/defhandler :-   [number?]         -)
(g/defhandler :neg [number?]         -)
(g/defhandler :-   [symbol?]         (fn [x] `(g/neg ~x)))
(g/defhandler :*   [number? number?] *)
(g/defhandler :*   [symbol? number?] (g/flip n-times-x))
(g/defhandler :*   [number? symbol?] n-times-x)
(g/defhandler :*   [symbol? symbol?] n-times-x)
(g/defhandler :/   [number? number?] /)
(g/defhandler :/   [symbol? number?] x-div-n)
(g/defhandler :/   [number? symbol?] n-div-x)
(g/defhandler :/   [symbol? symbol?] x-div-n)
(g/defhandler :/   [number?]         /)
(g/defhandler :/   [symbol?]         (fn [x] `(g/div ~x)))

(println "numbers initialized")
