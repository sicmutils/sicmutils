(ns math.numbers
  (:require [math.generic :as g]
            [math.numsymb :as ns]))

;; still to be done: constant folding across expressions

(extend-protocol g/Value
  Long
  (id+? [x] (zero? x))
  (id*? [x] (= x 1))
  Double
  (id+? [x] (zero? x))
  (id*? [x] (= x 1.0))
  )

;; nb. these become obsolete when make-numerical-computation is deployed!
(defn- n-times-x [n x]
  (if (g/id*? n) x `(g/mul ~n ~x)))

(defn- x-div-n [x n]
  (cond (g/id*? n) x
        (g/id+? n) (throw (IllegalArgumentException. "division by zero"))
        :else `(g/div ~x ~n)))

(defn- n-div-x [n x]
  (if (g/id+? n) 0 `(g/div ~n ~x)))

;; this one belongs here

;; (define (make-numerical-combination operator #!optional reverse?)
;;   (if (default-object? reverse?)
;;       (lambda operands 
;; 	(make-numsymb-expression operator operands))
;;       (lambda operands 
;; 	(make-numsymb-expression operator (reverse operands)))))

(defn- make-numerical-combination [operator & reversed]
  (if reversed
    (fn [& operands]
      (ns/make-numsymb-expression operator (reverse operands)))
    (fn [& operands]
      (ns/make-numsymb-expression operator operands))))

(g/defhandler :+   [number? number?] +)
(g/defhandler :+   [g/abstract-number? g/abstract-number?] (make-numerical-combination :+))
(g/defhandler :+   [number? g/abstract-number?] (make-numerical-combination :+))
(g/defhandler :+   [g/abstract-number? number?] (make-numerical-combination :+ :reversed))
(g/defhandler :-   [number? number?] -)
(g/defhandler :-   [g/abstract-number? g/abstract-number?] (make-numerical-combination :-))
(g/defhandler :-   [number? g/abstract-number?] (make-numerical-combination :-))
(g/defhandler :-   [g/abstract-number? number?] (make-numerical-combination :- :reversed))
(g/defhandler :neg [g/abstract-number?] (make-numerical-combination :negate))
;;(g/defhandler :-   [number?]         -)
(g/defhandler :neg [number?]         -)
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
