(ns math.numbers
  (:require [math.generic :as g]
            [math.numsymb :as ns]))

;; still to be done: constant folding across expressions

(extend-protocol g/Value
  Long
  (id+? [x] (zero? x))
  (id*? [x] (= x 1))
  (zero-like [x] 0)
  Double
  (id+? [x] (zero? x))
  (id*? [x] (= x 1.0))
  (zero-like [x] 0.0))

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
(g/defhandler :neg [number?] -)

(g/defhandler :inv [number?] /)
;; still need g/flip? XXX

(g/defhandler :*   [number? number?] *)
(g/defhandler :*   [g/abstract-number? g/abstract-number?] (make-numerical-combination :*))
(g/defhandler :*   [number? g/abstract-number?] (make-numerical-combination :*))
(g/defhandler :*   [g/abstract-number? number?] (make-numerical-combination :* :reversed))

(g/defhandler :/   [number? number?] /)
(g/defhandler :/   [g/abstract-number? g/abstract-number?] (make-numerical-combination :/))
(g/defhandler :/   [number? g/abstract-number?] (make-numerical-combination :/))
(g/defhandler :/   [g/abstract-number? number?] (make-numerical-combination :/))

(println "numbers initialized")
