(ns math.numbers
  (:require [math.generic :as g])
  (:gen-class))

;; XXX: these need to do zero-collapse, constant folding etc.

(g/defhandler :+ [number? number?] +)
(g/defhandler :+ [symbol? number?] (fn [a b] `(g/add ~b ~a)))
(g/defhandler :+ [number? symbol?] (fn [a b] `(g/add ~a ~b)))
(g/defhandler :- [number? number?] -)
(g/defhandler :- [symbol? number?] (fn [a b] `(g/sub ~b ~a)))
(g/defhandler :- [number? symbol?] (fn [a b] `(g/sub ~a ~b)))
(g/defhandler :* [number? number?] *)
(g/defhandler :* [symbol? number?] (fn [a b] `(g/mul ~b ~a)))
(g/defhandler :* [number? symbol?] (fn [a b] `(g/mul ~a ~b)))
(g/defhandler :/ [number? number?] /)
(g/defhandler :/ [symbol? number?] (fn [a b] `(g/div ~a ~b)))
(g/defhandler :/ [number? symbol?] (fn [a b] `(g/div ~a ~b)))
(g/defhandler :/ [number?] /)
(g/defhandler :/ [symbol?] (fn [a] `(g/div ~a)))




