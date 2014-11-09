(ns math.numbers
  (:refer-clojure :rename {zero? core-zero?})
  (:require [math.generic :as g]
            [math.numsymb :as ns]
            [clojure.math.numeric-tower :as nt]))

;; still to be done: constant folding across expressions

(extend-protocol g/Value
  Long
  (zero? [x] (= x 0))
  (one? [x] (= x 1))
  (zero-like [x] 0)
  (one-like [x] 1)
  (exact? [x] true)
  (compound? [x] false)
  (sort-key [x] 10)
  Double
  (zero? [x] (= x 0.0))
  (one? [x] (= x 1.0))
  (zero-like [x] 0.0)
  (one-like [x] 1.0)
  (exact? [x] false)
  (compound? [x] false)
  (sort-key [x] 10)
  clojure.lang.Ratio
  (zero? [x] (= x 0))
  (one? [x] (= x 1))
  (zero-like [x] 0)
  (one-like [x] 1)
  (exact? [x] true)
  (compound? [x] false)
  (sort-key [x] 10)
  )

(defn- make-numerical-combination
  ([operator] (make-numerical-combination operator false))
  ([operator commutative?]
     (fn [& operands]
       (ns/make-numsymb-expression operator
                                   (if commutative?
                                     (reverse operands)
                                     operands)))))

(defn- make-binary-operation [key operation commutative?]
  (g/defhandler key [number? number?] operation)
  (g/defhandler key [g/abstract-number? g/abstract-number?]
    (make-numerical-combination key))
  (g/defhandler key [number? g/abstract-number?]
    (make-numerical-combination key))
  (g/defhandler key [g/abstract-number? number?]
    (make-numerical-combination key commutative?)))

(defn- make-unary-operation [key operation]
  (g/defhandler key [number?] operation)
  (g/defhandler key [g/abstract-number?] (make-numerical-combination key)))

(make-binary-operation :+ + true)
(make-binary-operation :* * true)
(make-binary-operation :- - false)
(make-binary-operation :/ / false)
(make-binary-operation :** nt/expt false)
(make-unary-operation :sin #(Math/sin %))
(make-unary-operation :cos #(Math/cos %))
(make-unary-operation :square #(* % %))
(make-unary-operation :cube #(* % % %))
(make-unary-operation :abs nt/abs)
(make-unary-operation :negate -)
(make-unary-operation :invert /)
(make-unary-operation :sqrt nt/sqrt)
(make-unary-operation :log #(Math/log %))
(make-unary-operation :exp #(Math/exp %))

(println "numbers initialized")
