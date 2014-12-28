(ns math.numbers
  (:refer-clojure :rename {zero? core-zero?})
  (:require [math.generic :as g]
            [math.numsymb :as ns]
            [clojure.math.numeric-tower :as nt]))

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
(make-binary-operation :div / false)
(make-binary-operation :** nt/expt false)
(make-unary-operation :sin #(Math/sin %))
(make-unary-operation :cos #(Math/cos %))
(make-unary-operation :tan #(Math/tan %))
(make-unary-operation :square #(* % %))
(make-unary-operation :cube #(* % % %))
(make-unary-operation :abs nt/abs)
(make-unary-operation :negate -)
(make-unary-operation :invert /)
(make-unary-operation :sqrt nt/sqrt)
(make-unary-operation :log #(Math/log %))
(make-unary-operation :exp #(Math/exp %))

(println "numbers initialized")
