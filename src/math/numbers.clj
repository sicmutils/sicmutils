(ns math.numbers
  (:refer-clojure :rename {zero? core-zero?})
  (:require [math.generic :as g]
            [math.numsymb :as ns]))

;; still to be done: constant folding across expressions

(extend-protocol g/Value
  Long
  (zero? [x] (= x 0))
  (one? [x] (= x 1))
  (zero-like [x] 0)
  (exact? [x] true)
  Double
  (zero? [x] (= x 0.0))
  (one? [x] (= x 1.0))
  (zero-like [x] 0.0)
  (exact? [x] false)
  )

(defn- make-numerical-combination
  ([operator] (make-numerical-combination operator identity))
  ([operator transform-operands]
     (fn [& operands]
       (ns/make-numsymb-expression operator (transform-operands operands)))))

(defn- make-binary-operation [key operation commutative?]
  (g/defhandler key [number? number?] operation)
  (g/defhandler key [g/abstract-number? g/abstract-number?] (make-numerical-combination key))
  (g/defhandler key [number? g/abstract-number?] (make-numerical-combination key))
  (g/defhandler key [g/abstract-number? number?]
    (make-numerical-combination key (if commutative? reverse identity))))

(g/defhandler :negate [g/abstract-number?] (make-numerical-combination :negate))
(g/defhandler :negate [number?] -)
(g/defhandler :invert [number?] /)
;; interestingly, if we were to use :sin for number? we get the
;; heuristic simplificatins, which seem useful; but GJS's distribution
;; doesn't do this
(g/defhandler :sin [number?] #(Math/sin %))
(g/defhandler :sin [g/abstract-number?] (make-numerical-combination :sin))

(g/defhandler :zero? [number?] #(core-zero? %))

(make-binary-operation :+ + true)
(make-binary-operation :* * true)
(make-binary-operation :- - false)
(make-binary-operation :/ / false)

(println "numbers initialized")
