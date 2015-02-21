;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.numsymb
  (:require [math.value :as v]
            [math.generic :as g]
            [math.expression :as x]
            [clojure.math.numeric-tower :as nt]))

(declare symbolic-operator-table)
(defn- numerical-expression
  [expr]
  (cond (number? expr) expr
        (symbol? expr) expr
        (g/literal-number? expr) (:expression expr)
        :else (throw (IllegalArgumentException. (str "unknown numerical expression type " expr)))))

(defn make-numsymb-expression [operator operands]
  (let [operand-exprs (map numerical-expression operands)
        v (operator symbolic-operator-table)]
    (if v
      (let [newexp (apply v operand-exprs)]
        (x/literal-number newexp))
      (throw (IllegalArgumentException.
              (str "unknown numeric operator " operator))))))

(defmacro is-expression?
  "True if the expression is a form with symbol at its head."
  [symbol]
  `(fn [x#] (and (seq? x#) (= (first x#) ~symbol))))

(def ^:private sum? (is-expression? '+))
(def ^:private product? (is-expression? '*))
(def ^:private sqrt? (is-expression? 'sqrt))
(def expt? (is-expression? 'expt))

(def operator first)
(def operands rest)

;; BEGIN
;; these are without constructor simplifications!

(defn add [a b]
  (cond (and (number? a) (number? b)) (+ a b)
        (number? a) (cond (g/zero? a) b
                          (sum? b) `(~'+ ~a ~@(operands b))
                          :else `(~'+ ~a ~b))
        (number? b) (cond (g/zero? b) a
                          (sum? a) `(~'+ ~@(operands a) ~b)
                          :else `(~'+ ~a ~b))
        (sum? a) (cond (sum? b) `(~'+ ~@(operands a) ~@(operands b))
                       :else `(~'+ ~@(operands a) ~b))
        (sum? b) `(~'+ ~a ~@(operands b))
        :else `(~'+ ~a ~b)))

(defn- add-n [& args]
  (reduce add 0 args))

(defn- sub [a b]
  (cond (and (number? a) (number? b)) (- a b)
        (number? a) (if (g/zero? a) `(~'- ~b) `(~'- ~a ~b))
        (number? b) (if (g/zero? b) a `(~'- ~a ~b))
        :else `(~'- ~a ~b)))

(defn- sub-n [& args]
  (cond (nil? args) 0
        (nil? (next args)) (g/negate (first args))
        :else (sub (first args) (reduce add (next args)))))

(defn mul [a b]
  (cond (and (number? a) (number? b)) (* a b)
        (number? a) (cond (g/zero? a) a
                          (g/one? a) b
                          (product? b) `(~'* ~a ~@(operands b))
                          :else `(~'* ~a ~b)
                          )
        (number? b) (cond (g/zero? b) b
                          (g/one? b) a
                          (product? a) `(~'* ~@(operands a) ~b)
                          :else `(~'* ~a ~b)
                          )
        (product? a) (cond (product? b) `(~'* ~@(operands a) ~@(operands b))
                           :else `(~'* ~@(operands a) ~b))
        (product? b) `(~'* ~a ~@(operands b))
        :else `(~'* ~a ~b)))

(defn- mul-n [& args]
  (reduce mul 1 args))

(defn- div [a b]
  (cond (and (number? a) (number? b)) (/ a b)
        (number? a) (if (g/zero? a) a `(~'/ ~a ~b))
        (number? b) (cond (g/zero? b) (throw (ArithmeticException. "division by zero"))
                          (g/one? b) a
                          :else `(~'/ ~a ~b))
        :else `(~'/ ~a ~b)))

(defn- div-n [& args]
  (cond (nil? args) 1
        (nil? (next args)) (div 1 (first args))
        :else (div (first args) (reduce mul (next args)))))

;; END

;;
;; TRIG
;;

(def ^:private relative-integer-tolerance (* 100 v/machine-epsilon))
(def ^:private absolute-integer-tolerance 1e-20)

(defn- almost-integer? [x] ;; XXX make this private
  (or (integer? x)
      (and (float? x)
           (let [x (double x)
                 z (Math/round x)]
             (if (zero? z)
               (< (Math/abs x) absolute-integer-tolerance)
               (< (Math/abs (/ (- x z) z)) relative-integer-tolerance))))))

(def ^:private pi Math/PI)
(def ^:private pi-over-4 (/ pi 4))
(def ^:private two-pi (* 2 pi))
(def ^:private pi-over-2 (* 2 pi-over-4))
;; (def ^:private pi-over-3 (/ pi 3))
;; (def ^:private pi-over-6 (/ pi-over-2 3))

(defn- n:zero-mod-pi? [x]
  (almost-integer? (/ x pi)))
(def ^:private symb:zero-mod-pi? #{'-pi 'pi '-two-pi 'two-pi})
(defn- n:pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (- x pi-over-2 two-pi))))
(def ^:private symb:pi-over-2-mod-2pi? #{'pi-over-2})
(defn- n:-pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (+ x pi-over-2) two-pi)))
(def ^:private symb:-pi-over-2-mod-2pi? #{'-pi-over-2})
(defn- n:pi-mod-2pi? [x]
  (almost-integer? (/ (- x pi) two-pi)))
(def ^:private symb:pi-mod-2pi? #{'-pi 'pi})
(defn- n:pi-over-2-mod-pi? [x]
  (almost-integer? (/ (- x pi-over-2) pi)))
(def ^:private symb:pi-over-2-mod-pi? #{'-pi-over-2 'pi-over-2})
(defn- n:zero-mod-2pi? [x]
  (almost-integer? (/ x two-pi)))
(def ^:private symb:zero-mod-2pi? #{'-two-pi 'two-pi})
(defn- n:-pi-over-4-mod-pi? [x]
  (almost-integer? (/ (+ x pi-over-4) pi)))
(def ^:private symb:-pi-over-4-mod-pi? #{'-pi-over-4})
(defn- n:pi-over-4-mod-pi? [x]
  (almost-integer? (/ (- x pi-over-4) pi)))
(def ^:private symb:pi-over-4-mod-pi? #{'pi-over-4 '+pi-over-4})

(defn- sine [x]
  (cond (number? x) (if (v/exact? x)
                      (if (zero? x) 0 `(~'sin ~x))
                      (cond (n:zero-mod-pi? x) 0.0
                            (n:pi-over-2-mod-2pi? x) 1.0
                            (n:-pi-over-2-mod-2pi? x) -1.0
                            :else (Math/sin x)))
        (symbol? x) (cond (symb:zero-mod-pi? x) 0
                          (symb:pi-over-2-mod-2pi? x) 1
                          (symb:-pi-over-2-mod-2pi? x) -1
                          :else `(~'sin ~x))
        :else `(~'sin ~x)))

(defn- cosine [x]
  (cond (number? x) (if (v/exact? x)
                      (if (zero? x) 1 `(~'cos ~x))
                      (cond (n:pi-over-2-mod-pi? x) 0.0
                            (n:zero-mod-2pi? x) 1.0
                            (n:pi-mod-2pi? x) -1.0
                            :else (Math/cos x)))
        (symbol? x) (cond (symb:pi-over-2-mod-pi? x) 0
                          (symb:zero-mod-2pi? x) +1
                          (symb:pi-mod-2pi? x) -1
                          :else `(~'cos ~x))
        :else `(~'cos ~x)))

(defn- tangent [x]
  (cond (number? x) (if (v/exact? x)
                      (if (zero? x) 0 `(~'tan ~x))
                      (cond (n:zero-mod-pi? x) 0.
                            (n:pi-over-4-mod-pi? x) 1.
                            (n:-pi-over-4-mod-pi? x) -1.
                            (n:pi-over-2-mod-pi? x)
                              (throw (IllegalArgumentException. "Undefined: tan"))
                            :else `(~'tan ~x)))
        (symbol? x) (cond (symb:zero-mod-pi? x) 0
                          (symb:pi-over-4-mod-pi? x) 1
                          (symb:-pi-over-4-mod-pi? x) -1
                          (symb:pi-over-2-mod-pi? x)
                            (throw (IllegalArgumentException. "Undefined: tan"))
                          :else `(~'tan ~x))
        :else `(~'tan ~x)))

(defn- abs [x]
  (cond (number? x) (if (< x 0) (- x) x)
        :else `(~'abs ~x)))

(defn- sqrt [s]
  (if (number? s)
    (if-not (g/exact? s)
      (nt/sqrt s)
      (cond (g/zero? s) s
            (g/one? s) :one
            :else (let [q (nt/sqrt s)]
                    (if (g/exact? q)
                      q
                      `(~'sqrt ~s)))))
    `(~'sqrt ~s)))

(defn- log [s]
  (if (number? s)
    (if-not (v/exact? s)
      (Math/log s)
      (if (g/one? s) 0 `(~'log ~s)))
    `(~'log ~s)))

(defn- exp [s]
  (if (number? s)
    (if-not (v/exact? s)
      (Math/exp s)
      (if (g/zero? s) 1 `(~'exp ~s)))
    `(~'exp ~s)))

(defn expt [b e]
  (cond (and (number? b) (number? e)) (nt/expt b e)
        (number? b) (cond (g/one? b) 1
                          :else `(~'expt ~b ~e))
        (number? e) (cond (g/zero? e) 1
                          (g/one? e) b
                          (and (integer? e) (even? e) (sqrt? b))
                          (expt (first (operands b)) (quot e 2))
                          (and (expt? b)
                               (number? (second (operands b)))
                               (integer? (* (second (operands b)) e)))
                          (expt (first (operands b))
                                (* (second (operands b)) e))
                          (< e 0) (div-n 1 (expt b (- e)))
                          :else `(~'expt ~b ~e))
        :else `(~'expt ~b ~e)
        ))

(def ^:private g-symbolic-operator-table
  {'+ :+
   '- :-
   '* :*
   '/ :div
   'negate :negate
   'invert :invert
   'sin :sin
   'cos :cos
   'tan :tan
   'cube :cube
   'square :square
   'abs :abs
   'sqrt :sqrt
   'log :log
   'exp :exp
   'expt :**})

;; TODO: We learn at long last why using keywords instead of symbols was going
;; to wind up annoying us. (We chose them to kind of escape the symbol namespacing,
;; since keywords don't have them, but now we have this duplication, and so we have
;; to decide if we're going to stick with keywords or not.

(defn symbolic-operator
  "Given a symbol (like g/+) returns an applicable if there is a corresponding
  symbolic operator construction available."
  [s]
  (-> s g-symbolic-operator-table symbolic-operator-table))

(def ^:private symbolic-operator-table
  {:+ add-n
   :- sub-n
   :* mul-n
   :div div-n
   :negate #(sub 0 %)
   :invert #(div 1 %)
   :sin sine
   :cos cosine
   :tan tangent
   :cube #(expt % 3)
   :square #(expt % 2)
   :abs abs
   :sqrt sqrt
   :log log
   :exp exp
   :** expt})

(println "numsymb initialized")
