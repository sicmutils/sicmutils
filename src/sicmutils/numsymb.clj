;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.numsymb
  (:require [sicmutils
             [value :as v]
             [generic :as g]
             [complex :as c]
             [euclid :as euclid]
             [expression :as x]]
            [clojure.math.numeric-tower :as nt])
  (:import (clojure.lang Symbol)))

(defn ^:private numerical-expression
  [expr]
  (cond (number? expr) expr
        (symbol? expr) expr
        (c/complex? expr) expr
        (g/literal-number? expr) (:expression expr)
        :else (throw (IllegalArgumentException. (str "unknown numerical expression type " expr)))))

(defn ^:private make-numsymb-expression [operator operands]
  (->> operands (map numerical-expression) (apply operator) x/literal-number))

(defn ^:private is-expression?
  "Returns a function which will decide if its argument is a sequence
  commencing with s."
  [s]
  (fn [x] (and (seq? x) (= (first x) s))))

(def ^:private sum? (is-expression? '+))
(def product? (is-expression? '*))
(def sqrt? (is-expression? 'sqrt))
(def expt? (is-expression? 'expt))
(def quotient? (is-expression? (symbol "/")))
(def arctan? (is-expression? 'atan))
(def operator first)
(def operands rest)

;; BEGIN
;; these are without constructor simplifications!

(defn add [a b]
  (cond (and (number? a) (number? b)) (+ a b)
        (number? a) (cond (v/nullity? a) b
                          (sum? b) `(~'+ ~a ~@(operands b))
                          :else `(~'+ ~a ~b))
        (number? b) (cond (v/nullity? b) a
                          (sum? a) `(~'+ ~@(operands a) ~b)
                          :else `(~'+ ~a ~b))
        (sum? a) (cond (sum? b) `(~'+ ~@(operands a) ~@(operands b))
                       :else `(~'+ ~@(operands a) ~b))
        (sum? b) `(~'+ ~a ~@(operands b))
        :else `(~'+ ~a ~b)))

(defn ^:private sub [a b]
  (cond (and (number? a) (number? b)) (- a b)
        (number? a) (if (v/nullity? a) `(~'- ~b) `(~'- ~a ~b))
        (number? b) (if (v/nullity? b) a `(~'- ~a ~b))
        (= a b) 0
        :else `(~'- ~a ~b)))

(defn ^:private sub-n [& args]
  (cond (nil? args) 0
        (nil? (next args)) (g/negate (first args))
        :else (sub (first args) (reduce add (next args)))))

(defn mul [a b]
  (cond (and (number? a) (number? b)) (* a b)
        (number? a) (cond (v/nullity? a) a
                          (v/unity? a) b
                          (product? b) `(~'* ~a ~@(operands b))
                          :else `(~'* ~a ~b)
                          )
        (number? b) (cond (v/nullity? b) b
                          (v/unity? b) a
                          (product? a) `(~'* ~@(operands a) ~b)
                          :else `(~'* ~a ~b)
                          )
        (product? a) (cond (product? b) `(~'* ~@(operands a) ~@(operands b))
                           :else `(~'* ~@(operands a) ~b))
        (product? b) `(~'* ~a ~@(operands b))
        :else `(~'* ~a ~b)))

(defn div [a b]
  (cond (and (number? a) (number? b)) (/ a b)
        (number? a) (if (v/nullity? a) a `(~'/ ~a ~b))
        (number? b) (cond (v/nullity? b) (throw (ArithmeticException. "division by zero"))
                          (v/unity? b) a
                          :else `(~'/ ~a ~b))
        :else `(~'/ ~a ~b)))

(defn ^:private div-n [arg & args]
  (cond (nil? arg) 1
        (nil? args) (g/invert arg)
        :else (div arg (reduce mul args))))

;; END

;;
;; TRIG
;;

(def ^:private relative-integer-tolerance (* 100 v/machine-epsilon))
(def ^:private absolute-integer-tolerance 1e-20)

(defn ^:private almost-integer? [x]
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

(defn ^:private n:zero-mod-pi? [x]
  (almost-integer? (/ x pi)))
(def ^:private symb:zero-mod-pi? #{'-pi 'pi '-two-pi 'two-pi})
(defn ^:private n:pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (- x pi-over-2 two-pi))))
(def ^:private symb:pi-over-2-mod-2pi? #{'pi-over-2})
(defn ^:private n:-pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (+ x pi-over-2) two-pi)))
(def ^:private symb:-pi-over-2-mod-2pi? #{'-pi-over-2})
(defn ^:private n:pi-mod-2pi? [x]
  (almost-integer? (/ (- x pi) two-pi)))
(def ^:private symb:pi-mod-2pi? #{'-pi 'pi})
(defn ^:private n:pi-over-2-mod-pi? [x]
  (almost-integer? (/ (- x pi-over-2) pi)))
(def ^:private symb:pi-over-2-mod-pi? #{'-pi-over-2 'pi-over-2})
(defn ^:private n:zero-mod-2pi? [x]
  (almost-integer? (/ x two-pi)))
(def ^:private symb:zero-mod-2pi? #{'-two-pi 'two-pi})
(defn ^:private n:-pi-over-4-mod-pi? [x]
  (almost-integer? (/ (+ x pi-over-4) pi)))
(def ^:private symb:-pi-over-4-mod-pi? #{'-pi-over-4})
(defn ^:private n:pi-over-4-mod-pi? [x]
  (almost-integer? (/ (- x pi-over-4) pi)))
(def ^:private symb:pi-over-4-mod-pi? #{'pi-over-4 '+pi-over-4})

(defn ^:private sine [x]
  (cond (number? x) (cond (zero? x) 0
                          (n:zero-mod-pi? x) 0
                          (n:pi-over-2-mod-2pi? x) 1
                          (n:-pi-over-2-mod-2pi? x) -1
                          :else (Math/sin x))
        (symbol? x) (cond (symb:zero-mod-pi? x) 0
                          (symb:pi-over-2-mod-2pi? x) 1
                          (symb:-pi-over-2-mod-2pi? x) -1
                          :else `(~'sin ~x))
        :else `(~'sin ~x)))

(defn ^:private arcsine
  [x]
  `(~'asin ~x))

(defn ^:private cosine [x]
  (cond (number? x) (cond (zero? x) 1
                          (n:pi-over-2-mod-pi? x) 0
                          (n:zero-mod-2pi? x) 1
                          (n:pi-mod-2pi? x) -1
                          :else (Math/cos x))
        (symbol? x) (cond (symb:pi-over-2-mod-pi? x) 0
                          (symb:zero-mod-2pi? x) +1
                          (symb:pi-mod-2pi? x) -1
                          :else `(~'cos ~x))
        :else `(~'cos ~x)))

(defn ^:private arccosine
  [x]
  `(~'acos ~x))

(defn ^:private tangent [x]
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

(defn arctangent
  [y & x]
  (if (or (nil? x) (v/unity? (first x)))
    `(~'atan ~y)
    `(~'atan ~y ~@x)))

(defn ^:private abs [x]
  (cond (number? x) (if (< x 0) (- x) x)
        :else `(~'abs ~x)))

(defn sqrt [s]
  (if (number? s)
    (if-not (v/exact? s)
      (nt/sqrt s)
      (cond (v/nullity? s) s
            (v/unity? s) 1
            :else (let [q (nt/sqrt s)]
                    (if (v/exact? q)
                      q
                      `(~'sqrt ~s)))))
    `(~'sqrt ~s)))

(defn ^:private log [s]
  (if (number? s)
    (if-not (v/exact? s)
      (Math/log s)
      (if (v/unity? s) 0 `(~'log ~s)))
    `(~'log ~s)))

(defn ^:private exp [s]
  (if (number? s)
    (if-not (v/exact? s)
      (Math/exp s)
      (if (v/nullity? s) 1 `(~'exp ~s)))
    `(~'exp ~s)))

(defn expt [b e]
  (cond (and (number? b) (number? e)) (nt/expt b e)
        (number? b) (cond (v/unity? b) 1
                          :else `(~'expt ~b ~e))
        (number? e) (cond (v/nullity? e) 1
                          (v/unity? e) b
                          (and (integer? e) (even? e) (sqrt? b))
                          (expt (first (operands b)) (quot e 2))
                          (and (expt? b)
                               (number? (second (operands b)))
                               (integer? (* (second (operands b)) e)))
                          (expt (first (operands b))
                                (* (second (operands b)) e))
                          (< e 0) (div-n 1 (expt b (- e)))
                          :else `(~'expt ~b ~e))
        :else `(~'expt ~b ~e)))

(defn ^:private define-binary-operation
  [generic-operation symbolic-operation]
  (defmethod generic-operation [::x/numerical-expression
                                ::x/numerical-expression]
    [a b]
    (make-numsymb-expression symbolic-operation [a b])))

(defn ^:private define-unary-operation
  [generic-operation symbolic-operation]
  (defmethod generic-operation [::x/numerical-expression]
    [a]
    (make-numsymb-expression symbolic-operation [a])))

(derive Symbol ::x/numerical-expression)
(derive Number ::x/numerical-expression)

(define-binary-operation g/add add)
(define-binary-operation g/sub sub)
(define-binary-operation g/mul mul)
(define-binary-operation g/div div)
(define-binary-operation g/expt expt)
(define-binary-operation g/atan arctangent)
(define-unary-operation g/negate #(sub 0 %))
(define-unary-operation g/invert #(div 1 %))
(define-unary-operation g/sin sine)
(define-unary-operation g/asin arcsine)
(define-unary-operation g/cos cosine)
(define-unary-operation g/acos arccosine)
(define-unary-operation g/tan tangent)
(define-unary-operation g/atan arctangent)
(define-unary-operation g/square #(expt % 2))
(define-unary-operation g/cube #(expt % 3))
(define-unary-operation g/sqrt sqrt)
(define-unary-operation g/exp exp)
(define-unary-operation g/abs abs)
(define-unary-operation g/log log)

(defmethod g/gcd [Number Number] [a b] (euclid/gcd a b))

(def ^:private symbolic-operator-table
  {'+ #(reduce add 0 %&)
   '- sub-n
   '* #(reduce mul 1 %&)
   '/ div-n
   'negate #(sub 0 %)
   'invert #(div 1 %)
   'sin sine
   'asin arcsine
   'cos cosine
   'acos arccosine
   'tan tangent
   'atan arctangent
   'cube #(expt % 3)
   'square #(expt % 2)
   'abs abs
   'sqrt sqrt
   'log log
   'exp exp
   'expt expt})

(defn symbolic-operator
  "Given a symbol (like '+) returns an applicable if there is a corresponding
  symbolic operator construction available."
  [s]
  (symbolic-operator-table s))
