;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

;; TODO: remove if-polymorphism
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
  ;; TODO: the number ones should go to the number implementation, not here.
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


;; TODO: thought -- maybe having every type participate is too much.
;; Use derive, and define Number operations when that is convenient.

(defn ^:private arcsine
  [x]
  `(~'asin ~x))

(defn ^:private cosine [x]
  ;; TODO: the number ones should go to the number implementation, not here.
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
  ;; TODO remove if-polymorphism
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
  ;; TODO: if-polymorphism
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

(defmacro ^:private literal-binary-operation
  [generic-operation symbolic-operation expression-type primitive-type]
  `(do
     ))

(defmacro ^:private define-binary-operation
  [generic-operation symbolic-operation]
  `(do
     (defmethod ~generic-operation
       [::x/numerical-expression ::x/numerical-expression]
       [a# b#]
       (make-numsymb-expression ~symbolic-operation [a# b#]))
     (defmethod ~generic-operation
       [clojure.lang.Symbol ::x/numerical-expression]
       [a# b#]
       (x/literal-number (~symbolic-operation a# (:expression b#))))
     (defmethod ~generic-operation
       [::x/numerical-expression clojure.lang.Symbol]
       [a# b#]
       (x/literal-number (~symbolic-operation (:expression a#) b#)))
     (defmethod ~generic-operation
       [clojure.lang.Symbol clojure.lang.Symbol]
       [a# b#]
       (x/literal-number (~symbolic-operation a# b#)))
     (defmethod ~generic-operation
       [clojure.lang.Symbol ::native-numeric-type]
       [a# b#]
       (x/literal-number (~symbolic-operation a# b#)))
     (defmethod ~generic-operation
       [::native-numeric-type clojure.lang.Symbol]
       [a# b#]
       (x/literal-number (~symbolic-operation a# b#)))
     (defmethod ~generic-operation
       [::x/numerical-expression ::native-numeric-type]
       [a# b#]
       (x/literal-number (~symbolic-operation (:expression a#) b#)))
     (defmethod ~generic-operation
       [::native-numeric-type ::x/numerical-expression]
       [a# b#]
       (x/literal-number (~symbolic-operation a# (:expression b#))))
     ))

;; native-numeric-type
;;  - native-exact-type
;;    - native-integral-type
;;      - java.lang.Long
;;      - clojure.lang.BigInt
;;    - clojure.lang.Ratio
;;  - java.lang.Double


(derive ::native-exact-type ::native-numeric-type)
(derive ::native-integral-type ::native-exact-type)
(derive java.lang.Long ::native-integral-type)
(derive java.lang.Double ::native-numeric-type)
(derive clojure.lang.BigInt ::native-integral-type)
(derive java.math.BigInteger ::native-integral-type)
(derive clojure.lang.Ratio ::native-exact-type)

(defmacro ^:private define-unary-operation
  [generic-operation symbolic-operation]
  `(do
     (defmethod ~generic-operation [::x/numerical-expression]
       [a#]
       (x/literal-number (~symbolic-operation (:expression a#))))
     (defmethod ~generic-operation [clojure.lang.Symbol]
       [a#]
       (x/literal-number (~symbolic-operation a#)))
     ))

(define-binary-operation g/add add)
(defmethod g/add [::native-numeric-type ::native-numeric-type] [a b] (+ a b))

(define-binary-operation g/sub sub)
(defmethod g/sub [::native-numeric-type ::native-numeric-type] [a b] (- a b))

(define-binary-operation g/mul mul)
(defmethod g/mul [::native-numeric-type ::native-numeric-type] [a b] (* a b))

(define-binary-operation g/div div)
(defmethod g/div [::native-numeric-type ::native-numeric-type] [a b] (/ a b))

(define-binary-operation g/expt expt)
(defmethod g/expt [::native-numeric-type ::native-numeric-type] [a b] (nt/expt a b))

(define-binary-operation g/atan arctangent)

(define-unary-operation g/negate #(sub 0 %))
(defmethod g/negate [::native-numeric-type] [a] (- a))

(define-unary-operation g/invert #(div 1 %))
(defmethod g/invert [::native-numeric-type] [a] (/ a))

(define-unary-operation g/sin sine)
(defmethod g/sin [::native-numeric-type] [a] (if (zero? a) 0 (Math/sin a)))

(define-unary-operation g/asin arcsine)
(defmethod g/asin [::native-numeric-type]
  [a]
  (if (> (nt/abs a) 1)
    (g/asin (c/complex a))
    (Math/asin a)))

(define-unary-operation g/cos cosine)
(defmethod g/cos [::native-numeric-type] [a] (if (zero? a) 1 (Math/cos a)))

(define-unary-operation g/acos arccosine)
(defmethod g/acos [::native-numeric-type]
  [a]
  (if (> (nt/abs a) 1)
    (g/acos (c/complex a))
    (Math/acos a)))

(define-unary-operation g/tan tangent)
(defmethod g/tan [::native-numeric-type] [a] (if (zero? a) 0 (Math/tan a)))

(define-unary-operation g/atan arctangent)
;(defmethod g/atan)
(defmethod g/atan [::native-numeric-type] [a] (Math/atan a))
(defmethod g/atan [::native-numeric-type ::native-numeric-type] [y x] (Math/atan2 y x))


(define-unary-operation g/square #(expt % 2))
(defmethod g/square [::native-numeric-type] [a] (* a a))

(define-unary-operation g/cube #(expt % 3))
(defmethod g/cube [::native-numeric-type] [a] (* a a a))

(define-unary-operation g/sqrt sqrt)
(defmethod g/sqrt [::native-numeric-type]
  [a]
  (if (< a 0)
    (g/sqrt (c/complex a))
    (nt/sqrt a)))

(define-unary-operation g/exp exp)
(defmethod g/exp [::native-numeric-type] [a] (Math/exp a))

(define-unary-operation g/abs abs)
(defmethod g/abs [::native-numeric-type] [a] (nt/abs a))

(define-unary-operation g/log log)
(defmethod g/log [::native-numeric-type]
  [a]
  (if (< a 0)
    (g/log (c/complex a))
    (Math/log a)))

(defmethod g/gcd
  [::native-integral-type ::native-integral-type]
  [a b]
  (euclid/gcd a b))


(defmethod g/remainder
  [::native-integral-type ::native-integral-type]
  [a b]
  (mod a b))

(defmethod g/quotient
  [::native-integral-type ::native-integral-type]
  [a b]
  (quot a b))

(defn ^:private exact-integer-divide
  [a b]
  {:pre [(zero? (mod a b))]}
  (quot a b))

(defmethod g/exact-divide
  [::native-integral-type ::native-integral-type]
  [a b]
  (exact-integer-divide a b))

(defmethod g/exact-divide
  [::native-exact-type ::native-exact-type]
  [a b]
  (/ a b))

(defmethod g/negative?
  [::native-numeric-type]
  [a]
  (neg? a))

;; TODO: what are the anonymous functions doing here?
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
