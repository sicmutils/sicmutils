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
  "Implementations of the generic operations for numeric types that have
  optimizations available, and for the general symbolic case."
  (:require [sicmutils.complex :as c]
            [sicmutils.euclid]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.ratio]
            [sicmutils.value :as v]
            [sicmutils.util :as u]))

(defn ^:private is-expression?
  "Returns a function which will decide if its argument is a sequence
  commencing with s."
  [s]
  (fn [x] (and (seq? x) (= (first x) s))))

(def sum? (is-expression? '+))
(def product? (is-expression? '*))
(def sqrt? (is-expression? 'sqrt))
(def expt? (is-expression? 'expt))
(def quotient? (is-expression? '/))
(def arctan? (is-expression? 'atan))
(def operator first)
(def operands rest)

(defn- with-exactness-preserved
  "Returns a wrapper around f that attempts to preserve exactness if the input is
  numerically exact, else passes through to f."
  [f sym-or-fn]
  (let [process (if (symbol? sym-or-fn)
                  (fn [s] (list sym-or-fn s))
                  sym-or-fn)]
    (fn [s]
      (if (v/number? s)
        (let [q (f s)]
          (if-not (v/exact? s)
            q
            (if (v/exact? q)
              q
              (process s))))
        (process s)))))

;; these are without constructor simplifications!

(defn- add [a b]
  (cond (and (v/number? a) (v/number? b)) (g/add a b)
        (v/number? a) (cond (v/zero? a) b
                            (sum? b) `(~'+ ~a ~@(operands b))
                            :else `(~'+ ~a ~b))
        (v/number? b) (cond (v/zero? b) a
                            (sum? a) `(~'+ ~@(operands a) ~b)
                            :else `(~'+ ~a ~b))
        (sum? a) (cond (sum? b) `(~'+ ~@(operands a) ~@(operands b))
                       :else `(~'+ ~@(operands a) ~b))
        (sum? b) `(~'+ ~a ~@(operands b))
        :else `(~'+ ~a ~b)))

(defn- sub [a b]
  (cond (and (v/number? a) (v/number? b)) (g/sub a b)
        (v/number? a) (if (v/zero? a) `(~'- ~b) `(~'- ~a ~b))
        (v/number? b) (if (v/zero? b) a `(~'- ~a ~b))
        (= a b) 0
        :else `(~'- ~a ~b)))

(defn- sub-n [& args]
  (cond (nil? args) 0
        (nil? (next args)) (g/negate (first args))
        :else (sub (first args) (reduce add (next args)))))

(defn- mul [a b]
  (cond (and (v/number? a) (v/number? b)) (g/mul a b)
        (v/number? a) (cond (v/zero? a) a
                            (v/one? a) b
                            (product? b) `(~'* ~a ~@(operands b))
                            :else `(~'* ~a ~b)
                            )
        (v/number? b) (cond (v/zero? b) b
                            (v/one? b) a
                            (product? a) `(~'* ~@(operands a) ~b)
                            :else `(~'* ~a ~b)
                            )
        (product? a) (cond (product? b) `(~'* ~@(operands a) ~@(operands b))
                           :else `(~'* ~@(operands a) ~b))
        (product? b) `(~'* ~a ~@(operands b))
        :else `(~'* ~a ~b)))

(defn- div [a b]
  (cond (and (v/number? a) (v/number? b)) (g/div a b)
        (v/number? a) (if (v/zero? a) a `(~'/ ~a ~b))
        (v/number? b) (cond (v/zero? b) (u/arithmetic-ex "division by zero")
                            (v/one? b) a
                            :else `(~'/ ~a ~b))
        :else `(~'/ ~a ~b)))

(defn- div-n [arg & args]
  (cond (nil? arg) 1
        (nil? args) (g/invert arg)
        :else (div arg (reduce mul args))))

;; ## Trig Functions

(def ^:private relative-integer-tolerance (* 100 v/machine-epsilon))
(def ^:private absolute-integer-tolerance 1e-20)

(defn- almost-integer? [x]
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

(defn- sin
  "Implementation of sine that attempts to apply optimizations at the call site.
  If it's not possible to do this (if the expression is symbolic, say), returns
  a symbolic form."
  [x]
  (cond (v/number? x) (if (v/exact? x)
                        (if (v/zero? x) 0 (list 'sin x))
                        (cond (n:zero-mod-pi? x) 0
                              (n:pi-over-2-mod-2pi? x) 1
                              (n:-pi-over-2-mod-2pi? x) -1
                              :else (Math/sin x)))
        (symbol? x) (cond (symb:zero-mod-pi? x) 0
                          (symb:pi-over-2-mod-2pi? x) 1
                          (symb:-pi-over-2-mod-2pi? x) -1
                          :else (list 'sin x))
        :else (list 'sin x)))

(defn- cos
  "Implementation of cosine that attempts to apply optimizations at the call site.
  If it's not possible to do this (if the expression is symbolic, say), returns
  a symbolic form."
  [x]
  (cond (v/number? x) (if (v/exact? x)
                        (if (v/zero? x) 1 (list 'cos x))
                        (cond (n:pi-over-2-mod-pi? x) 0
                              (n:zero-mod-2pi? x) 1
                              (n:pi-mod-2pi? x) -1
                              :else (Math/cos x)))
        (symbol? x) (cond (symb:pi-over-2-mod-pi? x) 0
                          (symb:zero-mod-2pi? x) +1
                          (symb:pi-mod-2pi? x) -1
                          :else (list 'cos x))
        :else (list 'cos x)))

(defn- tan
  "Implementation of tangent that attempts to apply optimizations at the call site.
  If it's not possible to do this (if the expression is symbolic, say), returns
  a symbolic form."
  [x]
  (cond (v/number? x) (if (v/exact? x)
                        (if (v/zero? x) 0 (list 'tan x))
                        (cond (n:zero-mod-pi? x) 0
                              (n:pi-over-4-mod-pi? x) 1
                              (n:-pi-over-4-mod-pi? x) -1
                              (n:pi-over-2-mod-pi? x) (u/illegal "Undefined: tan")
                              :else (Math/tan x)))
        (symbol? x) (cond (symb:zero-mod-pi? x) 0
                          (symb:pi-over-4-mod-pi? x) 1
                          (symb:-pi-over-4-mod-pi? x) -1
                          (symb:pi-over-2-mod-pi? x) (u/illegal "Undefined: tan")
                          :else (list 'tan x))
        :else (list 'tan x)))

(defn- csc [x]
  (if (v/number? x)
    (if-not (v/exact? x)
      (g/csc x)
      (if (v/zero? x)
        (u/illegal (str "Zero argument -- g/csc" x))
        `(~'/ 1 ~(sin x))))
    `(~'/ 1 ~(sin x))))

(defn- sec [x]
  (if (v/number? x)
    (if-not (v/exact? x)
      (g/sec x)
      (if (v/zero? x)
        1
        `(~'/ 1 ~(cos x))))
    `(~'/ 1 ~(cos x))))

(defn- asin [x]
  (if (v/number? x)
    (if-not (v/exact? x)
      (g/asin x)
      (if (v/zero? x)
        0
        (list 'asin x)))
    (list 'asin x)))

(defn- acos [x]
  (if (v/number? x)
    (if-not (v/exact? x)
      (g/acos x)
      (if (v/one? x)
        0
        (list 'acos x)))
    (list 'acos x)))

(defn- atan
  ([y]
   (if (v/number? y)
     (if-not (v/exact? y)
       (g/atan y)
       (if (v/zero? y)
         0
         (list 'atan y)))
     (list 'atan y)))
  ([y x]
   (if (v/one? x)
     (atan y)
     (if (v/number? y)
       (if (v/exact? y)
         (if (v/zero? y)
           0
           (if (v/number? x)
             (if (v/exact? x)
               (if (v/zero? x)
                 (g/atan y x)
                 (list 'atan y x))
               (g/atan y x))
             (list 'atan y x)))
         (if (v/number? x)
           (g/atan y x)
           (list 'atan y x)))
       (list 'atan y x)))))

(defn- cosh [x]
  (if (v/number? x)
    (if-not (v/exact? x)
      (g/cosh x)
      (if (v/zero? x)
        1
        (list 'cosh x)))
    (list 'cosh x)))

(defn- sinh [x]
  (if (v/number? x)
    (if-not (v/exact? x)
      (g/sinh x)
      (if (v/zero? x)
        0
        (list 'sinh x)))
    (list 'sinh x)))

(defn- abs
  "Symbolic expression handler for abs."
  [x]
  (if (v/number? x)
    (g/abs x)
    (list 'abs x)))

(def sqrt
  "Square root implementation that attempts to preserve exact numbers wherever
  possible. If the incoming value is not exact, simply computes sqrt."
  (with-exactness-preserved g/sqrt 'sqrt))

(def ^:private log
  "Attempts to preserve exact precision if the argument is exact; else, evaluates
  symbolically or numerically."
  (with-exactness-preserved g/log 'log))

(def ^:private exp
  "Attempts to preserve exact precision if the argument is exact; else, evaluates
  symbolically or numerically."
  (with-exactness-preserved g/exp 'exp))

(defn- expt
  "Attempts to preserve exact precision if either argument is exact; else,
  evaluates symbolically or numerically."
  [b e]
  (cond (and (v/number? b) (v/number? e)) (g/expt b e)
        (v/number? b) (cond (v/one? b) 1
                            :else `(~'expt ~b ~e))
        (v/number? e) (cond (v/zero? e) 1
                            (v/one? e) b
                            (and (integer? e) (even? e) (sqrt? b))
                            (expt (first (operands b)) (quot e 2))
                            (and (expt? b)
                                 (v/number? (second (operands b)))
                                 (integer? (* (second (operands b)) e)))
                            (expt (first (operands b))
                                  (* (second (operands b)) e))
                            (< e 0) (div-n 1 (expt b (- e)))
                            :else `(~'expt ~b ~e))
        :else `(~'expt ~b ~e)))

(defn- negate [x] (sub 0 x))
(defn- invert [x] (div 1 x))

;; ## Complex Operations

(def ^:private conjugate-transparent-operators
  #{'negate 'invert 'square 'cube
    'sqrt
    'exp 'exp2 'exp10
    'log 'log2 'log10
    'sin 'cos 'tan 'sec 'csc
    'asin 'acos 'atan
    'sinh 'cosh 'tanh 'sech 'csch
    '+ '- '* '/ 'expt 'up 'down})

(defn- make-rectangular [r i]
  (cond (v/exact-zero? i) r

        (and (v/real? r) (v/real? i))
        (g/make-rectangular r i)

        :else (add r (mul c/I i))))

(defn- make-polar [m a]
  (cond (v/exact-zero? m) m
        (v/exact-zero? a) m
        (and (v/real? m) (v/real? a)) (g/make-polar m a)
        :else (mul m (add
                      (cos a)
                      (mul c/I (sin a))))))

(defn- conjugate [z]
  (cond (v/number? z) (g/conjugate z)
        (and (seq? z)
             (contains? conjugate-transparent-operators
                        (operator z)))
        (cons (operator z) (map conjugate (operands z)))
        :else (list 'conjugate z)))

(def ^:private magnitude
  (with-exactness-preserved g/magnitude
    (fn [a] (sqrt (mul (conjugate a) a)))))

(defn- real-part [z]
  (if (v/number? z)
    (g/real-part z)
    (mul (g/div 1 2)
         (add z (conjugate z)))))

(defn- imag-part [z]
  (if (v/number? z)
    (g/imag-part z)
    (mul (g/div 1 2)
         (mul (c/complex 0 -1)
              (sub z (conjugate z))))))

(def ^:private angle
  (with-exactness-preserved g/angle
    (fn [z]
      (atan (imag-part z)
            (real-part z)))))

;; ## Table

(def ^:private symbolic-operator-table
  {'+ #(reduce add 0 %&)
   '- sub-n
   '* #(reduce mul 1 %&)
   '/ div-n
   'negate negate
   'invert invert
   'sin sin
   'cos cos
   'tan tan
   'asin asin
   'acos acos
   'atan atan
   'sinh sinh
   'cosh cosh
   'sec sec
   'csc csc
   'cube #(expt % 3)
   'square #(expt % 2)
   'abs abs
   'sqrt sqrt
   'log log
   'exp exp
   'expt expt
   'make-rectangular make-rectangular
   'make-polar make-polar
   'real-part real-part
   'imag-part imag-part
   'conjugate conjugate
   'magnitude magnitude
   'angle angle})

(defn symbolic-operator
  "Given a symbol (like '+) returns an applicable operator if there is a
  corresponding symbolic operator construction available."
  [s]
  (symbolic-operator-table s))
