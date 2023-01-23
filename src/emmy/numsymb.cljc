#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numsymb
  "Implementations of the generic operations for numeric types that have
  optimizations available, and for the general symbolic case."
  (:refer-clojure :exclude [abs])
  (:require [emmy.complex :as c]
            [emmy.euclid]
            [emmy.generic :as g]
            [emmy.numbers]
            [emmy.ratio]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.util.logic :as ul]
            [emmy.value :as v]))

(def ^{:dynamic true
       :doc "When bound to a simplifier (a function from symbolic expression =>
  symbolic expression), this simplifier will be called after every operation
  performed on `emmy.abstract.number` instances.

  `nil` by default."}
  *incremental-simplifier* nil)

(def operator first)
(def operands rest)

(defn- is-expression?
  "Returns a function which will decide if its argument is a sequence commencing
  with s."
  [s]
  (fn [x]
    (and (seq? x)
         (= (operator x) s))))

(def sum? (is-expression? '+))
(def product? (is-expression? '*))
(def sqrt? (is-expression? 'sqrt))
(def expt? (is-expression? 'expt))
(def quotient? (is-expression? '/))
(def arctan? (is-expression? 'atan))
(def derivative? (is-expression? g/derivative-symbol))

(defn iterated-derivative? [expr]
  (and (seq? expr)
       (expt? (operator expr))
       (= g/derivative-symbol
          (second
           (operator expr)))))

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

(defn- mod-rem
  "Modulo and remainder are very similar, so can benefit from a shared set of
  simplifications."
  [a b f sym]
  (cond (and (v/number? a) (v/number? b)) (f a b)
        (= a b) 0
        (v/zero? a) 0
        (v/one? b) a
        :else (list sym a b)))

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

(defn- negate [x] (sub 0 x))

(defn- mul [a b]
  (cond (and (v/number? a) (v/number? b)) (g/mul a b)
        (v/number? a) (cond (v/zero? a) a
                            (v/one? a) b
                            (product? b) `(~'* ~a ~@(operands b))
                            :else `(~'* ~a ~b))
        (v/number? b) (cond (v/zero? b) b
                            (v/one? b) a
                            (product? a) `(~'* ~@(operands a) ~b)
                            :else `(~'* ~a ~b))
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

(defn- invert [x] (div 1 x))

(defn- modulo [a b]
  (mod-rem a b modulo 'modulo))

(defn- remainder [a b]
  (mod-rem a b remainder 'remainder))

(defn- floor [a]
  (if (v/number? a)
    (g/floor a)
    (list 'floor a)))

(defn- ceiling [a]
  (if (v/number? a)
    (g/ceiling a)
    (list 'ceiling a)))

(defn- integer-part [a]
  (if (v/number? a)
    (g/integer-part a)
    (list 'integer-part a)))

(defn- fractional-part [a]
  (if (v/number? a)
    (g/fractional-part a)
    (list 'fractional-part a)))

;; ## Trig Functions

(def ^:private pi Math/PI)
(def ^:private pi-over-4 (/ pi 4))
(def ^:private two-pi (* 2 pi))
(def ^:private pi-over-2 (* 2 pi-over-4))

(defn ^:private n:zero-mod-pi? [x]
  (v/almost-integral? (/ x pi)))

(defn ^:private n:pi-over-2-mod-2pi? [x]
  (v/almost-integral? (/ (- x pi-over-2 two-pi))))

(defn ^:private n:-pi-over-2-mod-2pi? [x]
  (v/almost-integral? (/ (+ x pi-over-2) two-pi)))

(defn ^:private n:pi-mod-2pi? [x]
  (v/almost-integral? (/ (- x pi) two-pi)))

(defn ^:private n:pi-over-2-mod-pi? [x]
  (v/almost-integral? (/ (- x pi-over-2) pi)))

(defn ^:private n:zero-mod-2pi? [x]
  (v/almost-integral? (/ x two-pi)))

(defn ^:private n:-pi-over-4-mod-pi? [x]
  (v/almost-integral? (/ (+ x pi-over-4) pi)))

(defn ^:private n:pi-over-4-mod-pi? [x]
  (v/almost-integral? (/ (- x pi-over-4) pi)))

(def ^:no-doc zero-mod-pi? #{'-pi 'pi '-two-pi 'two-pi})
(def ^:no-doc pi-over-2-mod-2pi? #{'pi-over-2})
(def ^:no-doc -pi-over-2-mod-2pi? #{'-pi-over-2})
(def ^:no-doc pi-mod-2pi? #{'-pi 'pi})
(def ^:no-doc pi-over-2-mod-pi? #{'-pi-over-2 'pi-over-2})
(def ^:no-doc zero-mod-2pi? #{'-two-pi 'two-pi})
(def ^:no-doc -pi-over-4-mod-pi? #{'-pi-over-4})
(def ^:no-doc pi-over-4-mod-pi? #{'pi-over-4 '+pi-over-4})

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
        (symbol? x) (cond (zero-mod-pi? x) 0
                          (pi-over-2-mod-2pi? x) 1
                          (-pi-over-2-mod-2pi? x) -1
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
        (symbol? x) (cond (pi-over-2-mod-pi? x) 0
                          (zero-mod-2pi? x) 1
                          (pi-mod-2pi? x) -1
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
        (symbol? x) (cond (zero-mod-pi? x) 0
                          (pi-over-4-mod-pi? x) 1
                          (-pi-over-4-mod-pi? x) -1
                          (pi-over-2-mod-pi? x) (u/illegal "Undefined: tan")
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
   (cond (v/one? x) (atan y)

         (v/exact-zero? y)
         (if (v/number? x)
           (if (g/negative? x) 'pi 0)
           (and (ul/assume! `(~'non-negative? ~x) 'numsymb-atan)
                0))

         (v/exact-zero? x)
         (if (v/number? y)
           (if (g/negative? y)
             '(- (/ pi 2))
             '(/ pi 2))
           (and (ul/assume! `(~'non-negative? ~y) 'numsymb-atan)
                '(/ pi 2)))

         (and (v/number? x)
              (v/number? y)
              (or (not (v/exact? x))
                  (not (v/exact? y))))
         (g/atan y x)

         :else (list 'atan y x))))

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

(defn- tanh [x]
  (div (sinh x)
       (cosh x)))

(defn- coth [x]
  (div (cosh x)
       (sinh x)))

(defn- sech [x]
  (div 1 (cosh x)))

(defn- csch [x]
  (div 1 (sinh x)))

(defn- acot [x]
  (sub '(/ pi 2) (atan x)))

(defn- abs
  "Symbolic expression handler for abs."
  [x]
  (if (v/number? x)
    (g/abs x)
    (list 'abs x)))

(defn- gcd [a b]
  (cond (and (v/number? a) (v/number? b)) (g/gcd a b)
        (v/number? a) (cond (v/zero? a) b
                            (v/one? a) 1
                            :else (list 'gcd a b))
        (v/number? b) (cond (v/zero? b) a
                            (v/one? b) 1
                            :else (list 'gcd a b))
        (= a b) a
        :else (list 'gcd a b)))

(defn- lcm [a b]
  (cond (and (v/number? a) (v/number? b)) (g/lcm a b)
        (v/number? a) (cond (v/zero? a) 0
                            (v/one? a) b
                            :else (list 'lcm a b))
        (v/number? b) (cond (v/zero? b) 0
                            (v/one? b) a
                            :else (list 'lcm a b))
        (= a b) a
        :else (list 'lcm a b)))

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
                            (< e 0) (invert (expt b (- e)))
                            :else `(~'expt ~b ~e))
        :else `(~'expt ~b ~e)))

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

(defn dot-product
  "Returns the symbolic dot product of the two supplied numbers `z1` and `z2`.

  If both are numbers, defers to [[emmy.generic/dot-product]]. Else,
  returns

  $$\\Re(z_1)\\Re(z_2) + \\Im(z_1)\\Im(z_2)$$"
  [z1 z2]
  (cond (and (v/number? z1) (v/number? z2))
        (g/dot-product z1 z2)

        (v/real? z1) (mul z1 (real-part z2))
        (v/real? z2) (mul (real-part z1) z2)
        :else (add
               (mul (real-part z1)
                    (real-part z2))
               (mul (imag-part z1)
                    (imag-part z2)))))

(defn ^:no-doc derivative
  "Returns the symbolic derivative of the expression `expr`, which should
  represent a function like `f`.

  If the expression is already a derivative like `(D f)` or `((expt D 2) f)`,
  `derivative` will increase the power of the exponent.

  For example:

  ```clojure
  (derivative 'f)              ;;=> (D f)
  (derivative '(D f))          ;;=> ((expt D 2) f)
  (derivative '((expt D 2) f)) ;;=> ((expt D 3) f)
  ```"
  [expr]
  (cond (derivative? expr)
        (let [f (first (operands expr))]
          (list (expt g/derivative-symbol 2)
                f))

        (iterated-derivative? expr)
        (let [pow (nth (operator expr) 2)
              f   (first (operands expr))]
          (list (expt g/derivative-symbol (inc pow))
                f))
        :else
        (list g/derivative-symbol expr)))

;; ## Boolean Operations

(defn- sym:and
  "For symbolic arguments, returns a symbolic expression representing the logical
  conjuction of `l` and `r`.

  If either side is `true?`, returns the other side. If either side is `false?`,
  returns `false`."
  [l r]
  (cond (true? l)  r
        (false? l) l
        (true? r)  l
        (false? r) r
        (= l r)    r
        :else (list 'and l r)))

(defn- sym:or
  "For symbolic arguments, returns a symbolic expression representing the logical
  disjunction of `l` and `r`.

  If either side is `true?`, returns `true`. If either side is `false?`,
  returns the other side."
  [l r]
  (cond (true? l)   l
        (false? l)  r
        (true?  r)  r
        (false?  r) l
        (= l r)     r
        :else (list 'or l r)))

(defn- sym:not
  "For symbolic `x`, returns a symbolic expression representing the logical
  negation of `x`. For boolean `x`, returns the negation of `x`."
  [x]
  (if (boolean? x)
    (not x)
    (list 'not x)))

(defn- sym:bin= [l r]
  (let [num-l? (v/number? l)
        num-r? (v/number? r)]
    (cond (and num-l? num-r?) (v/= l r)
          (or num-l? num-r?)  false
          (= l r)             true
          :else (list '= l r))))

(defn- sym:=
  ([] true)
  ([_] true)
  ([x y] (sym:bin= x y))
  ([x y & more]
   (let [xs    (cons x (cons y more))
         pairs (partition 2 1 xs)]
     (reduce (fn [acc [x y]]
               (if-let [eq (sym:bin= x y)]
                 (sym:and acc eq)
                 (reduced false)))
             true
             pairs))))

(defn- sym:zero? [x]
  (if (v/number? x)
    (v/zero? x)
    (list '= 0 x)))

(defn- sym:one? [x]
  (if (v/number? x)
    (v/one? x)
    (list '= 1 x)))

;; ## Table

(def ^:private symbolic-operator-table
  {'zero? sym:zero?
   'one? sym:one?
   'identity? sym:one?
   '= sym:=
   'not sym:not
   'and (ua/monoid sym:and true false?)
   'or (ua/monoid sym:or false true?)
   'negate negate
   'invert invert
   '+ (ua/monoid add 0)
   '- (ua/group sub add negate 0)
   '* (ua/monoid mul 1 v/zero?)
   '/ (ua/group div mul invert 1 v/zero?)
   'modulo modulo
   'remainder remainder
   'gcd (ua/monoid gcd 0)
   'lcm (ua/monoid lcm 1 v/zero?)
   'floor floor
   'ceiling ceiling
   'integer-part integer-part
   'fractional-part fractional-part
   'sin sin
   'cos cos
   'tan tan
   'sec sec
   'csc csc
   'asin asin
   'acos acos
   'acot acot
   'atan atan
   'sinh sinh
   'cosh cosh
   'tanh tanh
   'coth coth
   'sech sech
   'csch csch
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
   'dot-product dot-product
   'inner-product dot-product
   'angle angle
   'derivative derivative})

(defn symbolic-operator
  "Given a symbol (like `'+`) returns an applicable operator if there is a
  corresponding symbolic operator construction available."
  [s]
  (symbolic-operator-table s))
