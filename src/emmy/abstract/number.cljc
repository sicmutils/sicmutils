#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.abstract.number
  "Symbolic expressions in Emmy are created through the [[literal-number]]
  constructor, or implicitly by performing arithmetic between symbols and
  numbers.

  This namespace implements the [[literal-number]] constructor and installs the
  underlying type into the generic arithmetic system."
  (:require [emmy.expression :as x]
            [emmy.generic :as g]
            [emmy.numsymb :as sym]
            [emmy.simplify :as ss]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang Symbol))))

(extend-type Symbol
  v/Numerical
  (numerical? [_] true)

  v/Value
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (exact? [_] false)
  (freeze [o] o)
  (kind [_] Symbol))

(defn literal-number
  "Returns its argument, wrapped in a marker type that responds to the generic
  operations registered in [[emmy.numsymb]].

  Symbols are automatically treated as [[literal-number]] instances, so

  ```clojure
  (* 10 (literal-number 'x))
  ```

  is equivalent to

  ```clojure
  (* 10 'x)
  ```

  If you pass an actual number, emmy will attempt to preserve exact values
  through various operations:

  ```clojure
  (g/+ 1 (g/cos (g/* 2 (literal-number 4))))
  ;;=> (+ 1 (cos 8))
  ```

  Notice that the `(g/* 2 ...)` is evaluated, but `cos` evaluation is deferred,
  since the result is inexact. On the other hand, if the number is inexact to
  begin with:

  ```clojure
  (g/+ 1 (g/cos (g/* 2 (literal-number 2.2))))
  ;;=> 0.6926671300215806
  ```

  the system will go ahead and evaluate it."
  [x]
  (x/make-literal ::x/numeric x))

(defn literal-number?
  "Returns true if `x` is an explicit symbolic expression or something passed to
  `literal-number`, false otherwise.

  See [[abstract-number?]] for a similar function that also responds true to
  symbols."
  [x]
  (and (x/literal? x)
       (= (x/literal-type x) ::x/numeric)))

(defn abstract-number?
  "Returns true if `x` is:

  - a symbolic expression
  - some object wrapped by a call to [[literal-number]]
  - a symbol (which implicitly acts as a [[literal-number]])

  See [[literal-number?]] for a similar function that won't respond true to
  symbols, only to explicit symbolic expressions or wrapped literal numbers."
  [x]
  (or (literal-number? x)
      (symbol? x)))

;; ## Generic Installation

(derive Symbol ::x/numeric)
(derive ::x/numeric ::v/scalar)

;; This installs equality into `v/=` between symbolic expressions (and symbols,
;; see inheritance above), sequences where appropriate, and anything in the
;; standard numeric tower.

(defmethod v/= [Symbol v/seqtype] [_ _] false)
(defmethod v/= [v/seqtype Symbol] [_ _] false)
(defmethod v/= [Symbol ::v/number] [_ _] false)
(defmethod v/= [::v/number Symbol] [_ _] false)
(defmethod v/= [::x/numeric v/seqtype] [l r] (v/= (x/expression-of l) r))
(defmethod v/= [v/seqtype ::x/numeric] [l r] (v/= l (x/expression-of r)))
(defmethod v/= [::x/numeric ::v/number] [l r] (v/= (x/expression-of l) r))
(defmethod v/= [::v/number ::x/numeric] [l r] (v/= l (x/expression-of r)))
(defmethod v/= [::x/numeric ::x/numeric] [l r]
  (= (x/expression-of l)
     (x/expression-of r)))

(defn- defunary [generic-op op-sym]
  (if-let [op (sym/symbolic-operator op-sym)]
    (defmethod generic-op [::x/numeric] [a]
      (let [newexp (op (x/expression-of a))]
        (literal-number
         (if-let [simplify sym/*incremental-simplifier*]
           (simplify newexp)
           newexp))))

    (defmethod generic-op [::x/numeric] [a]
      (x/literal-apply ::x/numeric op-sym [a]))))

(defn- defbinary [generic-op op-sym]
  (let [pairs [[::x/numeric ::x/numeric]
               [::v/number ::x/numeric]
               [::x/numeric ::v/number]]]
    (if-let [op (sym/symbolic-operator op-sym)]
      (doseq [[l r] pairs]
        (defmethod generic-op [l r] [a b]
          (let [newexp (op (x/expression-of a)
                           (x/expression-of b))]
            (literal-number
             (if-let [simplify sym/*incremental-simplifier*]
               (simplify newexp)
               newexp)))))

      (doseq [[l r] pairs]
        (defmethod generic-op [l r] [a b]
          (x/literal-apply ::x/numeric op-sym [a b]))))))

(defbinary g/add '+)
(defbinary g/sub '-)
(defbinary g/mul '*)
(defbinary g/div '/)
(defbinary g/modulo 'modulo)
(defbinary g/remainder 'remainder)
(defbinary g/expt 'expt)
(defunary g/negate 'negate)
(defunary g/invert 'invert)
(defunary g/integer-part 'integer-part)
(defunary g/fractional-part 'fractional-part)
(defunary g/floor 'floor)
(defunary g/ceiling 'ceiling)

(defunary g/sin 'sin)
(defunary g/cos 'cos)
(defunary g/tan 'tan)
(defunary g/sec 'sec)
(defunary g/csc 'csc)

(defunary g/asin 'asin)
(defunary g/acos 'acos)
(defunary g/atan 'atan)
(defbinary g/atan 'atan)
(defunary g/acot 'acot)

(defunary g/sinh 'sinh)
(defunary g/cosh 'cosh)
(defunary g/tanh 'tanh)
(defunary g/coth 'coth)
(defunary g/sech 'sech)
(defunary g/csch 'csch)

(defunary g/abs 'abs)
(defunary g/sqrt 'sqrt)

(defunary g/log 'log)

(let [log (sym/symbolic-operator 'log)
      div (sym/symbolic-operator '/)]
  (defmethod g/log2 [::x/numeric] [a]
    (let [a (x/expression-of a)]
      (literal-number
       (div (log a)
            (log 2)))))

  (defmethod g/log10 [::x/numeric] [a]
    (let [a (x/expression-of a)]
      (literal-number
       (div (log a) (log 10))))))

(defunary g/exp 'exp)

(defbinary g/make-rectangular 'make-rectangular)
(defbinary g/make-polar 'make-polar)
(defunary g/real-part 'real-part)
(defunary g/imag-part 'imag-part)
(defunary g/magnitude 'magnitude)
(defunary g/angle 'angle)
(defunary g/conjugate 'conjugate)
(defbinary g/dot-product 'dot-product)
(defbinary g/inner-product 'inner-product)

(defbinary g/gcd 'gcd)
(defbinary g/lcm 'lcm)

;; We currently default to `false` here; once literals gain metadata saying
;; whether or not they are negative, we return /something/. Maybe this is
;; ill-founded, but it was required for some polynomial code.
(defmethod g/negative? [::x/numeric] [_] false)

(defmethod g/simplify [Symbol] [a] a)
(defmethod g/simplify [::x/numeric] [a]
  (literal-number
   (ss/simplify-expression
    (x/expression-of a))))

(def ^:private memoized-simplify
  (memoize g/simplify))

(defn ^:no-doc simplify-numerical-expression
  "This function will only simplify instances of [[expression/Literal]]; if `x` is
  of that type, [[simplify-numerical-expression]] acts as a memoized version
  of [[generic/simplify]]. Else, acts as identity.

  This trick is used in [[emmy.calculus.manifold]] to memoize
  simplification _only_ for non-[[differential/Differential]] types."
  [x]
  (if (literal-number? x)
    (memoized-simplify x)
    x))
