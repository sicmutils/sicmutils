;;
;; Copyright © 2020 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.abstract.number
  (:require [sicmutils.complex :as c]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as sym]
            [sicmutils.simplify :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang Symbol))))

(extend-protocol v/Value
  Symbol
  (zero? [o] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (exact? [sym] false)
  (numerical? [_] true)
  (freeze [o] o)
  (kind [_] Symbol))

(defn literal-number
  "Returns its argument, wrapped in a marker type that responds to the generic
  operations registered in `sicmutils.numsymb`.

  Symbols are automatically treated as `literal-number` instances, so

  (* 10 (literal-number 'x))

  is equivalent to

  (* 10 'x).

  If you pass an actual number, sicmutils will attempt to preserve exact values
  through various operations:

  (g/+ 1 (g/cos (g/* 2 (literal-number 4))))
  ;;=> (+ 1 (cos 8))

  Notice that the `(g/* 2 ...)` is evaluated, but `cos` evaluation is deferred,
  since the result is inexact. On the other hand, if the number is inexact to
  begin with:

  (g/+ 1 (g/cos (g/* 2 (literal-number 2.2))))
  ;;=> 0.6926671300215806

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
  - some object wrapped by a call to `literal-number
  - a symbol (which implicitly acts as a literal number)

  See [[literal-number?]] for a similar function that won't respond true to
  symbols, only to explicit symbolic expressions or wrapped literal numbers."
  [x]
  (or (literal-number? x)
      (symbol? x)))

;; ## Generic Installation

(derive Symbol ::x/numeric)
(derive ::x/numeric ::v/scalar)

(defn- literal=num
  "Equality helper; if the left side's a specifically numerical literal, unwrap
  and compare (otherwise false)."
  [l n]
  (and (abstract-number? l)
       (= (x/expression-of l) n)))

;; This installs equality into `v/=` between symbolic expressions (and symbols,
;; see inheritance above) and anything in the standard numeric tower.

(defmethod v/= [::x/numeric ::v/number] [l r] (literal=num l r))
(defmethod v/= [::v/number ::x/numeric] [l r] (literal=num r l))
(defmethod v/= [::x/numeric ::x/numeric] [l r]
  (= (x/expression-of l)
     (x/expression-of r)))

(defn- numerical-expression
  "For literal numbers, returns the unwrapped form. Else acts as identity. (If
  you've made it here, you've chosen to be absorbed into a `literal-number`
  wrapper!)"
  [expr]
  (if (literal-number? expr)
    (x/expression-of expr)
    expr))

(defn- defunary [generic-op op-sym]
  (if-let [op (sym/symbolic-operator op-sym)]
    (defmethod generic-op [::x/numeric] [a]
      (literal-number (op (numerical-expression a))))
    (defmethod generic-op [::x/numeric] [a]
      (x/literal-apply ::x/numeric op-sym [a]))))

(defn- defbinary [generic-op op-sym]
  (let [pairs [[::x/numeric ::x/numeric]
               [::v/number ::x/numeric]
               [::x/numeric ::v/number]]]
    (if-let [op (sym/symbolic-operator op-sym)]
      (doseq [[l r] pairs]
        (defmethod generic-op [l r] [a b]
          (literal-number
           (op (numerical-expression a)
               (numerical-expression b)))))

      (doseq [[l r] pairs]
        (defmethod generic-op [l r] [a b]
          (x/literal-apply ::x/numeric op-sym [a b]))))))

(defbinary g/add '+)
(defbinary g/sub '-)
(defbinary g/mul '*)
(defbinary g/div '/)
(defbinary g/expt 'expt)
(defunary g/negate 'negate)
(defunary g/invert 'invert)

(defunary g/sin 'sin)
(defunary g/cos 'cos)
(defunary g/tan 'tan)

(defunary g/asin 'asin)
(defunary g/acos 'acos)
(defunary g/atan 'atan)
(defbinary g/atan 'atan)

(defunary g/sinh 'sinh)
(defunary g/cosh 'cosh)
(defunary g/sec 'sec)
(defunary g/csc 'csc)

(defunary g/abs 'abs)
(defunary g/sqrt 'sqrt)
(defunary g/log 'log)
(defunary g/exp 'exp)

(defunary g/real-part 'real-part)
(defunary g/imag-part 'imag-part)
(defunary g/magnitude 'magnitude)
(defunary g/angle 'angle)
(defunary g/conjugate 'conjugate)

(defbinary g/gcd 'gcd)
(defmethod g/simplify [::x/numeric] [a]
  (s/simplify-expression (v/freeze a)))
