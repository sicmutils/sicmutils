;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.generic
  "The home of most of the SICMUtils extensible generic operations. The bulk of
  the others live in [[sicmutils.value]].

  See [the `Generics`
  cljdocs](https://cljdoc.org/d/sicmutils/sicmutils/CURRENT/doc/basics/generics)
  for a detailed discussion of how to use and extend the generic operations
  defined in [[sicmutils.generic]] and [[sicmutils.value]]."
  (:refer-clojure :rename {mod core-mod}
                  :exclude [/ + - * divide #?(:cljs mod)])
  (:require [sicmutils.value :as v]
            [sicmutils.util :as u])
  #?(:cljs (:require-macros [sicmutils.generic :refer [defgeneric]]))
  #?(:clj
     (:import [clojure.lang Keyword LazySeq PersistentVector Symbol Var])))

(defmacro ^:private fork
  "I borrowed this lovely, mysterious macro from `macrovich`:
  https://github.com/cgrand/macrovich. This allows us to fork behavior inside of
  a macro at macroexpansion time, not at read time."
  [& {:keys [cljs clj]}]
  (if (contains? &env '&env)
    `(if (:ns ~'&env) ~cljs ~clj)
    (if #?(:clj (:ns &env) :cljs true)
      cljs
      clj)))

(def ^:private lowercase-symbols
  (map (comp symbol str char)
       (range 97 123)))

(defn- arglists
  "returns a list of `:arglists` entries appropriate for a generic function with
  arities between `a` and `b` inclusive."
  [a b]
  (let [arities (if b
                  (range a (inc b))
                  [a])]
    (map #(into [] (take %) lowercase-symbols)
         arities)))

(defmacro ^:private defgeneric
  "Defines a multifn using the provided symbol. Arranges for the multifn
  to answer the :arity message, reporting either [:exactly a] or
  [:between a b], according to the arguments given.

  - `arities` can be either a single or a vector of 2 numbers.

  The `options` allowed differs slightly from `defmulti`:

  - the first optional argument is a docstring.

  - the second optional argument is a dict of metadata. When you query the
  defined multimethod with a keyword, it will pass that keyword along as a query
  to this metadata map. (`:arity` is always overridden if supplied, and `:name`
  defaults to the symbol `f`.)

  Any remaining options are passed along to `defmulti`."
  {:arglists '([name arities docstring? attr-map? & options])}
  [f arities & options]
  (let [[a b]     (if (vector? arities) arities [arities])
        arity     (if b [:between a b] [:exactly a])
        docstring (if (string? (first options))
                    (str "generic " f ".\n\n" (first options))
                    (str "generic " f ))
        options   (if (string? (first options))
                    (next options)
                    options)
        [attr options] (if (map? (first options))
                         [(first options) (next options)]
                         [{} options])
        kwd-klass (fork :clj clojure.lang.Keyword :cljs 'cljs.core/Keyword)
        attr (assoc attr
                    :arity arity
                    :name (:name attr `'~f))]
    `(do
       (defmulti ~f
         ~docstring
         {:arglists '~(arglists a b)}
         v/argument-kind ~@options)
       (defmethod ~f [~kwd-klass] [k#]
         (~attr k#)))))

;; Numeric functions.
(defgeneric add 2
  "Returns the sum of arguments `a` and `b`.

See [[+]] for a variadic version of [[add]]."
  {:name '+
   :dfdx (fn [_ _] 1)
   :dfdy (fn [_ _] 1)})

(defgeneric negate 1
  "Returns the negation of `a`. Equivalent to `([[sub]] 0 a)`."
  {:name '-
   :dfdx (fn [_] -1)})

(defgeneric negative? 1
  "Returns true if the argument `a` is less than `([[value/zero-like]] a)`,
  false otherwise. The default implementation depends on a proper Comparable
  implementation on the type.`")

(defmethod negative? :default [a] (< a (v/zero-like a)))

(defgeneric sub 2
  "Returns the difference of `a` and `b`. Equivalent to `([[add]] a ([[negate]]
  b))`.

See [[-]] for a variadic version of [[sub]]."
  {:name '-
   :dfdx (fn [_ _] 1)
   :dfdy (fn [_ _] -1)})

(defmethod sub :default [a b]
  (add a (negate b)))

(defgeneric mul 2
  "Returns the product of `a` and `b`.

See [[*]] for a variadic version of [[mul]]."
  {:name '*
   :dfdx (fn [_ y] y)
   :dfdy (fn [x _] x)})

(declare div)

(defgeneric invert 1
  {:name '/
   :dfdx (fn [x] (div -1 (mul x x)))})

(defgeneric div 2
  {:name '/
   :dfdx (fn [_ y] (div 1 y))
   :dfdy (fn [x y] (div (negate x)
                       (mul y y)))})

(defmethod div :default [a b]
  (mul a (invert b)))

(defgeneric abs 1)

(defgeneric sqrt 1
  {:dfdx (fn [x]
           (invert
            (mul (sqrt x) 2)))})

(defgeneric quotient 2)

(defgeneric remainder 2)
(defgeneric modulo 2)
(defmethod modulo :default [a b]
  (let [m (remainder a b)]
    (if (or (v/zero? m)
            (= (negative? a)
               (negative? b)))
      m
      (add m b))))

(declare log)

(defgeneric expt 2
  {:dfdx (fn [x y]
           (mul y (expt x (sub y 1))))
   :dfdy (fn [x y]
           (if (and (v/number? x) (v/zero? y))
             (if (v/number? y)
               (if (not (negative? y))
                 0
                 (u/illegal "Derivative undefined: expt"))
               0)
             (mul (log x) (expt x y))))})

(defmethod expt :default [s e]
  {:pre [(v/native-integral? e)]}
  (let [kind (v/kind s)]
    (if-let [mul' (get-method mul [kind kind])]
      (letfn [(expt' [base pow]
                (loop [n pow
                       y (v/one-like base)
                       z base]
                  (let [t (even? n)
                        n (quot n 2)]
                    (cond
                      t (recur n y (mul' z z))
                      (zero? n) (mul' z y)
                      :else (recur n (mul' z y) (mul' z z))))))]
        (cond (pos? e)  (expt' s e)
              (zero? e) (v/one-like e)
              :else (invert (expt' s (negate e)))))
      (u/illegal (str "No g/mul implementation registered for kind " kind)))))

(defgeneric exp 1
  "Returns the base-e exponential of `x`. Equivalent to `(expt e x)`, given
  some properly-defined `e` symbol."
  {:dfdx exp})

(defgeneric exp2 1
  "Returns the base-2 exponential of `x`. Equivalent to `(expt 2 x)`.")

(defmethod exp2 :default [x] (expt 2 x))

(defgeneric exp10 1
  "Returns the base-10 exponential of `x`. Equivalent to `(expt 10 x)`.")

(defmethod exp10 :default [x] (expt 10 x))

(defgeneric log 1
  "Returns the natural logarithm of `x`."
  {:dfdx invert})

(defgeneric log2 1
  "Returns the base-2 logarithm of `x`, ie, $log_2(x)$.")

(let [l2 (Math/log 2)]
  (defmethod log2 :default [x] (div (log x) l2)))

(defgeneric log10 1
  "Returns the base-10 logarithm of `x`, ie, $log_10(x)$.")

(let [l10 (Math/log 10)]
  (defmethod log10 :default [x] (div (log x) l10)))

(defgeneric gcd 2
  "Returns the [greatest common
  divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) of the two
  inputs `a` and `b`.")

(defgeneric lcm 2
  "Returns the [least common
  multiple](https://en.wikipedia.org/wiki/Least_common_multiple) of the two
  inputs `a` and `b`.")

(defgeneric exact-divide 2)

(defgeneric square 1)
(defmethod square :default [x] (expt x 2))

(defgeneric cube 1)
(defmethod cube :default [x] (expt x 3))

;; Trigonometric functions.

(declare sin)

(defgeneric cos 1
  {:dfdx (fn [x] (negate (sin x)))})

(defgeneric sin 1 {:dfdx cos})

(defgeneric asin 1
  {:dfdx (fn [x]
           (invert
            (sqrt (sub 1 (square x)))))})

(defgeneric acos 1
  {:dfdx (fn [x]
           (negate
            (invert
             (sqrt (sub 1 (square x))))))})

(defgeneric atan [1 2]
  {:dfdx (fn
           ([x]
            (invert (add 1 (square x))))
           ([y x]
            (div x (add (square x)
                        (square y)))))
   :dfdy (fn [y x]
           (div (negate y)
                (add (square x)
                     (square y))))})

(declare sinh)

(defgeneric cosh 1
  {:dfdx sinh})

(defgeneric sinh 1
  {:dfdx cosh})

;; Trig functions with default implementations provided.
(defgeneric tan 1
  {:dfdx (fn [x]
           (invert
            (square (cos x))))})

(defmethod tan :default [x] (div (sin x) (cos x)))

(defgeneric cot 1)
(defmethod cot :default [x] (div (cos x) (sin x)))

(defgeneric sec 1)
(defmethod sec :default [x] (invert (cos x)))

(defgeneric csc 1)
(defmethod csc :default [x] (invert (sin x)))

(defgeneric tanh 1
  {:dfdx (fn [x]
           (sub 1 (square (tanh x))))})

(defmethod tanh :default [x] (div (sinh x) (cosh x)))

(defgeneric sech 1)
(defmethod sech :default [x] (invert (cosh x)))

(defgeneric csch 1)
(defmethod csch :default [x] (invert (sinh x)))

(defgeneric acosh 1)
(defmethod acosh :default [x]
  (mul 2 (log (add
               (sqrt (div (add x 1) 2))
               (sqrt (div (sub x 1) 2))))))

(defgeneric asinh 1)
(defmethod asinh :default [x]
  (log (add x (sqrt (add 1 (square x))))))

(defgeneric atanh 1)
(defmethod atanh :default [x]
  (div (sub (log (add 1 x))
            (log (sub 1 x)))
       2))

;; Complex Operators

(defgeneric make-rectangular 2)
(defgeneric make-polar 2)
(defgeneric real-part 1)
(defgeneric imag-part 1)
(defgeneric magnitude 1)
(defgeneric angle 1)
(defgeneric conjugate 1)

;; Operations on structures

(defgeneric transpose 1)
(defgeneric trace 1)
(defgeneric determinant 1)
(defgeneric dimension 1)
(defgeneric dot-product 2)
(defgeneric inner-product 2)
(defgeneric outer-product 2)
(defgeneric cross-product 2)

;; Structure Defaults

(defmethod transpose [::v/scalar] [a] a)
(defmethod trace [::v/scalar] [a] a)
(defmethod determinant [::v/scalar] [a] a)
(defmethod dimension [::v/scalar] [a] 1)
(defmethod dot-product [::v/scalar ::v/scalar] [l r] (mul l r))
(defmethod inner-product [::v/scalar ::v/scalar] [l r] (mul (conjugate l) r))

;; More advanced generic operations.
(defgeneric Lie-derivative 1)

(defmulti partial-derivative v/argument-kind)
(defmethod partial-derivative [Keyword] [k]
  (k {:arity [:exactly 2]
      :name 'partial-derivative}))

(defmulti simplify v/argument-kind)
(defmethod simplify :default [a] (v/freeze a))
(defmethod simplify [::v/number] [a] a)
(defmethod simplify [Var] [a] (-> a meta :name))
(defmethod simplify [Symbol] [a] a)
(defmethod simplify [LazySeq] [a] (map simplify a))
(defmethod simplify [PersistentVector] [a] (mapv simplify a))
(defmethod simplify [v/seqtype] [a]
  (map simplify a))

(defn ^:private bin+ [a b]
  (cond (v/zero? a) b
        (v/zero? b) a
        :else (add a b)))

(defn + [& args]
  (reduce bin+ 0 args))

(defn ^:private bin- [a b]
  (cond (v/zero? b) a
        (v/zero? a) (negate b)
        :else (sub a b)))

(defn - [& args]
  (cond (nil? args) 0
        (nil? (next args)) (negate (first args))
        :else (bin- (first args) (reduce bin+ (next args)))))

(defn ^:private bin* [a b]
  (cond (and (v/numerical? a) (v/zero? a)) (v/zero-like b)
        (and (v/numerical? b) (v/zero? b)) (v/zero-like a)
        (v/one? a) b
        (v/one? b) a
        :else (mul a b)))

;;; In bin* we test for exact (numerical) zero
;;; because it is possible to produce a wrong-type
;;; zero here, as follows:
;;;
;;;               |0|             |0|
;;;       |a b c| |0|   |0|       |0|
;;;       |d e f| |0| = |0|, not  |0|
;;;
;;; We are less worried about the v/zero? below,
;;; because any invertible matrix is square.

(defn * [& args]
  (reduce bin* 1 args))

(defn- bin-div [a b]
  (cond (v/one? b) a
        :else (div a b)))

(defn / [& args]
  (cond (nil? args) 1
        (nil? (next args)) (invert (first args))
        :else (bin-div (first args) (reduce bin* (next args)))))

(def divide /)

(defn factorial
  "Returns the factorial of `n`, ie, the product of 1 to n inclusive."
  [n]
  (reduce * (range 1 (inc n))))

;; This call registers a symbol for any non-multimethod we care about. These
;; will be returned instead of the actual function body when the user
;; calls `(v/freeze fn)`, for example.

(v/add-object-symbols!
 {+ '+
  * '*
  - '-
  / '/
  clojure.core/+ '+
  clojure.core/* '*
  clojure.core/- '-
  clojure.core// '/
  clojure.core/mod 'modulo
  clojure.core/quot 'quotient
  clojure.core/rem 'remainder
  clojure.core/neg? 'negative?})
