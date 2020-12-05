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
  (:refer-clojure :rename {mod core-mod}
                  :exclude [/ + - * divide #?(:cljs mod)])
  (:require [sicmutils.value :as v]
            [sicmutils.util :as u])
  #?(:cljs (:require-macros [sicmutils.generic :refer [def-generic-function]]))
  #?(:clj
     (:import [clojure.lang LazySeq PersistentVector Symbol Var])))

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

(defmacro ^:private def-generic-function
  "Defines a multifn using the provided symbol. Arranges for the multifn
  to answer the :arity message, reporting either [:exactly a] or
  [:between a b], according to the arguments given.

  - `arities` can be either a single number or a list of numbers.

  The `options` allowed differs slightly from `defmulti`:

  - the first optional argument is a docstring.

  - the second optional argument is a dict of metadata. When you query the
  defined multimethod with a keyword, it will pass that keyword along as a query
  to this metadata map. (`:arity` is always overridden if supplied, and `:name`
  defaults to the symbol `f`.)

  Any remaining options are passed along to `defmulti`."
  {:arglists '([name arities docstring? attr-map? & options])}
  [f arities & options]
  (let [[a b] (if (vector? arities) arities [arities])
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
       (defmulti ~f ~docstring v/argument-kind ~@options)
       (defmethod ~f [~kwd-klass] [k#]
         (~attr k#)))))

;; Numeric functions.
(def-generic-function add 2    {:name '+})
(def-generic-function negate 1 {:name '-})
(def-generic-function negative? 1
  "Returns true if the argument `a` is less than `(v/zero-like a), false
  otherwise. The default implementation depends on a proper Comparable
  implementation on the type.`")
(defmethod negative? :default [a] (< a (v/zero-like a)))

(def-generic-function sub 2 {:name '-})
(defmethod sub :default [a b]
  (add a (negate b)))

(def-generic-function mul 2 {:name '*})
(def-generic-function invert 1 {:name '/})

(def-generic-function div 2 {:name '/})
(defmethod div :default [a b] (mul a (invert b)))

(def-generic-function abs 1)
(def-generic-function sqrt 1)
(def-generic-function quotient 2)

(def-generic-function remainder 2)
(def-generic-function modulo 2)
(defmethod modulo :default [a b]
  (let [m (remainder a b)]
    (if (or (v/zero? m)
            (= (negative? a)
               (negative? b)))
      m
      (add m b))))

(def-generic-function expt 2)
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

(def-generic-function exp 1
  "Returns the base-e exponential of `x`. Equivalent to `(expt e x)`, given
  some properly-defined `e` symbol.")

(def-generic-function exp2 1
  "Returns the base-2 exponential of `x`. Equivalent to `(expt 2 x)`.")

(defmethod exp2 :default [x] (expt 2 x))

(def-generic-function exp10 1
  "Returns the base-10 exponential of `x`. Equivalent to `(expt 10 x)`.")

(defmethod exp10 :default [x] (expt 10 x))

(def-generic-function log 1
  "Returns the natural logarithm of `x`.")

(def-generic-function log2 1
  "Returns the base-2 logarithm of `x`, ie, $log_2(x)$.")

(let [l2 (Math/log 2)]
  (defmethod log2 :default [x] (div (log x) l2)))

(def-generic-function log10 1
  "Returns the base-10 logarithm of `x`, ie, $log_10(x)$.")

(let [l10 (Math/log 10)]
  (defmethod log10 :default [x] (div (log x) l10)))

(def-generic-function gcd 2)
(def-generic-function lcm 2)
(def-generic-function exact-divide 2)

(def-generic-function square 1)
(defmethod square :default [x] (expt x 2))

(def-generic-function cube 1)
(defmethod cube :default [x] (expt x 3))

;; Trigonometric functions.
(def-generic-function cos 1)
(def-generic-function sin 1)
(def-generic-function asin 1)
(def-generic-function acos 1)
(def-generic-function atan [1 2])
(def-generic-function cosh 1)
(def-generic-function sinh 1)

;; Trig functions with default implementations provided.
(def-generic-function tan 1)
(defmethod tan :default [x] (div (sin x) (cos x)))

(def-generic-function cot 1)
(defmethod cot :default [x] (div (cos x) (sin x)))

(def-generic-function sec 1)
(defmethod sec :default [x] (invert (cos x)))

(def-generic-function csc 1)
(defmethod csc :default [x] (invert (sin x)))

(def-generic-function tanh 1)
(defmethod tanh :default [x] (div (sinh x) (cosh x)))

(def-generic-function sech 1)
(defmethod sech :default [x] (invert (cosh x)))

(def-generic-function csch 1)
(defmethod csch :default [x] (invert (sinh x)))

(def-generic-function acosh 1)
(defmethod acosh :default [x]
  (mul 2 (log (add
               (sqrt (div (add x 1) 2))
               (sqrt (div (sub x 1) 2))))))

(def-generic-function asinh 1)
(defmethod asinh :default [x]
  (log (add x (sqrt (add 1 (square x))))))

(def-generic-function atanh 1)
(defmethod atanh :default [x]
  (div (sub (log (add 1 x))
            (log (sub 1 x)))
       2))

;; Operations on structures

(def-generic-function transpose 1)
(def-generic-function trace 1)
(def-generic-function determinant 1)
(def-generic-function dimension 1)

;; Defaults

(defmethod transpose [::v/scalar] [a] a)
(defmethod trace [::v/scalar] [a] a)
(defmethod determinant [::v/scalar] [a] a)
(defmethod dimension [::v/scalar] [a] 1)

(def-generic-function cross-product 2)

;; Complex Operators
(def-generic-function real-part 1)
(def-generic-function imag-part 1)
(def-generic-function magnitude 1)
(def-generic-function angle 1)
(def-generic-function conjugate 1)

;; More advanced generic operations.
(def-generic-function Lie-derivative 1)

(defmulti partial-derivative v/argument-kind)

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

(defn ^:private bin-div [a b]
  (cond (v/one? b) a
        :else (div a b)))

(defn / [& args]
  (cond (nil? args) 1
        (nil? (next args)) (invert (first args))
        :else (bin-div (first args) (reduce bin* (next args)))))

(def divide /)

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
