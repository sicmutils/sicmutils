#_
"Copyright © 2021 Sam Ritchie.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

(ns sicmutils.polynomial.factor
  "This namespace contains functions for factoring polynomials and symbolic
  expressions."
  (:require [clojure.walk :as w]
            [pattern.rule :as r :refer [=> rule-simplifier]
             #?@(:cljs [:include-macros true])]
            [sicmutils.expression :as x]
            [sicmutils.expression.analyze :as a]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial :as poly]
            [sicmutils.polynomial.gcd :refer [gcd gcd-Dp]]
            [sicmutils.simplify.rules :as rules]
            [sicmutils.util.logic :as ul]
            [sicmutils.value :as v]))

(defn split-polynomial
  "Given a [[Polynomial]] `p`, returns a sequence of factors of in order of
  increasing power.

  The first element is a constant factor, the next is a factor with power 1, and
  so on."
  [p]
  (letfn [(answer [tracker const]
            (let [final (peek tracker)]
              (if (v/number? final)
                (into [final] (subvec (conj (pop tracker) 1) 1))
                (into [const] (subvec tracker 1)))))]
    (loop [m 0
           h p
           tracker []
           old-s p
           old-m 1]
      (if (v/one? m)
        (answer tracker h)
        (let [gg (gcd-Dp h)
              new-s (poly/evenly-divide h (gcd h gg))
              new-m (gcd gg new-s)

              ;; facts gets all the factors that were completely removed last
              ;; step, i.e. all those that were to the 1 or 2 power. The first
              ;; loop through will get a totally wrong `facts`, but its gcd with
              ;; the initial old-m=1 will be 1, so it won't result in incorrect
              ;; doublefacts or singlefacts.
              facts (poly/evenly-divide old-s new-s)

              ;; doublefacts gets all the factors which were to the power x > 1,
              ;; x <= 2, (ergo x=2), in the last step.
              doublefacts (gcd facts old-m)

              ;; takes out p = all factors only to the 1st power.
              singlefacts (poly/evenly-divide new-s new-m)]
          (recur new-m
                 ;; the following has all factors to the 1 or 2 power
                 ;; completely removed, others now to the power-2.
                 (poly/evenly-divide h (poly/mul new-m new-s))

                 ;; tracker of the form
                 ;;  h(vi) = (* (exponent (get tracker k) k))
                 (conj tracker doublefacts singlefacts)
                 new-s
                 new-m))))))

;; ## Symbolic Expression Factoring

(defn ^:no-doc factors->expression
  "Given some sequence of polynomial factors ordered by increasing power,
  symbolically evaluates each power and generates a symbolic expression
  representing the product of all factors.

  For example:

  ```clojure
  (factors->expression ['c 'x 'y 1 'z])
  ;;=> (* c x (expt y 2) (expt z 4))
  ```"
  [factors]
  (let [expt (sym/symbolic-operator 'expt)]
    (cons '* (map-indexed
              (fn [i f]
                (if (zero? i)
                  f
                  (expt f i)))
              factors))))

(def ^{:private true
       :doc "Simplifier that flattens nested products, converts singleton calls
  like `(* x) => x`, and squashes no-argument products like `(*)` into a
  constant `1`."}
  simplify-product
  (rule-simplifier
   (rules/associative '*)
   (rules/unary-elimination '*)
   (rules/constant-elimination '* 1)
   (r/rule (*) => 1)))

(defn poly->factored-expression
  "Given a polynomial `p`, and a sequence of variables `vars` (one for each
  indeterminate in `p`), returns a symbolic expression representing the product
  of all factors of `p`.

  Optionally accepts a `simplify` function that will be called on each factor of
  exponent 0, 1, 2 etc. Defaults to `identity`."
  ([p vars]
   (poly->factored-expression p vars identity))
  ([p vars simplify]
   (let [factors (map (fn [factor]
                        (simplify
                         (poly/->expression factor vars)))
                      (split-polynomial p))]
     (simplify-product
      (factors->expression factors)))))

(defn factor-expression
  "Given some symbolic expression containing only polynomial operations, returns a
  factored version of the expression with basic simplifications applied.

  Optionally accepts a `simplify` function that will be called on each factor of
  exponent 0, 1, 2 etc. Defaults to `identity`.

  NOTE prefer [[factor]], as [[factor]] can handle expressions with
  non-polynomial operations. The trigonometric functions, for example."
  ([expr]
   (factor-expression expr identity))
  ([expr simplify]
   (let [unwrapped (x/expression-of expr)
         cont #(poly->factored-expression %1 %2 simplify)]
     (poly/expression-> unwrapped cont))))

(def ^{:doc "Expression analyzer, identical to [[polynomial/analyzer]] except
  the symbolic expressions returned are in factored form."}
  analyzer
  (let [symgen (a/monotonic-symbol-generator "-f-")]
    (-> (reify a/ICanonicalize
          (expression-> [_ expr cont v-compare]
            (poly/expression-> expr cont v-compare))
          (->expression [_ p vars]
            (poly->factored-expression p vars))
          (known-operation? [_ o]
            (a/known-operation? poly/analyzer o)))
        (a/make-analyzer symgen))))

(def ^{:doc "Accepts a single symbolic expression and returns a factored version
 of that expression.

 Differs from [[factor-expression]] in that it can handle any expression, not
 just expressions limited to polynomial operations."
       :arglists '([expr])}
  factor
  (a/default-simplifier analyzer))

;; ## Square Root Simplification

(defn- process-sqrt
  "Given an unwrapped symbolic expression of the form `(sqrt x)`, returns a new,
  unsimplified symbolic expression with any even power removed from underneath
  the square root.

  For example:

  ```clojure
  (process-sqrt
    '(sqrt (* x (expt y 2) (expt z 4))))
  ;;=> (* (sqrt x) y (expt z 2))
  ```"
  [expr]
  (let [fact-exp (factor (first (sym/operands expr)))
        expt  (sym/symbolic-operator 'expt)
        *     (sym/symbolic-operator '*)
        sqrt  (sym/symbolic-operator 'sqrt)
        even? (fn [n]
                (and (v/native-integral? n)
                     (even? n)))]
    (loop [factors (if (sym/product? fact-exp)
                     (sym/operands fact-exp)
                     [fact-exp])
           odds  1
           evens 1]
      (if (empty? factors)
        (do (when-not (= evens 1)
              (ul/assume! `(~'non-negative? ~evens)
                          'root-out-squares))
            (* (sqrt odds) evens))
        (let [[f & more] factors]
          (if (sym/expt? f)
            (let [[b e] (sym/operands f)]
              (if-not (even? e)
                (recur more
                       (* f odds)
                       evens)
                (recur more
                       odds
                       (let [power (quot e 2)]
                         (cond (> power 1) (* evens (expt b power))
                               (= power 1) (* evens b)
                               :else evens)))))
            (recur more
                   (* f odds)
                   evens)))))))

(defn root-out-squares
  "Given an unwrapped symbolic expression, returns a new symbolic expression with
  any perfect square (exponent with an even power) removed from underneath any
  `sqrt` that appears in the expression.

  To use [[root-out-squares]] with a wrapped symbolic expression,
  use [[sicmutils.expression/fmap]]."
  [expr]
  (w/prewalk
   (fn [t]
     (if (sym/sqrt? t)
       (process-sqrt t)
       t))
   expr))
