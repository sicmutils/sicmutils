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

(ns sicmutils.rational-function
  (:require [clojure.set :as set]
            [sicmutils.expression.analyze :as a]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial :as p]
            [sicmutils.polynomial.gcd :as poly]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj))))

;; TODO fill in!

(declare evaluate)

(deftype RationalFunction [arity u v m]
  v/Value
  (zero? [_] (v/zero? u))
  (one? [_] (and (v/one? u) (v/one? v)))
  (identity? [_] (and (v/identity? u) (v/one? v)))

  (zero-like [_]
    (RationalFunction. arity (v/zero-like u) (v/one-like v) m))

  (one-like [_]
    (RationalFunction. arity (v/one-like u) (v/one-like v) m))

  (identity-like [_]
    (RationalFunction. arity (v/identity-like u) (v/one-like v) m))

  (freeze [_] `(~'/ ~(v/freeze u) ~(v/freeze v)))
  (kind [_] ::rational-function)

  #?@(:clj
      [Object
       (toString [p] (str u " : " v))
       (equals [_ b]
               (and (instance? RationalFunction b)
                    (let [b ^RationalFunction b]
                      (and (= arity (.-arity b))
                           (= u (.-u b))
                           (= v (.-v b))))))

       IObj
       (meta [_] m)
       (withMeta [_ m] (RationalFunction. arity u v m))

       IFn
       (invoke [this a]
               (evaluate this [a]))
       (invoke [this a b]
               (evaluate this [a b]))
       (invoke [this a b c]
               (evaluate this [a b c]))
       (invoke [this a b c d]
               (evaluate this [a b c d]))
       (invoke [this a b c d e]
               (evaluate this [a b c d e]))
       (invoke [this a b c d e f]
               (evaluate this [a b c d e f]))
       (invoke [this a b c d e f g]
               (evaluate this [a b c d e f g]))
       (invoke [this a b c d e f g h]
               (evaluate this [a b c d e f g h]))
       (invoke [this a b c d e f g h i]
               (evaluate this [a b c d e f g h i]))
       (invoke [this a b c d e f g h i j]
               (evaluate this [a b c d e f g h i j]))
       (invoke [this a b c d e f g h i j k]
               (evaluate this [a b c d e f g h i j k]))
       (invoke [this a b c d e f g h i j k l]
               (evaluate this [a b c d e f g h i j k l]))
       (invoke [this a b c d e f g h i j k l m-arg]
               (evaluate this [a b c d e f g h i j k l m-arg]))
       (invoke [this a b c d e f g h i j k l m-arg n]
               (evaluate this [a b c d e f g h i j k l m-arg n]))
       (invoke [this a b c d e f g h i j k l m-arg n o]
               (evaluate this [a b c d e f g h i j k l m-arg n o]))
       (invoke [this a b c d e f g h i j k l m-arg n o p]
               (evaluate this [a b c d e f g h i j k l m-arg n o p]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s t]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s t rest]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t rest]))
       (applyTo [this xs] (AFn/applyToHelper this xs))]

      :cljs
      [Object
       (toString [p] (str u " : " v))

       IEquiv
       (-equiv [_ b]
               (and (instance? RationalFunction b)
                    (and (= arity (.-arity b))
                         (= u (.-u b))
                         (= v (.-v b)))))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ m] (RationalFunction. arity u v m))

       IFn
       (-invoke [this a]
                (evaluate this [a]))
       (-invoke [this a b]
                (evaluate this [a b]))
       (-invoke [this a b c]
                (evaluate this [a b c]))
       (-invoke [this a b c d]
                (evaluate this [a b c d]))
       (-invoke [this a b c d e]
                (evaluate this [a b c d e]))
       (-invoke [this a b c d e f]
                (evaluate this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (evaluate this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (evaluate this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (evaluate this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (evaluate this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (evaluate this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (evaluate this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m-arg]
                (evaluate this [a b c d e f g h i j k l m-arg]))
       (-invoke [this a b c d e f g h i j k l m-arg n]
                (evaluate this [a b c d e f g h i j k l m-arg n]))
       (-invoke [this a b c d e f g h i j k l m-arg n o]
                (evaluate this [a b c d e f g h i j k l m-arg n o]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p]
                (evaluate this [a b c d e f g h i j k l m-arg n o p]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s t]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s t rest]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t rest]))

       IPrintWithWriter
       (-pr-writer
        [x writer _]
        (write-all writer
                   "#object[sicmutils.structure.RationalFunction \""
                   (.toString x)
                   "\"]"))]))

(do (ns-unmap 'sicmutils.rational-function '->RationalFunction)
    (defn ->RationalFunction
      "Positional factory function for [[RationalFunction]].

  The final argument `m` defaults to nil if not supplied."
      ([arity u v]
       (RationalFunction. arity u v nil))
      ([arity u v m]
       (RationalFunction. arity u v m))))

(defn rational-function? [r]
  (instance? RationalFunction r))

(defn make
  "Make the fraction of the two polynomials p and q, after dividing
  out their greatest common divisor."
  [u v]
  {:pre [(p/polynomial? u)
         (p/polynomial? v)
         (= (p/arity u) (p/arity v))]}
  (when (v/zero? v)
    (u/arithmetic-ex "Can't form rational function with zero denominator"))
  ;; annoying: we are using native operations here for the base coefficients
  ;; of the polynomial. Can we do better? That would involve exposing gcd as
  ;; a generic operation (along with lcm), and binding the euclid implmentation
  ;; in for language supported integral types. Perhaps also generalizing ratio?
  ;; and denominator. TODO.
  ;;
  ;; (note from sritchie: I don't think this is true anymore, as of 8.26.20.
  ;; Should we remove the comment, or is there some more work to do to make this
  ;; more efficient?)
  (let [arity (p/arity u)
        cv (p/coefficients v)
        lcv (last cv)
        cs (into (into #{} cv) (p/coefficients u))
        integerizing-factor (g/*
                             (if (< lcv 0) -1 1)
                             (reduce g/lcm 1 (map r/denominator (filter r/ratio? cs))))
        u' (if (v/one? integerizing-factor)
             u
             (p/map-coefficients #(g/* integerizing-factor %) u))
        v' (if (v/one? integerizing-factor)
             v
             (p/map-coefficients #(g/* integerizing-factor %) v))
        g (poly/gcd u' v')
        u'' (p/evenly-divide u' g)
        v'' (p/evenly-divide v' g)]
    (if (v/one? v'')
      u''
      (do (when-not (and (p/polynomial? u'')
                         (p/polynomial? v''))
            (u/illegal (str "bad RF" u v u' v' u'' v'')))
          (->RationalFunction arity  u'' v'')))))

(defn ^:private make-reduced
  [arity u v]
  (if (v/one? v)
    u
    (->RationalFunction arity u v)))

;;
;; Rational arithmetic is from Knuth vol 2 section 4.5.1
;;

(defn add
  "Add the rational functions r and s."
  [^RationalFunction r ^RationalFunction s]
  {:pre [(rational-function? r)
         (rational-function? s)
         (= (.-arity r) (.-arity s))]}
  (let [a (.-arity r)
        u (.-u r)
        u' (.-v r)
        v (.-u s)
        v' (.-v s)
        d1 (poly/gcd u' v')]
    (if (v/one? d1)
      (make-reduced a (p/poly:+ (p/poly:* u v')
                                (p/poly:* u' v))
                    (p/poly:* u' v'))
      (let [t (p/poly:+ (p/poly:* u (p/evenly-divide v' d1))
                        (p/poly:* v (p/evenly-divide u' d1)))
            d2 (poly/gcd t d1)]
        (make-reduced a
                      (p/evenly-divide t d2)
                      (p/poly:* (p/evenly-divide u' d1)
                                (p/evenly-divide v' d2)))))))

(defn addp
  "Add a rational function to a polynomial."
  [^RationalFunction r p]
  (if (v/zero? p)
    r
    (let [v (.-v r)]
      (make (p/poly:+ (.-u r) (p/poly:* v p)) v))))

(defn subp
  [^RationalFunction r p]
  {:pre [(rational-function? r)
         (p/polynomial? p)]}
  (if (v/zero? p)
    r
    (let [v (.-v r)]
      (make (p/poly:- (.-u r) (p/poly:* v p)) v))))

(defn negate
  [^RationalFunction r]
  {:pre [(rational-function? r)]}
  (->RationalFunction (.-arity r) (p/negate (.-u r)) (.-v r)))

(defn square [^RationalFunction r]
  {:pre [(rational-function? r)]}
  (let [u (.-u r)
        v (.-v r)]
    (->RationalFunction (.-arity r) (p/poly:* u u) (p/poly:* v v))))

(defn cube [^RationalFunction r]
  {:pre [(rational-function? r)]}
  (let [u (.-u r)
        v (.-v r)]
    (->RationalFunction (.-arity r) (p/poly:* u (p/poly:* u u)) (p/poly:* v (p/poly:* v v)))))

(defn sub [r s]
  (add r (negate s)))

(defn mul
  [^RationalFunction r ^RationalFunction s]
  {:pre [(rational-function? r)
         (rational-function? s)
         (= (.-arity r) (.-arity s))]}
  (let [a (.-arity r)
        u (.-u r)
        u' (.-v r)
        v (.-u s)
        v' (.-v s)]
    (cond (v/zero? r) r
          (v/zero? s) s
          (v/one? r) s
          (v/one? s) r
          :else (let [d1 (poly/gcd u v')
                      d2 (poly/gcd u' v)
                      u'' (p/poly:* (p/evenly-divide u d1) (p/evenly-divide v d2))
                      v'' (p/poly:* (p/evenly-divide u' d2) (p/evenly-divide v' d1))]
                  (make-reduced a u'' v'')))))

(defn invert
  [^RationalFunction r]
  ;; use make so that the - sign will get flipped if needed
  (make (.-v r) (.-u r)))

(defn div
  [r s]
  (g/mul r (invert s)))

(defn expt
  [^RationalFunction r n]
  {:pre [(rational-function? r)
         (v/integral? n)]}
  (let [u (.-u r)
        v (.-v r)
        [top bottom e] (if (g/negative? n)
                         [v u (g/negate n)]
                         [u v n])]
    (->RationalFunction (.-arity r) (p/expt top e) (p/expt bottom e))))

(def ^:private operator-table
  {'+ #(reduce g/add %&)
   '- (fn [arg & args]
        (if (some? args) (g/sub arg (reduce g/add args)) (g/negate arg)))
   '* (fn
        ([] 1)
        ([x] x)
        ([x y] (g/mul x y))
        ([x y & more]
         (reduce g/mul (g/mul x y) more)))
   '/ (fn [arg & args]
        (if (some? args) (g/div arg (reduce g/mul args)) (g/invert arg)))
   'negate negate
   'invert invert
   'expt g/expt
   'square g/square
   'cube cube
   'gcd g/gcd
   })

(def operators-known
  (u/keyset operator-table))

(deftype RationalFunctionAnalyzer [polynomial-analyzer]
  a/ICanonicalize
  (expression-> [this expr cont]
    (a/expression-> this expr cont compare))

  (expression-> [this expr cont v-compare]
    ;; Convert an expression into Rational Function canonical form. The
    ;; expression should be an unwrapped expression, i.e., not an instance
    ;; of the Literal type, nor should subexpressions contain type
    ;; information. This kind of simplification proceeds purely
    ;; symbolically over the known Rational Function operations;;  other
    ;; operations outside the arithmetic available R(x...) should be
    ;; factored out by an expression analyzer before we get here. The
    ;; result is a RationalFunction object representing the structure of
    ;; the input over the unknowns."
    (let [expression-vars (sort v-compare
                                (set/difference (x/variables-in expr)
                                                operators-known))
          arity    (count expression-vars)
          sym->var (zipmap expression-vars (p/new-variables arity))
          expr'    (x/evaluate expr sym->var operator-table)]
      (cont expr' expression-vars)))

  (->expression [_ r vars]
    ;; This is the output stage of Rational Function canonical form simplification.
    ;; The input is a RationalFunction, and the output is an expression
    ;; representing the evaluation of that function over the
    ;; indeterminates extracted from the expression at the start of this
    ;; process."
    (cond (rational-function? r)
          ((sym/symbolic-operator '/)
           (a/->expression polynomial-analyzer (.-u ^RationalFunction r) vars)
           (a/->expression polynomial-analyzer (.-v ^RationalFunction r) vars))

          (p/polynomial? r)
          (a/->expression polynomial-analyzer r vars)

          :else r))

  (known-operation? [_ o]
    (contains? operators-known o)))

;; ## Generic Method Implementations

(defmethod g/add [::rational-function ::rational-function] [a b] (add a b))

(defmethod g/add [::rational-function ::p/polynomial] [r p] (addp r p))
(defmethod g/add [::p/polynomial ::rational-function] [p r] (addp r p))

(defmethod g/add [::rational-function ::v/number] [a b]
  (addp a (p/make-constant (.-arity ^RationalFunction a) b)))

(defmethod g/add [::v/number ::rational-function] [b a]
  (addp a (p/make-constant (.-arity ^RationalFunction a) b)))

(defmethod g/sub [::rational-function ::rational-function] [a b] (sub a b))
(defmethod g/sub [::rational-function ::p/polynomial] [r p] (subp r p))

(defmethod g/sub [::rational-function ::v/integral] [^RationalFunction r c]
  (let [u (.-u r)
        v (.-v r)]
    (make (p/poly:- (g/mul c v) u) v)))

(defmethod g/sub [::rational-function ::p/polynomial] [r p]
  (addp r (g/negate p)))

(defmethod g/sub [::p/polynomial ::rational-function] [p r]
  (addp (g/negate r) p))

(defmethod g/mul [::rational-function ::rational-function] [a b] (mul a b))
(defmethod g/mul [::rational-function ::p/polynomial] [^RationalFunction r p]
  "Multiply the rational function r = u/v by the polynomial p"
  (let [u (.-u r)
        v (.-v r)
        a (.-arity r)]
    (cond (v/zero? p) 0
          (v/one? p) r
          :else (let [d (poly/gcd v p)]
                  (if (v/one? d)
                    (make-reduced a (p/poly:* u p) v)
                    (make-reduced a (p/poly:* u (p/evenly-divide p d)) (p/evenly-divide v d)))))))

(defmethod g/mul [::p/polynomial ::rational-function] [p ^RationalFunction r]
  "Multiply the polynomial p by the rational function r = u/v"
  (let [u (.-u r)
        v (.-v r)
        a (.-arity r)]
    (cond (v/zero? p) 0
          (v/one? p) r
          :else (let [d (poly/gcd p v) ]
                  (if (v/one? d)
                    (->RationalFunction a (p/poly:* p u) v)
                    (->RationalFunction a (p/poly:* (p/evenly-divide p d) u) (p/evenly-divide v d)))))))

(defmethod g/mul [::v/number ::rational-function] [c ^RationalFunction r]
  (make (g/mul c (.-u r)) (.-v r)))

(defmethod g/mul [::rational-function ::v/number] [^RationalFunction r c]
  (make (g/mul (.-u r) c) (.-v r)))

;; Ratio support for Clojure.
(defmethod g/mul [::rational-function r/ratiotype] [^RationalFunction r a]
  (make (g/mul (.-u r) (r/numerator a)) (g/mul (.-v r) (r/denominator a))))

(defmethod g/mul [r/ratiotype ::rational-function] [a ^RationalFunction r]
  (make (g/mul (r/numerator a) (.-u r)) (g/mul (r/denominator a) (.-v r))))

(defmethod g/div [::rational-function ::rational-function] [a b] (div a b))

(defmethod g/div [::rational-function ::p/polynomial] [^RationalFunction r p]
  (make (.-u r) (p/poly:* (.-v r) p)))

(defmethod g/div [::p/polynomial ::rational-function] [p ^RationalFunction r]
  (make (p/poly:* p (.-v r)) (.-u r)))

(defmethod g/div [::p/polynomial ::p/polynomial] [p q]
  (let [g (poly/gcd p q)]
    (make (p/evenly-divide p g)
          (p/evenly-divide q g))))

(defmethod g/div [::rational-function ::v/integral] [^RationalFunction r c]
  (make (.-u r) (g/mul c (.-v r))))

(defmethod g/div [::v/integral ::rational-function] [c ^RationalFunction r]
  (g/divide (p/make-constant (.-arity r) c) r))

(defmethod g/div [::v/integral ::p/polynomial] [c p]
  (make (p/make-constant (p/bare-arity p) c) p))

(defmethod g/invert [::p/polynomial] [p]
  (make (p/make-constant (p/bare-arity p) 1) p))

(defmethod g/expt [::rational-function ::v/integral] [b x] (expt b x))

(defmethod g/negate [::rational-function] [a] (negate a))

(defmethod g/gcd [::p/polynomial ::p/polynomial] [p q]
  (poly/gcd p q))

(defmethod g/gcd [::p/polynomial ::rational-function] [p ^RationalFunction u]
  (poly/gcd p (.-u u)))

(defmethod g/gcd [::rational-function ::p/polynomial] [^RationalFunction u p]
  (poly/gcd (.-u u) p))

(defmethod g/gcd [::rational-function ::rational-function] [^RationalFunction u ^RationalFunction v]
  (make (poly/gcd (.-u u) (.-u v)) (poly/gcd (.-v u) (.-v v))))

(defmethod g/gcd [::p/polynomial ::v/integral] [p a]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd [::v/integral ::p/polynomial] [a p]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd [::p/polynomial r/ratiotype] [p a]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd [r/ratiotype ::p/polynomial] [a p]
  (poly/primitive-gcd (cons a (p/coefficients p))))
