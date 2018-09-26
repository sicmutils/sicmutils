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

(ns sicmutils.rational-function
  (:require [clojure.set :as set]
            [sicmutils
             [expression :as x]
             [generic :as g]
             [euclid :as euclid]
             [numsymb :as sym]
             [value :as v]
             [polynomial :as p]
             [analyze :as a]
             [polynomial-gcd :as poly]])
  (:import [clojure.lang Ratio BigInt]
           [sicmutils.polynomial Polynomial]))

(declare operator-table operators-known)

(deftype RationalFunction [^long arity ^Polynomial u ^Polynomial v]
  v/Value
  (kind [_] ::rational-function)
  Object
  (equals [_ b]
    (and (instance? RationalFunction b)
         (let [^RationalFunction br b]
           (and (= arity (.arity br))
                (= u (.u br))
                (= v (.v br)))))))

(defn make
  "Make the fraction of the two polynomials p and q, after dividing
  out their greatest common divisor."
  [^Polynomial u ^Polynomial v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)
         (= (.arity u) (.arity v))]}
  (when (g/zero? v)
    (throw (ArithmeticException. "Can't form rational function with zero denominator")))
  ;; annoying: we are using native operations here for the base coefficients
  ;; of the polynomial. Can we do better? That would involve exposing gcd as
  ;; a generic operation (along with lcm), and binding the euclid implmentation
  ;; in for language supported integral types. Perhaps also generalizing ratio?
  ;; and denominator. TODO.
  (let [arity (.arity u)
        cv (p/coefficients v)
        lcv (last cv)
        cs (into (into #{} cv) (p/coefficients u))
        integerizing-factor (*
                             (if (< lcv 0) -1 1)
                             (reduce euclid/lcm 1 (map denominator (filter ratio? cs))))
        u' (if (not (g/one? integerizing-factor)) (p/map-coefficients #(g/* integerizing-factor %) u) u)
        v' (if (not (g/one? integerizing-factor)) (p/map-coefficients #(g/* integerizing-factor %) v) v)
        g (poly/gcd u' v')
        u'' (p/evenly-divide u' g)
        v'' (p/evenly-divide v' g)]
    (if (g/one? v'') u''
        (do (when-not (and (instance? Polynomial u'')
                           (instance? Polynomial v''))
              (throw (IllegalArgumentException. (str "bad RF" u v u' v' u'' v''))))
            (RationalFunction. arity  u'' v'')))))

(defn ^:private make-reduced
  [arity u v]
  (if (g/one? v)
    u
    (RationalFunction. arity u v)))

;;
;; Rational arithmetic is from Knuth vol 2 section 4.5.1
;;

(defn add
  "Add the ratiional functions r and s."
  [^RationalFunction r ^RationalFunction s]
  {:pre [(instance? RationalFunction r)
         (instance? RationalFunction s)
         (= (.arity r) (.arity s))]}
  (let [a (.arity r)
        u (.u r)
        u' (.v r)
        v (.u s)
        v' (.v s)
        d1 (poly/gcd u' v')]
    (if (g/one? d1)
      (make-reduced  a (p/add (p/mul u v') (p/mul u' v)) (p/mul u' v'))
      (let [t (p/add (p/mul u (p/evenly-divide v' d1))
                     (p/mul v (p/evenly-divide u' d1)))
            d2 (poly/gcd t d1)]
        (make-reduced a
                      (p/evenly-divide t d2)
                      (p/mul (p/evenly-divide u' d1)
                             (p/evenly-divide v' d2)))))))

(defn addp
  [^RationalFunction r ^Polynomial p]
  (if (g/zero? p)
    r
    (let [v (.v r)]
      (make (p/add (.u r) (p/mul v p)) v))))

(defn subp
  [^RationalFunction r ^Polynomial p]
  {:pre [(instance? RationalFunction r)
         (instance? Polynomial p)]}
  (if (g/zero? p)
    r
    (let [v (.v r)]
      (make (p/sub (.u r) (p/mul v p)) v))))

(defn negate
  [^RationalFunction r]
  {:pre [(instance? RationalFunction r)]}
  (RationalFunction. (.arity r) (p/negate (.u r)) (.v r)))

(defn square
  [^RationalFunction r]
  ;;{:pre [(instance? RationalFunction r)]}
  (println "RF square" r)
  (let [u (.u r)
        v (.v r)]
    (RationalFunction. (.arity r) (p/mul u u) (p/mul v v))))

(defn cube
  [^RationalFunction r]
  {:pre [(instance? RationalFunction r)]}
  (let [u (.u r)
        v (.v r)]
    (RationalFunction. (.arity r) (p/mul u (p/mul u u)) (p/mul v (p/mul v v)))))

(defn sub
  [r s]
  (add r (negate s)))

(defn mul
  [^RationalFunction r ^RationalFunction s]
  {:pre [(instance? RationalFunction r)
         (instance? RationalFunction s)
         (= (.arity r) (.arity s))]}
  (let [a (.arity r)
        u (.u r)
        u' (.v r)
        v (.u s)
        v' (.v s)]
    (cond (g/zero? r) r
          (g/zero? s) s
          (g/one? r) s
          (g/one? s) r
          :else (let [d1 (poly/gcd u v')
                      d2 (poly/gcd u' v)
                      u'' (p/mul (p/evenly-divide u d1) (p/evenly-divide v d2))
                      v'' (p/mul (p/evenly-divide u' d2) (p/evenly-divide v' d1))]
                  (make-reduced a u'' v'')))))

(defn invert
  [^RationalFunction r]
  ;; use make so that the - sign will get flipped if needed
  (make (.v r) (.u r)))

(defn div
  [r s]
  (g/mul r (invert s)))

(defn expt
  [^RationalFunction r n]
  {:pre [(instance? RationalFunction r)
         (integer? n)]}
  (let [u (.u r)
        v (.v r)
        [top bottom e] (if (< n 0) [v u (- n)] [u v n])]
    (RationalFunction. (.arity r) (p/expt top e) (p/expt bottom e))))

(def ^:private operator-table
  {'+ #(reduce g/add %&)
   '- (fn [arg & args]
        (if (some? args) (g/sub arg (reduce g/add args)) (g/negate arg)))
   '* #(reduce g/mul %&)
   '/ (fn [arg & args]
        (if (some? args) (g/div arg (reduce g/mul args)) (g/invert arg)))
   'negate negate
   'invert invert
   'expt g/expt
   'square g/square
   'cube cube
   'gcd g/gcd
   })

(def operators-known (set (keys operator-table)))           ;; XXX

(deftype RationalFunctionAnalyzer [polynomial-analyzer]
  a/ICanonicalize
  (expression-> [this expr cont] (a/expression-> this expr cont compare))
  (expression-> [this expr cont v-compare]
    ;; Convert an expression into Rational Function canonical form. The
    ;; expression should be an unwrapped expression, i.e., not an instance
    ;; of the Expression type, nor should subexpressions contain type
    ;; information. This kind of simplification proceeds purely
    ;; symbolically over the known Rational Function operations;;  other
    ;; operations outside the arithmetic available R(x...) should be
    ;; factored out by an expression analyzer before we get here. The
    ;; result is a RationalFunction object representing the structure of
    ;; the input over the unknowns."
    (let [expression-vars (sort v-compare (set/difference (x/variables-in expr) operators-known))
          arity (count expression-vars)]
      (let [variables (zipmap expression-vars (a/new-variables this arity))]
        (-> expr (x/walk-expression variables operator-table) (cont expression-vars)))))
  (->expression [_ r vars]
    ;; This is the output stage of Rational Function canonical form simplification.
    ;; The input is a RationalFunction, and the output is an expression
    ;; representing the evaluation of that function over the
    ;; indeterminates extracted from the expression at the start of this
    ;; process."
    (cond (instance? RationalFunction r)
          (let [rr ^RationalFunction r]
            (sym/div (a/->expression polynomial-analyzer (.u rr) vars)
                     (a/->expression polynomial-analyzer (.v rr) vars)))

          (instance? Polynomial r)
          (a/->expression polynomial-analyzer r vars)

          :else r))
  (known-operation? [_ o] (operators-known o))
  (new-variables [_ n] (a/new-variables polynomial-analyzer n)))


(defmethod g/add [::rational-function ::rational-function] [a b] (add a b))
(defmethod g/add [::rational-function ::p/polynomial] [r p] (addp r p))
(defmethod g/add [::p/polynomial ::rational-function] [p r] (addp r p))

(defmethod g/add
  [::rational-function Double]
  [^RationalFunction a b]
  (addp a (p/make-constant (.arity a) b)))

(defmethod g/sub
  [::rational-function ::p/polynomial]
  [r p]
  (addp r (g/negate p)))

(defmethod g/sub
  [::p/polynomial ::rational-function]
  [p r]
  (addp (g/negate r) p))

(defmethod g/mul [::rational-function ::rational-function] [a b] (mul a b))

(defmethod g/mul
  [::sym/numeric-type ::rational-function]
  [c ^RationalFunction r]
  (make (g/mul c (.u r)) (.v r)))

(defmethod g/mul
  [::rational-function ::sym/numeric-type]
  [^RationalFunction r c]
  (make (g/mul (.u r) c) (.v r)))

(defmethod g/mul
  [::rational-function Ratio]
  [^RationalFunction r a]
  (make (g/mul (.u r) (numerator a)) (g/mul (.v r) (denominator a))))

(defmethod g/mul
  [Ratio ::rational-function]
  [a ^RationalFunction r]
  (make (g/mul (numerator a) (.u r)) (g/mul (denominator a) (.v r))))

(defmethod g/mul
  [::rational-function ::p/polynomial]
  [^RationalFunction r p]
  "Multiply the rational function r = u/v by the polynomial p"
  (let [u (.u r)
        v (.v r)
        a (.arity r)]
    (cond (g/zero? p) 0
          (g/one? p) r
          :else (let [d (poly/gcd v p) ]
                  (if (g/one? d)
                    (make-reduced a (p/mul u p) v)
                    (make-reduced a (p/mul u (p/evenly-divide p d)) (p/evenly-divide v d)))))))

(defmethod g/mul
  [::p/polynomial ::rational-function]
  [^Polynomial p ^RationalFunction r]
  "Multiply the polynomial p by the rational function r = u/v"
  (let [u (.u r)
        v (.v r)
        a (.arity r)]
    (cond (g/zero? p) 0
          (g/one? p) r
          :else (let [d (poly/gcd p v) ]
                  (if (g/one? d)
                    (RationalFunction. a (p/mul p u) v)
                    (RationalFunction. a (p/mul (p/evenly-divide p d) u) (p/evenly-divide v d)))))))

(defmethod g/div [::rational-function ::rational-function] [a b] (div a b))
(defmethod g/sub [::rational-function ::rational-function] [a b] (sub a b))
(defmethod g/sub [::rational-function ::p/polynomial] [r p] (subp r p))

(defmethod g/sub
  [::rational-function ::sym/numeric-type]
  [^RationalFunction r c]
  (let [u (.u r)
        v (.v r)]
    (make (p/sub (g/mul c v) u) v)))

(defmethod g/sub
  [::sym/numeric-type ::rational-function]
  [c ^RationalFunction r]
  (let [u (.u r)
        v (.v r)]
    (make (p/sub u (g/mul c v)) v)))

(defmethod g/add
  [::sym/numeric-type ::rational-function]
  [c ^RationalFunction r]
  (let [v (.v r)]
    (make (p/add (.u r) (g/mul c v)) v)))

(defmethod g/add
  [::rational-function ::sym/numeric-type]
  [^RationalFunction r c]
  (let [v (.v r)]
    (make (p/add (.u r) (g/mul c v)) v)))

(defmethod g/div
  [::rational-function ::sym/numeric-type]
  [^RationalFunction r c]
  (make (.u r) (g/mul c (.v r))))

(defmethod g/div
  [::rational-function ::p/polynomial]
  [^RationalFunction r p]
  (make (.u r) (p/mul (.v r) p)))

(defmethod g/div
  [::p/polynomial ::rational-function]
  [^Polynomial p ^RationalFunction r]
  (make (p/mul p (.v r)) (.u r)))

(defmethod g/div
  [::p/polynomial ::p/polynomial]
  [p q]
  (let [g (poly/gcd p q)]
    (make (p/evenly-divide p g) (p/evenly-divide q g))))

(defmethod g/div
  [::sym/numeric-type ::p/polynomial]
  [c ^Polynomial p]
  (make (p/make-constant (.arity p) c) p))

(defmethod g/div
  [::sym/numeric-type ::rational-function]
  [c ^RationalFunction r]
  (g/divide (p/make-constant (.arity r) c) r))

(defmethod g/expt [::rational-function Integer] [b x] (expt b x))
(defmethod g/expt [::rational-function Long] [b x] (expt b x))
(defmethod g/negate [::rational-function] [a] (negate a))
(defmethod g/zero? [::rational-function] [^RationalFunction a] (g/zero? (.u a)))
(defmethod g/one? [::rational-function] [^RationalFunction a] (and (g/one? (.u a)) (g/one? (.v a))))

(defmethod g/gcd
  [::p/polynomial ::p/polynomial]
  [p q]
  (poly/gcd p q))

(defmethod g/gcd
  [::p/polynomial ::rational-function]
  [p ^RationalFunction u]
  (poly/gcd p (.u u)))

(defmethod g/gcd
  [::rational-function ::p/polynomial]
  [^RationalFunction u p]
  (poly/gcd (.u u) p))

(defmethod g/gcd
  [::rational-function ::rational-function]
  [^RationalFunction u ^RationalFunction v]
  (make (poly/gcd (.u u) (.u v)) (poly/gcd (.v u) (.v v))))

(defmethod g/gcd
  [::p/polynomial ::sym/native-integral-type]
  [p a]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd
  [::sym/native-integral-type ::p/polynomial]
  [a p]
  (poly/primitive-gcd (cons a (p/coefficients p))))
