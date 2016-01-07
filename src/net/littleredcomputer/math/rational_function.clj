;
; Copyright (C) 2015 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns net.littleredcomputer.math.rational-function
  (:require [clojure.set :as set]
            [net.littleredcomputer.math
             [expression :as x]
             [generic :as g]
             [euclid :as e]
             [numsymb :as sym]
             [value :as v]
             [polynomial :as p]
             [polynomial-gcd :refer [gcd]]])
  (:import [clojure.lang Ratio BigInt]
           [net.littleredcomputer.math.polynomial Polynomial]))

(declare operator-table operators-known)

(defrecord RationalFunction [^long arity ^Polynomial u ^Polynomial v]
  v/Value
  (nullity? [_] (v/nullity? u))
  (unity? [_] (and (v/unity? u) (v/unity? v)))
  (kind [_] ::rational-function))

(defn make
  "Make the fraction of the two polynomials p and q, after dividing
  out their greatest common divisor."
  [u v]
  {:pre [(instance? Polynomial u)
         (instance? Polynomial v)]}
  (when (v/nullity? v)
    (throw (ArithmeticException. "Can't form rational function with zero denominator")))
  ;; annoying: we are using native operations here for the base coefficients
  ;; of the polynomial. Can we do better? That would involve exposing gcd as
  ;; a generic operation (along with lcm), and binding the euclid implmentation
  ;; in for language supported integral types. Perhaps also generalizing ratio?
  ;; and denominator. TODO.
  (let [arity (p/check-same-arity u v)
        cv (p/coefficients v)
        lcv (last cv)
        cs (into (into #{} cv) (p/coefficients u))
        integerizing-factor (*
                             (if (< lcv 0) -1 1)
                             (reduce e/lcm 1 (map denominator (filter ratio? cs))))
        u' (if (not (v/unity? integerizing-factor)) (p/map-coefficients #(g/* integerizing-factor %) u) u)
        v' (if (not (v/unity? integerizing-factor)) (p/map-coefficients #(g/* integerizing-factor %) v) v)
        g (gcd u' v')
        u'' (p/evenly-divide u' g)
        v'' (p/evenly-divide v' g)]
    (if (v/unity? v'') u''
        (do (when-not (and (instance? Polynomial u'')
                           (instance? Polynomial v''))
              (throw (IllegalArgumentException. (str "bad RF" u v u' v' u'' v''))))
            (RationalFunction. arity  u'' v'')))))

(defn ^:private make-reduced
  [arity u v]
  (if (v/unity? v)
    u
    (RationalFunction. arity u v)))

;;
;; Rational arithmetic is from Knuth vol 2 section 4.5.1
;;

(defn add
  "Add the ratiional functions r and s."
  [{u :u u' :v :as p} {v :u v' :v :as q}]
  (let [a (p/check-same-arity p q)
        d1 (gcd u' v')]
    (if (v/unity? d1)
      (make-reduced  a (p/add (p/mul u v') (p/mul u' v)) (p/mul u' v'))
      (let [t (p/add (p/mul u (p/evenly-divide v' d1))
                     (p/mul v (p/evenly-divide u' d1)))
            d2 (gcd t d1)]
        (make-reduced a
                      (p/evenly-divide t d2)
                      (p/mul (p/evenly-divide u' d1)
                             (p/evenly-divide v' d2)))))))

(defn addp
  [{u :u u' :v :as r} v]
  (let [a (p/check-same-arity u v)]
    (if (v/nullity? v) r
        (make (p/add u (p/mul u' v)) u'))))

(defn subp
  [{u :u u' :v :as r} v]
  (let [a (p/check-same-arity u v)]
    (if (v/nullity? v) r
        (make (p/sub u (p/mul u' v)) u'))))

(defn negate
  [{u :u v :v arity :arity}]
  (RationalFunction. arity (p/negate u) v))

(defn square
  [{u :u v :v arity :arity}]
  (RationalFunction. arity (p/mul u u) (p/mul v v)))

(defn cube
  [{u :u v :v arity :arity}]
  (RationalFunction. arity (p/mul u (p/mul u u)) (p/mul v (p/mul v v))))

(defn sub
  [r s]
  (add r (negate s)))

(defn mul
  [{u :u u' :v :as U} {v :u v' :v :as V}]
  (let [a (p/check-same-arity U V)]
    (cond (v/nullity? U) U
          (v/nullity? V) V
          (v/unity? U) V
          (v/unity? V) U
          :else (let [d1 (gcd u v')
                      d2 (gcd u' v)
                      u'' (p/mul (p/evenly-divide u d1) (p/evenly-divide v d2))
                      v'' (p/mul (p/evenly-divide u' d2) (p/evenly-divide v' d1))]
                  (make-reduced a u'' v'')))))

(defn invert
  [{arity :arity u :u v :v}]
  ;; use make so that the - sign will get flipped if needed
  (make v u))

(defn div
  [r s]
  (g/mul r (invert s)))

(defn expt
  [{pr :u qr :v arity :arity} n]
  (let [[top bottom e] (if (< n 0) [qr pr (- n)] [pr qr n])]
    (RationalFunction. arity (p/expt top e) (p/expt bottom e))))

(defn expression->
  "Convert an expression into Rational Function canonical form. The
  expression should be an unwrapped expression, i.e., not an instance
  of the Expression type, nor should subexpressions contain type
  information. This kind of simplification proceeds purely
  symbolically over the known Rational Function operations; other
  operations outside the arithmetic available R(x...) should be
  factored out by an expression analyzer before we get here. The
  result is a RationalFunction object representing the structure of
  the input over the unknowns."
  [expr cont]
  (let [expression-vars (sort-by p/variable-sort-key (set/difference (x/variables-in expr) operators-known))
        arity (count expression-vars)]
    (let [new-bindings (zipmap expression-vars (p/new-variables arity))
          environment (into operator-table new-bindings)
          transformer (x/walk-expression environment)]
      (-> expr transformer (cont expression-vars)))))

(defn ->expression
  "This is the output stage of Rational Function canonical form simplification.
  The input is a RationalFunction, and the output is an expression
  representing the evaluation of that function over the
  indeterminates extracted from the expression at the start of this
  process."
  [r vars]
  (cond (instance? RationalFunction r)
        (sym/div (p/->expression (:u r) vars) (p/->expression (:v r) vars))

        (instance? Polynomial r)
        (p/->expression r vars)

        :else r))

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
   'square square
   'cube cube
   ;;`'g/gcd gcd
   })

(def operators-known (set (keys operator-table)))

(defmethod g/add [::rational-function ::rational-function] [a b] (add a b))
(defmethod g/add [::rational-function :net.littleredcomputer.math.polynomial/polynomial] [r p] (addp r p))
(defmethod g/add [:net.littleredcomputer.math.polynomial/polynomial ::rational-function] [p r] (addp r p))

(defmethod g/sub
  [::rational-function :net.littleredcomputer.math.polynomial/polynomial]
  [r p]
  (addp r (g/negate p)))

(defmethod g/sub
  [:net.littleredcomputer.math.polynomial/polynomial ::rational-function]
  [p r]
  (addp (g/negate r) p))

(defmethod g/mul [::rational-function ::rational-function] [a b] (mul a b))
(defmethod g/mul [Long ::rational-function] [c {u :u v :v}] (make (g/mul c u) v))
(defmethod g/mul [::rational-function Long] [{u :u v :v} c] (make (g/mul u c) v))
(defmethod g/mul [::rational-function Ratio] [{u :u v :v} r] (make (g/mul u (numerator r)) (g/mul v (denominator r))))

(defmethod g/mul
  [::rational-function :net.littleredcomputer.math.polynomial/polynomial]
  [{u :u u' :v arity :arity :as U} v]
  "Multiply the rational function U = u/u' by the polynomial v"
  (cond (v/nullity? v) 0
        (v/unity? v) U
        :else (let [d (gcd u' v) ]
                (if (v/unity? d)
                  (make-reduced arity
                                (p/mul u v) u')
                  (make-reduced arity
                                (p/mul u (p/evenly-divide v d))
                                (p/evenly-divide u' d))))))

(defmethod g/mul
  [:net.littleredcomputer.math.polynomial/polynomial ::rational-function]
  [u {v :u v' :v arity :arity :as V}]
  "Multiply the polynomial u by the rational function V = v/v'"
  (cond (v/nullity? u) 0
        (v/unity? u) V
        :else (let [d (gcd u v') ]
                (if (v/unity? d)
                  (RationalFunction. arity
                                     (p/mul u v) v')
                  (RationalFunction. arity
                                     (p/mul (p/evenly-divide u d) v)
                                     (p/evenly-divide v' d))))))


(defmethod g/sub [::rational-function ::rational-function] [a b] (sub a b))
(defmethod g/sub [::rational-function :net.littleredcomputer.math.polynomial/polynomial] [r p] (subp r p))
(defmethod g/sub [::rational-function Long] [{u :u v :v} c] (make (g/sub u c) v))
(defmethod g/div [::rational-function ::rational-function] [a b] (div a b))

(defmethod g/div
  [:net.littleredcomputer.math.polynomial/polynomial ::rational-function]
  [p {u :u v :v arity :arity}]
  (make (p/mul p v) u))

(defmethod g/mul
  [Ratio :net.littleredcomputer.math.polynomial/polynomial]
  [r p]
  (make (g/mul (numerator r) p)
        (p/make-constant (:arity p) (denominator r))))

(defmethod g/mul
  [:net.littleredcomputer.math.polynomial/polynomial Ratio]
  [p r]
  (make (g/mul p (numerator r))
        (p/make-constant (:arity p) (denominator r))))

(defmethod g/add
  [:net.littleredcomputer.math.polynomial/polynomial Ratio]
  [p r]
  (let [a (:arity p)]
    (addp (make (p/make-constant a (numerator r))
                (p/make-constant a (denominator r))) p)))

(defmethod g/div
  [:net.littleredcomputer.math.polynomial/polynomial :net.littleredcomputer.math.polynomial/polynomial]
  [p q]
  (let [g (gcd p q)]
    (make (p/evenly-divide p g) (p/evenly-divide q g))))

(defmethod g/div
  [Long :net.littleredcomputer.math.polynomial/polynomial]
  [c p]
  (make (p/make-constant (:arity p) c) p))

(defmethod g/div
  [BigInt :net.littleredcomputer.math.polynomial/polynomial]
  [c p]
  (make (p/make-constant (:arity p) c) p))

(defmethod g/div
  [:net.littleredcomputer.math.polynomial/polynomial Long]
  [p c]
  (make p (p/make-constant (:arity p) c)))

(defmethod g/div
  [:net.littleredcomputer.math.polynomial/polynomial BigInt]
  [p c]
  (make p (p/make-constant (:arity p) c)))


(defmethod g/expt [::rational-function Integer] [b x] (expt b x))
(defmethod g/expt [::rational-function Long] [b x] (expt b x))
(defmethod g/negate ::rational-function [a] (negate a))
