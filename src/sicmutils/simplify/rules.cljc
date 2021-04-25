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

(ns sicmutils.simplify.rules
  (:refer-clojure :exclude [even? odd?])
  (:require [clojure.set :as cs]
            [pattern.match :as pm]
            [pattern.rule :as r :refer [=> ruleset rule-simplifier]
             #?@(:cljs [:include-macros true])]
            [sicmutils.complex :as c]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as sym]
            [sicmutils.value :as v]))

(defn assume!
  "see logic-utils.scm... to get this working. AND share with the implementation
  we already have in `factor`, stick these somewhere common and add a gate."
  ([predicate-expr responsible-party]
   true)
  ([predicate-expr responsible-party if-false]
   true))

(def ^{:dynamic true
       :doc " allows (log (exp x)) => x.

can confuse x=(x0+n*2pi)i with x0."}
  *log-exp-simplify?*
  true)

(def ^{:dynamic true
       :doc "Allows (x^a)^b => x^(a*b).

This is dangerous, because can lose or gain a root, e.g.
x = (x^(1/2))^2 != ((x^2)^1/2)=+-x"}
  *exponent-product-simplify?*
  true)

(def ^{:dynamic true
       :doc " Traditionally sqrt(x) is the positive square root but x^(1/2) is
both positive and negative roots.

This confuses these, potentially losing a root."}
  *expt-half->sqrt?*
  true)

(def ^{:dynamic true
       :doc "If x is real then (sqrt (square x)) = (abs x).

  This is hard to work with, but we usually want to allow (sqrt (square x)) =>
  x, but this is not necessarily good if x is negative."}
  *sqrt-expt-simplify?*
  true)

(def ^{:dynamic true
       :doc "If x, y are real non-negative then
(* (sqrt x) (sqrt y)) = (sqrt (* x y))
but this is not true for negative factors."}
  *sqrt-factor-simplify?*
  true)

(def ^{:dynamic true
       :doc "allows (atan y x) => (atan (/ y d) (/ x d)) where d=(gcd x y).

OK if d is a number (gcd is always positive) but may lose quadrant if gcd can be
negative for some values of its variables."}
  *aggressive-atan-simplify?*
  true)

(def ^{:dynamic true
       :doc "allows (asin (sin x)) => x, etc.

loses multivalue info, as in log-exp"}
  *inverse-simplify?*
  true)

(def ^{:dynamic true
       :doc "Allows reduction of sin, cos of rational multiples of :pi"}
  *sin-cos-simplify?*
  true)

(def ^{:dynamic true
       :doc "Allow half-angle reductions. Sign of result is hairy!

TODO: change this to more than half angle... use the routine from the book."}
  *half-angle-simplify?*
  true)

(def ^{:dynamic true
       :doc "allows commutation of partial derivatives.

 Only ok if components selected by partials are unstructured (e.g. real)"}
  *commute-partials?*
  true)

(def ^{:dynamic true
       :doc "allows division through by numbers

e.g. (/ (+ (* 4 x) 5) 3) => (+ (* 4/3 x) 5/3)"}
  *divide-numbers-through-simplify?*
  false)

(def ^{:dynamic true
       :doc "Transforms products of trig functions into functions of sums
of angles.

e.g. (* (sin x) (cos y)) ==> (+ (* 1/2 (sin (+ x y))) (* 1/2 (sin (+ x (* -1
y)))) )"}

  *trig-product-to-sum-simplify?*
  false)

(defn- negative-number? [x]
  (and (v/number? x)
       (g/negative? x)))

(defn- complex-number? [z]
  (and (c/complex? z)
       (not (v/zero? (g/real-part z)))
       (not (v/zero? (g/imag-part z)))))

(defn- imaginary-number? [z]
  (and (c/complex? z)
       (not (v/zero? z))
       (v/zero? (g/real-part z))))

(defn- imaginary-integer? [z]
  (and (c/complex? z)
       (not (v/zero? z))
       (v/zero? (g/real-part z))
       (v/integral? (g/imag-part z))))

(defn non-integer? [x]
  (not (v/integral? x)))

(defn even? [x]
  (v/zero? (g/modulo x 2)))

(defn odd? [x]
  (v/one? (g/modulo x 2)))

(defn- even-integer? [x]
  (and (v/integral? x)
       (even? x)))

(defn odd-integer? [x]
  (and (v/integral? x)
       (odd? x)))

(defn even-positive-integer? [x]
  (and (even-integer? x)
       (> x 1)))

(defn odd-positive-integer? [x]
  (and (odd-integer? x)
       (> x 2)))

(defn- more-than-two? [x]
  (and (v/number? x) (> x 2)))

(defn- at-least-two? [x]
  (and (v/number? x) (>= x 2)))

(defn unary-elimination
  "Takes a sequence `ops` of operator symbols like `'+`, `'*` and returns a rule
  that strips these operations off of unary applications.

  ```clojure
  (let [rule (unary-elimination '+ '*)
        f    (rule-simplifier rule)]
    (f '(+ x y (* z) (+ a))))
  ;;=> (+ x y z a)
  ```"
  [& ops]
  (let [op-set (into #{} ops)]
    (ruleset
     ((? _ op-set) ?x) => ?x)))

(defn constant-elimination
  "Takes an operator symbol `op` and an identity element `constant` and returns a
  rule that eliminates instances of `constant` inside binary forms like

  ```clojure
  (<op> l r)
  ```"
  [op constant]
  (ruleset
   (~op ~constant ?x) => (~op ?x)
   (~op ?x ~constant) => (~op ?x)))

(defn constant-promotion
  "Takes an operator symbol `op` and an identity element `constant` and returns a
  rule that turns binary forms with `constant` on either side into `constant`.

  This rule is useful for commutative annihalators like:

  ```clojure
  (* 0 <anything>) => 0
  (and false <anything>) => false
  (or true <anything>) => true
  ```"
  [op constant]
  (ruleset
   (~op _ ~constant) => ~constant
   (~op ~constant _) => ~constant))

(defn associative
  "Takes any number of operator symbols `ops` like `'+`, `'*` and returns a rule
  that collapses nested applications of each operation into a single
  sequence. (The associative property lets us strip parentheses.)

  ```clojure
  (let [rule (associative '+ '*)
        f    (rule-simplifier rule)]
    (= (+ x y z a (* b c d) cake face)
       (f '(+ x (+ y (+ z a) (* b (* c d))
                   (+ cake face))))))
  ```"
  [& ops]
  (let [op-set  (into #{} ops)
        flatten (fn [op]
                  (r/ruleset
                   (~op ??xs) => [??xs]
                   ?x         => [?x]))]
    (ruleset
     ((? ?op op-set) ??a (?op ??b) ??c)
     =>
     (?op ??a (?? (fn [{op '?op, b '??b, c '??c}]
                    (mapcat (flatten op)
                            (concat b c))))))))

(defn commutative
  "Takes any number of operator symbols `ops` like `'+`, `'*` and returns a rule
  that sorts the argument list of any multiple-arity call to any of the supplied
  operators. Sorting is accomplished with [[sicmutils.expression/sort]].

  For example:

  ```clojure
  (let [rule (commutative '* '+)]
    (= '(* 2 3 a b c (+ c a b))
       (rule '(* c a b (+ c a b) 3 2))))
  ```"
  [& ops]
  (let [op-set (into #{} ops)]
    (ruleset
     ((? ?op op-set) ??xs)
     => (?op (?? #(x/sort (% '??xs)))))))

(defn idempotent
  "Returns a simplifier that will remove consecutive duplicate arguments to any
  of the operations supplied as `ops`. Acts as identity otherwise.

  ```clojure
  (let [rule (idempotent 'and)]
    (= '(and a b c d)
       (rule '(and a b b c c c d))))
  ```"
  [& ops]
  (let [op-set (into #{} ops)]
    (ruleset
     ((? ?op op-set) ??pre ?x ?x ??post)
     =>
     (?op (?? (fn [m]
                (dedupe
                 (r/template
                  m (??pre ?x ??post)))))))))

(def ^{:doc "Set of rules that collect adjacent products, exponents and nested
 exponents into exponent terms."}
  exponent-contract
  (ruleset
   ;; nested exponent case.
   (expt (expt ?op (? ?n v/integral?))
         (? ?m v/integral?))
   => (expt ?op (? #(g/+ (% '?n) (% '?m))))

   ;; adjacent pairs of exponents
   (* ??pre
      (expt ?op (? ?n v/integral?))
      (expt ?op (? ?m v/integral?))
      ??post)
   => (* ??pre
         (expt ?op (? #(g/+ (% '?n) (% '?m))))
         ??post)

   ;; exponent on right, non-expt on left
   (* ??pre
      ?op (expt ?op (? ?n v/integral?))
      ??post)
   => (* ??pre
         (expt ?op (? #(g/+ (% '?n) 1)))
         ??post)

   ;; exponent on left, non-expt on right
   (* ??pre
      (expt ?op (? ?n v/integral?)) ?op
      ??post)
   => (* ??pre
         (expt ?op (? #(g/+ (% '?n) 1)))
         ??post)

   ;; non-exponent pairs
   (* ??pre ?op ?op ??post)
   => (* ??pre (expt ?op 2) ??post)))

(defn logexp
  "Returns a rule simplifier that attempts to simplify nested exp and log forms.

  You can tune the behavior of this simplifier with [[*log-exp-simplify?*]]
  and [[*sqrt-expt-simplify?*]].

  NOTE: [[logexp]] returns a `rule-simplifier`, which memoizes its traversal
  through the supplied expression. This means that if you try to
  customize [[logexp]] with dynamic binding variables AFTER passing an
  expression into it, you may get a memoized result which used the previous
  dynamic binding.

  This is a problem we should address!"
  [simplify]
  (rule-simplifier
   (r/ruleset*
    (r/rule
     (exp (* (? ?n v/integral?) (log ?x)))
     => (expt ?x ?n))

    (r/rule
     (exp (log ?x)) => ?x)

    (r/guard
     (fn [_] *log-exp-simplify?*)
     (r/rule
      (log (exp ?x))
      (fn [{x '?x}]
        (let [xs (simplify x)]
          (and (assume!
                (r/template
                 (= (log (exp ~xs)) ~xs))
                'logexp1)
               x)))))

    (r/guard
     (fn [_] *sqrt-expt-simplify?*)
     (r/rule (sqrt (exp ?x))
             (fn [{x '?x}]
               (let [xs (simplify x)]
                 (and (assume!
                       (r/template
                        (= (sqrt (exp ~xs))
                           (exp (/ ~xs 2))))
                       'logexp2)
                      (r/template (exp (/ ~x 2))))))))

    (r/rule
     (log (sqrt ?x)) => (* (/ 1 2) (log ?x))))))

(def ^{:doc "Rule simplifier for forms that contain `magnitude` entries."}
  magnitude
  (rule-simplifier
   (ruleset
    (magnitude (? ?n v/real?))
    => (? (comp g/magnitude '?n))

    (magnitude (* ??xs))
    => (* (?? (fn [{xs '??xs}]
                (map #(list 'magnitude %)
                     xs))))

    (magnitude (expt ?x 1))
    => (magnitude ?x)

    (magnitude (expt ?x (? ?n even-integer?)))
    => (expt ?x ?n)

    (magnitude (expt ?x (? ?n v/integral?)))
    => (* (magnitude ?x) (expt ?x (? #(g/- (% '?n) 1)))))))

(defn miscsimp
  "TODO break these apart into more rulesets, AND note that we have some
  similarities to [[exponent-contract]] above."
  [simplify]
  (let [sym:* (sym/symbolic-operator '*)]
    (rule-simplifier
     (ruleset
      (expt _ 0)  => 1
      (expt ?x 1) => ?x

      (expt (expt ?x ?a) ?b)
      (fn [{a '?a b '?b x '?x}]
        (let [as (simplify a)
              bs (simplify b)]
          (when (or (and (v/integral? as)
                         (v/integral? bs))

                    (and (even-integer? bs)
                         (v/integral?
                          (simplify (sym:* as bs))))

                    (and *exponent-product-simplify?*
                         (assume!
                          (r/template
                           (= (expt (expt ~x ~as) ~bs)
                              (expt ~x ~(sym:* as bs))))
                          'exponent-product)))
            {'?ab (g/* a b)})))
      (expt ?x ?ab)

      (expt ?x (/ 1 2))
      (fn [_] *expt-half->sqrt?*)
      (sqrt ?x)

      ;; Collect duplicate terms into exponents. TODO this is missing the case
      ;; where non-exponent duplicates get collected into exponents.
      ;;
      ;; GJS notes: "a rare, expensive luxury."
      (* ??fs1* ?x ??fs2 (expt ?x ?y) ??fs3)
      => (* ??fs1 ??fs2 (expt ?x (+ 1 ?y)) ??fs3)

      (* ??fs1 (expt ?x ?y) ??fs2 ?x ??fs3)
      => (* ??fs1 (expt ?x (+ 1 ?y)) ??fs2 ??fs3)

      (* ??fs1 (expt ?x ?y1) ??fs2 (expt ?x ?y2) ??fs3)
      => (* ??fs1 ??fs2 (expt ?x (+ ?y1 ?y2)) ??fs3)))))

;; ## Square Root Simplification

(defn simplify-square-roots [simplify]
  (rule-simplifier
   (r/rule
    (expt (sqrt ?x) (? ?n even-integer?))
    => (expt ?x (? #(g// (% '?n) 2))))

   ;; TODO verify that these trigger and that the simplifier passes them
   ;; correctly.
   (r/ruleset*
    (r/guard
     (fn [_] *sqrt-expt-simplify?*)
     (r/ruleset*
      (r/rule
       (sqrt (expt ?x (? ?n even-integer?)))
       (fn [{x '?x n '?n}]
         (let [xs (simplify x)
               half-n (g// n 2)]
           (when (assume!
                  (r/template
                   (= (sqrt (expt ~xs ~n))
                      (expt ~xs ~half-n)))
                  'simsqrt1)
             {'?new-n half-n})))
       (expt ?x ?new-n))

      (r/rule
       (sqrt (expt ?x (? ?n odd-positive-integer?)))
       (fn [{x '?x n '?n}]
         (let [xs (simplify x)
               half-dec-n (g// (g/- n 1) 2)]
           (when (assume!
                  (r/template
                   (= (sqrt (expt ~xs ~n))
                      (expt ~xs ~half-dec-n)))
                  'simsqrt2)
             {'?new-n half-dec-n})))
       (* (sqrt ?x) (expt ?x ?new-n))))))

   (ruleset
    (expt (sqrt ?x) (? ?n odd-integer?))
    => (* (sqrt ?x)
          (expt ?x (? #(g// (g/- (% '?n) 1) 2))))

    (/ ?x (sqrt ?x)) => (sqrt ?x)

    (/ (sqrt ?x) ?x) => (/ 1 (sqrt ?x))

    (/ (* ??u ?x ??v) (sqrt ?x))
    =>
    (* ??u (sqrt ?x) ??v)

    (/ (* ??u (sqrt ?x) ??v) ?x)
    =>
    (/ (* ??u ??v) (sqrt ?x))

    (/ ?x (* ??u (sqrt ?x) ??v))
    =>
    (/ (sqrt ?x) (* ??u ??v))

    (/ (sqrt ?x) (* ??u ?x ??v))
    =>
    (/ 1 (* ??u (sqrt ?x) ??v))

    (/ (* ??p ?x ??q)
       (* ??u (sqrt ?x) ??v))
    =>
    (/ (* ??p (sqrt ?x) ??q)
       (* ??u ??v))

    (/ (* ??p (sqrt ?x) ??q)
       (* ??u ?x ??v))
    =>
    (/ (* ??p ??q)
       (* ??u (sqrt ?x) ??v)))))

(defn non-negative-factors!
  "TODO note that I now check if the two sides are equal."
  [simplify x y id]
  (let [xs (simplify x)
        ys (simplify y)]
    (if (v/= xs ys)
      (assume! `(~'non-negative? ~xs) id (fn [] false))
      (and (assume! `(~'non-negative? ~xs) id (fn [] false))
           (assume! `(~'non-negative? ~ys) id (fn [] false))))))

(defn sqrt-expand
  "distribute the radical sign across products and quotients. The companion rule
  sqrt-contract reassembles what remains.

  NOTE that doing this may allow equal subexpressions within the radicals to
  cancel in various ways."
  [simplify]
  (letfn [(pred [label]
            (fn [{x '?x y '?y}]
              (non-negative-factors! simplify x y label)))]
    ;; TODO test that the dynamic variable actually gates access, and that this
    ;; is working like we want!
    ;;
    ;; TODO test that IF the two sides are equal in some of these cases, we can
    ;; just pull out a NON square root... why not, since we have `simplify` at
    ;; hand, AND since we do it below? YES, get this done!
    (r/attempt
     (r/guard
      (fn [_] *sqrt-factor-simplify?*)
      (rule-simplifier
       (ruleset
        (sqrt (* ?x ?y))
        (pred 'e1)
        (* (sqrt ?x) (sqrt ?y))

        (sqrt (* ?x ?y ??ys))
        (pred 'e2)
        (* (sqrt ?x) (sqrt (* ?y ??ys)))

        (sqrt (/ ?x ?y))
        (pred 'e3)
        (/ (sqrt ?x) (sqrt ?y))

        (sqrt (/ ?x ?y ??ys))
        (pred 'e4)
        (/ (sqrt ?x) (sqrt (* ?y ??ys)))))))))

(defn sqrt-contract [simplify]
  (let [if-false (fn [] false)
        non-negative!
        (fn
          ([xs sym]
           (assume! `(~'non-negative? ~xs) sym if-false))
          ([xs ys sym]
           ;; TODO use non-negative-factors above, and duplicate the checks
           ;; above.
           (and (assume! `(~'non-negative? ~xs) sym if-false)
                (assume! `(~'non-negative? ~ys) sym if-false))))]

    (rule-simplifier
     (r/ruleset*
      (r/rule
       (* ??a (sqrt ?x) ??b (sqrt ?y) ??c)
       (fn [{x '?x y '?y :as m}]
         (let [xs (simplify x)
               ys (simplify y)]
           (if (v/= xs ys)
             (and (non-negative! xs 'c1)
                  (r/template
                   m (* ??a ~xs ??b ??c)))
             (and (non-negative! xs ys 'c1)
                  (r/template
                   m (* ??a (sqrt (* ~xs ~ys)) ??b ??c)))))))

      (r/rule
       (/ (sqrt ?x) (sqrt ?y))
       (fn [{x '?x y '?y}]
         (let [xs (simplify x)
               ys (simplify y)]
           (if (v/= xs ys)
             (and (non-negative! xs 'c2)
                  1)
             (and (non-negative! xs ys 'c2)
                  (r/template (sqrt (/ ~xs ~ys))))))))

      (r/rule
       (/ (* ??a (sqrt ?x) ??b) (sqrt ?y))
       (fn [{x '?x y '?y :as m}]
         (let [xs (simplify x)
               ys (simplify y)]
           (if (v/= xs ys)
             (and (non-negative! xs 'c3)
                  (r/template m (* ??a ??b)))
             (and (non-negative! xs ys 'c3)
                  (r/template
                   m (* ??a (sqrt (/ ~xs ~ys)) ??b)))))))

      (r/rule
       (/ (sqrt ?x) (* ??a (sqrt ?y) ??b))
       (fn [{x '?x y '?y :as m}]
         (let [xs (simplify x)
               ys (simplify y)]
           (if (v/= xs ys)
             (and (non-negative! xs 'c4)
                  (r/template m (/ 1 (* ??a ??b))))
             (and (non-negative! xs ys 'c4)
                  (r/template
                   m (/ (sqrt (/ ~xs ~ys))
                        (* ??a ??b))))))))

      (r/rule
       (/ (* ??a (sqrt ?x) ??b)
          (* ??c (sqrt ?y) ??d))
       (fn [{x '?x y '?y :as m}]
         (let [xs (simplify x)
               ys (simplify y)]
           (if (v/= xs ys)
             (and (non-negative! xs 'c5)
                  (r/template
                   m (/ (* ??a ??b)
                        (* ??c ??d))))
             (and (non-negative! xs ys 'c5)
                  (r/template
                   m (/ (* ??a (sqrt (/ ~xs ~ys)) ??b)
                        (* ??c ??d))))))))))))

;; ## Log / Exp

(def specfun->logexp
  (rule-simplifier
   (ruleset
    (sqrt ?x) => (exp (* (/ 1 2) (log ?x)))

    (atan ?z)
    => (/ (- (log (+ 1 (* (complex 0.0 1.0) ?z)))
             (log (- 1 (* (complex 0.0 1.0) ?z))))
          (complex 0.0 2.0))

    (asin ?z)
    => (* (complex 0.0 -1.0)
          (log (+ (* (complex 0.0 1.0) ?z)
                  (sqrt (- 1 (expt ?z 2))))))

    (acos ?z)
    => (* (complex 0.0 -1.0)
          (log (+ ?z (* (complex 0.0 1.0)
                        (sqrt (- 1 (expt ?z 2)))))))

    (sinh ?u) => (/ (- (exp ?u) (exp (* -1 ?u))) 2)

    (cosh ?u) => (/ (+ (exp ?u) (exp (* -1 ?u))) 2)

    (expt ?x (? ?y non-integer?)) => (exp (* ?y (log ?x))))))

(def logexp->specfun
  (rule-simplifier
   (ruleset
    (exp (* -1 (log ?x))) => (expt ?x -1)

    (exp (* (/ 1 2) (log ?x))) => (sqrt ?x)

    (exp (* (/ -1 2) (log ?x))) => (/ 1 (sqrt ?x))

    (exp (* (/ 3 2) (log ?x))) => (expt (sqrt ?x) 3)

    (exp (* (/ -3 2) (log ?x))) => (expt (sqrt ?x) -3)

    (exp (* ??n1 (log ?x) ??n2))
    => (expt ?x (* ??n1 ??n2)))))

(defn log-contract [simplify]
  (rule-simplifier
   (ruleset
    (+ ??x1 (log ?x2) ??x3 (log ?x4) ??x5)
    => (+ ??x1 ??x3 ??x5 (log (* ?x2 ?x4)))

    (- (log ?x) (log ?y))
    => (log (/ ?x ?y))

    (+ ??x1
       (* ??f1 (log ?x) ??f2)
       ??x2
       (* ??f3 (log ?y) ??f4)
       ??x3)
    (fn [m]
      (let [s1 (simplify (r/template m (* ??f1 ??f2)))
            s2 (simplify (r/template m (* ??f3 ??f4)))]
        (when (v/exact-zero?
               (simplify (list '- s1 s2)))
          {'??s1 s1})))
    (+ (* (log (* ?x ?y)) ??s1)
       ??x1 ??x2 ??x3))))

(def log-expand
  (rule-simplifier
   (ruleset
    (log (* ?x1 ?x2 ??xs))
    => (+ (log ?x1) (log (* ?x2 ??xs)))

    (log (/ ?x1 ?x2))
    => (- (log ?x1) (log ?x2))

    (log (expt ?x ?e))
    => (* ?e (log ?x)))))

(def log-extra
  (rule-simplifier
   (ruleset
    (* (? ?n v/integral?) ??f1 (log ?x) ??f2)
    => (* ??f1 (log (expt ?x ?n)) ??f2))))

;; ## Partials

(def canonicalize-partials
  (rule-simplifier
   (ruleset
    ;; Convert nests into products.
    ((partial ??i) ((partial ??j) ?f))
    => ((* (partial ??i) (partial ??j)) ?f)

    ((partial ??i) ((* (partial ??j) ??more) ?f))
    => ((* (partial ??i) (partial ??j) ??more) ?f)

    ;; Gather exponentiated partials into products
    ((expt (partial ??i) ?n) ((partial ??j) ?f))
    => ((* (expt (partial ??i) ?n) (partial ??j)) ?f)

    ((partial ??i) ((expt (partial ??j) ?n) ?f))
    => ((* (partial ??i) (expt (partial ??j) ?n)) ?f)

    ((expt (partial ??i) ?n) ((expt (partial ??j) ?m) ?f))
    => ((* (expt (partial ??i) ?n) (expt (partial ??j) ?m)) ?f)

    ;; Same idea, trickier when some accumulation has already occurred.
    ((expt (partial ??i) ?n) ((* (partial ??j) ??more) ?f))
    => ((* (expt (partial ??i) ?n) (partial ??j) ??more) ?f)

    ((partial ??i) ((* (expt (partial ??j) ?m) ??more) ?f))
    => ((* (partial ??i) (expt (partial ??j) ?m) ??more) ?f)

    ((expt (partial ??i) ?n) ((* (expt (partial ??j) ?m) ??more) ?f))
    => ((* (expt (partial ??i) ?n) (expt (partial ??j) ?m) ??more) ?f)

    #_
    (comment
      ;; example:

      (((* (partial 2 1) (partial 1 1)) FF) (up t (up x y) (down p_x p_y)))

      ;; since the partial indices in the outer derivative are lexically greater
      ;; than those of the inner, we canonicalize by swapping the order. This is
      ;; the "equality of mixed partials."
      )
    (((* ??xs (partial ??i) ??ys (partial ??j) ??zs) ?f) ??args)
    (fn [m]
      ;; TODO implement `symb:elementary-access?` and make sure that this only
      ;; sorts if we can prove that we go all the way to the botom.
      ;;
      ;; TODO move this to its own rule with a guard.
      (and *commute-partials?*
           (pos? (compare (vec ('??i m))
                          (vec ('??j m))))))
    (((* ??xs (partial ??j) ??ys (partial ??i) ??zs) ?f) ??args))))

;; ## Trigonometric Rules
;;
;; the following rules are used to convert all trig expressions to ones
;; involving only sin and cos functions, and to make 1-arg atan into 2-arg atan.

(def trig->sincos
  (rule-simplifier
   (ruleset
    (tan ?x) => (/ (sin ?x) (cos ?x))

    (cot ?x) => (/ (cos ?x) (sin ?x))

    (sec ?x) => (/ 1 (cos ?x))

    (csc ?x) => (/ 1 (sin ?x))

    (atan (/ ?y ?x)) => (atan ?y ?x)

    (atan ?y) => (atan ?y 1))))

(def sincos->trig
  (rule-simplifier
   (ruleset
    (/ (sin ?x) (cos ?x)) => (tan ?x)

    (/ (* ??n1 (sin ?x) ??n2) (cos ?x))
    => (* ??n1 (tan ?x) ??n2)

    (/ (sin ?x) (* ??d1 (cos ?x) ??d2))
    => (/ (tan ?x) (* ??d1 ??d2))

    (/ (* ??n1 (sin ?x) ??n2)
       (* ??d1 (cos ?x) ??d2))
    => (/ (* ??n1 (tan ?x) ??n2)
          (* ??d1 ??d2)))))

(comment
  ;; TODO this is busted in the original scmutils: -0.7853981633974483
  ((triginv sicmutils.simplify/*rf-analyzer*) '(atan -1 1)))

(defn triginv [simplify]
  (r/rule-simplifier
   (let [sym:atan (sym/symbolic-operator 'atan)]
     (r/guard
      (fn [_] *aggressive-atan-simplify?*)
      (r/rule
       (atan ?y ?x)
       (fn [{x '?x y '?y}]
         (let [xs (simplify x)
               ys (simplify y)]
           (if (v/= ys xs)
             (if (v/number? ys)
               (if (g/negative? ys)
                 '(- (/ (* 3 pi) 4))
                 '(/ pi 4))
               (and (assume!
                     (list 'positive? xs)
                     'aggressive-atan-1)
                    '(/ pi 4)))
             (if (and (v/number? ys)
                      (v/number? xs))
               (sym:atan ys xs)
               (let [s (simplify (list 'gcd ys xs))]
                 (when-not (v/one? s)
                   (and (assume!
                         (list 'positive? s)
                         'aggressive-atan-2)
                        (let [yv (simplify (list '/ ys s))
                              xv (simplify (list '/ xs s))]
                          (r/template
                           (atan ~yv ~xv)))))))))))))

   (ruleset
    (sin (asin ?x)) => ?x
    (cos (acos ?x)) => ?x
    (tan (atan ?x)) => ?x
    (sin (acos ?x)) => (sqrt (- 1 (expt ?x 2)))
    (cos (asin ?y)) => (sqrt (- 1 (expt ?y 2)))
    (tan (asin ?y)) => (/ ?y (sqrt (- 1 (expt ?y 2))))
    (tan (acos ?x)) => (/ (sqrt (- 1 (expt ?x 2))) ?x)

    (sin (atan ?a ?b))
    => (/ ?a (sqrt (+ (expt ?a 2) (expt ?b 2))))

    (cos (atan ?a ?b))
    => (/ ?b (sqrt (+ (expt ?a 2) (expt ?b 2)))))

   (r/guard
    (fn [_] *inverse-simplify?*)
    (ruleset
     (asin (sin ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (assume!
          (r/template
           (= (asin (sin ~xs)) ~xs))
          'asin-sin)))
     ?x

     (acos (cos ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (assume!
          (r/template
           (= (acos (cos ~xs)) ~xs))
          'acos-cos)))
     ?x

     (atan (tan ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (assume!
          (r/template
           (= (atan (tan ~xs)) ~xs))
          'atan-tan)))
     ?x

     (atan (sin ?x) (cos ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (assume!
          (r/template
           (= (atan (sin ~xs) (cos ~xs)) ~xs))
          'atan-sin-cos)))
     ?x

     (asin (cos ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (assume!
          (r/template
           (= (asin (cos ~xs))
              (- (* (/ 1 2) pi) ~xs)))
          'asin-cos)))
     (- (* (/ 1 2) pi) ?x)

     (acos (sin ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (assume!
          (r/template
           (= (acos (sin ~xs))
              (- (* (/ 1 2) pi) ~xs)))
          'acos-sin)))
     (- (* (/ 1 2) pi) ?x)))))

(defn special-trig
  "TODO check that we can actually handle these damned symbols for 2pi etc!"
  [simplify]
  (let [sym:+ (sym/symbolic-operator '+)
        sym:- (sym/symbolic-operator '-)
        sym:* (sym/symbolic-operator '*)
        sym:div (sym/symbolic-operator '/)]
    (letfn [(zero-mod-pi? [x]
              (v/integral?
               (simplify (sym:div x 'pi))))

            (pi-over-2-mod-2pi? [x]
              (v/integral?
               (simplify
                (sym:div (sym:- x (sym:div 'pi 2))
                         (sym:* 2 'pi)))))

            (-pi-over-2-mod-2pi? [x]
              (v/integral?
               (simplify
                (sym:div (sym:+ x (sym:div 'pi 2))
                         (sym:* 2 'pi)))))

            (pi-over-2-mod-pi? [x]
              (v/integral?
               (simplify
                (sym:div (sym:- x (sym:div 'pi 2))
                         'pi))))

            (zero-mod-2pi? [x]
              (v/integral?
               (simplify
                (sym:div x (sym:* 2 'pi)))))

            (pi-mod-2pi? [x]
              (v/integral?
               (simplify
                (sym:div (sym:- x 'pi)
                         (sym:* 2 'pi)))))

            (pi-over-4-mod-pi? [x]
              (v/integral?
               (simplify
                (sym:div (sym:- x (sym:div 'pi 4)) 'pi))))

            (-pi-over-4-mod-pi? [x]
              (v/integral?
               (simplify
                (sym:div (sym:+ x (sym:div 'pi 4))
                         'pi))))]
      (rule-simplifier
       (ruleset
        (sin (? _ zero-mod-pi?))   =>  0
        (sin (? _ pi-over-2-mod-2pi?))  => +1
        (sin (? _ -pi-over-2-mod-2pi?)) => -1

        (cos (? _ pi-over-2-mod-pi?))   =>  0
        (cos (? _ zero-mod-2pi?))  => +1
        (cos (? _ pi-mod-2pi?))    => -1

        (tan (? _ zero-mod-pi?))   =>  0
        (tan (? _ pi-over-4-mod-pi?))   => +1
        (tan (? _ -pi-over-4-mod-pi?))  => -1)))))



(def ^{:doc "sin is odd, and cos is even. we canonicalize by moving the sign out
of the first term of the argument."}
  angular-parity
  (rule-simplifier
   (ruleset
    (cos (? ?n negative-number?))
    => (cos (? #(- (% '?n))))

    (cos (* (? ?n negative-number?) ??x))
    => (cos (* (? #(- (% '?n))) ??x))

    (cos (+ (* (? ?n negative-number?) ??x) ??y))
    => (cos (- (* (? #(- (% '?n))) ??x) ??y))

    (sin (? ?n negative-number?))
    => (- (sin (? #(- (% '?n)))))

    (sin (* (? ?n negative-number?) ??x))
    => (- (sin (* (? #(- (% '?n))) ??x)))

    (sin (+ (* (? ?n negative-number?) ??x) ??y))
    => (- (sin (- (* (? #(- (% '?n))) ??x) ??y))))))

(def expand-multiangle
  (letfn [(exact-integer>3? [x]
            (and (v/integral? x)
                 (v/exact? x)
                 (> x 3)))]
    (rule-simplifier
     (ruleset
      (sin (* 2 ?x ??y))
      => (* 2 (sin (* ?x ??y)) (cos (* ?x ??y)))

      (cos (* 2 ?x ??y))
      => (- (* 2 (expt (cos (* ?x ??y)) 2)) 1)

      (sin (* 3 ?x ??y))
      => (+ (* 3 (sin (* ?x ??y))) (* -4 (expt (sin (* ?x ??y)) 3)))

      (cos (* 3 ?x ??y))
      => (+ (* 4 (expt (cos (* ?x ??y)) 3)) (* -3 (cos (* ?x ??y))))

      ;; at least one f
      (sin (* (? ?n exact-integer>3?) ?f ??fs))
      => (+ (* (sin (* (? #(g/- (% '?n) 1)) ?f ??fs))
               (cos (* ?f ??fs)))
            (* (cos (* (? #(g/- (% '?n) 1)) ?f ??fs))
               (sin (* ?f ??fs))))

      ;; at least one y
      (sin (+ ?x ?y ??ys))
      => (+ (* (sin ?x) (cos (+ ?y ??ys)))
            (* (cos ?x) (sin (+ ?y ??ys))))

      ;; at least one f
      (cos (* (? n exact-integer>3?) ?f ??fs))
      => (- (* (cos (* (? #(g/- (% '?n) 1)) ?f ??fs))
               (cos (* ?f ??fs)))
            (* (sin (* (? #(g/- (% '?n) 1)) ?f ??fs))
               (sin (* ?f ??fs))))

      ;; at least one y
      (cos (+ ?x ?y ??ys))
      => (- (* (cos ?x) (cos (+ ?y ??ys)))
            (* (sin ?x) (sin (+ ?y ??ys))))))))

(def trig-sum-to-product
  (rule-simplifier
   (ruleset
    (+ ??a (sin ?x) ??b (sin ?y) ??c )
    => (+ (* 2 (sin (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2)))
          ??a ??b ??c)

    (+ ??a (sin ?x) ??b (* -1 (sin ?y)) ??c )
    => (+ (* 2 (sin (/ (- ?x ?y) 2)) (cos (/ (+ ?x ?y) 2)))
          ??a ??b ??c)

    (+ ??a (* -1 (sin ?y)) ??b (sin ?x) ??c )
    => (+ (* 2 (sin (/ (- ?x ?y) 2)) (cos (/ (+ ?x ?y) 2)))
          ??a ??b ??c)

    (+ ??a (cos ?x) ??b (cos ?y) ??c )
    => (+ (* 2 (cos (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2)))
          ??a ??b ??c)

    (+ ??a (cos ?x) ??b (* -1 (cos ?y)) ??c )
    => (+ (* -2 (sin (/ (+ ?x ?y) 2)) (sin (/ (- ?x ?y) 2)))
          ??a ??b ??c)

    (+ ??a (* -1 (cos ?y)) ??b (cos ?x) ??c )
    => (+ (* -2 (sin (/ (+ ?x ?y) 2)) (sin (/ (- ?x ?y) 2)))
          ??a ??b ??c))))

(def trig-product-to-sum
  (rule-simplifier
   (ruleset
    (* ??u (sin ?x) ??v (sin ?y) ??w)
    => (* (/ 1 2) (- (cos (- ?x ?y)) (cos (+ ?x ?y)))
          ??u ??v ??w)

    (* ??u (cos ?x) ??v (cos ?y) ??w)
    => (* (/ 1 2) (+ (cos (- ?x ?y)) (cos (+ ?x ?y)))
          ??u ??v ??w)

    (* ??u (sin ?x) ??v (cos ?y) ??w)
    => (* (/ 1 2) (+ (sin (+ ?x ?y)) (sin (- ?x ?y)))
          ??u ??v ??w)

    (* ??u (cos ?y) ??v (sin ?x) ??w)
    => (* (/ 1 2) (+ (sin (+ ?x ?y)) (sin (- ?x ?y)))
          ??u ??v ??w))))

(def contract-expt-trig
  (letfn [(exact-integer>1? [x]
            (and (v/integral? x)
                 (v/exact? x)
                 (> x 1)))]
    (rule-simplifier
     (ruleset
      (expt (sin ?x) (? ?n exact-integer>1?))
      => (* (/ 1 2)
            (expt (sin ?x) (? #(- (% '?n) 2)))
            (- 1 (cos (* 2 ?x))))

      (expt (cos ?x) (? ?n exact-integer>1?))
      => (* (/ 1 2)
            (expt (cos ?x) (? #(- (% '?n) 2)))
            (+ 1 (cos (* 2 ?x))))))))

(defn half-angle [simplify]
  (letfn [(sin-half-angle-formula [theta]
            (let [thetas (simplify theta)]
              (and (assume!
                    (r/template
                     (non-negative?
                      (+ (* 2 pi)
                         (* -1 ~thetas)
                         (* 4 pi (floor (/ ~thetas (* 4 pi)))))))
                    'sin-half-angle-formula)
                   (r/template
                    (sqrt (/ (- 1 (cos ~theta)) 2))))))

          (cos-half-angle-formula [theta]
            (let [thetas (simplify theta)]
              (and (assume!
                    (r/template
                     (non-negative?
                      (+ pi ~thetas
                         (* 4 pi (floor
                                  (/ (- pi ~thetas)
                                     (* 4 pi)))))))
                    'cos-half-angle-formula)
                   (r/template
                    (sqrt (/ (+ 1 (cos ~theta)) 2))))))]
    (rule-simplifier
     (r/guard
      (fn [_] *half-angle-simplify?*)
      (r/ruleset*
       (r/rule
        (sin (* (/ 1 2) ?x ??y))
        #(sin-half-angle-formula
          (r/template % (* ?x ??y))))

       (r/rule
        (sin (/ ?x 2))
        #(sin-half-angle-formula (% '?x)))

       (r/rule
        (cos (* (/ 1 2) ?x ??y))
        #(cos-half-angle-formula
          (r/template % (* ?x ??y))))

       (r/rule
        (cos (/ ?x 2))
        #(cos-half-angle-formula (% '?x))))))))

(def sin-sq->cos-sq
  (rule-simplifier
   (ruleset
    (expt (sin ?x) (? ?n at-least-two?))
    => (* (expt (sin ?x) (? #(- (% '?n) 2)))
          (- 1 (expt (cos ?x) 2))))))

(def cos-sq->sin-sq
  (rule-simplifier
   (ruleset
    (expt (cos ?x) (? ?n at-least-two?))
    => (* (expt (cos ?x) (? #(- (% '?n) 2)))
          (- 1 (expt (sin ?x) 2))))))

(def split-high-degree-sincos
  (letfn [(remaining [{n '?n :as m}]
            (let [n-2 (g/- n 2)]
              (if (v/one? n-2)
                (r/template m (?op ?x))
                (r/template
                 m (expt (?op ?x) ~n-2)))))]
    (rule-simplifier
     (ruleset
      (* ??f1
         (expt ((? ?op #{'sin 'cos}) ?x) (? ?n more-than-two?))
         ??f2)
      => (* ??f1
            (expt (?op ?x) 2) (? ~remaining)
            ??f2)

      (+ ??a1
         (expt ((? ?op #{'sin 'cos}) ?x) (? ?n more-than-two?))
         ??a2)
      => (+ ??a1
            (* (expt (?op ?x) 2) (? ~remaining))
            ??a2)))))

(defn flush-obvious-ones
  "TODO: test that this ONLY catches sin and cos in opposite order, NOT the same
  stuff or anything random."
  [simplify]
  (let [?op      (pm/bind '?op #{'sin 'cos})
        ?flipped (pm/or (pm/and 'cos (pm/frame-predicate
                                      #(= 'sin (% '?op))))
                        (pm/and 'sin (pm/frame-predicate
                                      #(= 'cos (% '?op)))))]
    (letfn [(pred [m]
              (let [s1 (simplify (r/template m (* ??f1 ??f2)))
                    s2 (simplify (r/template m (* ??f3 ??f4)))]
                (when (v/exact-zero?
                       (simplify (list '- s1 s2)))
                  {'?s1 s1})))]
      (rule-simplifier
       (ruleset
        (+ ??a1 (expt (~?op ?x) 2)
           ??a2 (expt (~?flipped ?x) 2)
           ??a3)
        => (+ 1 ??a1 ??a2 ??a3)

        (+ ??a1 (* ??f1 (expt (~?op ?x) 2) ??f2)
           ??a2 (* ??f3 (expt (~?flipped ?x) 2) ??f4)
           ??a3)
        pred
        (+ ??a1 ??a2 ??a3 ?s1))))))

(defn sincos-flush-ones [simplify]
  (r/pipe
   split-high-degree-sincos
   (flush-obvious-ones simplify)))

(defn sincos-random [simplify]
  (let [simplifies-to-zero? (comp v/zero? simplify)
        ops #{'cos 'sin}
        flip {'cos 'sin, 'sin 'cos}]
    (rule-simplifier
     (letfn [(pred [{n '?n op '?op :as m}]
               (when (simplifies-to-zero?
                      (r/template m (+ ?a (expt (?op ?x) ~(g/- n 2)))))
                 {'?other-op (flip op)}))]
       (ruleset
        ;;  ... + a + ... + cos^n x + ...   if a + cos^(n-2) x = 0: a sin^2 x
        (+ ??a1 ?a ??a2 (expt ((? ?op ops) ?x) (? ?n at-least-two?)) ??a3)
        pred
        (+ ??a1 ??a2 ??a3 (* ?a (expt (?other-op ?x) 2)))

        (+ ??a1 (expt ((? ?op ops) ?x) (? ?n at-least-two?)) ??a2 ?a ??a3)
        pred
        (+ ??a1 ??a2 ??a3 (* ?a (expt (?other-op ?x) 2)))))

     (letfn [(pred [{n '?n op '?op :as m}]
               (when (simplifies-to-zero?
                      (r/template m (+ ?a (* ??b1 ??b2 (expt (?op ?x) ~(g/- n 2))))))
                 {'?other-op (flip op)}))]
       (ruleset
        (+ ??a1 ?a ??a2 (* ??b1 (expt ((? ?op ops) ?x) (? ?n at-least-two?)) ??b2)
           ??a3)
        pred
        (+ ??a1 ??a2 ??a3 (* ?a (expt (?other-op ?x) 2)))

        (+ ??a1 (* ??b1 (expt ((? ?op ops) ?x) (? ?n at-least-two?)) ??b2) ??a2 ?a
           ??a3)
        pred
        (+ ??a1 ??a2 ??a3 (* ?a (expt (?other-op ?x) 2))))))))

;; we can eliminate sin and cos in favor of complex exponentials.

(def sincos->exp1
  (let [i  '(complex 0.0 1.0)
        -i '(complex 0.0 -1.0)]
    (rule-simplifier
     (ruleset
      (sin ?x)
      => (/ (- (exp (* ~i ?x)) (exp (* ~-i ?x)))
            (complex 0.0 2.0))

      (cos ?x)
      => (/ (+ (exp (* ~i ?x)) (exp (* ~-i ?x)))
            2)))))

(def sincos->exp2
  (let [i '(complex 0.0 1.0)]
    (rule-simplifier
     (ruleset
      (sin ?x)
      => (/ (- (exp (* ~i ?x)) (/ 1 (exp (* ~i ?x))))
            (complex 0.0 2.0))

      (cos ?x)
      => (/ (+ (exp (* ~i ?x)) (/ 1 (exp (* ~i ?x))))
            2)))))

;; under favorable conditions, we can replace the trig functions.

(def exp->sincos
  (let [i  '(complex 0.0 1.0)
        -i '(complex 0.0 -1.0)]
    (letfn [(positive? [x]
              (not (or (g/negative? x)
                       (v/zero? x))))

            (pos-pred [m]
              (let [im (g/imag-part (m '?c1))]
                (when (positive? im)
                  {'?im im})))

            (neg-pred [m]
              (let [im (g/imag-part (m '?c1))]
                (when (g/negative? im)
                  {'?im im})))]
      (rule-simplifier
       (ruleset
        (exp (? ?c1 imaginary-number?))
        pos-pred
        (+ (cos ?im)
           (* ~i (sin ?im)))

        (exp (? ?c1 imaginary-number?))
        neg-pred
        (+ (cos (? #(- (% '?im))))
           (* ~-i (sin (? #(- (% '?im))))))

        (exp (* (? ?c1 imaginary-number?) ??f))
        pos-pred
        (+ (cos (* ?im ??f))
           (* ~i (sin (* ?im ??f))))

        (exp (* (? ?c1 imaginary-number?) ??f))
        neg-pred
        (* (exp (? #(g/real-part (% '?c1))))
           (+ (cos (* (? #(- (% '?im))) ??f))
              (* ~-i (sin (* (? #(- (% '?im))) ??f)))))

        (exp (? ?c1 complex-number?))
        pos-pred
        (* (exp (? #(g/real-part (% '?c1))))
           (+ (cos ?im)
              (* ~i (sin ?im))))

        (exp (? ?c1 complex-number?))
        neg-pred
        (* (exp (? #(g/real-part (% '?c1))))
           (+ (cos (? #(- (% '?im))))
              (* ~-i (sin (? #(- (% '?im)))))))

        (exp (* (? ?c1 complex-number?) ??f))
        pos-pred
        (* (exp (? #(g/real-part (% '?c1))))
           (+ (cos (* ?im ??f))
              (* ~i (sin (* ?im ??f)))))

        (exp (* (? ?c1 complex-number?) ??f))
        neg-pred
        (* (exp (? #(g/real-part (% '?c1))))
           (+ (cos (* (? #(- (% '?im))) ??f))
              (* ~-i (sin (* (? #(- (% '?im))) ??f))))))))))

(def exp-contract
  (rule-simplifier
   (ruleset
    (* ??x1 (exp ?x2) ??x3 (exp ?x4) ??x5)
    =>
    (* ??x1 ??x3 ??x5 (exp (+ ?x2 ?x4)))

    (expt (exp ?x) (? p)) => (exp (* ?p ?x))

    (/ (exp ?x) (exp ?y)) => (exp (- ?x ?y))

    (/ (* ??x1 (exp ?x) ??x2) (exp ?y))
    =>
    (* ??x1 ??x2 (exp (- ?x ?y)))

    (/ (exp ?x) (* ??y1 (exp ?y) ??y2))
    =>
    (/ (exp (- ?x ?y)) (* ??y1 ??y2))

    (/ (* ??x1 (exp ?x) ??x2)
       (* ??y1 (exp ?y) ??y2))
    =>
    (/ (* ??x1 ??x2 (exp (- ?x ?y)))
       (* ??y1 ??y2)))))

(def exp-expand
  (let [i '(complex 0.0 1.0)
        -i '(complex 0.0 -1.0)
        exact-integer? (fn [x]
                         (and (v/integral? x)
                              (v/exact? x)))]
    (rule-simplifier
     (ruleset
      (exp (- ?x1)) => (/ 1 (exp ?x1))

      (exp (- ?x1 ?x2)) => (/ (exp ?x1) (exp ?x2))

      (exp (+ ?x1 ?x2 ??xs))
      =>
      (* (exp ?x1) (exp (+ ?x2 ??xs)))

      (exp (* (? ?x imaginary-integer? #(> (g/imag-part %) 1))
              ??factors))
      =>
      (expt (exp (* ~i ??factors))
            (? #(g/imag-part (% '?x))))

      (exp (* (? ?x imaginary-integer? #(< (g/imag-part %) -1))
              ??factors))
      =>
      (expt (exp (* ~-i ??factors))
            (? #(g/- (g/imag-part (% '?x)))))

      (exp (* (? ?n exact-integer? #(> % 1))
              ??factors))
      =>
      (expt (exp (* ??factors)) ?n)

      (exp (* (? ?n exact-integer? #(< % -1))
              ??factors))
      =>
      (expt (exp (* -1 ??factors))
            (? #(- (% '?n))))

      (exp (? ?x complex-number?))
      =>
      (* (exp (? #(g/real-part (% '?x))))
         (exp (? #(g/* (g/imag-part (% '?x)) ~i))))

      (exp (* (? ?x complex-number?)
              ??factors))
      =>
      (* (exp (* (? #(g/real-part (% '?x)))
                 ??factors))
         (exp (* (? #(g/* (g/imag-part (% '?x)) ~i))
                 ??factors)))))))

(def complex-trig
  (rule-simplifier
   (ruleset
    (cos (* ?z (complex 0.0 1.0)))
    => (cosh ?z)

    (sin (* ?z (complex 0.0 1.0)))
    => (* (complex 0.0 1.0) (sinh ?z))

    ;; Does this really belong here?
    ;; It works by reducing n mod 4 and then indexing into [1 i -1 -i].
    (expt (complex 0.0 1.0) (? ?n v/integral?))
    => (? #([1 '(complex 0.0 1.0) -1 '(complex 0.0 -1.0)]
            (mod (% '?n) 4))))))

(def complex-rules
  (rule-simplifier
   (ruleset
    (make-rectangular (cos ?theta) (sin ?theta))
    => (exp (* (complex 0.0 1.0) ?theta))

    (real-part (make-rectangular ?x _)) => ?x
    (imag-part (make-rectangular _ ?y)) => ?y

    (magnitude (make-rectangular ?x ?y))
    => (sqrt (+ (expt ?x 2) (expt ?y 2)))

    (angle (make-rectangular ?x ?y)) => (atan ?y ?x)

    (real-part (make-polar ?m ?a)) => (* ?m (cos ?a))
    (imag-part (make-polar ?m ?a)) => (* ?m (sin ?a))

    (magnitude (make-polar ?m _)) => ?m
    (angle (make-polar _ ?a)) => ?a)))

(def divide-numbers-through
  (rule-simplifier
   (ruleset
    (* 1 ?factor) => ?factor
    (* 1 ??factors) => (* ??factors)

    (/ (? ?n v/number?)
       (? ?d v/number?))
    => (? (fn [{n '?n d '?d}]
            (g// n d)))

    (/ (+ ??terms)
       (? ?d v/number?))
    => (+ (?? #(map (fn [n]
                      (r/template % (/ ~n ?d)))
                    (% '??terms))))

    (/ (* (? ?n v/number?) ??factors)
       (? ?d v/number?))
    =>
    (* (? (fn [{n '?n d '?d}]
            (g// n d)))
       ??factors)

    (/ (* ??factors) (? ?d v/number?))
    =>
    (* (? #(g/invert (% '?d))) ??factors)

    (/ ?n
       (* (? ?d v/number?) ?factor))
    =>
    (/ (/ ?n ?d) ?factor)

    (/ ?n
       (* (? ?d v/number?) ??factors))
    =>
    (/ (/ ?n ?d) (* ??factors))

    (/ ?n (? ?d v/number?))
    =>
    (* (? #(g/invert (% '?d))) ?n))))

(defn ^:no-doc occurs-in? [syms all]
  (not
   (empty?
    (cs/intersection syms all))))

(defn universal-reductions [simplify]
  (let [misc     (miscsimp simplify)
        le       (logexp simplify)
        st       (special-trig simplify)
        ti       (triginv simplify)
        sim-root (simplify-square-roots simplify)]
    (fn [expr]
      (let [syms     (x/variables-in expr)
            logexp?  (occurs-in? #{'log 'exp} syms)
            sincos?  (occurs-in? #{'sin 'cos} syms)
            invtrig? (occurs-in? #{'asin 'acos 'atan} syms)
            logexp?  (occurs-in? #{'log 'exp} syms)
            sqrt?    (contains? syms 'sqrt)
            mag?     (contains? syms 'magnitude)
            expr'    (cond-> (misc expr)
                       logexp? (le)
                       mag? (magnitude)
                       invtrig? (ti)
                       (and sincos? *sin-cos-simplify?*) (st))]
        (cond (and sincos? invtrig?) (sim-root (ti expr'))
              sqrt? (sim-root expr')
              :else expr')))))
