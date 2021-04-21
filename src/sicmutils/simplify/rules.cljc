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
  (:require [clojure.set :as cs]
            [pattern.rule :as r :refer [=> ruleset rule-simplifier]
             #?@(:cljs [:include-macros true])]
            [sicmutils.complex :as c]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as sym]
            [sicmutils.value :as v]))

;; TODO try and get all `rule-simplifier` out of here. These are just rulesets.

(defn assume!
  "see logic-utils.scm... to get this working."
  ([predicate-expr responsible-party]
   true)
  ([predicate-expr responsible-party if-false]
   true))

(defn note-that! [note]
  ;; do nothing for now.
  )

(defn meta-conj [obj k v]
  (vary-meta obj update k (fn [old]
                            (conj (or old []) v))))

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
  false)

(def ^{:dynamic true
       :doc "Allow half-angle reductions.  Sign of result is hairy!"}
  *half-angle-simplify?*
  true)

(def ^{:dynamic true
       :doc "wierd case: ((d magnitude) (square x)) => 1"}
  *ignore-zero?*
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
  true)

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

(defn even-integer? [x]
  (and (v/integral? x)
       (v/zero? (g/modulo x 2))))

(defn odd-integer? [x]
  (and (v/integral? x)
       (not (v/zero? (g/modulo x 2)))))

(defn even-positive-integer? [x]
  (and (even-integer? x)
       (> x 1)))

(defn odd-positive-integer? [x]
  (and (odd-integer? x)
       (> x 2)))

(defn more-than-two? [x]
  (and (v/number? x) (> x 2)))

(defn at-least-two? [x]
  (and (v/number? x) (>= x 2)))

;; Ported from Alexey's Rules.

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
     ((:? _ op-set) ?x) => ?x)))

(defn constant-elimination
  "Takes an operation `op` and an identity element `constant` and returns a rule
  that eliminates instances of `constant` inside binary forms like `(<op> l
  r)`."
  [op constant]
  (ruleset
   (~op ~constant ?x) => (~op ?x)
   (~op ?x ~constant) => (~op ?x)))

(defn constant-promotion [op constant]
  (ruleset
   (~op _ ~constant) => ~constant
   (~op ~constant _) => ~constant))

(defn associative
  "Takes a sequence `ops` of operator symbols like `'+`, `'*` and returns a rule
  that collapses nested applications of each operation into a single list. (The
  associative property lets us strip parentheses.)
  ```clojure
  (let [rule (associative '+ '*)
        f    (rule-simplifier rule)]
    (f '(+ x (+ y (+ z a) (* b (* c d))
                (+ cake face)))))
  ;;=> (+ x y z a (* b c d) cake face)
  ```"
  [& ops]
  (let [op-set  (into #{} ops)
        flatten (fn [op]
                  (fn [term]
                    ;; TODO we COULD use rules for this too :)
                    (if (and (sequential? term)
                             (= op (first term)))
                      (rest term)
                      [term])))]
    (ruleset
     ((:? ?op op-set) ??a (?op ??b) ??c)
     =>
     (?op ??a (:?? (fn [{op '?op, b '??b, c '??c}]
                     (mapcat (flatten op)
                             (concat b c))))))))

(defn commutative
  "Flipping one at a time is bubble sort
  (rule `(,operator (?? a) (? y) (? x) (?? b))
        (and (expr<? x y)
             `(,operator ,@a ,x ,y ,@b)))
  Finding a pair out of order and sorting is still quadratic,
  because the matcher matches N times, and each requires
  constructing the segments so they can be handed to the handler
  (laziness would help).
  (rule `(,operator (?? a) (? y) (? x) (?? b))
        (and (expr<? x y)
             `(,operator ,@(sort `(,@a ,x ,y ,@b) expr<?))))"
  [& ops]
  (let [op-set (into #{} ops)]
    (ruleset
     ((:? ?op op-set) ??xs)
     #(not (x/sorted? (% '??xs)))
     (?op (:?? #(x/sort (% '??xs)))))))

(defn idempotent [& ops]
  (let [op-set (into #{} ops)]
    (ruleset
     ((:? ?op op-set) ??pre ?x ?x ??post)
     =>
     (?op (:?? (fn [m]
                 (dedupe
                  (concat (m '??pre)
                          [(m '?x)]
                          (m '??post)))))))))

(def ^{:doc "Set of rules that collect adjacent products, exponents and nested
 exponents into exponent terms."}
  exponent-contract
  (ruleset
   ;; nested exponent case.
   (expt (expt ?op (:? ?n v/integral?))
         (:? ?m v/integral?))
   => (expt ?op (:? #(g/+ (% '?n) (% '?m))))

   ;; adjacent pairs of exponents
   (* ??pre
      (expt ?op (:? ?n v/integral?))
      (expt ?op (:? ?m v/integral?))
      ??post)
   => (* ??pre
         (expt ?op (:? #(g/+ (% '?n) (% '?m))))
         ??post)

   ;; exponent on right, non-expt on left
   (* ??pre
      ?op (expt ?op (:? ?n v/integral?))
      ??post)
   => (* ??pre
         (expt ?op (:? #(g/+ (% '?n) 1)))
         ??post)

   ;; exponent on left, non-expt on right
   (* ??pre
      (expt ?op (:? ?n v/integral?)) ?op
      ??post)
   => (* ??pre
         (expt ?op (:? #(g/+ (% '?n) 1)))
         ??post)

   ;; non-exponent pairs
   (* ??pre ?op ?op ??post)
   => (* ??pre (expt ?op 2) ??post)))

(defn logexp [simplify]
  (ruleset
   (exp (* (:? ?n v/integral?) (log ?x))) => (expt ?x ?n)

   (exp (log ?x)) => ?x

   (log (exp ?x))
   (fn [m]
     (and *log-exp-simplify?*
          (let [xs (simplify (m '?x))]
            (assume!
             `(~'= (~'log (~'exp ~xs)) ~xs) 'logexp1))))
   ?x

   (sqrt (exp ?x))
   (fn [m]
     (and *sqrt-expt-simplify?*
          (let [xs (simplify (m '?x))]
	          (assume!
             `(~'= (~'sqrt (~'exp ~xs)) (~'exp (~'/ ~xs 2)))
             'logexp2))))
   (exp (/ ?x 2))

   (log (sqrt ?x)) => (* (/ 1 2) (log ?x))))

(def magsimp
  (ruleset
   (magnitude (* ?x ?y ??ys))
   => (* (magnitude ?x) (magnitude (* ?y ??ys)))

   (magnitude (expt ?x (:? ?n even-integer?)))
   => (expt ?x ?n)))

(defn miscsimp [simplify]
  (ruleset
   (expt _ 0) => 1

   (expt ?x 1) => ?x

   (expt (expt ?x ?a) ?b)
   (fn [m]
     (let [a (simplify (m '?a))
           b (simplify (m '?b))
           x (m '?x)
           sym:* (sym/symbolic-operator '*)]
       (or (and (v/integral? a) (v/integral? b))

           (and (even-integer? b)
                (v/integral? (simplify (sym:* a b))))

           (and *exponent-product-simplify?*
                (assume!
                 `(~'= (~'expt (~'expt ~x ~a) ~b)
                   (~'expt ~x (~sym:* ~a ~b)))
                 'exponent-product)))))
   (expt ?x (:? #(g/* (% '?a) (% '?b))))

   (expt ?x (/ 1 2))
   (fn [_] *expt-half->sqrt?*)
   (sqrt ?x)

   ;; a rare, expensive luxury
   (* ??fs1* ?x ??fs2 (expt ?x ?y) ??fs3)
   =>
   (* ??fs1 ??fs2 (expt ?x (+ 1 ?y)) ??fs3)

   ;; a rare, expensive luxury
   (* ??fs1 (expt ?x ?y) ??fs2 ?x ??fs3)
   =>
   (* ??fs1 (expt ?x (+ 1 ?y)) ??fs2 ??fs3)

   ;; a rare, expensive luxury
   (* ??fs1 (expt ?x ?y1) ??fs2 (expt ?x ?y2) ??fs3)
   =>
   (* ??fs1 ??fs2 (expt ?x (+ ?y1 ?y2)) ??fs3)))

;; ## Square Root Simplification

(defn simplify-square-roots [simplify]
  (rule-simplifier
   (ruleset
    (expt (sqrt ?x) (:? ?n even-integer?))
    => (expt ?x (:? #(g// (% '?n) 2)))

    (sqrt (expt ?x (:? ?n even-integer?)))
    (fn [m]
      (and *sqrt-expt-simplify?*
           (let [xs (simplify (m '?x))
                 n  (m '?n)]
	           (assume!
              `(~'=
                (~'sqrt (~'expt ~xs ~n))
                (~'expt ~xs ~(g// n 2)))
              'simsqrt1))))
    (expt ?x (:? #(g// (% '?n) 2)))

    (sqrt (expt ?x (:? ?n odd-positive-integer?)))
    (fn [m]
      (and *sqrt-expt-simplify?*
	         (let [xs (simplify (m '?x))
                 n  (m '?n)]
	           (assume!
              `(~'=
                (~'sqrt (~'expt ~xs ~n))
                (~'expt ~xs ~(g// (g/- n 1) 2)))
              'simsqrt2))))
    (* (sqrt ?x)
       (expt ?x (:? #(g// (g/- (% '?n) 1) 2))))

    (expt (sqrt ?x) (:? ?n odd-integer?))
    => (* (sqrt ?x)
          (expt ?x (:? #(g// (g/- (% '?n) 1) 2))))

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

(defn non-negative-factors [simplify x y id]
  (let [xs (simplify x)
	      ys (simplify y)]
    (and (assume! `(~'non-negative? ~xs) id (fn [] false))
         (assume! `(~'non-negative? ~ys) id (fn [] false)))))

;; distribute the radical sign across products and quotients. but doing this
;; may allow equal subexpressions within the radicals to cancel in various
;; ways. The companion rule sqrt-contract reassembles what remains.

(defn sqrt-expand [simplify]
  (rule-simplifier
   (ruleset
    (sqrt (* ?x ?y))
    (fn [m]
      (and *sqrt-factor-simplify?*
	         (non-negative-factors simplify (m '?x) (m '?y) 'e1)))
    (* (sqrt ?x) (sqrt ?y))

    (sqrt (* ?x ?y ??ys))
    (fn [m]
      (and *sqrt-factor-simplify?*
	         (non-negative-factors simplify (m '?x) (m '?y) 'e2)))
    (* (sqrt ?x) (sqrt (* ?y ??ys)))

    (sqrt (/ ?x ?y))
    (fn [m]
      (and *sqrt-factor-simplify?*
	         (non-negative-factors simplify (m '?x) (m '?y) 'e3)))
    (/ (sqrt ?x) (sqrt ?y))

    (sqrt (/ ?x ?y ??ys))
    (fn [m]
      (and *sqrt-factor-simplify?*
	         (non-negative-factors simplify (m '?x) (m '?y) 'e4)))
    (/ (sqrt ?x) (sqrt (* ?y ??ys))))))

(defn sqrt-contract [simplify]
  (let [if-false (fn [] false)]
    (r/rule-simplifier
     (r/choice
      (r/rule
       (* ??a (sqrt ?x) ??b (sqrt ?y) ??c)
       (fn [m]
         (let [xs (simplify (m '?x))
               ys (simplify (m '?y))]
           (if (v/= xs ys)
             (and (assume! `(~'non-negative? ~xs) 'c1 if-false)
                  `(~'* ~@(m '??a) ~xs ~@(m '??b) ~@(m '??c)))
             (and (assume! `(~'non-negative? ~xs) 'c1 if-false)
                  (assume! `(~'non-negative? ~ys) 'c1 if-false)
                  `(~'* ~@(m '??a)
                    (~'sqrt (~'* ~xs ~ys))
                    ~@(m '??b) ~@(m '??c)))))))

      (r/rule
       (/ (sqrt ?x) (sqrt ?y))
       (fn [m]
         (let [xs (simplify (m '?x))
               ys (simplify (m '?y))]
           (if (v/= xs ys)
             (and (assume! `(~'non-negative? ~xs) 'c2 if-false)
                  1)
             (and (assume! `(~'non-negative? ~xs) 'c2 if-false)
                  (assume! `(~'non-negative? ~ys) 'c2 if-false)
                  `(~'sqrt (~'/ ~xs ~ys)))))))

      (r/rule
       (/ (* ??a (sqrt ?x) ??b) (sqrt ?y))
       (fn [m]
         (let [xs (simplify (m '?x))
               ys (simplify (m '?y))]
           (if (v/= xs ys)
             (and (assume! `(~'non-negative? ~xs) 'c3 if-false)
                  `(~'* ~@(m '??a) ~@(m '??b)))
             (and (assume! `(~'non-negative? ~xs) 'c3 if-false)
                  (assume! `(~'non-negative? ~ys) 'c3 if-false)
                  `(~'* ~@(m '??a)
                    (~'sqrt (~'/ ~xs ~ys))
                    ~@(m '??b)))))))

      (r/rule
       (/ (sqrt ?x) (* ??a (sqrt ?y) ??b))
       (fn [m]
         (let [xs (simplify (m '?x))
               ys (simplify (m '?y))]
           (if (v/= xs ys)
             (and (assume! `(~'non-negative? ~xs) 'c4 if-false)
                  `(~'/ 1 (~'* ~@(m '??a) ~@(m '??b))))
             (and (assume! `(~'non-negative? ~xs) 'c4 if-false)
                  (assume! `(~'non-negative? ~ys) 'c4 if-false)
                  `(~'/ (~'sqrt (~'/ ~xs ~ys))
                    (~'* ~@(m '??a) ~@(m '??b))))))))

      (r/rule
       (/ (* ??a (sqrt ?x) ??b)
          (* ??c (sqrt ?y) ??d))
       (fn [m]
         (let [xs (simplify (m '?x))
               ys (simplify (m '?y))]
           (if (v/= xs ys)
             (and (assume! `(~'non-negative? ~xs) 'c5 if-false)
                  `(~'/
                    (~'* ~@(m '??a) ~@(m '??b))
                    (~'* ~@(m '??c) ~@(m '??d))))
             (and (assume! `(~'non-negative? ~xs) 'c5 if-false)
                  (assume! `(~'non-negative? ~ys) 'c5 if-false)
                  `(~'/
                    (~'* ~@(m '??a) (~'sqrt (~'/ ~xs ~ys)) ~@(m '??b))
                    (~'* ~@(m '??c) ~@(m '??d))))))))))))

;; ## Log / Exp

(def specfun->logexp
  (ruleset
   (sqrt ?x) => (exp (* (/ 1 2) (log ?x)))

   (atan ?z)
   =>
   (/ (- (log (+ 1 (* (complex 0.0 1.0) ?z)))
         (log (- 1 (* (complex 0.0 1.0) ?z))))
      (complex 0.0 2.0))

   (asin ?z)
   =>
   (* (complex 0.0 -1.0)
      (log (+ (* (complex 0.0 1.0) ?z)
              (sqrt (- 1 (expt ?z 2))))))

   (acos ?z)
   =>
   (* (complex 0.0 -1.0)
      (log (+ ?z (* (complex 0.0 1.0)
                    (sqrt (- 1 (expt ?z 2)))))))

   (sinh ?u) => (/ (- (exp ?u) (exp (* -1 ?u))) 2)

   (cosh ?u) => (/ (+ (exp ?u) (exp (* -1 ?u))) 2)

   (expt ?x (:? ?y non-integer?)) => (exp (* ?y (log ?x)))))

(def logexp->specfun
  (ruleset
   (exp (* -1 (log ?x))) => (expt ?x -1)

   (exp (* (/ 1 2) (log ?x))) => (sqrt ?x)

   (exp (* (/ -1 2) (log ?x))) => (/ 1 (sqrt ?x))

   (exp (* (/ 3 2) (log ?x))) => (expt (sqrt ?x) 3)

   (exp (* (/ -3 2) (log ?x))) => (expt (sqrt ?x) -3)

   (exp (* ??n1 (log ?x) ??n2))
   =>
   (expt ?x (* ??n1 ??n2))))

(defn log-contract [simplify]
  (ruleset
   (+ ??x1 (log ?x2) ??x3 (log ?x4) ??x5)
   =>
   (+ ??x1 ??x3 ??x5 (log (* ?x2 ?x4)))

   (- (log ?x) (log ?y))
   =>
   (log (/ ?x ?y))

   (+ ??x1
      (* ??f1 (log ?x) ??f2)
      ??x2
      (* ??f3 (log ?y) ??f4)
      ??x3)
   (fn [m]
     (let [s1 (simplify `(~'* ~@(m '??f1) ~@(m '??f2)))
           s2 (simplify `(~'* ~@(m '??f3) ~@(m '??f4)))]
       (when (v/exact-zero? (simplify `(~'- ~s1 ~s2)))
         {'??s1 s1})))
   (+ (* (log (* ?x ?y)) ??s1)
      ??x1 ??x2 ??x3)))

(def log-expand
  (ruleset
   (log (* ?x1 ?x2 ??xs))
   =>
   (+ (log ?x1) (log (* ?x2 ??xs)))

   (log (/ ?x1 ?x2))
   =>
   (- (log ?x1) (log ?x2))

   (log (expt ?x ?e))
   =>
   (* ?e (log ?x))))

(def log-extra
  (ruleset
   (* (:? ?n v/integral?) ??f1 (log ?x) ??f2)
   =>
   (* ??f1 (log (expt ?x ?n)) ??f2)))

;; ## Partials

(def canonicalize-partials
  (rule-simplifier
   (ruleset
    ;; Convert nests into products.
    ((partial ??i) ((partial ??j) ?f))
    =>
    ((* (partial ??i) (partial ??j)) ?f)

    ((partial ??i) ((* (partial ??j) ??more) ?f))
    =>
    ((* (partial ??i) (partial ??j) ??more) ?f)

    ;; Gather exponentiated partials into products
    ((expt (partial ??i) ?n) ((partial ??j) ?f))
    =>
    ((* (expt (partial ??i) ?n) (partial ??j)) ?f)

    ((partial ??i) ((expt (partial ??j) ?n) ?f))
    =>
    ((* (partial ??i) (expt (partial ??j) ?n)) ?f)

    ((expt (partial ??i) ?n) ((expt (partial ??j) ?m) ?f))
    =>
    ((* (expt (partial ??i) ?n) (expt (partial ??j) ?m)) ?f)

    ;; Same idea, trickier when some accumulation has already occurred.
    ((expt (partial ??i) ?n) ((* (partial ??j) ??more) ?f))
    =>
    ((* (expt (partial ??i) ?n) (partial ??j) ??more) ?f)

    ((partial ??i) ((* (expt (partial ??j) ?m) ??more) ?f))
    =>
    ((* (partial ??i) (expt (partial ??j) ?m) ??more) ?f)

    ((expt (partial ??i) ?n) ((* (expt (partial ??j) ?m) ??more) ?f))
    =>
    ((* (expt (partial ??i) ?n) (expt (partial ??j) ?m) ??more) ?f)

    ;; example:
    #_(((* (partial 2 1) (partial 1 1)) FF) (up t (up x y) (down p_x p_y)))
    ;; since the partial indices in the outer derivative are lexically
    ;; greater than those of the inner, we canonicalize by swapping the
    ;; order. This is the "equality of mixed partials."
    (((* ??xs (partial ??i) ??ys (partial ??j) ??zs) ?f) ??args)
    (fn [m]
      (and *commute-partials?*
           ;; TODO check elementary-access?
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

(def triginv
  (rule-simplifier
   (ruleset
    (sin (asin :x))          => :x
    (asin (sin :x))          => :x
    (sin (atan :y :x))       => (/ :y (sqrt (+ (expt :x 2) (expt :y 2))))
    (cos (atan :y :x))       => (/ :x (sqrt (+ (expt :x 2) (expt :y 2))))
    (cos (asin :t))          => (sqrt (- 1 (square :t)))
    )
   (ruleset
    (acos (cos :x))          => :x
    (atan (tan :x))          => :x
    (atan (sin :x) (cos :x)) => :x
    )))

(defn triginv [simplify]
  (rule-simplifier
   (r/choice
    (r/rule
     (atan (* ?c ?y) (* ?c ?x)) => (atan ?y ?x))

    (r/rule
     (atan ?y ?x)
     (fn [m]
       (and *aggressive-atan-simplify?*
            (let [xs (simplify (m '?x))
                  ys (simplify (m '?y))]
              (if (v/= ys xs)
                (if (v/number? ys)
                  (if (g/negative? ys)
                    '(- (/ (* 3 pi) 4))
                    '(/ pi 4))
                  (let [note `(~'assuming (~'positive? ~xs))]
                    (note-that!
                     (meta-conj note :rules 'aggressive-atan-1))
                    '(/ pi 4)))

                (if (and (v/number? ys)
                         (v/number? xs))
                  (g/atan ys xs)
                  (let [s (simplify `(~'gcd ~ys ~xs))]
                    (if (v/= s 1)
                      false ;; do nothing
                      (let [note `(~'assuming (~'positive? ~s))
                            yv (simplify `(~'/ ~ys ~s))
                            xv (simplify `(~'/ ~xs ~s))]
                        (note-that!
                         (meta-conj note :rules 'aggressive-atan-2))
                        `(~'atan ~yv ~xv))))))))))

    (ruleset
     (sin (asin ?x)) => ?x
     (asin (sin ?x))
     (fn [m]
       (and *inverse-simplify?*
            (let [xs (simplify (m '?x))]
              (assume!
               `(~'= (~'asin (~'sin ~xs)) ~xs) 'asin-sin))))
     ?x

     (cos (acos ?x)) => ?x
     (acos (cos ?x))
     (fn [m]
       (and *inverse-simplify?*
            (let [xs (simplify (m '?x))]
              (assume!
               `(~'= (~'acos (~'cos ~xs)) ~xs) 'acos-cos))))
     ?x

     (tan (atan ?x)) => ?x
     (atan (tan ?x))
     (fn [m]
       (and *inverse-simplify?*
            (let [xs (simplify (m '?x))]
              (assume!
               `(~'= (~'atan (~'tan ~xs)) ~xs) 'atan-tan))))
     ?x

     (sin (acos ?x)) => (sqrt (- 1 (expt ?x 2)))
     (cos (asin ?y)) => (sqrt (- 1 (expt ?y 2)))
     (tan (asin ?y)) => (/ ?y (sqrt (- 1 (expt ?y 2))))
     (tan (acos ?x)) => (/ (sqrt (- 1 (expt ?x 2))) ?x)

     (atan (sin ?x) (cos ?x))
     (fn [m]
       (and *inverse-simplify?*
            (let [xs (simplify (m '?x))]
              (assume!
               `(~'= (~'atan (~'sin ~xs) (cos ~xs)) ~xs) 'atan-sin-cos))))
     ?x

     (asin (cos ?x))
     (fn [m]
       (and *inverse-simplify?*
            (let [xs (simplify (m '?x))]
              (assume!
               `(~'=
                 (~'asin (~'cos ~xs))
                 (~'- (~'* (~'/ 1 2) ~'pi) ~xs))
               'asin-cos))))
     (- (* (/ 1 2) pi) ?x)

     (acos (sin ?x))
     (fn [m]
       (and *inverse-simplify?*
            (let [xs (simplify (m '?x))]
              (assume!
               `(~'=
                 (~'acos (~'sin ~xs))
                 (~'- (~'* (~'/ 1 2) ~'pi) ~xs))
               'acos-sin))))
     (- (* (/ 1 2) pi) ?x)

     (sin (atan ?a ?b))
     =>
     (/ ?a (sqrt (+ (expt ?a 2) (expt ?b 2))))

     (cos (atan ?a ?b))
     =>
     (/ ?b (sqrt (+ (expt ?a 2) (expt ?b 2))))))))

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
      (ruleset
       (sin (:? _ zero-mod-pi?))   =>  0
       (sin (:? _ pi-over-2-mod-2pi?))  => +1
       (sin (:? _ -pi-over-2-mod-2pi?)) => -1

       (cos (:? _ pi-over-2-mod-pi?))   =>  0
       (cos (:? _ zero-mod-2pi?))  => +1
       (cos (:? _ pi-mod-2pi?))    => -1

       (tan (:? _ zero-mod-pi?))   =>  0
       (tan (:? _ pi-over-4-mod-pi?))   => +1
       (tan (:? _ -pi-over-4-mod-pi?))  => -1))))

;;; sin is odd, and cos is even.  we canonicalize by moving the sign
;;; out of the first term of the argument.

(def angular-parity
  (ruleset
   (cos (:? ?n negative-number?))
   =>
   (cos (:? #(- (% '?n))))

   (cos (* (:? ?n negative-number?) ??x))
   =>
   (cos (* (:? #(- (% '?n))) ??x))

   (cos (+ (* (:? ?n negative-number?) ??x) ??y))
   =>
   (cos (- (* (:? #(- (% '?n))) ??x) ??y))

   (sin (:? ?n negative-number?))
   =>
   (- (sin (:? #(- (% '?n)))))

   (sin (* (:? ?n negative-number?) ??x))
   =>
   (- (sin (* (:? #(- (% '?n))) ??x)))

   (sin (+ (* (:? ?n negative-number?) ??x) ??y))
   =>
   (- (sin (- (* (:? #(- (% '?n))) ??x) ??y)))))

(def expand-multiangle
  (letfn [(exact-integer>3? [x]
            (and (v/integral? x)
                 (v/exact? x)
                 (> x 3)))]
    (ruleset
     (sin (* 2 ?x ??y))
     =>
     (* 2 (sin (* ?x ??y)) (cos (* ?x ??y)))

     (cos (* 2 ?x ??y))
     =>
     (- (* 2 (expt (cos (* ?x ??y)) 2)) 1)

     (sin (* 3 ?x ??y))
     =>
     (+ (* 3 (sin (* ?x ??y))) (* -4 (expt (sin (* ?x ??y)) 3)))

     (cos (* 3 ?x ??y))
     =>
     (+ (* 4 (expt (cos (* ?x ??y)) 3)) (* -3 (cos (* ?x ??y))))

     ;; at least one f
     (sin (* (:? ?n exact-integer>3?) ?f ??fs))
     =>
     (+ (* (sin (* (:? #(g/- (% '?n) 1)) ?f ??fs)) (cos (* ?f ??fs)))
        (* (cos (* (:? #(g/- (% '?n) 1)) ?f ??fs)) (sin (* ?f ??fs))))

     ;; at least one y
     (sin (+ ?x ?y ??ys))
     =>
     (+ (* (sin ?x) (cos (+ ?y ??ys)))
        (* (cos ?x) (sin (+ ?y ??ys))))

     ;; at least one f
     (cos (* (? n exact-integer>3?) ?f ??fs))
     =>
     (- (* (cos (* (:? #(g/- (% '?n) 1)) ?f ??fs)) (cos (* ?f ??fs)))
        (* (sin (* (:? #(g/- (% '?n) 1)) ?f ??fs)) (sin (* ?f ??fs))))

     ;; at least one y
     (cos (+ ?x ?y ??ys))
     =>
     (- (* (cos ?x) (cos (+ ?y ??ys)))
        (* (sin ?x) (sin (+ ?y ??ys)))))))

(def trig-sum-to-product
  (ruleset
   (+ ??a (sin ?x) ??b (sin ?y) ??c )
   =>
   (+ (* 2 (sin (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2))) ??a ??b ??c)

   (+ ??a (sin ?x) ??b (* -1 (sin ?y)) ??c )
   =>
   (+ (* 2 (sin (/ (- ?x ?y) 2)) (cos (/ (+ ?x ?y) 2))) ??a ??b ??c)

   (+ ??a (* -1 (sin ?y)) ??b (sin ?x) ??c )
   =>
   (+ (* 2 (sin (/ (- ?x ?y) 2)) (cos (/ (+ ?x ?y) 2))) ??a ??b ??c)

   (+ ??a (cos ?x) ??b (cos ?y) ??c )
   =>
   (+ (* 2 (cos (/ (+ ?x ?y) 2)) (cos (/ (- ?x ?y) 2))) ??a ??b ??c)

   (+ ??a (cos ?x) ??b (* -1 (cos ?y)) ??c )
   =>
   (+ (* -2 (sin (/ (+ ?x ?y) 2)) (sin (/ (- ?x ?y) 2))) ??a ??b ??c)

   (+ ??a (* -1 (cos ?y)) ??b (cos ?x) ??c )
   =>
   (+ (* -2 (sin (/ (+ ?x ?y) 2)) (sin (/ (- ?x ?y) 2))) ??a ??b ??c)))

(def trig-product-to-sum
  (ruleset
   (* ??u (sin ?x) ??v (sin ?y) ??w)
   =>
   (* (/ 1 2) (- (cos (- ?x ?y)) (cos (+ ?x ?y))) ??u ??v ??w)

   (* ??u (cos ?x) ??v (cos ?y) ??w)
   =>
   (* (/ 1 2) (+ (cos (- ?x ?y)) (cos (+ ?x ?y))) ??u ??v ??w)

   (* ??u (sin ?x) ??v (cos ?y) ??w)
   =>
   (* (/ 1 2) (+ (sin (+ ?x ?y)) (sin (- ?x ?y))) ??u ??v ??w)

   (* ??u (cos ?y) ??v (sin ?x) ??w)
   =>
   (* (/ 1 2) (+ (sin (+ ?x ?y)) (sin (- ?x ?y))) ??u ??v ??w)))

(def contract-expt-trig
  (letfn [(exact-integer>1? [x]
            (and (v/integral? x)
                 (v/exact? x)
                 (> x 1)))]
    (ruleset
     (expt (sin ?x) (:? ?n exact-integer>1?))
     =>
     (* (/ 1 2)
        (expt (sin ?x) (:? #(- (% '?n) 2)))
        (- 1 (cos (* 2 ?x))))

     (expt (cos ?x) (:? ?n exact-integer>1?))
     =>
     (* (/ 1 2)
        (expt (cos ?x) (:? #(- (% '?n) 2)))
        (+ 1 (cos (* 2 ?x)))))))

(defn half-angle [simplify]
  (letfn [(sin-half-angle-formula [theta]
            (let [thetas (simplify theta)]
              (assume!
               `(~'non-negative?
                 (~'+
                  (~'* 2 ~'pi)
                  (~'* -1 ~thetas)
                  (~'* 4 ~'pi
                   (~'floor (~'/ ~thetas (~'* 4 ~'pi))))))
               'sin-half-angle-formula)
              `(~'sqrt (/ (- 1 (~'cos ~thetas)) 2))))

          (cos-half-angle-formula [theta]
            (let [thetas (simplify theta)]
              (assume!
               `(~'non-negative?
                 (~'+
                  ~'pi
                  ~thetas
                  (~'* 4 ~'pi
                   (~'floor (~'/ (~'- ~'pi ~thetas)
                             (~'* 4 ~'pi))))))
               'cos-half-angle-formula)
              `(~'sqrt (~'/ (~'+ 1 (~'cos ~thetas)) 2))))]
    (r/make-ruleset
     [(r/rule
       (sin (* (/ 1 2) ?x ??y))
       (fn [m]
         (and *half-angle-simplify?*
              (sin-half-angle-formula
               `(~'* ~(m '?x) ~@(m '??y))))))

      (r/rule
       (sin (/ ?x 2))
       (fn [m]
         (and *half-angle-simplify?*
              (sin-half-angle-formula (m '?x)))))

      (r/rule
       (cos (* (/ 1 2) ?x ??y))
       (fn [m]
         (and *half-angle-simplify?*
              (cos-half-angle-formula
               `(~'* ~(m '?x) ~@(m '??y))))))

      (r/rule
       (cos (/ ?x 2))
       (fn [m]
         (and *half-angle-simplify?*
              (cos-half-angle-formula (m '?x)))))])))

(def sin-sq->cos-sq
  (rule-simplifier
   (ruleset
    (expt (sin ?x) (:? ?n at-least-two?))
    => (* (expt (sin ?x) (:? #(- (% '?n) 2)))
          (- 1 (expt (cos ?x) 2))))))

(def cos-sq->sin-sq
  (rule-simplifier
   (ruleset
    (expt (cos ?x) (:? ?n at-least-two?))
    => (* (expt (cos ?x) (:? #(- (% '?n) 2)))
          (- 1 (expt (sin ?x) 2))))))

(def split-high-degree-sincos
  (letfn [(remaining [m]
            (let [leftover (- (m '?n) 2)]
              (if (v/one? leftover)
                (list (m '?op) (m '?x))
                `(~'expt (~(m '?op) ~(m '?x)) ~leftover))))]
    (ruleset
     (* ??f1
        (expt ((:? ?op #{'sin 'cos}) ?x) (:? ?n more-than-two?))
        ??f2)
     => (* ??f1
           (expt (?op ?x) 2)
           (:? ~remaining)
           ??f2)

     (+ ??a1
        (expt ((:? ?op #{'sin 'cos}) ?x) (:? ?n more-than-two?))
        ??a2)
     => (+ ??a1
           (* (expt (?op ?x) 2)
              (:? ~remaining))
           ??a2))))

(defn flush-obvious-ones [simplify]
  ;; TODO can we count an order here of sin vs cos??
  (ruleset
   (+ ??a1 (expt (sin ?x) 2)
      ??a2 (expt (cos ?x) 2)
      ??a3)
   => (+ 1 ??a1 ??a2 ??a3)

   (+ ??a1 (expt (cos ?x) 2)
      ??a2 (expt (sin ?x) 2)
      ??a3)
   => (+ ??a1 ??a2 ??a3 1)

   (+ ??a1
	    (* (?? f1) (expt (sin (? x)) 2) (?? f2))
	    ??a2
	    (* (?? f3) (expt (cos (? x)) 2) (?? f4))
	    ??a3)
   (fn [m]
     (let [s1 (simplify `(~'* ~@(m '??f1) ~@(m '??f2)))
	         s2 (simplify `(~'* ~@(m '??f3) ~@(m '??f4)))]
       (when (v/exact-zero?
              (simplify `(~'- ~s1 ~s2)))
	       {'??s1 s1})))
   (+ ??a1 ??a2 ??a3 ??s1)

   (+ ??a1
	    (* (?? f1) (expt (cos (? x)) 2) (?? f2))
	    ??a2
	    (* (?? f3) (expt (sin (? x)) 2) (?? f4))
	    ??a3)
   (fn [m]
     (let [s1 (simplify `(~'* ~@(m '??f1) ~@(m '??f2)))
	         s2 (simplify `(~'* ~@(m '??f3) ~@(m '??f4)))]
       (when (v/exact-zero?
              (simplify `(~'- ~s1 ~s2)))
	       {'??s1 s1})))
   (+ ??a1 ??a2 ??a3 ??s1)))

(defn sincos-flush-ones [simplify]
  (rule-simplifier
   split-high-degree-sincos
   (flush-obvious-ones simplify)))

(defn sincos-random [simplify]
  (ruleset
   (+ ??a1 ?a ??a2 (expt (cos ?x) (:? ?n at-least-two?)) ??a3)
   (fn [{a '?a x '?x n '?n}]
     (v/exact-zero?
      (simplify `(~'+ ~a (~'expt (~'cos ~x) ~(g/- n 2))))))
   (+ ??a1 ??a2 ??a3 (* (expt (sin ?x) 2) ?a))

   (+ ??a1 (expt (cos ?x) (:? ?n at-least-two?)) ??a2 ?a ??a3)
   (fn [{a '?a x '?x n '?n}]
     (v/exact-zero?
      (simplify `(~'+ ~a (~'expt (~'cos ~x) ~(g/- n 2))))))
   (+ ??a1 ??a2 ??a3 (* (expt (sin ?x) 2) ?a))

   (+ ??a1 ?a ??a2 (expt (sin ?x) (:? ?n at-least-two?)) ??a3)
   (fn [{a '?a x '?x n '?n}]
     (v/exact-zero?
      (simplify `(~'+ ~a (~'expt (~'sin ~x) ~(g/- n 2))))))
   (+ ??a1 ??a2 ??a3 (* (expt (cos ?x) 2) ?a))

   (+ ??a1 (expt (sin ?x) (:? ?n at-least-two?)) ??a2 ?a ??a3)
   (fn [{a '?a x '?x n '?n}]
     (v/exact-zero?
      (simplify `(~'+ ~a (~'expt (~'sin ~x) ~(g/- n 2))))))
   (+ ??a1 ??a2 ??a3 (* (expt (cos ?x) 2) ?a))

   (+ ??a1
      ?a
      ??a2
      (* ??b1 (expt (cos ?x) (:? ?n at-least-two?)) ??b2)
      ??a3)
   (fn [{a '?a x '?x n '?n b1 '??b1 b2 '??b2}]
     (v/exact-zero?
      (simplify `(~'+ (~'* ~@b1 ~@b2 (~'expt (~'cos ~x) ~(g/- n 2))) ~a))))
   (+ ??a1 ??a2 ??a3 (* ?a (expt (sin ?x) 2)))

   (+ ??a1
      ?a
      ??a2
      (* ??b1 (expt (sin ?x) (:? ?n at-least-two?)) ??b2)
      ??a3)
   (fn [{a '?a x '?x n '?n b1 '??b1 b2 '??b2}]
     (v/exact-zero?
      (simplify `(~'+ (~'* ~@b1 ~@b2 (~'expt (~'sin ~x) ~(g/- n 2))) ~a))))
   (+ ??a1 ??a2 ??a3 (* ?a (expt (cos ?x) 2)))


   ( (+ ??a1
        (* ??b1 (expt (cos ?x) (:? ?n at-least-two?)) ??b2)
        ??a2
        ?a
        ??a3)
    (fn [{a '?a x '?x n '?n b1 '??b1 b2 '??b2}]
      (v/exact-zero?
       (simplify `(~'+ (~'* ~@b1 ~@b2 (~'expt (~'cos ~x) ~(g/- n 2))) ~a))))
    (+ ??a1 ??a2 ??a3 (* ?a (expt (sin ?x) 2))) )

   ( (+ ??a1
        (* ??b1 (expt (sin ?x) (:? ?n at-least-two?)) ??b2)
        ??a2
        ?a
        ??a3)
    (fn [{a '?a x '?x n '?n b1 '??b1 b2 '??b2}]
      (v/exact-zero?
       (simplify `(~'+ (~'* ~@b1 ~@b2 (~'expt (~'sin ~x) ~(g/- n 2))) ~a))))
    (+ ??a1 ??a2 ??a3 (* ?a (expt (cos ?x) 2))))))

;; we can eliminate sin and cos in favor of complex exponentials.

(def sincos->exp1
  (let [i  '(complex 0.0 1.0)
        -i '(complex 0.0 -1.0)]
    (ruleset
     (sin ?x)
     => (/ (- (exp (* ~i ?x)) (exp (* ~-i ?x)))
           (complex 0.0 2.0))

     (cos ?x)
     => (/ (+ (exp (* ~i ?x)) (exp (* ~-i ?x)))
           2))))

(def sincos->exp2
  (let [i '(complex 0.0 1.0)]
    (ruleset
     (sin ?x)
     => (/ (- (exp (* ~i ?x)) (/ 1 (exp (* ~i ?x))))
           (complex 0.0 2.0))

     (cos ?x)
     => (/ (+ (exp (* ~i ?x)) (/ 1 (exp (* ~i ?x))))
           2))))

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
      (ruleset
       (exp (:? ?c1 imaginary-number?))
       pos-pred
       (+ (cos ?im)
          (* ~i (sin ?im)))

       (exp (:? ?c1 imaginary-number?))
       neg-pred
       (+ (cos (:? #(- (% '?im))))
          (* ~-i (sin (:? #(- (% '?im))))))

       (exp (* (:? ?c1 imaginary-number?) (?? f)))
       pos-pred
       (+ (cos (* ?im ??f))
          (* ~i (sin (* ?im ??f))))

       (exp (* (:? ?c1 imaginary-number?) (?? f)))
       neg-pred
       (* (exp (:? #(g/real-part (% '?c1))))
          (+ (cos (* (:? #(- (% '?im))) ??f))
             (* ~-i (sin (* (:? #(- (% '?im))) ??f)))))

       (exp (? c1 complex-number?))
       pos-pred
       (* (exp (:? #(g/real-part (% '?c1))))
          (+ (cos ?im)
             (* ~i (sin ?im))))

       (exp (? c1 complex-number?))
       neg-pred
       (* (exp (:? #(g/real-part (% '?c1))))
          (+ (cos (:? #(- (% '?im))))
             (* ~-i (sin (:? #(- (% '?im)))))))

       (exp (* (? c1 complex-number?) (?? f)))
       pos-pred
       (* (exp (:? #(g/real-part (% '?c1))))
          (+ (cos (* ?im ??f))
             (* ~i (sin (* ?im ??f)))))

       (exp (* (? c1 complex-number?) (?? f)))
       neg-pred
       (* (exp (:? #(g/real-part (% '?c1))))
          (+ (cos (* (:? #(- (% '?im))) ??f))
             (* ~-i (sin (* (:? #(- (% '?im))) ??f)))))))))

(def exp-contract
  (ruleset
   (* ??x1 (exp ?x2) ??x3 (exp ?x4) ??x5)
   =>
   (* ??x1 ??x3 ??x5 (exp (+ ?x2 ?x4)))

   (expt (exp ?x) (? p)) => (exp (* ?p ?x))

   (/ (exp ?x) (exp ?y)) => (exp (- ?x ?y))

   (/ (* ??x1 (exp ?x) ??x2) (exp ?y))
   =>
   (* ??x1 ??x2 (exp (- ?x ?y)))

   (/ (exp ?x) (* (?? y1) (exp ?y) (?? y2)))
   =>
   (/ (exp (- ?x ?y)) (* ??y1 ??y2))

   (/ (* ??x1 (exp ?x) ??x2)
      (* (?? y1) (exp ?y) (?? y2)))
   =>
   (/ (* ??x1 ??x2 (exp (- ?x ?y)))
      (* ??y1 ??y2))))

(def exp-expand
  (let [i '(complex 0.0 1.0)
        -i '(complex 0.0 -1.0)
        exact-integer? (fn [x]
                         (and (v/integral? x)
                              (v/exact? x)))]
    (ruleset
     (exp (- ?x1)) => (/ 1 (exp ?x1))

     (exp (- ?x1 ?x2)) => (/ (exp ?x1) (exp ?x2))

     (exp (+ ?x1 ?x2 ??xs))
     =>
     (* (exp ?x1) (exp (+ ?x2 ??xs)))

     (exp (* (:? ?x imaginary-integer? #(> (g/imag-part %) 1))
             ??factors))
     =>
     (expt (exp (* ~i ??factors))
           (:? #(g/imag-part (% '?x))))

     (exp (* (:? ?x imaginary-integer? #(< (g/imag-part %) -1))
             ??factors))
     =>
     (expt (exp (* ~-i ??factors))
           (:? #(g/- (g/imag-part (% '?x)))))

     (exp (* (:? ?n exact-integer? #(> % 1))
             ??factors))
     =>
     (expt (exp (* ??factors)) ?n)

     (exp (* (:? ?n exact-integer? #(< % -1))
             ??factors))
     =>
     (expt (exp (* -1 ??factors))
           (:? #(- (% '?n))))

     (exp (:? ?x complex-number?))
     =>
     (* (exp (:? #(g/real-part (% '?x))))
        (exp (:? #(g/* (g/imag-part (% '?x)) ~i))))

     (exp (* (:? ?x complex-number?)
             ??factors))
     =>
     (* (exp (* (:? #(g/real-part (% '?x)))
                ??factors))
        (exp (* (:? #(g/* (g/imag-part (% '?x)) ~i))
                ??factors))))))

(def complex-trig
  (rule-simplifier
   (ruleset
    (cos (* ?z (complex 0.0 1.0)))
    => (cosh ?z)

    (sin (* ?z (complex 0.0 1.0)))
    => (* (complex 0.0 1.0) (sinh ?z))

    ;; Does this really belong here?
    ;; It works by reducing n mod 4 and then indexing into [1 i -1 -i].
    (expt (complex 0.0 1.0) (:? ?n v/integral?))
    => (:? #([1 '(complex 0.0 1.0) -1 '(complex 0.0 -1.0)]
             (mod (% '?n) 4))))))

(def complex-rules
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
   (angle (make-polar _ ?a)) => ?a))

(def divide-numbers-through
  (rule-simplifier
   (ruleset
    (* 1 ?factor) => ?factor
    (* 1 ??factors) => (* ??factors)

    (/ (:? ?n v/number?)
       (:? ?d v/number?))
    => (:? (fn [{n '?n d '?d}]
             (g// n d)))

    (/ (+ ??terms)
       (:? ?d v/number?))
    => (+ (:?? #(map (fn [n]
                       `(~'/ ~n ~(% '?d)))
                     (% '??terms))))

    (/ (* (:? ?n v/number?) ??factors)
       (:? ?d v/number?))
    =>
    (* (:? (fn [{n '?n d '?d}]
             (g// n d)))
       ??factors)

    (/ (* ??factors) (:? ?d v/number?))
    =>
    (* (:? #(g/invert (% '?d))) ??factors)

    (/ ?n
       (* (:? ?d v/number?) ?factor))
    =>
    (/ (/ ?n ?d) ?factor)

    (/ ?n
       (* (:? ?d v/number?) ??factors))
    =>
    (/ (/ ?n ?d) (* ??factors))

    (/ ?n (:? ?d v/number?))
    =>
    (* (:? #(g/invert (% '?d))) ?n))))

(defn- occurs-in? [syms all]
  (not
   (empty?
    (cs/intersection syms all))))

(defn universal-reductions [simplify]
  (let [misc     (miscsimp simplify)
        le       (logexp simplify)
        st       (special-trig simplify)
        ti       (triginv simplify)
        sim-root (simplify-square-roots simplify)]
    (fn [x]
      (let [syms     (x/variables-in x)
            logexp?  (occurs-in? #{'log 'exp} syms)
            sincos?  (occurs-in? #{'sin 'cos} syms)
            invtrig? (occurs-in? #{'asin 'acos 'atan} syms)
            sqrt?    (contains? syms 'sqrt)
            mag?     (contains? syms 'magnitude)
            e0       (misc x)
            e1       (if logexp? (le e0) e0)
            e2       (if mag? (magsimp e1) e1)
            e3       (if (and sincos?
                              *sin-cos-simplify?*)
                       (st e2)
                       e2)]
        (cond (and sincos? invtrig?) (sim-root (ti e3))
              sqrt? (sim-root e3)
              :else e3)))))
