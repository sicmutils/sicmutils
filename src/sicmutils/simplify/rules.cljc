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
  (:require [pattern.rule :refer [=> ruleset rule-simplifier]
             #?@(:cljs [:include-macros true])]
            [sicmutils.complex :as c]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

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

(defn- even-integer? [x]
  (and (v/integral? x)
       (v/zero? (g/modulo x 2))))

(defn- odd-integer? [x]
  (and (v/integral? x)
       (not (v/zero? (g/modulo x 2)))))

(defn- more-than-two? [x]
  (and (v/number? x) (> x 2)))

(defn- at-least-two? [x]
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


;; - return NEW bindings from the predicate!
;; move this sorted stuff to exprression, plus a `sort` function

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

(def sin-sq->cos-sq
  (rule-simplifier
   (ruleset
    (expt (sin ?x) (:? ?n at-least-two?))
    => (* (expt (sin ?x) (:? #(- (% '?n) 2)))
          (- 1 (expt (cos ?x) 2))))))

;; TODO consolidate these two into split-high-degree-trig by binding the op.
(def ^:private split-high-degree-cosines
  (ruleset
   (* ??f1
      (expt (cos ?x) (:? ?n more-than-two?))
      ??f2)
   => (* ??f1
         (expt (cos ?x) 2)
         (expt (cos ?x) (:? #(- (% '?n) 2)))
         ??f2)

   (+ ??a1
      (expt (cos ?x) (:? ?n more-than-two?))
      ??a2)
   => (+ ??a1
         (* (expt (cos ?x) 2)
            (expt (cos ?x) (:? #(- (% '?n) 2))))
         ??a2)))

(def ^:private split-high-degree-sines
  (ruleset
   (* ??f1
      (expt (sin ?x) (:? ?n more-than-two?))
      ??f2)
   => (* ??f1
         (expt (sin ?x) 2)
         (expt (sin ?x) (:? #(- (% '?n) 2)))
         ??f2)

   (+ ??a1
      (expt (sin ?x) (:? ?n more-than-two?))
      ??a2)
   => (+ ??a1
         (* (expt (sin ?x) 2)
            (expt (sin ?x) (:? #(- (% '?n) 2))))
         ??a2)))

(def simplify-square-roots
  (rule-simplifier
   (ruleset
    (expt (sqrt ?x) (:? ?n even-integer?))
    => (expt ?x (:? #(/ (% '?n) 2)))

    (sqrt (expt ?x (:? ?n even-integer?)))
    => (expt ?x (:? #(/ (% '?n) 2)))

    (sqrt (expt ?x (:? ?n odd-integer?)))
    => (* (sqrt ?x) (expt ?x (:? #(/ (dec (% '?n)) 2))))

    (expt (sqrt ?x) (:? ?n odd-integer?))
    => (* (sqrt ?x) (expt ?x (:? #(/ (dec (% '?n)) 2))))

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
       (* ??u (sqrt ?x) ??v))

    ;; Following are the new rules we added to approach
    ;; the simplification of the time-invariant-canonical
    ;; test.

    ;; TODO check these. If they are the SAME, a and b, then totally just
    ;; combine them! These seem like they should belong in sqrt-contract. And in
    ;; fact they probably ARE there.

    ;; ... (sqrt a) ... (sqrt b) ... => ... (sqrt a b)
    (* ??f1 (sqrt ?a) ??f2 (sqrt ?b) ??f3)
    => (* ??f1 ??f2 ??f3 (sqrt (* ?a ?b)))

    ;; (/ (* ... (sqrt a) ...)
    ;;    (* ... (sqrt b) ...)  => ... (sqrt (/ a b)) ... / ... ...
    (/ (* ??f1 (sqrt ?a) ??f2)
       (* ??g1 (sqrt ?b) ??g2))
    => (/ (* ??f1 ??f2 (sqrt (/ ?a ?b)))
          (* ??g1 ??g2))
    )))

(def sqrt-expand
  (rule-simplifier
   (ruleset
    ;; "distribute the radical sign across products and quotients.
    ;; but doing this may allow equal subexpressions within the
    ;; radicals to cancel in various ways. The companion rule
    ;; sqrt-contract reassembles what remains."

    ;; Scmutils, in each of these expansions, will `assume!`
    ;; that the expressions named ?x and ?y are non-negative
    (sqrt (* ?x ?y)) => (* (sqrt ?x) (sqrt ?y))

    (sqrt (* ?x ?y ??ys)) => (* (sqrt ?x) (sqrt (* ?y ??ys)))

    (sqrt (/ ?x ?y)) => (/ (sqrt ?x) (sqrt ?y))

    (sqrt (/ ?x ?y ??ys)) => (/ (sqrt ?x) (sqrt (* ?y ??ys)))

    )))

;; TODO continue from here!

(defn sqrt-contract
  ([] (sqrt-contract identity))
  ([simplify]
   (rule-simplifier
    (ruleset
     ;; NOTE: Scmutils, in each of these contractions, will `assume!` that the
     ;; expressions named ?x and ?y are non-negative.
     (* ??a (sqrt ?x) ??b (sqrt ?y) ??c)
     => (:? (fn [m]
              (let [xs (simplify ('?x m))
                    ys (simplify ('?y m))]
                (if (v/= xs ys)
                  `(~'* ~@('??a m) ~xs ~@('??b m) ~@('??c m))
                  `(~'* ~@('??a m)
                    (~'sqrt (~'* ~xs ~ys))
                    ~@('??b m) ~@('??c m))))))

     (/ (sqrt ?x) (sqrt ?y))
     => (:? (fn [m]
              (let [xs (simplify ('?x m))
                    ys (simplify ('?y m))]
                (if (v/= xs ys)
                  1
                  `(~'sqrt (~'/ ~xs ~ys))))))

     (/ (* ??a (sqrt ?x) ??b) (sqrt ?y))
     => (:? (fn [m]
              (let [xs (simplify ('?x m))
                    ys (simplify ('?y m))]
                (if (v/= xs ys)
                  `(~'* ~@('??a m) ~@('??b m))
                  `(~'* ~@('??a m)
                    (~'sqrt (~'/ ~xs ~ys))
                    ~@('??b m))))))

     (/ (sqrt ?x) (* ??a (sqrt ?y) ??b))
     => (:? (fn [m]
              (let [xs (simplify ('?x m))
                    ys (simplify ('?y m))]
                (if (v/= xs ys)
                  `(~'/ 1 (~'* ~@('??a m) ~@('??b m)))
                  `(~'/ (~'sqrt (~'/ ~xs ~ys))
                    (~'* ~@('??a m) ~@('??b m)))))))

     (/ (* ??a (sqrt ?x) ??b)
        (* ??c (sqrt ?y) ??d))
     => (:? (fn [m]
              (let [xs (simplify ('?x m))
                    ys (simplify ('?y m))]
                (if (v/= xs ys)
                  `(~'/
                    (~'* ~@('??a m) ~@('??b m))
                    (~'* ~@('??c m) ~@('??d m)))
                  `(~'/
                    (~'* ~@('??a m) (~'sqrt (~'/ ~xs ~ys)) ~@('??b m))
                    (~'* ~@('??c m) ~@('??d m)))))))))))

(def complex-trig
  ;; TODO: clearly more of these are needed.
  (rule-simplifier
   (ruleset
    (cos (* ?z (complex 0.0 1.0)))
    => (cosh ?z)

    (sin (* ?z (complex 0.0 1.0)))
    => (* (complex 0.0 1.0) (sinh ?z))

    ;; Does this really belong here?
    ;; It works by reducing n mod 4 and then indexing into [1 i -1 -i].
    (expt (complex 0.0 1.0) (:? ?n v/integral?))
    => (:? #([1 '(complex 0 1) -1 '(complex 0 -1)] (mod (% '?n) 4))))))

(def divide-numbers-through
  (rule-simplifier
   (ruleset
    (* 1 ?factor) => ?factor
    (* 1 ??factors) => (* ??factors)

    (/ (:? ?n v/number?) (:? ?d v/number?))
    => (:? #(g// (% '?n) (% '?d)))

    (/ (+ ??terms) (:? ?d v/number?))
    => (+ (:?? #(map (fn [n] `(~'/ ~n ~(% '?d))) (% '??terms)))))))

(def ^:private flush-obvious-ones
  (ruleset
   (+ ??a1 (expt (sin ?x) 2) ??a2 (expt (cos ?x) 2) ??a3)
   => (+ 1 ??a1 ??a2 ??a3))
  ;; are sines always before cosines after we poly simplify?
  ;; they are in scmutils, so we should be alert for this.
  ;;
  ;; TODO they should NOT be!!
  ;;
  ;; in scmutils, there are a couple of others that involve rcf:simplify, which
  ;; we dont' have, and we don't know if pcf:simplify is an acceptable
  ;; substitute here; and we don't have a method for pasting the value of a
  ;; predicate into a rule, so this is far from complete.
  ;;
  ;; TODO we do now, and this comment is totally out of date.
  )

(def trig->sincos
  (rule-simplifier
   (ruleset
    ;; GJS has other rules: to map cot, sec and csc to sin/cos, but
    ;; I don't think we need those since we transform those to sin/cos
    ;; in the env namespace.
    (tan ?x) => (/ (sin ?x) (cos ?x)))))

;; note the difference in interface between rulesets and rule simplifiers.
;; rulesets return nil when they're not applicable (unless you specify a
;; custom fail continuation). Rule-simplifiers pass expressions through.

(def sincos->trig
  (rule-simplifier
   (ruleset
    ;; undoes the effect of trig->sincos
    (/ (sin ?x) (cos ?x))
    => (tan ?x)

    (/ (sin ?x) (* ??d1 (cos ?x) ??d2))
    => (/ (tan ?x) (* ??d1 ??d2))

    (/ (* ??n1 (sin ?x) ??n2)
       (* ??d1 (cos ?x) ??d2))
    => (/ (* ??n1 (tan ?x) ??n2)
          (* ??d1 ??d2)))))

(def triginv
  (rule-simplifier
   (ruleset
    (sin (asin ?x))          => ?x
    (asin (sin ?x))          => ?x
    (sin (atan ?y ?x))       => (/ ?y (sqrt (+ (expt ?x 2) (expt ?y 2))))
    (cos (atan ?y ?x))       => (/ ?x (sqrt (+ (expt ?x 2) (expt ?y 2))))
    (cos (asin ?t))          => (sqrt (- 1 (square ?t)))
    )
   (ruleset
    (acos (cos ?x))          => ?x
    (atan (tan ?x))          => ?x
    (atan (sin ?x) (cos ?x)) => ?x
    (atan (* ?c (sin ?x)) (* ?c (cos ?x))) => ?x)))

(def sincos-flush-ones
  (rule-simplifier split-high-degree-cosines
                   split-high-degree-sines
                   flush-obvious-ones))

(defn universal-reductions [x]
  (triginv x))

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
      (pos? (compare (vec ('??i m))
                     (vec ('??j m)))))
    (((* ??xs (partial ??j) ??ys (partial ??i) ??zs) ?f) ??args))))
