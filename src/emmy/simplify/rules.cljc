#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.simplify.rules
  "This namespace contains many sets of algebraic simplification rules you can use
  to build simplifiers for algebraic structures.

  [[rules]] currently holds a mix of:

  - rulesets ported from the scmutils library
  - more fine-grained, tuneable rulesets like [[associative]]
    and [[constant-elimination]] designed to help you build lightweight custom
    term-rewriting simplifiers.

  NOTE: Expect this namespace to be broken out into more fine-grained
  namespaces. There are TODO entries throughout the docstrings left as tips for
  how to proceed here."
  (:refer-clojure :exclude [even? odd?])
  (:require [clojure.set :as cs]
            [pattern.match :as pm]
            [pattern.rule :as r :refer [=> ruleset rule-simplifier]]
            [emmy.complex :as c]
            [emmy.expression :as x]
            [emmy.generic :as g]
            [emmy.numsymb :as sym]
            [emmy.util.logic :as ul]
            [emmy.value :as v]))

;; ## Simplifier Configuration Variables
;;
;; Emmy uses a number of dynamic variables to tune the behavior of the
;; simplifier without having to thread explicit maps of options through the
;; rulesets. This design is not optimal; in a future version, we'll move any
;; rules that depend on these out to their own rulesets, and move the
;; configuration variables over to [[emmy.simplify]].

(def ^{:dynamic true
       :doc "If true, allows the following simplification to proceed:

```clojure
(log (exp x)) => x.
```

Because `exp(i*x) == exp(i*(x+n*2pi))` for all integral `n`, this setting can
confuse `x` with `x+n*2pi`."}
  *log-exp-simplify?*
  true)

(def ^{:dynamic true
       :doc "Allows `(x^a)^b => x^(a*b)`.

This is dangerous, because can lose or gain a root:

```
x = (x^(1/2))^2 != ((x^2)^1/2)=+-x
```
"}
  *exponent-product-simplify?*
  true)

(def ^{:dynamic true
       :doc " Traditionally, `sqrt(x)` is the positive square root, but
`x^(1/2)` is both positive and negative roots.

Setting [[*expt-half->sqrt?*]] to `true` maps `x^(1/2)` to `sqrt(x)`,
potentially losing a root."}
  *expt-half->sqrt?*
  true)

(def ^{:dynamic true
       :doc "If x is real, then `(sqrt (square x)) = (abs x)`.

  Setting [[*sqrt-expt-simplify?*]] to `true` allows `(sqrt (square x)) = x`,
  potentially causing a problem if `x` is in fact negative."}
  *sqrt-expt-simplify?*
  true)

(def ^{:dynamic true
       :doc "If `x` and `y` are real and non-negative, then

```
(* (sqrt x) (sqrt y)) = (sqrt (* x y))
```

This is not true for negative factors. Setting [[*sqrt-factor-simplify?*]] to
true enables this simplification, causing a problem if `x` or `y` are in fact
negative."}
  *sqrt-factor-simplify?*
  true)

(def ^{:dynamic true
       :doc "When `true`, allows:

```
(atan y x) => (atan (/ y d) (/ x d))
```

where `d=(gcd x y)`.

This is fine if `d` is a number (Numeric `gcd` is always positive), but may lose
quadrant information if `d` is a symbolic expression that can be negative for
some values of its variables."}
  *aggressive-atan-simplify?*
  true)

(def ^{:dynamic true
       :doc "When `true`, allows trigonometric inverse functions to simplify:

```
(asin (sin x)) => x
```

Because trigonometric functions like `sin` and `cos` are cyclic, this can lose
multi-value info (as with [[*log-exp-simplify*]])."}
  *inverse-simplify?*
  true)

(def ^{:dynamic true
       :doc "When `true`, allows arguments of `sin`, `cos` and `tan` that are
  rational multiples of `'pi` to be reduced. See [[trig:special]] for these
  rules."}
  *sin-cos-simplify?*
  true)

(def ^{:dynamic true
       :doc "When `true`, enables the half-angle reductions described in [[half-angle]].

Note from GJS: 'Sign of result is hairy!'"}
  *half-angle-simplify?*
  true)

(def ^{:dynamic true
       :doc "When true, allows commutation of partial derivatives so that partial derivatives appear in order.

For example:

```clojure
(((* (partial 2 1) (partial 1 1)) FF) (up t (up x y) (down p_x p_y)))
```

Since the partial indices in the outer derivative are lexically greater than
those of the inner, we canonicalize by swapping the order:

```clojure
(((* (partial 1 1) (partial 2 1)) FF) (up t (up x y) (down p_x p_y)))
```

When the components selected by the partials are unstructured (e.g. real), this
is okay due to the 'equality of mixed partials'."}
  *commute-partials?*
  true)

(def ^{:dynamic true
       :doc "When `true`, allows division through the numerator by numbers in
       the denominator:

```
(/ (+ (* 4 x) 5) 3) => (+ (* 4/3 x) 5/3)
```

This setting is `true` by default."}
  *divide-numbers-through-simplify?*
  true)

(def ^{:dynamic true
       :doc "Transforms products of trig functions into functions of sums of
       angles.

For example:

```
(* (sin x) (cos y))
;;=>
 (+ (* 1/2 (sin (+ x y)))
    (* 1/2 (sin (+ x (* -1 y)))) )
```"}
  *trig-product-to-sum-simplify?*
  false)

;; ## Binding Predicates
;;
;; The following predicates are used to restrict bindings in the rules that
;; follow. Bindings can take multiple predicates, but it reads a bit better to
;; have them tightened up into a single predicate.

(defn- negative-number? [x]
  (and (v/number? x)
       (g/negative? x)))

(defn- imaginary-number?
  "Returns true if `z` is a complex number with nonzero imaginary part and zero
  real part, false otherwise."
  [z]
  (and (c/complex? z)
       (not (v/zero? z))
       (v/zero? (g/real-part z))))

(defn- complex-number?
  "Returns true if `z` is a complex number with nonzero real AND imaginary parts,
  false otherwise."
  [z]
  (and (c/complex? z)
       (not (v/zero? (g/real-part z)))
       (not (v/zero? (g/imag-part z)))))

(defn- imaginary-integer?
  "Returns true if `z` is an imaginary number with an integral (or VERY close to
  integral) imaginary part, false otherwise."
  [z]
  (and (imaginary-number? z)
       (v/almost-integral?
        (g/imag-part z))))

(defn not-integral? [x]
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

(defn odd-positive-integer? [x]
  (and (odd-integer? x)
       (> x 2)))

(defn- more-than-two? [x]
  (and (v/number? x) (> x 2)))

(defn- at-least-two? [x]
  (and (v/number? x) (>= x 2)))

;; ## Rule Sets

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
  rule that eliminates instances of `constant` inside any-arity forms like

  ```clojure
  (<op> ,,,args,,,)
  ```"
  [op constant]
  (ruleset
   (~op ??xs)
   => (~op (?? (fn [{xs '??xs}]
                 (remove #{constant} xs))))))

(defn constant-promotion
  "Takes an operator symbol `op` and an identity element `constant` and returns a
  rule that turns binary forms with `constant` on either side into `constant`.

  This rule is useful for commutative annihilators like:

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
  operators. Sorting is accomplished with [[emmy.expression/sort]].

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
          (and (ul/assume!
                (r/template
                 (= (log (exp ~xs)) ~xs))
                'logexp1)
               x)))))

    (r/guard
     (fn [_] *sqrt-expt-simplify?*)
     (r/rule (sqrt (exp ?x))
             (fn [{x '?x}]
               (let [xs (simplify x)]
                 (and (ul/assume!
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
  "Simplifications for various exponent forms (assuming commutative multiplication).

  NOTE that we have some similarities to [[exponent-contract]] above - that
  function works for non-commutative multiplication - AND that this needs a new
  name."
  [simplify]
  (let [sym:* (sym/symbolic-operator '*)]
    (rule-simplifier
     (ruleset
      (expt _ 0)  => 1
      (expt ?x 1) => ?x

      ;; e^{ni} == 0,i,-1 or -i.
      (expt ~c/I (? ?n v/integral?))
      => (? #([1 c/I -1 (g/- c/I)]
              (mod (% '?n) 4)))

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
                         (ul/assume!
                          (r/template
                           (= (expt (expt ~x ~as) ~bs)
                              (expt ~x ~(sym:* as bs))))
                          'exponent-product)))
            {'?ab (g/* a b)})))
      (expt ?x ?ab)

      (expt ?x (/ 1 2))
      (fn [_] *expt-half->sqrt?*)
      (sqrt ?x)

      ;; Collect duplicate terms into exponents.
      ;;
      ;; TODO this is missing the case where non-exponent duplicates get
      ;; collected into exponents. At least note this; in the current library,
      ;; like terms are collected by the polynomial simplifier.
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
           (when (ul/assume!
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
           (when (ul/assume!
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
  "Takes one or two simplified expressions `x` and `y` and a symbolic identifier
  `id` and registers an assumption that both sides are non-negative.

  Returns the conjuction of both assumptions in the two argument case, or the
  single assumption in the one-argument case."
  ([x id]
   (ul/assume! `(~'non-negative? ~x) id (fn [] false)))
  ([x y id]
   (and (ul/assume! `(~'non-negative? ~x) id (fn [] false))
        (ul/assume! `(~'non-negative? ~y) id (fn [] false)))))

(defn sqrt-expand
  "Returns a rule simplifier that distributes the radical sign across products and
  quotients. The companion rule [[sqrt-contract]] reassembles what remains.

  NOTE that doing this may allow equal subexpressions within the radicals to
  cancel in various ways.

  Turn this simplifier on and off with [[*sqrt-factor-simplify?*]]."
  [simplify]
  (letfn [(pred [label]
            (fn [{x '?x y '?y}]
              (let [xs (simplify x)
                    ys (simplify y)]
                (if (v/= xs ys)
                  (non-negative-factors! xs label)
                  (non-negative-factors! xs ys label)))))]
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
  (rule-simplifier
   (r/ruleset*
    (r/rule
     (* ??a (sqrt ?x) ??b (sqrt ?y) ??c)
     (fn [{x '?x y '?y :as m}]
       (let [xs (simplify x)
             ys (simplify y)]
         (if (v/= xs ys)
           (and (non-negative-factors! xs 'c1)
                (r/template
                 m (* ??a ~xs ??b ??c)))
           (and (non-negative-factors! xs ys 'c1)
                (r/template
                 m (* ??a (sqrt (* ~xs ~ys)) ??b ??c)))))))

    (r/rule
     (/ (sqrt ?x) (sqrt ?y))
     (fn [{x '?x y '?y}]
       (let [xs (simplify x)
             ys (simplify y)]
         (if (v/= xs ys)
           (and (non-negative-factors! xs 'c2)
                1)
           (and (non-negative-factors! xs ys 'c2)
                (r/template (sqrt (/ ~xs ~ys))))))))

    (r/rule
     (/ (* ??a (sqrt ?x) ??b) (sqrt ?y))
     (fn [{x '?x y '?y :as m}]
       (let [xs (simplify x)
             ys (simplify y)]
         (if (v/= xs ys)
           (and (non-negative-factors! xs 'c3)
                (r/template m (* ??a ??b)))
           (and (non-negative-factors! xs ys 'c3)
                (r/template
                 m (* ??a (sqrt (/ ~xs ~ys)) ??b)))))))

    (r/rule
     (/ (sqrt ?x) (* ??a (sqrt ?y) ??b))
     (fn [{x '?x y '?y :as m}]
       (let [xs (simplify x)
             ys (simplify y)]
         (if (v/= xs ys)
           (and (non-negative-factors! xs 'c4)
                (r/template m (/ 1 (* ??a ??b))))
           (and (non-negative-factors! xs ys 'c4)
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
           (and (non-negative-factors! xs 'c5)
                (r/template
                 m (/ (* ??a ??b)
                      (* ??c ??d))))
           (and (non-negative-factors! xs ys 'c5)
                (r/template
                 m (/ (* ??a (sqrt (/ ~xs ~ys)) ??b)
                      (* ??c ??d)))))))))))

;; ## Log / Exp

(def specfun->logexp
  (let [two-i (c/complex 0.0 2.0)]
    (rule-simplifier
     (ruleset
      (sqrt ?x) => (exp (* (/ 1 2) (log ?x)))

      (atan ?z)
      => (/ (- (log (+ 1 (* ~c/I ?z)))
               (log (- 1 (* ~c/I ?z))))
            ~two-i)

      (asin ?z)
      => (* ~(g/- c/I)
            (log (+ (* ~c/I ?z)
                    (sqrt (- 1 (expt ?z 2))))))

      (acos ?z)
      => (* ~(g/- c/I)
            (log
             (+ ?z (* ~c/I (sqrt (- 1 (expt ?z 2)))))))

      (sinh ?u) => (/ (- (exp ?u) (exp (* -1 ?u))) 2)

      (cosh ?u) => (/ (+ (exp ?u) (exp (* -1 ?u))) 2)

      (expt ?x (? ?y not-integral?)) => (exp (* ?y (log ?x)))))))

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
    => ((* (expt (partial ??i) ?n) (expt (partial ??j) ?m) ??more) ?f))

   (r/guard
    (fn [_] *commute-partials?*)
    ;; See [[*commute-partials?*]] above for a description of this
    ;; transformation.
    ;;
    ;; TODO in the predicate, implement `symb:elementary-access?` and make sure
    ;; that this only sorts if we can prove that we go all the way to the botom.
    (r/rule
     (((* ??xs (partial ??i) ??ys (partial ??j) ??zs) ?f) ??args)

     (fn [{i '??i j '??j}]
       (pos?
        (compare (vec i) (vec j))))
     (((* ??xs (partial ??j) ??ys (partial ??i) ??zs) ?f) ??args)))))

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
               (and (ul/assume!
                     (list 'positive? xs)
                     'aggressive-atan-1)
                    '(/ pi 4)))
             (if (and (v/number? ys)
                      (v/number? xs))
               (sym:atan ys xs)
               (let [s (simplify (list 'gcd ys xs))]
                 (when-not (v/one? s)
                   (and (ul/assume!
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
         (ul/assume!
          (r/template
           (= (asin (sin ~xs)) ~xs))
          'asin-sin)))
     ?x

     (acos (cos ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (ul/assume!
          (r/template
           (= (acos (cos ~xs)) ~xs))
          'acos-cos)))
     ?x

     (atan (tan ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (ul/assume!
          (r/template
           (= (atan (tan ~xs)) ~xs))
          'atan-tan)))
     ?x

     (atan (sin ?x) (cos ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (ul/assume!
          (r/template
           (= (atan (sin ~xs) (cos ~xs)) ~xs))
          'atan-sin-cos)))
     ?x

     (asin (cos ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (ul/assume!
          (r/template
           (= (asin (cos ~xs))
              (- (* (/ 1 2) pi) ~xs)))
          'asin-cos)))
     (- (* (/ 1 2) pi) ?x)

     (acos (sin ?x))
     (fn [{x '?x}]
       (let [xs (simplify x)]
         (ul/assume!
          (r/template
           (= (acos (sin ~xs))
              (- (* (/ 1 2) pi) ~xs)))
          'acos-sin)))
     (- (* (/ 1 2) pi) ?x)))))

(defn trig:special
  "TODO consolidate the symbolic checkers here with the constructor
  simplifications in [[trig:special]]. "
  [simplify]
  (let [sym:+   (sym/symbolic-operator '+)
        sym:-   (sym/symbolic-operator '-)
        sym:*   (sym/symbolic-operator '*)
        sym:div (sym/symbolic-operator '/)]
    (letfn [(zero-mod-pi? [x]
              (or (sym/zero-mod-pi? x)
                  (v/integral?
                   (simplify (sym:div x 'pi)))))

            (pi-over-2-mod-2pi? [x]
              (or (sym/pi-over-2-mod-2pi? x)
                  (v/integral?
                   (simplify
                    (sym:div (sym:- x (sym:div 'pi 2))
                             (sym:* 2 'pi))))))

            (-pi-over-2-mod-2pi? [x]
              (or (sym/-pi-over-2-mod-2pi? x)
                  (v/integral?
                   (simplify
                    (sym:div (sym:+ x (sym:div 'pi 2))
                             (sym:* 2 'pi))))))

            (pi-over-2-mod-pi? [x]
              (or (sym/pi-over-2-mod-pi? x)
                  (v/integral?
                   (simplify
                    (sym:div (sym:- x (sym:div 'pi 2))
                             'pi)))))

            (zero-mod-2pi? [x]
              (or (sym/zero-mod-2pi? x)
                  (v/integral?
                   (simplify
                    (sym:div x (sym:* 2 'pi))))))

            (pi-mod-2pi? [x]
              (or (sym/pi-mod-2pi? x)
                  (v/integral?
                   (simplify
                    (sym:div (sym:- x 'pi)
                             (sym:* 2 'pi))))))

            (pi-over-4-mod-pi? [x]
              (or (sym/pi-over-4-mod-pi? x)
                  (v/integral?
                   (simplify
                    (sym:div (sym:- x (sym:div 'pi 4)) 'pi)))))

            (-pi-over-4-mod-pi? [x]
              (or (sym/-pi-over-4-mod-pi? x)
                  (v/integral?
                   (simplify
                    (sym:div (sym:+ x (sym:div 'pi 4))
                             'pi)))))]
      (rule-simplifier
       (ruleset
        (sin (? _ zero-mod-pi?)) => 0
        (sin (? _ pi-over-2-mod-2pi?)) => +1
        (sin (? _ -pi-over-2-mod-2pi?)) => -1

        (cos (? _ pi-over-2-mod-pi?)) => 0
        (cos (? _ zero-mod-2pi?)) => +1
        (cos (? _ pi-mod-2pi?)) => -1

        (tan (? _ zero-mod-pi?)) => 0
        (tan (? _ pi-over-4-mod-pi?)) => +1
        (tan (? _ -pi-over-4-mod-pi?)) => -1)))))

(def ^{:doc "`sin` is odd, and `cos` is even. we canonicalize by moving the sign
out of the first term of the argument."}
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

      ;; at least one f:
      (sin (* (? n exact-integer>3?) ?f ??fs))
      => (+ (* (sin (* (? #(g/- (% 'n) 1)) ?f ??fs))
               (cos (* ?f ??fs)))
            (* (cos (* (? #(g/- (% 'n) 1)) ?f ??fs))
               (sin (* ?f ??fs))))

      ;; at least one y:
      (sin (+ ?x ?y ??ys))
      => (+ (* (sin ?x) (cos (+ ?y ??ys)))
            (* (cos ?x) (sin (+ ?y ??ys))))

      ;; at least one f:
      (cos (* (? n exact-integer>3?) ?f ??fs))
      => (- (* (cos (* (? #(g/- (% 'n) 1)) ?f ??fs))
               (cos (* ?f ??fs)))
            (* (sin (* (? #(g/- (% 'n) 1)) ?f ??fs))
               (sin (* ?f ??fs))))

      ;; at least one y:
      (cos (+ ?x ?y ??ys))
      => (- (* (cos ?x) (cos (+ ?y ??ys)))
            (* (sin ?x) (sin (+ ?y ??ys))))))))

(def trig:sum->product
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

(def trig:product->sum
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
              (and (ul/assume!
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
              (and (ul/assume!
                    (r/template
                     (non-negative?
                      (+ pi ~thetas
                         (* 4 pi (floor
                                  (/ (- pi ~thetas)
                                     (* 4 pi)))))))
                    'cos-half-angle-formula)
                   (r/template
                    (sqrt (/ (+ 1 (cos ~theta)) 2))))))]
    (r/attempt
     (r/guard
      (fn [_] *half-angle-simplify?*)
      (rule-simplifier
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
         #(cos-half-angle-formula (% '?x)))))))))

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

(defn flush-obvious-ones [simplify]
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
  (let [-I (g/- c/I)]
    (rule-simplifier
     (ruleset
      (sin ?x)
      => (/ (- (exp (* ~c/I ?x)) (exp (* ~-I ?x)))
            ~(c/complex 0.0 2.0))

      (cos ?x)
      => (/ (+ (exp (* ~c/I ?x)) (exp (* ~-I ?x)))
            2)))))

(def sincos->exp2
  (rule-simplifier
   (ruleset
    (sin ?x)
    => (/ (- (exp (* ~c/I ?x)) (/ 1 (exp (* ~c/I ?x))))
          ~(g/* 2 c/I))

    (cos ?x)
    => (/ (+ (exp (* ~c/I ?x)) (/ 1 (exp (* ~c/I ?x))))
          2))))

;; under favorable conditions, we can replace the trig functions.

(def exp->sincos
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
         (* ~c/I (sin ?im)))

      (exp (? ?c1 imaginary-number?))
      neg-pred
      (+ (cos (? #(- (% '?im))))
         (* ~(g/- c/I) (sin (? #(- (% '?im))))))

      (exp (* (? ?c1 imaginary-number?) ??f))
      pos-pred
      (+ (cos (* ?im ??f))
         (* ~c/I (sin (* ?im ??f))))

      (exp (* (? ?c1 imaginary-number?) ??f))
      neg-pred
      (* (exp (? #(g/real-part (% '?c1))))
         (+ (cos (* (? #(- (% '?im))) ??f))
            (* ~(g/- c/I) (sin (* (? #(- (% '?im))) ??f)))))

      (exp (? ?c1 complex-number?))
      pos-pred
      (* (exp (? #(g/real-part (% '?c1))))
         (+ (cos ?im)
            (* ~c/I (sin ?im))))

      (exp (? ?c1 complex-number?))
      neg-pred
      (* (exp (? #(g/real-part (% '?c1))))
         (+ (cos (? #(- (% '?im))))
            (* ~(g/- c/I) (sin (? #(- (% '?im)))))))

      (exp (* (? ?c1 complex-number?) ??f))
      pos-pred
      (* (exp (? #(g/real-part (% '?c1))))
         (+ (cos (* ?im ??f))
            (* ~c/I (sin (* ?im ??f)))))

      (exp (* (? ?c1 complex-number?) ??f))
      neg-pred
      (* (exp (? #(g/real-part (% '?c1))))
         (+ (cos (* (? #(- (% '?im))) ??f))
            (* ~(g/- c/I) (sin (* (? #(- (% '?im))) ??f)))))))))

(def exp-contract
  (rule-simplifier
   (ruleset
    (* ??x1 (exp ?x2) ??x3 (exp ?x4) ??x5)
    =>
    (* ??x1 ??x3 ??x5 (exp (+ ?x2 ?x4)))

    (expt (exp ?x) ?p) => (exp (* ?p ?x))

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
  (let [exact-integer? (fn [x]
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
      (expt (exp (* ~c/I ??factors))
            (? #(g/imag-part (% '?x))))

      (exp (* (? ?x imaginary-integer? #(< (g/imag-part %) -1))
              ??factors))
      =>
      (expt (exp (* ~(g/- c/I) ??factors))
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
         (exp (? #(g/* (g/imag-part (% '?x)) ~c/I))))

      (exp (* (? ?x complex-number?)
              ??factors))
      =>
      (* (exp (* (? #(g/real-part (% '?x)))
                 ??factors))
         (exp (* (? #(g/* (g/imag-part (% '?x)) ~c/I))
                 ??factors)))))))

(def complex-trig
  (rule-simplifier
   (ruleset
    (cos ~c/I) => (cosh 1)
    (cos (* ?z ~c/I)) => (cosh ?z)
    (cos (* ~c/I ?z)) => (cosh ?z)
    (cos (* ??xs ~c/I ??ys)) => (cosh (* ??xs ??ys))

    (sin ~c/I) => (* ~c/I (sinh 1))
    (sin (* ?z ~c/I)) => (* ~c/I (sinh ?z))
    (sin (* ~c/I ?z)) => (* ~c/I (sinh ?z))
    (sin (* ??xs ~c/I ??ys)) => (* ~c/I (sinh (* ??xs ??ys))))))

(def complex-rules
  (let [ctor '(? ?op #{complex make-rectangular})]
    (rule-simplifier
     (ruleset
      (~ctor (cos ?theta) (sin ?theta))
      => (exp (* ~c/I ?theta))

      (real-part (~ctor ?re _)) => ?re
      (imag-part (~ctor _ ?im)) => ?im

      (magnitude (~ctor ?re ?im))
      => (sqrt (+ (expt ?re 2) (expt ?im 2)))

      (angle (~ctor ?re ?im)) => (atan ?im ?re)

      (real-part (make-polar ?m ?a)) => (* ?m (cos ?a))
      (imag-part (make-polar ?m ?a)) => (* ?m (sin ?a))

      (magnitude (make-polar ?m _)) => ?m
      (angle (make-polar _ ?a)) => ?a))))

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
  (boolean
   (seq
    (cs/intersection syms all))))

(defn universal-reductions
  "Returns a rule simplifier of rules that are almost always helpful."
  [simplify]
  (let [misc     (miscsimp simplify)
        le       (logexp simplify)
        st       (trig:special simplify)
        ti       (triginv simplify)
        sim-root (simplify-square-roots simplify)]
    (fn [expr]
      (let [syms     (x/variables-in expr)
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
