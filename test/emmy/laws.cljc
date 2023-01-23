#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.laws
  "test.check laws useful for checking the algebraic properties of different types
  that implement the emmy.generic operations, and the additive and
  multiplicative options in emmy.value.Value."
  (:require [clojure.test :refer [is]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish?]]
            [emmy.generic :as g]
            [emmy.value :as v]))

(defn nullity [options generator type-name]
  (checking (str type-name " v/zero? agrees with v/zero-like.")
            options
            [a generator]
            (is (v/zero? (v/zero-like a)))))

(defn unity [options generator type-name]
  (checking (str type-name " v/one? agrees with v/one-like.")
            options
            [a generator]
            (is (v/one? (v/one-like a)))))

(defn zero-like [options generator type-name]
  (nullity options generator type-name)
  (checking (str type-name " has a valid zero-like implementation.")
            options
            [a generator]
            (is (ish? a (g/add a (v/zero-like a))))
            (is (ish? a (g/add (v/zero-like a) a)))))

(defn one-like [options generator type-name]
  (unity options generator type-name)
  (checking (str type-name " has a valid one-like implementation.")
            options
            [a generator]
            (is (ish? a (g/mul a (v/one-like a))))
            (is (ish? a (g/mul (v/one-like a) a)))))

(defn associative-add [options generator type-name]
  (checking (str type-name " implements associative g/add.")
            options
            [a generator
             b generator
             c generator]
            (is (ish? (g/add a (g/add b c))
                      (g/add (g/add a b) c)))))

(defn commutative-add [options generator type-name]
  (checking (str type-name " implements commutative g/add.")
            options
            [a generator
             b generator]
            (is (ish? (g/add a b)
                      (g/add b a)))))

(defn associative-mul [options generator type-name]
  (checking (str type-name " implements associative g/mul.")
            options
            [a generator
             b generator
             c generator]
            (is (ish? (g/add a (g/add b c))
                      (g/add (g/add a b) c)))))

(defn commutative-mul [options generator type-name]
  (checking (str type-name " implements commutative g/mul.")
            options
            [a generator
             b generator]
            (is (ish? (g/mul a b)
                      (g/mul b a)))))

(defn additive-inverse [options generator type-name]
  (checking (str type-name " has additive inverses via g/negate and g/sub")
            options
            [a generator]
            (is (ish? (v/zero-like a) (g/add a (g/negate a))))
            (is (v/zero? (g/add a (g/negate a))))
            (is (ish? (v/zero-like a) (g/add (g/negate a) a)))
            (is (ish? (v/zero-like a) (g/sub a a)))))

(defn multiplicative-inverse [options generator type-name]
  (checking (str type-name " has multiplicative inverses via g/div and g/invert (excluding zero.)")
            options
            [a generator :when (not (v/zero? a))]
            (is (ish? (v/one-like a) (g/mul a (g/invert a))))
            (is (ish? (v/one-like a) (g/mul (g/invert a) a)))
            (is (ish? (v/one-like a) (g/div a a)))))

(defn mul-distributes-over-add [options generator type-name]
  (checking (str type-name " g/mul distributes over g/add, left and right")
            options
            [a generator
             b generator
             c generator]
            (is (ish? (g/mul a (g/add b c))
                      (g/add (g/mul a b) (g/mul a c))))
            (is (ish? (g/mul (g/add b c) a)
                      (g/add (g/mul b a) (g/mul c a))))))

;; These put the above laws together into known abstract-algebraic bundles.

(defn additive-semigroup
  "An additive semigroup is a type `a` with a `g/add` implementation that's
  associative:

      a + (b + c) == (a + b) + c

  and, optionally, commutative:

      a + b == b + a

  a commutative semigroup is also called an \"abelian semigroup\"."
  [opts generator type-name & {:keys [commutative?]}]
  (associative-add opts generator type-name)
  (when commutative?
    (commutative-add opts generator type-name)))

(defn additive-monoid
  "an additive monoid is a type `a` with a \"zero\", that acts as an identity
  element when added to any other element:

      0 + a == a + 0 == a

  `(v/zero-like a)` should always return this element,
  and `(v/zero? (v/zero-like))` should always be true.`"
  [opts generator type-name & {:keys [commutative?]}]
  (additive-semigroup opts generator type-name :commutative? commutative?)
  (zero-like opts generator type-name))

(defn multiplicative-semigroup
  "A multiplicative semigroup is a type `a` with a `g/mul` implementation that's
  associative:

      a * (b * c) == (a * b) * c

  and, optionally, commutative:

      a * b == b * a

  a commutative semigroup is also called an \"abelian semigroup\"."
  [opts generator type-name & {:keys [commutative?]}]
  (one-like opts generator type-name)
  (associative-mul opts generator type-name)
  (when commutative?
    (commutative-mul opts generator type-name)))

(defn multiplicative-monoid
  "a multiplicative monoid is a type `a` with a \"one\", that acts as an identity
  element when multiplied by any other element:

      0 * a == a * 0 == a

  `(v/one-like a)` should always return this element,
  and `(v/one? (v/one-like))` should always be true.`"
  [opts generator type-name & {:keys [commutative?]}]
  (multiplicative-semigroup opts generator type-name  :commutative? commutative?)
  (one-like opts generator type-name))

(defn additive-group
  "An additive group is a type `a` that passes all of the laws in
  `additive-monoid` (optionally commutative), and additionally can generate an
  additive inverse for every element. `g/negate` returns the additive inverse.
  The additional rules are:

      a + (-a) == 0
      (-a) + a == 0
      a - a == 0

  where `(-a)` is `(g/negate a)`, and `a - a` is `(g/sub a a)`.

  a commutative group is also called an \"abelian group\"."
  [opts generator type-name & {:keys [commutative?]}]
  (additive-monoid opts generator type-name :commutative? commutative?)
  (additive-inverse opts generator type-name))

(defn multiplicative-group
  "A multiplicative group is a type `a` that passes all of the laws in
  `multiplicative-monoid` (optionally commutative), and additionally can
  generate a multiplicative inverse for every element. `g/invert` returns the
  multiplicative inverse. The additional rules are:

      a * (1 / a) == 1
      (1 / a) * a == 1
      a / a == 1

  where `(1 / a)` is `(g/invert a)`, and `a / a` is `(g/div a a)`.

  a commutative group is also called an \"abelian group\"."
  [opts generator type-name & {:keys [commutative?]}]
  (multiplicative-monoid opts generator type-name :commutative? commutative?)
  (multiplicative-inverse opts generator type-name))

(defn ring
  "A ring is a type `a` that:

  - passes all of the laws in `additive-group` with `:commutative? true`
  - passes `multiplicative-semigroup`

  and additionally can distribute its `g/mul` implementation over `g/add` on the
  right or the left of a sum:

      a * (b + c) == (a * b) + (a * c)
      (b + c) * a == (b * a) + (c * a)

  if `:with-one? true` is passed, `ring` will check that `a`'s `g/mul`
  implementation has an identity, ie, that `a` is a multiplicative monoid, not
  just a multiplicative semigroup. A type with this structure is called a \"ring
  with unity\".

  If `:commutative? true` is passed, `ring` will check that `a` has a
  commutative `g/mul` implementation; ie, that `a` is a \"commutative ring\". "
  [opts generator type-name & {:keys [with-one? commutative?]}]
  (let [mul-check (if with-one?
                    multiplicative-monoid
                    multiplicative-semigroup)]
    (additive-group opts generator type-name :commutative? true)
    (mul-check opts generator type-name :commutative? commutative?)
    (mul-distributes-over-add opts generator type-name)))

(defn field
  "A field is a type `a` that:

  - passes all of the laws in `additive-group` with `:commutative? true`
  - passes `multiplicative-group` with `:commutative? true`

  and additionally can distribute its `g/mul` implementation over `g/add` on the
  right or the left of a sum:

      a * (b + c) == (a * b) + (a * c)
      (b + c) * a == (b * a) + (c * a)

  if `:skew? true` is passed, `field` will drop the condition that the `g/mul`
  implementation is commutative.

  A type `a` with this structure is called a \"skew field\", or \"division
  ring\"."
  [opts generator type-name & {:keys [skew?]}]
  (additive-group opts generator type-name :commutative? true)
  (multiplicative-group opts generator type-name :commutative? (not skew?))
  (mul-distributes-over-add opts generator type-name))
