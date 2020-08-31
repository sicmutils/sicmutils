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

(ns sicmutils.laws
  "test.check laws useful for checking the algebraic properties of different types
  that implement the sicmutils.generic operations, and the additive and
  multiplicative options in sicmutils.value.Value."
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish? zeroish?]]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(defn nullity [options generator type-name]
  (checking (str type-name " v/nullity? agrees with v/zero-like.")
            options
            [a generator]
            (is (v/nullity? (v/zero-like a)))))

(defn unity [options generator type-name]
  (checking (str type-name " v/unity? agrees with v/one-like.")
            options
            [a generator]
            (is (v/unity? (v/one-like a)))))

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
            (is (v/nullity? (g/add a (g/negate a))))
            (is (ish? (v/zero-like a) (g/add (g/negate a) a)))
            (is (ish? (v/zero-like a) (g/sub a a)))))

(defn multiplicative-inverse [options generator type-name]
  (checking (str type-name " has multiplicative inverses via g/div and g/invert (excluding zero.)")
            options
            [a generator :when (not (v/nullity? a))]
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
  and `(v/nullity? (v/zero-like))` should always be true.`"
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
  and `(v/unity? (v/one-like))` should always be true.`"
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

  if `:with-unity? true` is passed, `ring` will check that `a`'s `g/mul`
  implementation has an identity, ie, that `a` is a multiplicative monoid, not
  just a multiplicative semigroup. A type with this structure is called a \"ring
  with unity\".

  If `:commutative? true` is passed, `ring` will check that `a` has a
  commutative `g/mul` implementation; ie, that `a` is a \"commutative ring\". "
  [opts generator type-name & {:keys [with-unity? commutative?]}]
  (let [mul-check (if with-unity?
                    multiplicative-monoid
                    multiplicative-semigroup)]
    (additive-group opts generator type-name :commutative? true))
  (multiplicative-semigroup opts generator type-name :commutative? commutative?))

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
