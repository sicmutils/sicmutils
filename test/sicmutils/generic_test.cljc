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

(ns sicmutils.generic-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g]
            [sicmutils.laws :as l]
            [sicmutils.value :as v]))

;; Remaining

(defmulti s* v/argument-kind)
(defmulti s+ v/argument-kind)

(defn multiply-string
  [n s]
  (apply str (repeat n s)))

(defn product-string
  [s t]
  (apply str (for [cs s ct t] (str cs ct))))

(def string #?(:clj String :cljs js/String))

(defmethod s* [v/numtype string] [n s] (multiply-string n s))
(defmethod s* [string v/numtype] [s n] (multiply-string n s))
(defmethod s* [string string] [s t] (product-string s t))
(defmethod s+ [string string] [s t] (str s t))

(deftest handler-fn
  (testing "multiply-string"
    (is (= "foofoofoo" (multiply-string 3 "foo")))
    (is (= "" (multiply-string 0 "bar")))
    (is (= "" (multiply-string -2 "bar"))))
  (testing "mul"
    (is (= "bazbaz" (s* 2 "baz")))
    (is (= "quxquxqux" (s* 3 "qux")))
    (is (= "quxquxqux" (s* "qux" 3)))
    (is (= "cecrcicnoeoroionlelrlilnieiriiinnenrninn" (s* "colin" "erin")))
    (is (= "eceoeleienrcrorlrirnicioiliiinncnonlninn" (s* "erin" "colin"))))
  (testing "add"
    (is (= "foobar" (s+ "foo" "bar")))
    (is (= "zzz" (s+ "" "zzz")))))

(deftest type-assigner
  (testing "types"
    (is (= #?(:clj Long :cljs ::v/native-integral) (v/kind 9)))
    (is (= #?(:clj Double :cljs ::v/native-integral) (v/kind 99.0)))
    (is (= #?(:clj Double :cljs js/Number) (v/kind 99.5)))))

(deftest generic-plus
  (is (= 0 (g/+)) "no args returns additive identity")
  (checking "g/+"
            100
            [x gen/any]
            (is (= x (g/+ x)) "single arg should return itself, for any type.")
            (is (= (if (v/nullity? x) 0 x)
                   (g/+ x 0))
                "adding a 0 works for any input. The first zero element gets
                returned.")
            (is (= x (g/+ 0 x)) "adding a leading 0 acts as identity.")
            (is (= (if (v/nullity? x) 0 x)
                   (g/+ 0 x 0.0 0 0)) "multi-arg works as long as zeros
            appear.")))

(deftest generic-minus
  (is (= 0 (g/-)) "no-arity returns the additive identity.")
  (checking "Subtracting a zero acts as id, with no implementations registered."
            100
            [x gen/any]
            (is (= x (g/- x 0)))))

(deftest generic-times
  (is (= 1 (g/*)) "No args returns the multiplicative identity.")
  (checking "g/*"
            100
            [x gen/any]
            (is (= x (g/* x)) "single arg returns itself.")
            (is (= (if (v/unity? x) 1 x)
                   (g/* x 1)) "First unity gets returned.")
            (is (= x (g/* 1 x)) "Anything times a 1 returns itself.")))

(deftest generic-divide
  (is (= 1 (g/divide)) "division with no args returns multiplicative identity")
  (checking "g/divide"
            100
            [x gen/any]
            (is (= x (g/divide x 1)) "dividing by one a single time returns the input")
            (is (= x (g/divide x 1 1 1 1.0 1)) "dividing by 1 returns the input")))

(defn ^:private is* [eq actual expected]
  (is (eq actual expected)
      #?(:clj (format "expected: %s\n  actual: %s"
                      (pr-str expected)
                      (pr-str actual)))))


(defn integral-unary-tests
  [int->a & {:keys [exclusions eq]
             :or {eq =}}]
  (letfn [(check [op x expected]
            (is* eq
                 (op (int->a x))
                 (int->a expected)))]
    (when-not (:negate exclusions)
      (testing "negate"
        (check g/negate 4 -4)
        (check (comp g/negate g/negate) 4 4)))

    (when-not (:abs exclusions)
      (testing "abs"
        (check g/abs -1 1)
        (check g/abs 1 1)))

    (when-not (:magnitude exclusions)
      (testing "magnitude"
        (check g/magnitude -123 123)
        (check g/magnitude 123 123)))

    (when-not (:square exclusions)
      (testing "square"
        (check g/square 2 4)
        (check g/square -2 4)))

    (when-not (:cube exclusions)
      (testing "cube"
        (check g/cube 3 27)
        (check g/cube -3 -27)))

    (when-not (:negative? exclusions)
      (testing "negative?"
        (is (g/negative? (g/negate (int->a 4))))
        (is (not (g/negative? (g/negate (g/negate (int->a 4))))))))))

(defn integral-binary-tests
  [int->a int->b & {:keys [exclusions eq]
                    :or {eq =}}]
  (letfn [(check [op l r expected]
            (let [a (int->a expected)
                  al-br (op (int->a l) (int->b r))
                  bl-ar (op (int->b l) (int->a r))]
              (is* eq al-br a)
              (is* eq bl-ar a)))]

    (when-not (:add exclusions)
      (testing "add"
        (check g/add 2 2 4)
        (check g/add 2 0 2)
        (is (eq (int->a 10)
                (reduce g/add (map int->a [1 2 3 4]))))))

    (when-not (:mul exclusions)
      (testing "mul"
        (check g/mul 5 4 20)
        (check g/mul 2 2 4)
        (is (eq (int->a 8)
                (reduce g/mul (map int->a [2 2 2]))))))

    (when-not (:sub exclusions)
      (testing "sub"
        (check g/sub 0 4 -4)))

    (when-not (:expt exclusions)
      (testing "expt"
        (check g/expt 2 5 32)))

    (when-not (:gcd exclusions)
      (testing "gcd"
        (check g/gcd (g/* 2 3 5 7) (g/* 2 5 7 11) (g/* 2 5 7))
        (check g/gcd 4 0 4)
        (check g/gcd 0 4 4)
        (check g/gcd 1 4 1)
        (check g/gcd 4 1 1)))

    (when-not (:quotient exclusions)
      (testing "quotient"
        (check g/quotient 5 2 2)))

    (when-not (:remainder exclusions)
      (testing "remainder"
        (check g/remainder 5 2 1)

        ;; Clojure's `rem` and `mod` differ in how they handle negative numbers.
        (check g/remainder -7 4 -3)))

    (when-not (:modulo exclusions)
      (testing "modulo"
        (check g/modulo 5 2 1)

        ;; Clojure's `rem` and `mod` differ in how they handle negative numbers.
        (check g/modulo -7 4 1)))

    (when-not (:exact-divide exclusions)
      (testing "exact-divide"
        (check g/exact-divide 20 5 4)))))

(defn floating-point-unary-tests
  [float->a & {:keys [exclusions eq]
               :or {eq =}}]
  (letfn [(check [op x expected]
            (is* eq
                 (op (float->a x))
                 (float->a expected)))]
    (when-not (:negate exclusions)
      (testing "negate"
        (check g/negate 4.2 -4.2)
        (check (comp g/negate g/negate) 4.2 4.2)))

    (when-not (:abs exclusions)
      (testing "abs"
        (check g/abs 1.4 1.4)
        (check g/abs -1.4 1.4)))

    (when-not (:magnitude exclusions)
      (testing "magnitude"
        (check g/magnitude 123.4 123.4)
        (check g/magnitude -123.4 123.4)))

    (when-not (:square exclusions)
      (testing "square"
        (check g/square 2.2 4.84)
        (check g/square -2.2 4.84)))

    (when-not (:cube exclusions)
      (testing "cube"
        (check g/cube 3.2 32.768)
        (check g/cube -3.2 -32.768)))

    (when-not (:invert exclusions)
      (testing "invert"
        (check g/invert 1 1)
        (check g/invert 5 0.2)))

    (when-not (:exp exclusions)
      (testing "exp"
        (check g/exp 0 1)))

    (when-not (:log exclusions)
      (testing "log"
        (check g/log 1 0)))

    (when-not (or (:log exclusions)
                  (:exp exclusions))
      (testing "log/exp"
        (check (comp g/log g/exp) 10 10)))))

(defn floating-point-binary-tests
  [float->a float->b & {:keys [exclusions eq]
                        :or {eq =}}]
  (letfn [(check [op l r expected]
            (let [a (float->a expected)
                  al-br (op (float->a l) (float->b r))
                  bl-ar (op (float->b l) (float->a r))]
              (is* eq al-br a)
              (is* eq bl-ar a)))]
    (when-not (:add exclusions)
      (testing "add"
        (check g/add 2 2 4)
        (check g/add 2 0 2)))

    (when-not (:mul exclusions)
      (testing "mul"
        (check g/mul 5 4 20)
        (check g/mul 2 2 4)))

    (when-not (:sub exclusions)
      (testing "sub"
        (check g/sub 3.14 1 2.14)
        (check g/sub 0 4 -4)))

    (when-not (:expt exclusions)
      (testing "expt"
        (check g/expt 2 5 32)))

    (when-not (:div exclusions)
      (testing "div"
        (check g/div 20 4 5)))

    (when-not (:invert exclusions)
      (testing "invert"
        (is (eq (float->a 0.05)
                (g/invert (float->a 20))))
        (is (eq (g/invert (float->a 21))
                (g/div (float->a 1) (float->a 21))))
        (is (eq (g/invert (float->a 21))
                (g/div (float->b 1) (float->b 21))))))))

(defn integral-tests
  "Battery of tests that check the behavior of standard generic operations on
  types that handle integral numbers."
  [int->a & {:keys [exclusions eq]}]
  (integral-unary-tests int->a :exclusions exclusions :eq (or eq =))
  (integral-binary-tests int->a int->a :exclusions exclusions :eq (or eq =)))

(defn integral-a->b-tests
  "Battery of tests that check the behavior of standard generic operations on
  types that handle integral numbers."
  [int->a int->b & {:keys [exclusions eq]}]
  (integral-binary-tests int->a int->b :exclusions exclusions :eq (or eq =)))

(defn floating-point-tests
  "Battery of tests that check the behavior of standard generic operations on
  types that handle floating point numbers."
  [float->a & {:keys [exclusions eq]}]
  (floating-point-unary-tests float->a
                              :exclusions exclusions
                              :eq (or eq =))
  (floating-point-binary-tests float->a float->a
                               :exclusions exclusions
                               :eq (or eq =)))
