#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numbers-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.complex :as c]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.generic-test :as gt]
            [emmy.laws :as l]
            [emmy.util :as u]
            [emmy.value :as v]))

(def near (v/within 1e-12))

(defn gen-integer
  "Integral generator that takes a `max` magnitude size parameter."
  ([] sg/any-integral)
  ([max] (gen/fmap #(g/modulo % (int max))
                   sg/any-integral)))

(defn gen-real
  "Integral generator that takes a `max` magnitude size parameter."
  ([] sg/real)
  ([max] (gen/fmap #(g/modulo % (int max))
                   sg/real)))

(deftest numeric-laws
  ;; All native types available on clj and cljs form fields.
  (l/field 100 sg/bigint #?(:clj "clojure.lang.BigInt" :cljs "js/BigInt"))
  (l/field 100 sg/long #?(:clj "java.lang.Long" :cljs "goog.math.Long"))
  (l/field 100 sg/integer #?(:clj "java.lang.Integer" :cljs "goog.math.Integer"))

  #?(:clj
     ;; There's no biginteger / bigint distinction in cljs.
     (l/field 100 sg/biginteger "java.math.BigInteger"))

  #?(:cljs
     ;; this is covered by sg/long in clj.
     (l/field 100 sg/native-integral "integral js/Number")))

#?(:clj
   ;; Same note here about no bigint/biginteger distinction in cljs.
   (deftest biginteger-generics
     (gt/integral-tests u/biginteger)))

(deftest floating-point-laws
  ;; Doubles form a field too.
  (with-comparator (v/within 1e-3)
    (l/field 100 (sg/reasonable-double) #?(:clj  "java.lang.Double"
                                           :cljs "floating point js/Number"))))

(deftest number-generics
  (gt/integral-tests identity :exclusions #{:exact-divide})
  (gt/floating-point-tests identity :eq near)

  (testing "log, exp works on bigint"
    (is (ish? 59874.14171519782
              (g/exp #sicm/bigint 11)))

    (is (ish? 2.3978952727983707
              (g/log #sicm/bigint 11)))

    (is (ish? 3.4594316186372978
              (g/log2 #sicm/bigint 11)))

    (is (ish? 1.041392685158225
              (g/log10 #sicm/bigint 11))))

  (testing "log converts to complex"
    (is (c/complex? (g/log -10)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "expt goes rational with negative expt"
    (is (= #sicm/ratio 1/4 (g/expt 2 -2))))

  (testing "expt goes imaginary with negative base and non-integral expt"
    (is (ish? (c/complex 0 (g/sqrt 2))
              (g/expt -2 #sicm/ratio 1/2))
        "integer, ratio")

    (is (ish? (c/complex 0 (g/sqrt 2))
              (g/expt -2 0.5))
        "integer, float")

    (is (ish? (c/complex 0 (g/sqrt 2.2))
              (g/expt -2.2 0.5))
        "both floating point")

    (is (ish? (c/complex 0 (g/sqrt 0.5))
              (g/expt #sicm/ratio -1/2 0.5))
        "ratio, float"))

  (testing "exp/log round-trip, but coerce to double on the JVM"
    (is (= 0.0 (g/log (g/exp 0))))
    (is (= 10.0 (g/log (g/exp 10)))))

  (testing "div"
    (is (= 5 (g/div 20 4))))

  (testing "invert"
    (is (= 1 (g/invert 1)))
    (is (= (g/div 1 21) (g/invert 21))))

  (testing "sqrt handles negative numbers, 0"
    (is (= 0 (g/sqrt 0)))
    (is (= 9 (g/sqrt 81)))
    (is (c/complex? (g/sqrt -81)))
    (is (= (c/complex 0 9) (g/sqrt -81))))

  (testing "sqrt of one preserves type"
    (is (v/one-like (g/sqrt c/ONE)))
    (is (c/complex? (g/sqrt c/ONE))))

  (checking "transpose, determinant, trace act as id" 100 [x sg/real]
            (is (= x (g/transpose x)))
            (is (= x (g/determinant x)))
            (is (= x (g/trace x))))

  (checking "dimension always returns 1" 100 [x sg/real]
            (is (= 1 (g/dimension x))))

  (checking "dot-product, inner-product" 100
            [x sg/real y sg/real]
            (is (v/= (g/* x y)
                     (g/dot-product x y))
                "dot-product == mul for 1-d scalars.")

            (is (= (g/dot-product x y)
                   (g/inner-product x y))
                "dot-product == inner-product for scalars, where conjugate acts
                as identity")))

(deftest integer-generics
  (gt/integral-tests u/int)
  (gt/integral-a->b-tests u/int identity :exclusions #{:exact-divide})

  (testing "g/expt"
    (is (= (g/expt (u/int 2) (u/int -2))
           (g/invert (u/int 4))))))

(deftest long-generics
  (gt/integral-tests u/long)
  (gt/integral-a->b-tests u/long identity :exclusions #{:exact-divide})

  (testing "g/expt"
    (is (= (g/expt (u/long 2) (u/long -2))
           (g/invert (u/long 4))))))

(deftest bigint-generics
  (gt/integral-tests u/bigint)
  (gt/integral-a->b-tests u/bigint identity)
  (gt/integral-a->b-tests identity u/bigint)

  (testing "bigint/float compatibility"
    (testing "g/expt"
      (is (= (g/expt (u/bigint 2) (u/bigint -2))
             (g/invert (u/bigint 4)))))

    (testing "g/add with floating point"
      (is (= 12.5 (g/add (u/bigint 10) 2.5)))
      (is (= 12.5 (g/add 2.5 (u/bigint 10)))))

    (testing "g/mul with floating point"
      (is (ish? 25 (g/mul (u/bigint 10) 2.5)))
      (is (ish? 25 (g/mul 2.5 (u/bigint 10)))))

    (testing "g/sub with floating point"
      (is (= 7.5 (g/sub (u/bigint 10) 2.5)))
      (is (= -7.5 (g/sub 2.5 (u/bigint 10)))))

    (testing "g/expt with floating point"
      (is (ish? 316.2277660168379 (g/expt (u/bigint 10) 2.5)))
      (is (ish? 9536.7431640625 (g/expt 2.5 (u/bigint 10)))))))

(deftest double-generics
  (gt/integral-tests double
                     :eq near
                     :exclusions #{:exact-divide :gcd :remainder :modulo :quotient})
  (gt/floating-point-tests double :eq near))

(deftest arithmetic
  (testing "misc trig"
    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0)))
    (is (ish? (/ Math/PI 2) (g/asin 1)))
    (is (ish? (/ Math/PI 2) (g/acos 0)))
    (is (ish? (/ Math/PI 6) (g/asin 0.5)))
    (is (ish? (/ Math/PI 4) (g/atan 1)))
    (is (ish? (/ Math/PI 4) (g/atan 1 1)))
    (is (ish? (- (/ Math/PI 4)) (g/atan -1)))
    (is (ish? (* -3 (/ Math/PI 4)) (g/atan -1 -1)))
    (is (ish? (* 3 (/ Math/PI 4)) (g/atan 1 -1)))
    (is (ish? (/ Math/PI -4) (g/atan -1 1)))
    (is (ish? (/ Math/PI 3) (g/acos #?(:clj 1/2 :cljs (/ 1 2))))))

  (testing ">1 gets promoted to complex for asin, acos"
    (is (c/complex? (g/asin 2)))
    (is (c/complex? (g/acos 2))))

  (testing "sqrt handles negative numbers, 0"
    (is (= 0 (g/sqrt 0)))
    (is (= 9 (g/sqrt 81)))
    (is (c/complex? (g/sqrt -81)))
    (is (= (c/complex 0 9) (g/sqrt -81))))

  (testing "sqrt of one preserves type"
    (is (v/one-like (g/sqrt c/ONE)))
    (is (c/complex? (g/sqrt c/ONE))))

  (testing "log"
    (is (c/complex? (g/log -10)))
    (is (= 0.0 (g/log 1)))
    (is (= (c/complex 0 Math/PI) (g/log -1))))

  (testing "exp"
    (is (= 1 (g/exp 0))
        "exp(0) evaluates to an exact 1, vs 1.0")))

;; Test of generic wrapper operations.

(deftest generic-plus
  (testing "emmy.numbers provides implementations of the methods needed by g/+. Test
  that these functions now come work with numbers."
    (testing "simple"
      (is (= 7 (g/+ 3 4)))
      (is (= 4 (g/+ 2 2)))
      (is (= 3.5 (g/+ 1.5 2))))

    (testing "many"
      (is (= 10 (g/+ 1 2 3 4)))
      (is (= 33 (g/+ 3 4 5 6 7 8))))))

(deftest generic-minus
  (testing "numbers provides implementations, so test behaviors."
    (is (= -3.14 (g/- 3.14)))
    (is (= 2.14 (g/- 3.14 1))))

  (testing "many"
    (is (= -14 (g/- 10 9 8 7)))))

(deftest generic-times
  (testing "numbers provides implementations, so test behaviors."
    (is (= 20 (g/* 5 4)))
    (is (= 4 (g/* 2 2)))
    (is (= 8 (g/* 2 2 2)))))

(deftest generic-divide
  (testing "numbers provides implementations, so test behaviors."
    (is (= 5 (g/divide 20 4)))
    (is (= 2 (g/divide 8 2 2)))))

(deftest fractional-integer-tests
  (checking "fractional-part" 100
            [x sg/real]
            (testing "0 <= frac(x) < 1"
              (is (<= 0 (g/fractional-part x)))
              (is (< (g/fractional-part x) 1)))

            (is (= (g/fractional-part x)
                   (g/- x (g/floor x)))
                "for neg or pos x, x - floor(x) == frac(x)"))

  (checking "frac(x) + int(x) == x for non-negative real x" 100
            [x (gen/fmap g/abs sg/real)]
            (is (== x (g/+ (g/integer-part x)
                           (g/fractional-part x)))))

  (checking "fractional, integer-part are idempotent" 100
            [x sg/real]
            (is (= (g/fractional-part x)
                   (g/fractional-part
                    (g/fractional-part x))))

            (is (= (g/integer-part x)
                   (g/integer-part
                    (g/integer-part x))))))

(deftest floor-ceil-tests
  (checking "floor and ceiling relations" 100
            [n sg/any-integral]
            (is (= n (g/floor n) (g/ceiling n))
                "floor(n) == ceiling(n) == n for ints")

            (is (not
                 (pos?
                  (v/compare (g/floor n) (g/ceiling n))))
                "floor(n) <= ceiling(n)"))

  (checking "negating arg switches floor and ceiling, changes sign" 100
            [n sg/any-integral]
            (is (v/zero?
                 (g/+ (g/floor n)
                      (g/ceiling (g/- n))))
                "floor(x) + ceil(-x) == 0")

            (is (= (g/- (g/floor n))
                   (g/ceiling (g/- n)))
                "-floor(x) == ceil(-x)")

            (is (= (g/- (g/ceiling n))
                   (g/floor (g/- n)))
                "-ceil(x) == floor(-x)"))

  (checking "floor, ceiling are idempotent" 100
            [x sg/real]
            (is (= (g/floor x)
                   (g/floor (g/floor x))))

            (is (= (g/ceiling x)
                   (g/ceiling (g/ceiling x)))))

  (checking "⌊x + n⌋ == ⌊x⌋ + n for all integers n" 100
            [x (gen-real 1e4)
             n (gen-integer 1e4)]
            (is (ish? (g/floor (g/+ x n))
                      (g/+ (g/floor x) n))))

  (checking "⌈x + n⌉ == ⌈x⌉ + n for all integers n" 100
            [x (gen-real 1e4)
             n (gen-integer 1e4)]
            (is (ish? (g/ceiling (g/+ x n))
                      (g/+ (g/ceiling x) n))))

  ;; These nice identities come from [John Cook's
  ;; blog](https://www.johndcook.com/blog/2020/08/14/multiply-divide-and-floor/).
  (checking "for nat n, real x, ⌊⌊n x⌋ / n⌋ == ⌊x⌋" 100
            [n (gen/fmap inc gen/nat)
             x sg/real]
            (is (= (g/floor x)
                   (g/floor
                    (g/div (g/floor (g/* n x)) n)))))

  (checking "for nat n, real x, ⌈⌈n x⌉ / n⌉ == ⌈x⌉" 100
            [n (gen/fmap inc gen/nat)
             x sg/real]
            (is (= (g/ceiling x)
                   (g/ceiling
                    (g// (g/ceiling (g/* n x)) n))))))

(deftest modulo-remainder-tests
  ;; NOTE: Some of these use integer on one side simply to control numerical
  ;; instability, not because the operations are invalid for real.
  ;;
  ;; TODO unit test with real side for anything that bakes in integer.
  (letfn [(nonzero [g]
            (gen/fmap (fn [x]
                        (if (v/zero? x)
                          (v/one-like x)
                          x))
                      g))]
    (checking "mod, rem identity" 100
              [x (gen-real 1e4)
               y (nonzero (gen-integer 1e4))]
              (is (ish? (g/modulo x y)
                        (g/modulo
                         (g/modulo x y) y)))

              (is (ish? (g/remainder x y)
                        (g/remainder
                         (g/remainder x y) y))))

    (checking "x mod y == x - y ⌊x/y⌋ for real x, y" 100
              [x (gen-real 1e4)
               y (nonzero (gen-integer 1e4))]
              (is (ish? (g/modulo x y)
                        (g/- x (g/* y (g/floor
                                       (g/div x y)))))
                  "y == int to keep things numerically stable."))

    (checking "mod(x y) == rem(x y) for positive x, y" 100
              [x (gen/fmap g/abs (gen-real 1e4))
               y (nonzero
                  (gen/fmap g/abs (gen-integer 1e4)))]
              (is (ish? (g/modulo x y)
                        (g/remainder x y))))

    (testing "quotient, exact-divide error when exact results aren't possible"
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (g/exact-divide 4 1.2)))

      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (g/quotient 4 1.2))))

    (checking "quotient, remainder with floats" 100
              [x (gen/such-that (complement v/zero?) sg/real)]
              (is (= (g/quotient x x)
                     (g/exact-divide x x))
                  "exact-divide is fine if passed identical inputs")

              (is (v/= 1 (g/quotient x x))
                  "x/x == 1")

              (is (v/= 1 (g/quotient (- x) (- x)))
                  "-x/-x == 1")

              (is (v/= -1 (g/quotient (- x) x))
                  "-x/x == -1")

              (is (v/= -1 (g/quotient x (- x)))
                  "x/-x == -1")

              (testing "remainder"
                (is (v/zero? (g/remainder x x)))
                (is (v/zero? (g/remainder x (- x))))
                (is (v/zero? (g/remainder (- x) (- x))))
                (is (v/zero? (g/remainder (- x) x)))))

    (checking "x == y*quot(x,y) + rem(x,y)" 100
              [x (gen-integer 1e4)
               y (nonzero (gen-integer 1e4))]
              (let [rem (g/remainder x y)]
                (is (= x (g/+ (g/* y (g/quotient x y))
                              rem)))

                (when-not (v/zero? rem)
                  (is (= (v/compare 0 x)
                         (v/compare 0 rem))
                      "`g/remainder` returns a result of either 0 or the same
              sign as the numerator."))))

    (checking "x == y*floor(x/y) + mod(x,y)" 100
              [x (gen-integer 1e4)
               y (nonzero (gen-integer 1e4))]
              (let [mod (g/modulo x y)]
                (is (= x (g/+ (g/* y (g/floor (g// x y)))
                              mod)))

                (when-not (v/zero? mod)
                  (is (= (v/compare 0 y)
                         (v/compare 0 mod))
                      "`g/modulo` returns a result of either 0 or the same sign
              as the denominator."))))

    (checking "gcd identities" 100 [x sg/any-integral]
              (let [ax (g/abs x)]
                (is (= ax (g/gcd 0 x)))
                (is (= ax (g/gcd x 0)))
                (is (= 1 (g/gcd 1 x)))
                (is (= 1 (g/gcd x 1)))))

    (checking "gcd identities even with real" 100 [x sg/real]
              (is (= (g/abs x)
                     (g/gcd x x))))

    (letfn [(nonzero [g]
              (gen/fmap (fn [x]
                          (let [small (g/remainder x 10000)]
                            (if (v/zero? small) 1 small)))
                        g))]
      (checking "gcd" 100 [x (nonzero sg/small-integral)
                           y (nonzero sg/small-integral)
                           z (nonzero sg/small-integral)]
                (let [gxy (g/gcd x y)
                      x (g/div x gxy)
                      y (g/div y gxy)
                      z (g/abs z)

                      ;; `x` and `y` are now relatively prime, `z` positive.
                      xz (g/* x z)
                      yz (g/* y z)
                      g (g/gcd xz yz)]
                  (is (not (g/negative? gxy)))
                  (is (= x (g/exact-divide xz g)))
                  (is (= y (g/exact-divide yz g)))
                  (is (= (g/abs z) g))

                  (testing "1, -1 on right is id or negate"
                    (is (= z (g/exact-divide z 1)))
                    (is (= (- z) (g/exact-divide z -1)))))))

    (testing "lcm"
      (is (zero? (g/lcm 0 0))))

    (testing "exact-divide floats"
      (is (= -1.0 (g/exact-divide 1.2 -1.2)))
      (is (= 1.0 (g/exact-divide -1.2 -1.2)))
      (is (= -1.0 (g/exact-divide -1.2 1.2)))
      (is (= 1.0 (g/exact-divide 1.2 1.2))))))

(deftest numeric-trig-tests
  (testing "trig"
    (testing "sinc"
      (is (= 0 (g/sinc ##Inf)))
      (is (= 0 (g/sinc ##-Inf)))
      (is (= 1 (g/sinc 0)))
      (is (= 1 (g/sinc #sicm/bigint 0)))

      (checking "sinc nonzero" 100 [n (sg/reasonable-double)]
                (is (ish? (/ (Math/sin n) n)
                          (g/sinc n)))))

    (testing "tanc"
      (is (= 0 (g/sinc ##Inf)))
      (is (= 0 (g/sinc ##-Inf)))
      (is (= 1 (g/sinc 0)))
      (is (= 1 (g/sinc #sicm/bigint 0)))

      (checking "sinc nonzero" 100 [n (sg/reasonable-double)]
                (is (ish? (/ (Math/sin n) n)
                          (g/sinc n)))))

    (is (near (/ Math/PI 4) (g/asin (/ (g/sqrt 2) 2))))
    (is (near (/ Math/PI 4) (g/acos (/ (g/sqrt 2) 2))))
    (is (zero? (g/asin 0)))
    (is (near (/ Math/PI 2) (g/acos 0))))

  (testing ">1 gets promoted to complex for asin, acos"
    (is (c/complex? (g/asin 2)))
    (is (c/complex? (g/acos 2))))

  (checking "sin" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/sin n) (g/sin n))))

  (checking "cos" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/cos n) (g/cos n))))

  (checking "tan native" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/tan n) (g/tan n))))

  (checking "tan" 100 [n sg/real]
            (when-not (v/zero? (g/cos n))
              (is (ish? (g/div (g/sin n)
                               (g/cos n))
                        (g/tan n)))))

  (checking "asin native" 100 [n (sg/reasonable-double
                                  {:min (+ -1 1e-10)
                                   :max (- 1 1e-10)})]
            (is (ish? (Math/asin n) (g/asin n))))

  (with-comparator (v/within 1e-8)
    (checking "cos/acos" 100 [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/cos (g/acos n)))))

    (checking "sin/asin" 100 [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/sin (g/asin n)))))

    (checking "tan/atan" 100 [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/tan (g/atan n))))))

  (testing "tan/atan, 2 arity version"
    (is (ish? (/ 0.5 0.2)
              (g/tan (g/atan 0.5 0.2)))))

  (checking "cot" 100 [n sg/real]
            (when-not (or (v/zero? (g/sin n))
                          (v/zero? (g/cos n)))
              (is (ish? (g/cot n)
                        (g/invert (g/tan n))))))

  (checking "cosh" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/cosh n) (g/cosh n))))

  (checking "sinh" 100 [n (sg/reasonable-double)]
            (is (ish? (Math/sinh n) (g/sinh n))))

  (checking "tanh" 100 [n (sg/reasonable-double {:min -100 :max 100})]
            (when-not (v/zero? (g/cosh n))
              (is (ish? (g/tanh n)
                        (g/div (g/sinh n) (g/cosh n))))))

  (checking "sec" 100 [n sg/real]
            (when-not (v/zero? (g/cosh n))
              (is (ish? (g/sec n)
                        (g/invert (g/cos n))))))

  (checking "csc" 100 [n sg/real]
            (when-not (v/zero? (g/sin n))
              (is (ish? (g/csc n)
                        (g/invert (g/sin n))))))

  (checking "sech" 100 [n sg/real]
            (let [cosh-n (g/cosh n)]
              (when-not (v/zero? cosh-n)
                (is (ish? (g/sech n)
                          (g/invert cosh-n))))))

  (testing "acosh, numbers that need to convert to complex"
    (is (ish? (g/acosh 0) (g/acosh (u/int 0))))
    (is (ish? (g/acosh 0) (g/acosh (u/long 0))))
    (is (ish? (g/acosh 0) (g/acosh (u/bigint 0)))))

  (with-comparator (v/within 1e-8)
    (checking "acosh" 100
              [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/cosh (g/acosh n)))))

    (checking "asinh" 100
              [n (sg/reasonable-double {:min -100 :max 100})]
              (is (ish? n (g/sinh (g/asinh n)))))

    (checking "atanh" 100
              [n (sg/reasonable-double {:min -10 :max 10})]
              (when-not (v/one? (g/abs n))
                (is (ish? n (g/tanh (g/atanh n))))))))

(deftest complex-constructor-tests
  (checking "make-rectangular with zero (exact and inexact) acts as id" 100
            [n sg/real]
            (doseq [z [(g/make-rectangular n 0.0)
                       (g/make-rectangular n 0)]]
              (is (= n z))
              (is (not (c/complex? z)))))

  (checking "make-polar with zero angle or radius acts as id" 100
            [n sg/real]
            (doseq [z [(g/make-polar n 0)
                       (g/make-polar n 0.0)]]
              (is (= n z))
              (is (not (c/complex? z)))))

  (checking "make-rectangular" 100
            [real-part      (sg/reasonable-real 1e4)
             imaginary-part (sg/reasonable-real 1e4)]
            (let [z (g/make-rectangular real-part imaginary-part)]
              (is (== real-part (g/real-part z)))
              (is (== imaginary-part (g/imag-part z)))))

  (with-comparator (v/within 1e-8)
    (checking "make-rect, make-polar together" 100
              [real-part (sg/reasonable-real 1e3)
               imaginary-part (sg/reasonable-real 1e3)]
              (let [z (g/make-rectangular real-part imaginary-part)]
                (is (ish? z (g/make-polar
                             (g/magnitude z)
                             (g/angle z)))))))

  (checking "make-polar" 100
            [radius (sg/reasonable-real 1e4)
             angle (sg/reasonable-real 1e4)]
            (let [z (g/make-polar radius angle)]
              (is (ish? (g/magnitude z)
                        (g/sqrt (g/* z (g/conjugate z)))))
              (is (ish? (g/* radius (g/exp (g/* c/I angle)))
                        z))
              (is (ish? (g/abs radius)
                        (g/magnitude z))))))

(deftest complex-accessor-tests
  (checking "real/imag-part" 100 [x sg/real]
            (is (= x (g/real-part x)))
            (is (zero? (g/imag-part x))))

  (checking "conjugate" 100 [x sg/real]
            (is (= x (g/conjugate x))))

  (checking "angle" 100 [x sg/real]
            (if (neg? x)
              (is (ish? Math/PI (g/angle x))
                  "the angle of a negative number is pi in the complex plane.")
              (is (v/zero? (g/angle x))))))
