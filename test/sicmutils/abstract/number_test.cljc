;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.abstract.number-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.abstract.number :as an]
            [sicmutils.expression :as x]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as sym]
            [sicmutils.value :as v]))

(deftest predicate-tests
  (testing "v/eq"
    (doall
     (for [l ['x (an/literal-number 'x)]
           r ['x (an/literal-number 'x)]]
       (is (v/eq l r))))

    (doall
     (for [l [12 (an/literal-number 12)]
           r [12 (an/literal-number 12)]]
       (is (v/eq l r)))))

  (checking "interaction with symbols"
            100
            [x gen/symbol]
            (is (not (an/literal-number? x))
                "symbols are not literal-numbers")

            (is (an/abstract-number? x)
                "Symbols ARE abstract numbers!"))

  (checking "literal-number? behavior with numbers."
            100
            [x gen/small-integer]
            (let [n (an/literal-number x)]
              (is (an/literal-number? n)
                  "Any wrapped number returns true to literal-number?")

              (is (not (an/literal-number? x))
                  "numbers are NOT explicit literal-numbers"))))

(deftest abstract-number-tests
  (checking "literal-number"
            100
            [x gen/small-integer]
            (let [n (an/literal-number x)
                  result (g/+ 1 (g/cos n))]
              (is (= (if (zero? x)
                       2
                       `(~'+ 1 (~'cos ~x)))
                     (v/freeze result))
                  "When literal-number wraps an actual number, it attempts to
        keep the result exact instead of evaluating the fns... UNLESS specific
        values like (cos 0) can be exactly evaluated.")))

  (checking "inexact numbers" 100 [x gen/small-integer]
            (is (=  (+ 1 (Math/cos x))
                    (g/+ 1 (g/cos x)))
                "You get a floating-point inexact result by calling generic fns
    on a number directly, by comparison.")))

(deftest literal-number-arithmetic-tests
  (letfn [(check [op l r]
            (let [expected (op l r)
                  others   [[(an/literal-number l) r]
                            [l (an/literal-number r)]
                            [(an/literal-number l) (an/literal-number r)]]]
              (doall
               (for [[x y] others]
                 (is (v/eq expected (op x y)))))))]
    (checking "+, -, *, / fall through to number ops"
              100 [x gen/small-integer
                   y gen/small-integer]
              (check g/* x y)
              (check g/+ x y)
              (check g/- x y)
              (when-not (zero? y)
                (check g// x y))))

  (testing "negate"
    (is (= (g/+ 'x (g/- 'x) (g/+ 'x (g/negate 'x)))))
    (is (= '(+ x (- x)) (v/freeze
                         (g/+ 'x (g/negate 'x))))))

  (testing "invert"
    (is (= (g/div 1 'x) (g/invert 'x)))
    (is (= '(/ 1 x) (v/freeze
                     (g/invert 'x))))
    )
  (testing "square"
    (is (= (g/expt 'x 2) (g/square 'x))))

  (testing "cube"
    (is (= (g/expt 'x 3) (g/cube 'x))))

  (testing "expt"
    (is (v/eq 1 (g/expt 'x 0)))
    (is (v/eq 'x (g/expt 'x 1))))


  (checking "abs" 100 [x gen/symbol]
            (is (= (an/literal-number
                    (list 'abs x))
                   (g/abs x))
                "You can wrap a symbolic expression in literal-number if you
                like."))

  (checking "sqrt" 100 [x gen/symbol]
            (is (= (list 'sqrt x)
                   (v/freeze (g/sqrt x)))))

  (checking "log" 100 [x gen/symbol]
            (is (= (list 'log x)
                   (v/freeze (g/log x))))
            (is (v/eq (g// (g/log x)
                           (Math/log 2))
                      (g/log2 x))
                "log2 divides by the inexact (log 2).")
            (is (v/eq (g// (g/log x)
                           (Math/log 10))
                      (g/log10 x))
                "log10 divides by the inexact (log 10)."))

  (checking "exp" 100 [x gen/symbol]
            (is (= (list 'exp x)
                   (v/freeze (g/exp x))))
            (is (= (g/expt 2 x) (g/exp2 x)))
            (is (= (g/expt 10 x) (g/exp10 x))))

  (testing "conjugate"
    (is (= '(conjugate (random x))
           (v/freeze
            (g/conjugate (an/literal-number
                          '(random x))))))
    (doall
     (for [op @#'sym/conjugate-transparent-operators]
       (is (= (v/freeze
               (an/literal-number
                (list op
                      (g/conjugate 'x)
                      (g/conjugate 'y))))
              (v/freeze
               (g/conjugate (an/literal-number
                             (list op 'x 'y)))))
           "This is a little busted, since we don't check for the proper number
           of inputs... but conjugates move inside these operators."))))

  (checking "real-part" 100 [z gen/symbol]
            (is (= (g/* (g// 1 2)
                        (g/+ z (g/conjugate z)))
                   (g/real-part z))))

  (checking "imag-part" 100 [z gen/symbol]
            (is (= (g/* (g// 1 2)
                        (g/* #sicm/complex "0-1i"
                             (g/- z (g/conjugate z))))
                   (g/imag-part z))))

  (checking "angle" 100 [z gen/symbol]
            (is (= (g/atan
                    (g/imag-part z)
                    (g/real-part z))
                   (g/angle z))))

  (checking "magnitude" 100 [z gen/symbol]
            (is (= (g/sqrt (g/mul (g/conjugate z) z))
                   (g/magnitude z)))))

(deftest literal-number-trig-tests
  (checking "inexact literal number trig"
            100
            [x (->> (gen/double* {:infinite? false
                                  :NaN? false
                                  :min 1e-8
                                  :max 1e8})
                    (gen/fmap (fn [x]
                                (if (v/exact? x)
                                  (+ x 0.5)
                                  x))))]
            (testing "cosine"
              (is (ish? (an/literal-number
                         (cond (@#'sym/n:pi-over-2-mod-pi? x) 0
                               (@#'sym/n:zero-mod-2pi? x) 1
                               (@#'sym/n:pi-mod-2pi? x) -1
                               :else (Math/cos x)))
                        (g/cos (an/literal-number x)))))

            (testing "sine"
              (is (ish? (an/literal-number
                         (cond (@#'sym/n:zero-mod-pi? x) 0
                               (@#'sym/n:pi-over-2-mod-2pi? x) 1
                               (@#'sym/n:-pi-over-2-mod-2pi? x) -1
                               :else (Math/sin x)))
                        (g/sin (an/literal-number x)))))

            (testing "tangent"
              (if (and (not (or (@#'sym/n:zero-mod-pi? x)
                                (@#'sym/n:pi-over-4-mod-pi? x)
                                (@#'sym/n:-pi-over-4-mod-pi? x)))
                       (@#'sym/n:pi-over-2-mod-pi? x))
                (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                             (g/tan (an/literal-number x)))
                    "if `x` sits on the vertical axis (ie, the angle minus pi/2
                    is zero, mod pi), then the cosine is 0 and tan is
                    undefined.")

                (is (ish? (an/literal-number
                           (cond (@#'sym/n:zero-mod-pi? x) 0
                                 (@#'sym/n:pi-over-4-mod-pi? x) 1
                                 (@#'sym/n:-pi-over-4-mod-pi? x) -1
                                 :else (Math/tan x)))
                          (g/tan (an/literal-number x)))
                    "Otherwise, test out some mild optimizations, and if these
                    don't work bail out to Math/tan."))))

  (comment
    'asin
    'acos
    'atan
    'sinh
    'cosh
    'sec
    'csc))

(deftest symbolic-arithmetic-tests
  (testing "+ constructor optimizations"
    (is (v/eq 'x (g/add 0 'x)))
    (is (v/eq 'x (g/add 'x 0))))

  (testing "sums fuse together in the constructor"
    (is (= '(+ x y z) (v/freeze (g/add 'x (g/add 'y 'z)))))
    (is (= '(+ 10 y z) (v/freeze (g/add 10 (g/add 'y 'z)))))
    (is (= '(+ y z 10) (v/freeze (g/add (g/add 'y 'z) 10))))
    (is (= '(+ y z a b)
           (v/freeze (g/add (g/add 'y 'z)
                            (g/add 'a 'b))))))

  (testing "sub constructor optimizations"
    (is (= (g/negate 'x) (g/- 0 'x)))
    (is (v/eq 'x (g/- 'x 0)))
    (is (v/eq 0 (g/- 'x 'x))))

  (testing "+/- with symbols"
    (is (= (g/+ 15 'x) (g/+ 10 3 2 'x)))
    (is (= (g/+ 10 'x 3 2) (g/+ 10 'x 3 2)))
    (is (= (g/+ 10 'x 3 2 1) (g/+ 10 'x 3 2 1)))
    (is (= (g/+ 30 'x 3 2 1) (g/+ 10 20 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 5 'x)) (g/- 10 3 2 'x)))
    (is (= (g/- 10 (g/+ 'x 3 2)) (g/- 10 'x 3 2)))
    (is (= (g/- 10 (g/+ 'x 3 2 1)) (g/- 10 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 20 'x 3 2 1)) (g/- 10 20 'x 3 2 1))))

  (testing "mul constructor optimizations"
    (is (v/eq 0 (g/* 0 'x)))
    (is (v/eq 0 (g/* 'x 0)))
    (is (v/eq 'x (g/* 1 'x)))
    (is (v/eq 'x (g/* 'x 1))))

  (testing "products fuse together in the constructor"
    (is (= '(* x y z) (v/freeze (g/mul 'x (g/mul 'y 'z)))))
    (is (= '(* 10 y z) (v/freeze (g/mul 10 (g/mul 'y 'z)))))
    (is (= '(* y z 10) (v/freeze (g/mul (g/mul 'y 'z) 10))))
    (is (= '(* y z a b)
           (v/freeze (g/mul (g/mul 'y 'z)
                            (g/mul 'a 'b))))))

  (testing "* with symbols"
    (is (= (g/* 60 'x) (g/* 10 3 2 'x)))
    (is (= (g/* 10 'x 3 2) (g/* 10 'x 3 2)))
    (is (= (g/* 10 'x 3 2 1) (g/* 10 'x 3 2 1)))
    (is (= (g/* 'x 10 'x 3 2 1) (g/* 'x 10 'x 3 2 1)))
    (is (= (g/* 200 'x 3 2) (g/* 10 20 'x 3 2 1))))

  (testing "div constructor optimizations"
    (is (v/eq 0 (g/divide 0 'x)))
    (is (v/eq 'x (g/divide 'x 1)))
    (is (thrown? #?(:clj ArithmeticException :cljs js/Error)
                 (g/divide 'x 0))))

  (testing "/ with symbols"
    (is (= (g// 'x (g/* 10 'x 3 2))
           (g// 'x 10 'x 3 2 1)))

    (is (= (g// (g/* 200 'x) (g/* 'x 3 2))
           (g// (g/* 10 20 'x) (g/* 'x 3 2 1)))))

  (testing "negate"
    (is (= (g/+ 'x (g/- 'x) (g/+ 'x (g/negate 'x)))))
    (is (= '(+ x (- x)) (v/freeze
                         (g/+ 'x (g/negate 'x))))))

  (testing "invert"
    (is (= (g/div 1 'x) (g/invert 'x)))
    (is (= '(/ 1 x) (v/freeze
                     (g/invert 'x)))))

  (testing "square"
    (is (= (g/expt 'x 2) (g/square 'x))))

  (testing "cube"
    (is (= (g/expt 'x 3) (g/cube 'x))))

  (testing "expt"
    (is (v/eq 1 (g/expt 'x 0)))
    (is (v/eq 'x (g/expt 'x 1))))

  (checking "abs" 100 [x gen/symbol]
            (is (= (an/literal-number
                    (list 'abs x))
                   (g/abs x))
                "You can wrap a symbolic expression in literal-number if you
                like."))

  (checking "sqrt" 100 [x gen/symbol]
            (is (= (list 'sqrt x)
                   (v/freeze (g/sqrt x)))))

  (checking "log" 100 [x gen/symbol]
            (is (= (list 'log x)
                   (v/freeze (g/log x))))
            (is (v/eq (g// (g/log x)
                           (Math/log 2))
                      (g/log2 x))
                "log2 divides by the inexact (log 2).")
            (is (v/eq (g// (g/log x)
                           (Math/log 10))
                      (g/log10 x))
                "log10 divides by the inexact (log 10)."))

  (checking "exp" 100 [x gen/symbol]
            (is (= (list 'exp x)
                   (v/freeze (g/exp x))))
            (is (= (g/expt 2 x) (g/exp2 x)))
            (is (= (g/expt 10 x) (g/exp10 x))))

  (testing "conjugate"
    (is (= '(conjugate (random x))
           (v/freeze
            (g/conjugate (an/literal-number
                          '(random x))))))
    (doall
     (for [op @#'sym/conjugate-transparent-operators]
       (is (= (v/freeze
               (an/literal-number
                (list op
                      (g/conjugate 'x)
                      (g/conjugate 'y))))
              (v/freeze
               (g/conjugate (an/literal-number
                             (list op 'x 'y)))))
           "This is a little busted, since we don't check for the proper number
           of inputs... but conjugates move inside these operators."))))

  (checking "real-part" 100 [z gen/symbol]
            (is (= (g/* (g// 1 2)
                        (g/+ z (g/conjugate z)))
                   (g/real-part z))))

  (checking "imag-part" 100 [z gen/symbol]
            (is (= (g/* (g// 1 2)
                        (g/* #sicm/complex "0-1i"
                             (g/- z (g/conjugate z))))
                   (g/imag-part z))))

  (checking "angle" 100 [z gen/symbol]
            (is (= (g/atan
                    (g/imag-part z)
                    (g/real-part z))
                   (g/angle z))))

  (checking "magnitude" 100 [z gen/symbol]
            (is (= (g/sqrt (g/mul (g/conjugate z) z))
                   (g/magnitude z)))))

(deftest symbolic-trig-tests
  (testing "trig shortcuts - sin"
    (is (ish? 0 (g/sin 0))
        "The ::v/number implementation takes over for g/sin and returns a float
        on the JVM.")
    (is (v/eq 0 (g/sin (an/literal-number 0)))
        "the symbolic operator is exact.")

    (testing "sin=0 symbolics"
      (is (v/eq 0 (g/sin 'pi)))
      (is (v/eq 0 (g/sin 'two-pi)))
      (is (v/eq 0 (g/sin '-pi)))
      (is (v/eq 0 (g/sin '-two-pi))))

    (testing "sin=1,-1 symbolics"
      (is (v/eq 1 (g/sin 'pi-over-2)))
      (is (v/eq -1 (g/sin '-pi-over-2))))

    (testing "literal numbers collapse too if they're close to multiples"
      (is (v/eq 0 (g/sin (an/literal-number Math/PI))))
      (is (v/eq 0 (g/sin (an/literal-number (* 2 Math/PI)))))
      (is (v/eq 0 (g/sin (an/literal-number (- Math/PI)))))
      (is (v/eq 1.0 (g/sin (/ Math/PI 2))))))

  (testing "trig shortcuts - cos"
    (is (ish? 1 (g/cos 0))
        "The ::v/number implementation takes over for g/cos and returns a float
        on the JVM.")
    (is (v/eq 1 (g/cos (an/literal-number 0)))
        "the symbolic operator is exact.")

    (testing "cos=0 symbolics"
      (is (v/eq 0 (g/cos 'pi-over-2)))
      (is (v/eq 0 (g/cos '-pi-over-2))))

    (testing "cos=1,-1 symbolics"
      (is (v/eq 1 (g/cos 'two-pi)))
      (is (v/eq 1 (g/cos '-two-pi)))
      (is (v/eq -1 (g/cos 'pi)))
      (is (v/eq -1 (g/cos '-pi))))

    (testing "literal numbers collapse too"
      (is (v/eq -1 (g/cos (an/literal-number Math/PI))))
      (is (v/eq 1 (g/cos (an/literal-number (* 2 Math/PI)))))))

  (testing "trig shortcuts - tan"
    (is (ish? 0 (g/tan 0))
        "The ::v/number implementation takes over for g/tan and returns a
          float on the JVM.")
    (is (v/eq 0 (g/tan (an/literal-number 0)))
        "The symbolic operator is exact.")

    (testing "tan=0 symbolics"
      (is (v/eq 0 (g/tan 'pi)))
      (is (v/eq 0 (g/tan '-pi)))
      (is (v/eq 0 (g/tan '-two-pi)))
      (is (v/eq 0 (g/tan '-two-pi))))

    (testing "tan=1,-1 symbolics"
      (is (v/eq 1 (g/tan 'pi-over-4)))
      (is (v/eq 1 (g/tan '+pi-over-4)))
      (is (v/eq -1 (g/tan '-pi-over-4)))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (g/tan 'pi-over-2))))))
