#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.abstract.number-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [pattern.rule :as rule :refer [=>]]
            [same :refer [ish?]]
            [emmy.abstract.number :as an]
            [emmy.complex :as c]
            [emmy.expression :as x]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.numsymb :as sym]
            #?(:cljs [emmy.ratio :as r])
            [emmy.simplify :as simpl]
            [emmy.value :as v]))

(def gen-literal-element
  (gen/one-of [sg/real sg/complex gen/symbol]))

(def gen-literal
  (gen/one-of
   [(gen/fmap an/literal-number gen-literal-element)
    gen/symbol]))

(deftest value-protocol-tests
  (checking "v/zero? returns true for wrapped zero"
            100 [n (gen/one-of [sg/real (gen/fmap v/zero-like sg/real)])]
            (if (v/zero? n)
              (is (v/zero? (an/literal-number n)))
              (is (not (v/zero? (an/literal-number n))))))

  (checking "v/one? returns true for wrapped zero"
            100 [n (gen/one-of [sg/real (gen/fmap v/one-like sg/real)])]
            (if (v/one? n)
              (is (v/one? (an/literal-number n)))
              (is (not (v/one? (an/literal-number n))))))

  (checking "v/identity? returns true for wrapped zero"
            100 [n (gen/one-of [sg/real (gen/fmap v/identity-like sg/real)])]
            (if (v/identity? n)
              (is (v/identity? (an/literal-number n)))
              (is (not (v/identity? (an/literal-number n))))))

  (checking "v/{zero?,one?,identity?} etc match v/{zero,one,identity}-like" 100 [n gen-literal]
            (is (v/zero? (v/zero-like n)))
            (is (v/one? (v/one-like n)))
            (is (v/identity? (v/identity-like n))))

  (checking "v/numerical? returns true for all literal-number instances" 100 [n gen-literal]
            (is (v/numerical? n)))

  (checking "exact? mirrors input" 100 [n gen-literal-element]
            (if (v/exact? n)
              (is (v/exact? (an/literal-number n)))
              (is (not (v/exact? (an/literal-number n))))))

  (checking "v/freeze" 100 [n gen-literal-element]
            (is (= (v/freeze n)
                   (v/freeze (an/literal-number n)))))

  (checking "v/kind" 100 [n gen-literal-element]
            (is (= ::x/numeric (v/kind (an/literal-number n))))))

(deftest predicate-tests
  (testing "v/="
    (doseq [l ['x (an/literal-number 'x)]
            r ['x (an/literal-number 'x)]]
      (is (v/= l r)))

    (doseq [l [12 (an/literal-number 12)]
            r [12 (an/literal-number 12)]]
      (is (v/= l r))
      #?(:cljs
         (is (= l r)
             "cljs overrides equality, and can compare literals with
                        true numbers on the left side."))))

  (checking "interaction with symbols" 100 [x gen/symbol]
            (is (not (an/literal-number? x))
                "symbols are not literal-numbers")

            (is (an/abstract-number? x)
                "Symbols ARE abstract numbers!"))

  (checking "literal-number? behavior with numbers." 100 [x sg/real]
            (let [n (an/literal-number x)]
              (is (an/literal-number? n)
                  "Any wrapped number returns true to literal-number?")

              (is (not (an/literal-number? x))
                  "numbers are NOT explicit literal-numbers")))

  (checking "`literal-number` is transparent to compare" 100
            [l sg/real, r sg/real]
            (let [compare-bit (v/compare l r)]
              (is (= compare-bit
                     (v/compare (an/literal-number l) r))
                  "literal left, number right")

              (is (= compare-bit
                     (v/compare l (an/literal-number r)))
                  "number left, literal right")

              (is (= compare-bit
                     (v/compare (an/literal-number l) (an/literal-number r)))
                  "literal on both sides")))

  #?(:cljs
     (testing "comparison unit"
       (is (= 0 (an/literal-number 0)))))

  #?(:cljs
     ;; NOTE: JVM Clojure won't allow non-numbers on either side of < and
     ;; friends. Once we implement `v/<` we can duplicate this test for those
     ;; overridden versions, which should piggyback on compare.
     (checking "`literal-number` is transparent to native comparison operators" 100
               ;; NOTE: This is cased to NOT consider rational numbers for now.
               [[l-num r-num] (gen/vector sg/real-without-ratio 2)]
               (let [compare-bit (v/compare l-num r-num)]
                 (doseq [l [l-num (an/literal-number l-num)]
                         r [r-num (an/literal-number r-num)]]
                   (cond (neg? compare-bit)  (is (< l r))
                         (zero? compare-bit) (is (and (<= l r) (= l r) (>= l r)))
                         :else (is (> l r)))))))

  #?(:cljs
     (checking "`literal-number` implements valueOf properly" 100 [n sg/real]
               (let [wrapped (an/literal-number n)]
                 (is (not (v/real? wrapped)))
                 (is (v/real? (.valueOf wrapped)))
                 (if (r/ratio? n)
                   (is (not= n (.valueOf wrapped))
                       "ratios turn into doubles when you call valueOf, so
                           the passthrough valueOf on literal-number kills
                           equality.")
                   (is (= n (.valueOf wrapped))
                       "other real numbers respond the same way."))

                 (is (= (.valueOf n) (.valueOf wrapped))
                     "valueOf on both sides always matches.")))))

(deftest abstract-number-tests
  (checking "literal-number"
            100
            [x sg/native-integral]
            (let [n (an/literal-number x)
                  result (g/+ 1 (g/cos n))]
              (is (= (if (zero? x)
                       2
                       `(~'+ 1 (~'cos ~x)))
                     (v/freeze result))
                  "When literal-number wraps an actual number, it attempts to
        keep the result exact instead of evaluating the fns... UNLESS specific
        values like (cos 0) can be exactly evaluated.")))

  (checking "inexact numbers" 100 [x sg/native-integral]
            (is (=  (+ 1 (Math/cos x))
                    (g/+ 1 (g/cos x)))
                "You get a floating-point inexact result by calling generic fns
    on a number directly, by comparison."))

  (testing "literal-number properly prints wrapped sequences, even if they're lazy."
    (is (= "(* 2 x)"
           (pr-str
            (an/literal-number (g/simplify (g/* 'x 2))))))
    (is (= "(* x 2)"
           (pr-str
            (an/literal-number (lazy-seq ['* 'x 2])))))))

(deftest literal-number-arithmetic-tests
  (letfn [(check [op l r]
            (let [expected (op l r)
                  others   [[(an/literal-number l) r]
                            [l (an/literal-number r)]
                            [(an/literal-number l) (an/literal-number r)]]]
              (doseq [[x y] others]
                (is (v/= expected (op x y))))))]
    (checking "+, -, *, / fall through to number ops"
              100 [x sg/native-integral
                   y sg/native-integral]
              (check g/* x y)
              (check g/+ x y)
              (check g/- x y)
              (when-not (zero? y)
                (check g// x y))))

  (checking "negate" 100 [x (sg/inexact-double)]
            (is (= (an/literal-number (- x))
                   (g/negate (an/literal-number x)))))

  (checking "invert" 100 [x (sg/inexact-double)]
            (is (= (an/literal-number (g// x))
                   (g/invert (an/literal-number x)))))

  (checking "square" 100 [x (sg/inexact-double)]
            (is (ish? (an/literal-number (g/square x))
                      (g/square (an/literal-number x)))))

  (checking "cube" 100 [x (sg/reasonable-double {:min -1e3 :max 1e3})]
            (is (= (an/literal-number (g/cube x))
                   (g/cube (an/literal-number x)))))

  (checking "expt" 100 [x (sg/inexact-double)
                        e (gen/choose 0 3)]
            (is (= (an/literal-number (g/expt x e))
                   (g/expt (an/literal-number x) e))))

  (checking "abs" 100 [x (sg/inexact-double)]
            (is (= (an/literal-number (Math/abs ^double x))
                   (g/abs (an/literal-number x)))))

  (checking "sqrt" 100 [x (sg/inexact-double)]
            (is (= (an/literal-number (g/sqrt x))
                   (g/sqrt (an/literal-number x)))))

  (checking "log" 100 [x (sg/inexact-double)]
            (is (= (an/literal-number (g/log x))
                   (g/log (an/literal-number x))))

            (is (= (an/literal-number (g/log2 x))
                   (-> (g/log2 (an/literal-number x))
                       (x/evaluate {} {'/ g// 'log g/log})))
                "(log 2) in the denom stays exact, so force evaluation.")

            (is (= (an/literal-number
                    (if (neg? x)
                      (g/log10 (c/complex x))
                      (/ (g/log x) (g/log 10))))
                   (-> (g/log10 (an/literal-number x))
                       (x/evaluate {} {'/ g// 'log g/log})))
                "(log 10) in the denom stays exact, so force evaluation."))

  (testing "log2, log10 stay exact"
    (is (v/= '(/ (log x) (log 2))
             (g/simplify (g/log2 'x))))

    (is (v/= '(log x)
             (g/simplify
              (g/* (g/log (an/literal-number 2))
                   (g/log2 'x)))))

    (is (v/= '(/ (log x) (log 10))
             (g/simplify (g/log10 'x))))

    (is (v/= '(log x)
             (g/simplify
              (g/* (g/log (an/literal-number 10))
                   (g/log10 'x))))))

  (checking "exp" 100 [x (sg/inexact-double)]
            (is (= (an/literal-number (Math/exp x))
                   (g/exp (an/literal-number x))))

            (is (= (an/literal-number (g/exp2 x))
                   (g/exp2 (an/literal-number x))))

            (is (= (an/literal-number (g/exp10 x))
                   (g/exp10 (an/literal-number x)))))

  (checking "trace" 100 [z sg/complex]
            (is (= (an/literal-number (g/trace z))
                   (g/trace (an/literal-number z)))))

  (checking "dimension" 100 [z sg/complex]
            (is (v/= 1 (an/literal-number (g/dimension z))))
            (is (v/= 1 (g/dimension (an/literal-number z)))))

  (checking "make-rectangular" 100 [l sg/real r sg/real]
            (is (= (an/literal-number (g/make-rectangular l r))
                   (g/make-rectangular
                    (an/literal-number l)
                    (an/literal-number r))
                   (g/make-rectangular l (an/literal-number r))
                   (g/make-rectangular (an/literal-number l) r))))

  (checking "make-polar" 100 [l sg/real r sg/real]
            (is (= (an/literal-number (g/make-polar l r))
                   (g/make-polar
                    (an/literal-number l)
                    (an/literal-number r))
                   (g/make-polar l (an/literal-number r))
                   (g/make-polar (an/literal-number l) r))))

  (checking "conjugate" 100 [z sg/complex]
            (is (= (an/literal-number (g/conjugate z))
                   (g/conjugate (an/literal-number z)))))

  (checking "real/imag-part" 100 [z sg/complex]
            (is (= (an/literal-number (g/real-part z))
                   (g/real-part (an/literal-number z))))

            (is (= (an/literal-number (g/imag-part z))
                   (g/imag-part (an/literal-number z)))))

  (checking "angle" 100 [z sg/complex]
            (let [z'       (an/literal-number z)
                  result   (g/angle z)
                  expected (cond (and (v/exact? z') (v/exact? result))
                                 (an/literal-number result)

                                 (v/exact? z')
                                 (g/atan (g/imag-part z')
                                         (g/real-part z'))

                                 :else (an/literal-number result))]
              (is (= expected (g/angle z')))))

  (checking "magnitude" 100 [z sg/complex]
            (let [expected (if (v/exact? z)
                             (g/sqrt
                              (an/literal-number
                               (g/* z (g/conjugate z))))
                             (an/literal-number (g/magnitude z)))]
              (is (= expected
                     (g/magnitude (an/literal-number z)))))))

(deftest literal-number-trig-tests
  (checking "inexact literal number trig"
            100
            [x (sg/inexact-double)]
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

  (checking "asin" 100 [x sg/real]
            (is (= (cond (v/zero? x) (v/zero-like x)
                         (v/exact? x)   (list 'asin x)
                         :else          (g/asin x))
                   (x/expression-of
                    (g/asin (an/literal-number x))))))

  (checking "acos" 100 [x sg/real]
            (is (= (cond (v/one? x) (v/zero-like x)
                         (v/exact? x) (list 'acos x)
                         :else        (g/acos x))
                   (x/expression-of
                    (g/acos (an/literal-number x))))))

  (checking "atan, both arities" 100 [x sg/real
                                      y sg/real]
            (is (= (g/atan (an/literal-number x))
                   (g/atan (an/literal-number x) 1)))

            (is (= (cond (v/zero? y) (v/zero-like y)
                         (v/exact? y)   (list 'atan y)
                         :else          (g/atan y))
                   (x/expression-of
                    (g/atan (an/literal-number y))))
                "single arity")

            (let [y-exact? (v/exact? y)
                  x-exact? (v/exact? x)
                  y-zero?  (v/zero? y)
                  x-zero?  (v/zero? x)
                  x-one?   (v/one? x)]
              (is (= (cond (and x-one? y-zero?)            0
                           (and x-one? y-exact?)           (list 'atan y)
                           x-one?                          (g/atan y)
                           (and y-exact? y-zero?)          (if (neg? x) 'pi 0)
                           (and x-exact? x-zero?)          (if (neg? y)
                                                             '(- (/ pi 2))
                                                             '(/ pi 2))
                           (and y-exact? x-exact?)         (list 'atan y x)
                           :else                           (g/atan y x))
                     (x/expression-of
                      (g/atan
                       (an/literal-number y)
                       (an/literal-number x))))
                  "double arity")))

  (checking "cosh" 100 [x sg/real]
            (is (= (cond (v/zero? x) 1
                         (v/exact? x)   (list 'cosh x)
                         :else          (g/cosh x))
                   (x/expression-of
                    (g/cosh (an/literal-number x))))))

  (checking "sinh" 100 [x sg/real]
            (is (= (cond (v/zero? x) 0
                         (v/exact? x)   (list 'sinh x)
                         :else          (g/sinh x))
                   (x/expression-of
                    (g/sinh (an/literal-number x))))))

  (checking "sec" 100 [x sg/real]
            (is (= (cond (v/zero? x) 1
                         (v/exact? x)   (list '/ 1 (list 'cos x))
                         :else          (g/sec x))
                   (x/expression-of
                    (g/sec (an/literal-number x))))))

  (checking "csc" 100 [x sg/real]
            (if (v/zero? x)
              (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                           (g/csc (an/literal-number x))))

              (is (= (if (v/exact? x)
                       (list '/ 1 (list 'sin x))
                       (g/csc x))
                     (x/expression-of
                      (g/csc (an/literal-number x))))))))

(deftest symbolic-arithmetic-tests
  (testing "0-arity cases for symbolic operators"
    (is (false? ((sym/symbolic-operator 'or))))
    (is (true? ((sym/symbolic-operator 'and))))
    (is (= 0 ((sym/symbolic-operator '+))))
    (is (= 0 ((sym/symbolic-operator '-))))
    (is (= 1 ((sym/symbolic-operator '*))))
    (is (= 1 ((sym/symbolic-operator '/))))
    (is (= 0 ((sym/symbolic-operator 'gcd))))
    (is (= 1 ((sym/symbolic-operator 'lcm)))))

  (checking "+ constructor optimizations" 100
            [x gen/symbol]
            (is (v/= x (g/add 0 x)))
            (is (v/= x (g/add x 0))))

  (testing "sums fuse together in the constructor"
    (is (= '(+ x y z) (v/freeze (g/add 'x (g/add 'y 'z)))))
    (is (= '(+ 10 y z) (v/freeze (g/add 10 (g/add 'y 'z)))))
    (is (= '(+ y z 10) (v/freeze (g/add (g/add 'y 'z) 10))))
    (is (= '(+ y z a b)
           (v/freeze (g/add (g/add 'y 'z)
                            (g/add 'a 'b))))))

  (checking "- constructor optimizations" 100
            [x gen/symbol]
            (is (= (g/negate x) (g/sub 0 x)))
            (is (v/= x (g/sub x 0)))
            (is (v/= 0 (g/sub x x))))

  (testing "+/- with symbols"
    (is (= (g/+ 15 'x) (g/+ 10 3 2 'x)))
    (is (= (g/+ 10 'x 3 2) (g/+ 10 'x 3 2)))
    (is (= (g/+ 10 'x 3 2 1) (g/+ 10 'x 3 2 1)))
    (is (= (g/+ 30 'x 3 2 1) (g/+ 10 20 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 5 'x)) (g/- 10 3 2 'x)))
    (is (= (g/- 10 (g/+ 'x 3 2)) (g/- 10 'x 3 2)))
    (is (= (g/- 10 (g/+ 'x 3 2 1)) (g/- 10 'x 3 2 1)))
    (is (= (g/- 10 (g/+ 20 'x 3 2 1)) (g/- 10 20 'x 3 2 1))))

  (testing "products fuse together in the constructor"
    (is (= '(* x y z) (v/freeze (g/mul 'x (g/mul 'y 'z)))))
    (is (= '(* 10 y z) (v/freeze (g/mul 10 (g/mul 'y 'z)))))
    (is (= '(* y z 10) (v/freeze (g/mul (g/mul 'y 'z) 10))))
    (is (= '(* y z a b)
           (v/freeze (g/mul (g/mul 'y 'z)
                            (g/mul 'a 'b))))))

  (checking "* constructor optimizations" 100
            [x gen/symbol]
            (is (v/= 0 (g/mul 0 x)))
            (is (v/= 0 (g/mul x 0)))
            (is (v/= x (g/mul 1 x)))
            (is (v/= x (g/mul x 1))))

  (testing "* with symbols"
    (is (= (g/* 60 'x) (g/* 10 3 2 'x)))
    (is (= (g/* 10 'x 3 2) (g/* 10 'x 3 2)))
    (is (= (g/* 10 'x 3 2 1) (g/* 10 'x 3 2 1)))
    (is (= (g/* 'x 10 'x 3 2 1) (g/* 'x 10 'x 3 2 1)))
    (is (= (g/* 200 'x 3 2) (g/* 10 20 'x 3 2 1))))

  (checking "zero annihilates any symbolic products" 100
            [pre (gen/vector gen/symbol)
             post (gen/vector gen/symbol)]
            (is (zero?
                 (apply (sym/symbolic-operator '*)
                        (concat pre [0] post)))))

  (checking "div constructor optimizations" 100 [x gen/symbol]
            (is (v/= 0 (g/div 0 x)))
            (is (v/= x (g/div x 1)))
            (is (thrown? #?(:clj ArithmeticException :cljs js/Error)
                         (g/div x 0))))

  (checking "modulo constructor optimizations" 100
            [x gen/symbol
             y gen/symbol]
            (is (v/= 0 (g/modulo 0 x)))
            (is (v/= 0 (g/modulo x x)))
            (if (= x y)
              (is (= 0 (v/freeze (g/modulo x y))))
              (is (= (list 'modulo x y)
                     (v/freeze (g/modulo x y)))))
            (is (v/= x (g/modulo x 1))))

  (testing "unary ops with symbols"
    (is (= '(floor x) (v/freeze (g/floor 'x))))
    (is (= '(ceiling x) (v/freeze (g/ceiling 'x))))
    (is (= '(integer-part x) (v/freeze (g/integer-part 'x))))
    (is (= '(fractional-part x) (v/freeze (g/fractional-part 'x)))))

  (checking "gcd, lcm annihilation" 100
            [pre (gen/vector gen/symbol)
             post (gen/vector gen/symbol)]
            (is (v/one?
                 (apply (sym/symbolic-operator 'gcd)
                        (concat pre [1] post))))

            (is (v/zero?
                 (apply (sym/symbolic-operator 'lcm)
                        (concat pre [0] post)))))

  (let [non-one-zero (gen/fmap (fn [n]
                                 (if (or (v/zero? n) (v/one? n))
                                   2
                                   n))
                               sg/any-integral)]
    (checking "symbolic gcd" 100 [sym gen/symbol
                                  n non-one-zero]
              (is (v/= (list 'gcd sym n) (g/gcd sym n)))
              (is (v/= (list 'gcd n sym) (g/gcd n sym)))

              (is (v/= sym (g/gcd sym sym))
                  "gcd(x,x)==x")

              (is (v/= sym (g/gcd (v/zero-like n) sym))
                  "gcd(x,0)==x")

              (is (v/= sym (g/gcd sym (v/zero-like n)))
                  "gcd(0,x)==x")

              (is (v/one? (g/gcd (v/one-like n) sym))
                  "gcd(1,x)==1")

              (is (v/one? (g/gcd sym (v/one-like n)))
                  "gcd(x,1)==1"))

    (checking "symbolic lcm" 100 [sym gen/symbol
                                  n non-one-zero]
              (is (v/= (list 'lcm sym n) (g/lcm sym n)))
              (is (v/= (list 'lcm n sym) (g/lcm n sym)))

              (is (v/= sym (g/lcm sym sym))
                  "lcm(x,x)==x")

              (is (v/zero? (g/lcm (v/zero-like n) sym))
                  "lcm(x,0)==0")

              (is (v/zero? (g/lcm sym (v/zero-like n)))
                  "lcm(0,x)==0")

              (is (v/= sym (g/lcm (v/one-like n) sym))
                  "lcm(1,x)==x")

              (is (v/= sym (g/lcm sym (v/one-like n)))
                  "lcm(x,1)==x")))

  (testing "/ with symbols"
    (is (= (g// 'x (g/* 10 'x 3 2))
           (g// 'x 10 'x 3 2 1)))

    (is (= (g// (g/* 200 'x) (g/* 'x 3 2))
           (g// (g/* 10 20 'x) (g/* 'x 3 2 1)))))

  (testing "negate"
    (is (= (g/+ 'x (g/- 'x)) (g/+ 'x (g/negate 'x))))
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
    (is (v/= 1 (g/expt 'x 0)))
    (is (v/= 'x (g/expt 'x 1))))

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
            (is (v/= (list 'log x)
                     (g/log x)))

            (is (v/= (g// (g/log x)
                          (g/log (an/literal-number 2)))
                     (g/log2 x))
                "log2 divides by the exact (log 2).")
            (is (v/= (g// (g/log x)
                          (g/log (an/literal-number 10)))
                     (g/log10 x))
                "log10 divides by the exact (log 10)."))

  (checking "exp" 100 [x gen/symbol]
            (is (= (list 'exp x)
                   (v/freeze (g/exp x))))
            (is (= (g/expt 2 x) (g/exp2 x)))
            (is (= (g/expt 10 x) (g/exp10 x))))

  (checking "transpose, determinant act as id" 100
            [x (gen/one-of
                [gen/symbol
                 (gen/fmap an/literal-number sg/any-integral)])]
            (is (= x (g/transpose x)))
            (is (= x (g/determinant x))))

  (testing "conjugate"
    (is (= '(conjugate (random x))
           (v/freeze
            (g/conjugate (an/literal-number
                          '(random x))))))
    (doseq [op @#'sym/conjugate-transparent-operators]
      (is (= (v/freeze
              (an/literal-number
               (list op
                     (g/conjugate 'x)
                     (g/conjugate 'y))))
             (v/freeze
              (g/conjugate (an/literal-number
                            (list op 'x 'y)))))
          "This is a little busted, since we don't check for the proper number
           of inputs... but conjugates move inside these operators.")))

  (checking "make-rectangular" 100 [re gen/symbol
                                    im gen/symbol]
            (is (= (g/+ re (g/* c/I im))
                   (g/make-rectangular re im))))

  (checking "make-rectangular with numbers"
            100 [n   sg/real
                 sym gen/symbol]
            (is (v/= (g/+ n (g/* c/I sym))
                     (g/make-rectangular n sym)))
            (is (v/= (g/+ sym (g/* c/I n))
                     (g/make-rectangular sym n))))

  (checking "make-polar" 100 [r     gen/symbol
                              theta gen/symbol]
            (is (= (g/* r (g/+ (g/cos theta)
                               (g/* c/I (g/sin theta))))
                   (g/make-polar r theta))))

  (checking "make-polar with numbers" 100
            [n     sg/real
             sym   gen/symbol]
            ;; numeric radius, symbolic angle:
            (is (v/= (g/* n (g/+ (g/cos sym)
                                 (g/* c/I (g/sin sym))))
                     (g/make-polar n sym))
                "for other cases, the complex number is evaluated.")

            ;; Case of symbolic radius, angle `n`:
            (if (v/exact? n)
              (if (v/zero? n)
                (is (v/= sym (g/make-polar sym n))
                    "an exact zero returns the symbolic radius.")
                (is (= `(~'* ~sym (~'+ (~'cos ~(v/freeze n))
                                   (~'* (~'complex 0.0 1.0)
                                    (~'sin ~(v/freeze n)))))
                       (v/freeze
                        (g/make-polar sym n)))
                    "otherwise, an exact numeric angle stays exact and is
                    treated as a literal number."))

              (is (v/= (g/* sym (g/+ (g/cos n)
                                     (g/* c/I (g/sin n))))
                       (g/make-polar sym n))
                  "for other cases, the complex number is evaluated.")))

  (testing "helpful unit tests from generative testing"
    (let [r 'A, theta 'pi]
      (is (= (g/* r (g/+ (g/cos theta)
                         (g/* c/I (g/sin theta))))
             (g/make-polar r theta))))

    (let [n -1, sym 'pi]
      (is (= (g/* n (g/+ (g/cos sym)
                         (g/* c/I (g/sin sym))))
             (g/make-polar n sym)))))

  (checking "real-part" 100 [z gen/symbol]
            (is (= (g/* (g// 1 2)
                        (g/+ z (g/conjugate z)))
                   (g/real-part z))))

  (checking "imag-part" 100 [z gen/symbol]
            (is (= (g/* (g// 1 2)
                        (g/* (c/complex 0 -1)
                             (g/- z (g/conjugate z))))
                   (g/imag-part z))))

  (checking "angle" 100 [z gen/symbol]
            (is (= (g/atan
                    (g/imag-part z)
                    (g/real-part z))
                   (g/angle z))))

  (checking "magnitude" 100 [z gen/symbol]
            (is (= (g/sqrt (g/mul (g/conjugate z) z))
                   (g/magnitude z))))

  (testing "dot-product"
    (is (= '(+ (* 0.5 x (conjugate y))
               (* 0.5 y (conjugate x)))
           (v/freeze
            (g/simplify
             (g/dot-product 'x 'y)))))

    (is (= (g/dot-product 'x 'y)
           (g/inner-product 'x 'y))
        "identical for abstract complex numbers")))

(deftest symbolic-trig-tests
  (testing "trig shortcuts - sin"
    (is (ish? 0 (g/sin 0))
        "The ::v/number implementation takes over for g/sin and returns a float
        on the JVM.")
    (is (v/= 0 (g/sin (an/literal-number 0)))
        "the symbolic operator is exact.")

    (testing "sin=0 symbolics"
      (is (v/= 0 (g/sin 'pi)))
      (is (v/= 0 (g/sin 'two-pi)))
      (is (v/= 0 (g/sin '-pi)))
      (is (v/= 0 (g/sin '-two-pi))))

    (testing "sin=1,-1 symbolics"
      (is (v/= 1 (g/sin 'pi-over-2)))
      (is (v/= -1 (g/sin '-pi-over-2))))

    (testing "literal numbers collapse too if they're close to multiples"
      (is (v/= 0 (g/sin (an/literal-number Math/PI))))
      (is (v/= 0 (g/sin (an/literal-number (* 2 Math/PI)))))
      (is (v/= 0 (g/sin (an/literal-number (- Math/PI)))))
      (is (v/= 1.0 (g/sin (/ Math/PI 2))))))

  (testing "trig shortcuts - cos"
    (is (ish? 1 (g/cos 0))
        "The ::v/number implementation takes over for g/cos and returns a float
        on the JVM.")
    (is (v/= 1 (g/cos (an/literal-number 0)))
        "the symbolic operator is exact.")

    (testing "cos=0 symbolics"
      (is (v/= 0 (g/cos 'pi-over-2)))
      (is (v/= 0 (g/cos '-pi-over-2))))

    (testing "cos=1,-1 symbolics"
      (is (v/= 1 (g/cos 'two-pi)))
      (is (v/= 1 (g/cos '-two-pi)))
      (is (v/= -1 (g/cos 'pi)))
      (is (v/= -1 (g/cos '-pi))))

    (testing "literal numbers collapse too"
      (is (v/= -1 (g/cos (an/literal-number Math/PI))))
      (is (v/= 1 (g/cos (an/literal-number (* 2 Math/PI)))))))

  (testing "trig shortcuts - tan"
    (is (ish? 0 (g/tan 0))
        "The ::v/number implementation takes over for g/tan and returns a
          float on the JVM.")
    (is (v/= 0 (g/tan (an/literal-number 0)))
        "The symbolic operator is exact.")

    (testing "tan=0 symbolics"
      (is (v/= 0 (g/tan 'pi)))
      (is (v/= 0 (g/tan '-pi)))
      (is (v/= 0 (g/tan '-two-pi)))
      (is (v/= 0 (g/tan '-two-pi))))

    (testing "tan=1,-1 symbolics"
      (is (v/= 1 (g/tan 'pi-over-4)))
      (is (v/= 1 (g/tan '+pi-over-4)))
      (is (v/= -1 (g/tan '-pi-over-4)))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (g/tan 'pi-over-2)))))

  (testing "asin"
    (is (= '(asin x) (v/freeze (g/asin 'x)))))

  (testing "acos"
    (is (= '(acos x) (v/freeze (g/acos 'x)))))

  (testing "atan"
    (is (= '(atan x) (v/freeze (g/atan 'x)))))

  (testing "sinh"
    (is (= '(sinh x) (v/freeze (g/sinh 'x)))))

  (testing "cosh"
    (is (= '(cosh x) (v/freeze (g/cosh 'x)))))

  (testing "tan"
    (is (= '(tan x) (v/freeze (g/tan 'x)))))

  (testing "cot"
    (is (= '(/ (cos x) (sin x))
           (v/freeze (g/cot 'x)))))

  (testing "sec"
    (is (= '(/ 1 (cos x)) (v/freeze (g/sec 'x)))))

  (testing "csc"
    (is (= '(/ 1 (sin x)) (v/freeze (g/csc 'x)))))

  (testing "acot"
    (is (= '(- (/ pi 2) (atan x))
           (v/freeze (g/acot 'x)))))

  (testing "asec"
    (is (= '(atan (sqrt (- (expt x 2) 1)))
           (v/freeze
            (g/asec 'x)))))

  (testing "tanh"
    (is (= '(/ (sinh x) (cosh x))
           (v/freeze (g/tanh 'x)))))

  (testing "coth"
    (is (= '(/ (cosh x) (sinh x))
           (v/freeze (g/coth 'x)))))

  (testing "sech"
    (is (= '(/ 1 (cosh x))
           (v/freeze (g/sech 'x)))))

  (testing "csch"
    (is (= '(/ 1 (sinh x))
           (v/freeze (g/csch 'x)))))

  (testing "acosh"
    (is (= '(* 2 (log
                  (+ (sqrt (/ (+ x 1) 2))
                     (sqrt (/ (- x 1) 2)))))
           (v/freeze (g/acosh 'x)))))

  (testing "asinh"
    (is (= '(log (+ x (sqrt (+ 1 (expt x 2)))))
           (v/freeze (g/asinh 'x)))))

  (testing "atanh"
    (is (= '(/ (- (log (+ 1 x))
                  (log (- 1 x)))
               2)
           (v/freeze (g/atanh 'x))))))

(deftest boolean-tests
  ;; These don't QUITE belong in the namespace for abstract number; TODO move
  ;; these to emmy.abstract.boolean when we make that namespace.
  (let [sym:or (sym/symbolic-operator 'or)
        sym:and (sym/symbolic-operator 'and)
        sym:not (sym/symbolic-operator 'not)
        sym:=   (sym/symbolic-operator '=)]
    (is (= '(= (and (or a b) (not (or c d)))
               (or x z))
           (sym:= (sym:and (sym:or 'a 'b)
                           (sym:not (sym:or 'c 'd)))
                  (sym:or 'x 'z)))
        "all forms work as expected with symbols.")

    (checking "symbolic function annihilators" 100
              [pre (gen/vector gen/symbol)
               post (gen/vector gen/symbol)]
              (is (true? (apply sym:or (concat pre [true] post)))
                  "`or` returns a bare `true` if any `true` appears.")

              (is (false? (apply sym:and (concat pre [false] post)))
                  "`and` returns a bare `true` if any `true` appears."))

    (checking "symbolic matches actual" 100 [l gen/boolean
                                             r gen/boolean]
              (is (= (and l r) (sym:and l r)))
              (is (= (or l r) (sym:or l r)))
              (is (= (not l) (sym:not l))))

    (checking "sym:= matches v/= for numbers"
              100 [l sg/number
                   r sg/number]
              (is (true? (sym:= l))
                  "sym:= with 1 arg is always true")

              (is (= (v/= l r)
                     (sym:= l r)))

              (is (false? (sym:= l 'x))
                  "numbers are never equal to symbols, so false comes back")

              (is (false? (sym:= 'x r))
                  "numbers are never equal to symbols, so false comes back"))

    (testing "sym:="
      (is (true? (sym:=)))

      (is (= '(and (= a b) (= b c))
             (sym:= 'a 'b 'c)))

      (is (= '(and (= a b) (= b c))
             (sym:= 'a 'a 'a 'a 'b 'b 'c))
          "contiguous duplicates get filtered out")

      (is (false?
           (sym:= 'a 'a 1 2 'a 'b 'b 'c))
          "If any contiguous elements are obviously not equal, the computation
          short-circuits. "))

    (checking "symbolic `or` behavior with boolean"
              100 [n gen/any-equatable]
              (is (true? (sym:or n true))
                  "true on right is always true")

              (is (true? (sym:or true n))
                  "true on left is always true")

              (is (= n (sym:or false n))
                  "false on left returns right side")

              (is (= n (sym:or n false))
                  "false on right returns left side"))

    (checking "symbolic `and` behavior with boolean"
              100 [n gen/any-equatable]
              (is (false? (sym:and n false))
                  "false on right is always false")

              (is (false? (sym:and false n))
                  "false on left is always false")

              (is (= n (sym:and true n))
                  "true on left returns right side")

              (is (= n (sym:and n true))
                  "true on right returns left side"))))

(deftest symbolic-derivative-tests
  (let [derivative (sym/symbolic-operator 'derivative)]
    (testing "structural utilities"
      (is (sym/derivative? '(D f)))
      (is (not (sym/derivative? '(e f))))

      (is (not (sym/iterated-derivative?
                '(expt D 2))))

      (is (sym/iterated-derivative?
           '((expt D 2) f)))

      (is (= '((expt D 2) f)
             (derivative '(D f))))

      (is (= '((expt D 3) f)
             (derivative '((expt D 2) f)))))))

(deftest incremental-simplifier-tests
  (testing "incremental simplifier works for unary, binary"
    (binding [sym/*incremental-simplifier* simpl/simplify-expression]
      (is (= 1 (v/freeze
                (g/+ (g/square (g/cos 'x))
                     (g/square (g/sin 'x)))))))

    (let [flip (rule/ruleset*
                (rule/rule (+ ?a ?a) => (* 2 ?a))
                (rule/rule (cos x) => 12))]
      (binding [sym/*incremental-simplifier* flip]
        (is (= '(* 2 (cos theta))
               (v/freeze
                (g/+ (g/cos 'theta) (g/cos 'theta))))
            "The rule applies a single simplification.")

        (is (= 24 (v/freeze
                   (g/+ (g/cos 'x) (g/cos 'x))))
            "rule here maps `(g/cos 'x)` to 12 internally, then `g/+` actually
            performs the addition."))))

  (testing "simplify-numerical-expression"
    (is (v/= '(+ x x x)
             (an/simplify-numerical-expression
              '(+ x x x)))
        "acts as identity for non-Literal instances...")

    (is (v/= '(* 3 x)
             (an/simplify-numerical-expression
              (an/literal-number '(+ x x x))))
        "expressions are simplified.")))
