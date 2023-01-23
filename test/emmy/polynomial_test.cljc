#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.polynomial-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish?] :include-macros true]
            [emmy.abstract.number :as an]
            [emmy.calculus.derivative :refer [D]]
            [emmy.differential :as sd]
            [emmy.expression :as x :refer [variables-in expression-of]]
            [emmy.expression.analyze :as a]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.laws :as sl]
            [emmy.modint :as modular]
            [emmy.polynomial :as p]
            [emmy.polynomial.exponent :as xpt]
            [emmy.polynomial.impl :as pi]
            [emmy.series :as ss]
            [emmy.simplify]
            [emmy.util :as u]
            [emmy.value :as v]))

(deftest impl-tests
  (testing "tests of polynomial backing impl"
    (checking "make-term round trip" 100
              [expts (sg/poly:exponents 10)
               coeff sg/number]
              (let [term  (pi/make-term expts coeff)]
                (is (= expts (pi/exponents term)))
                (is (= coeff (pi/coefficient term)))))

    (testing "term getter defaults"
      (is (= 0 (pi/coefficient [])))
      (is (= xpt/empty (pi/exponents []))))))

(deftest polynomial-tests
  (testing "type definition, protocols"
    (checking "IArity" 100 [p (sg/polynomial)]
              (is (= (f/arity p)
                     [:between 0 (p/arity p)])))

    (testing "IPerturbed"
      (letfn [(f [x] (p/make [1 2 (g/square x) 3]))]
        (is (= (p/make [1 2 (g/square 'x) 3])
               (g/simplify
                (f 'x)))
            "verifying the result...")

        (is (= (p/make [0 0 (g/* 2 'x)])
               ((D f) 'x))
            "polynomial derivatives with respect to some coefficient work!")))

    (checking "perturbed?" 100
              [p (sg/polynomial :coeffs (sg/differential))]
              (is (sd/perturbed? p)
                  "A polynomial with perturbed coefficients is perturbed."))

    (checking "polynomials are polynomial?, v/kind, misc others" 100
              [p (sg/polynomial)]
              (is (p/polynomial? p))
              (is (not (p/coeff? p)))
              (is (not (v/exact? p)))
              (is (= ::p/polynomial (v/kind p))
                  "kind works"))

    (checking "->seq" 100 [p (sg/polynomial)]
              (is (= (p/bare-terms p)
                     (seq p))
                  "calling `seq` returns a sequence of terms"))

    (checking "meta, with-meta"
              100 [p (sg/polynomial)
                   m (gen/map gen/keyword gen/any)]
              (is (nil? (meta p))
                  "nil by default")

              (is (= m (meta (with-meta p m)))
                  "metadata works")

              (is (= m (meta
                        (p/->Polynomial (p/bare-arity p)
                                        (p/bare-terms p)
                                        m)))
                  "three-arity constructor allows metadata"))

    (testing "->str, freeze"
      (is (= "1: (1*{} + 2*{0 1} + 3*{0 2})"
             (str (p/make [1 2 3]))
             (p/->str (p/make [1 2 3]))))

      (is (= "1: (1*{} + 2*{0 1}... and 1 more terms)"
             (p/->str (p/make [1 2 3]) 2))
          "term limitation in printing")

      (is (= '(polynomial 1 [[{} 1] [{0 1} 2] [{0 2} 3]])
             (v/freeze
              (p/make [1 2 3])))
          "freeze representation isn't THAT great yet..."))

    (checking "zero-like" 100 [p (sg/polynomial)]
              (is (v/zero?
                   (v/zero-like p))))

    (testing "one"
      (is (not (v/one? (p/make []))))
      (is (v/one? (p/make [1])))
      (is (v/one? (p/make 2 {[0 0] 1})))
      (is (v/one? (p/make 3 {[0 0 0] 1})))
      (is (not (v/one? (p/make 3 {[0 0 0] 1 [0 0 1] 2}))))
      (is (not (v/one? (p/make [1.1]))))
      (is (v/one? (p/make [1.0])))
      (is (v/one? (p/make [(p/make [1])])))
      (is (not (v/one? (p/make [(p/make [2])])))))

    (checking "one-like" 100 [p (sg/polynomial)]
              (is (v/one?
                   (v/one-like p))))

    (testing "one-like unit tests"
      (is (= (p/constant 1 1)
             (v/one-like (p/make [1 2 3]))))

      (is (= (p/constant 2 1)
             (v/one-like
              (p/make 2 {[1 0] 1
                         [2 1] 3}))))

      (is (= (p/constant 3 1)
             (v/one-like
              (p/make 3 {[1 2 1] 4
                         [0 1 0] 5}))))

      (is (= (p/make 2 {[0 0] 1})
             (v/one-like (p/make 2 [])))
          "If we can't deduce the unit element from the zero polynomial over an
          unknown ring, assume it's 1"))

    (checking "identity-like (only on monomials)" 100
              [p (sg/polynomial :arity 1)]
              (is (v/identity?
                   (v/identity-like p))))

    (testing "identity unit tests"
      (is (v/identity? (p/make [0 1])))
      (is (not (v/identity? (p/make []))))
      (is (not (v/identity? (p/make [0]))))

      (testing "identity? only returns true for monomials."
        (is (v/identity? (p/identity 1)))
        (is (not (v/identity? (p/identity 2 1))))))

    (testing "identity-like unit tests"
      (is (= (p/make [0 1])
             (v/identity-like (p/make [0 0 0 1]))))

      (is (= (p/make [0 1])
             (v/identity-like (p/make [1 2 3]))))

      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (v/identity-like (p/constant 10 1)))
          "identity-like is only supported on monomials."))))

(deftest constructor-accessor-tests
  (testing "constructors"
    (checking "constant" 100 [x sg/number]
              (let [c (p/constant 1 x)]
                (is (p/polynomial? c)
                    "constant is always a polynomial.")

                (is (p/coeff? (p/make [x]))
                    "p/make drops coefficients back down out of poly.")

                (is (= c x)
                    "constant polynomial can equal actual constant on right")

                (is (v/= x c)
                    "scalar on left requires v/=")))

    (testing "identity"
      (is (= {0 1}
             (p/leading-exponents
              (p/identity 1)))
          "no leading exponents in a constant term.")

      (is (= (p/make [0 1])
             (p/identity))
          "bare p/identity returns arity 1, first variable")

      (checking "p/identity constructor" 100
                [arity (gen/fmap inc gen/nat)
                 i (gen/choose 0 (dec arity))]
                (is (= (p/make arity {{0 1} 1})
                       (p/identity arity)
                       (p/identity arity 0))
                    "index defaults to 0.")

                (is (= (p/make arity {{i 1} 1})
                       (p/identity arity i))
                    "specifying an explicit variable index sets that variable to
                    1, all others to 0.")

                (is (= 1 (p/lowest-degree
                          (p/identity arity i))))))

    (checking "p/linear" 50
              [arity (gen/fmap inc gen/nat)
               i (gen/choose 0 (dec arity))
               root gen/nat]
              (let [line (p/linear arity i root)]
                (is (= (g/- (p/identity arity i)
                            root)
                       line)
                    "linear matches explicitly built linear poly")

                (if (zero? root)
                  (is (= 1 (p/lowest-degree line)))
                  (is (= 0 (p/lowest-degree line))))))

    (checking "p/c*xn" 50
              [arity (gen/fmap inc gen/nat)
               c (gen/fmap inc gen/nat)
               n (gen/fmap inc gen/nat)]
              (let [cxn (p/c*xn arity c n)]
                (is (= (g/* c (g/expt (p/identity arity) n))
                       cxn)
                    "c*x^n match when built with constructor or explicitly.")

                (is (= {0 n}
                       (p/leading-exponents
                        (p/c*xn arity c n))))

                (is (= n (p/lowest-degree cxn)))))

    (checking "arity" 100 [x sg/number]
              (is (= 0 (p/arity x))
                  "coeffs always have zero arity"))

    (testing "p/new-variables"
      (is (= [(p/make [0 1])]
             (p/new-variables 1)))

      (is (= [(p/make 3 [[[1 0 0] 1]])
              (p/make 3 [[[0 1 0] 1]])
              (p/make 3 [[[0 0 1] 1]])]
             (p/new-variables 3))))

    (checking "check-same-arity" 50
              [arity (gen/fmap inc gen/nat)
               c     gen/nat
               p     (sg/polynomial :arity arity)]
              (is (= arity
                     (p/check-same-arity c p)
                     (p/check-same-arity p c)
                     (p/check-same-arity p p))
                  "arity always matches the poly arity, no matter the side.")

              (is (= p/coeff-arity
                     (p/check-same-arity c c))
                  "unless both sides are coeffs!")))

  (testing "dense make returns 0 for no entries or a zero first entry"
    (is (v/zero? (p/make [])))
    (is (v/zero? (p/make [0])))
    (is (not (v/zero? (p/make [1])))))

  (checking "dense construction round-trips with univariate->dense" 100
            [prefix (gen/vector gen/nat 1 20)
             n-zeros gen/nat]
            (let [x (conj prefix 1)]
              (is (= x (p/univariate->dense
                        (p/make
                         (concat x (repeat n-zeros 0)))))
                  "trailing zeros aren't round-tripped")))

  (checking "->power-series" 100
            [p (sg/polynomial :arity 1)]
            (let [series (p/->power-series p)]
              (is (ss/power-series? series))

              (is (ss/power-series?
                   (p/->power-series series))
                  "->power-series is idempotent")

              (let [coeffs (p/univariate->dense p)]
                (is (= coeffs
                       (take (count coeffs) series))
                    "series values are correct"))))

  (checking "power-series round trip" 100
            [p (sg/polynomial :arity 1)]
            (let [d (p/degree p)
                  rt (-> (p/->power-series p)
                         (p/from-power-series d))]
              (is (= p rt)
                  "polynomial round trips through power series")))

  (testing "from-power-series unit"
    (is (= '(+ (* (/ 1 24) (expt x 4))
               (* (/ 1 6) (expt x 3))
               (* (/ 1 2) (expt x 2))
               x
               1)
           (v/freeze
            (g/simplify
             ((p/from-power-series ss/exp-series 4) 'x))))))

  (checking "p/make returns zero only if first entry is zero" 100
            [arity gen/nat
             x sg/number]
            (if (v/zero? x)
              (is (v/zero? (p/make [x])))
              (is (= x (p/make [x]))))

            (if (v/zero? x)
              (is (v/zero? (p/constant arity x)))
              (is (v/= x (p/constant arity x)))))

  (checking "terms, lead term" 100
            [arity gen/nat
             x sg/any-integral]
            (let [cx (p/constant arity x)]
              (is (p/polynomial? cx))

              (is (= (p/->terms x)
                     (p/->terms cx))
                  "->terms works on poly, constant itself")

              (is (= xpt/empty
                     (p/leading-exponents x)
                     (p/leading-exponents cx))
                  "no leading exponents in a constant term.")

              (is (= (pi/make-term x)
                     (p/leading-term x)
                     (p/leading-term cx))
                  "leading-terms works identically on poly, constant itself")

              (is (= x
                     (p/leading-coefficient x)
                     (p/trailing-coefficient x)
                     (p/leading-coefficient cx))
                  "leading, trailing act as identity for non-polynomials")

              (is (= (p/trailing-coefficient cx)
                     (p/leading-coefficient cx))
                  "trailing and leading are the same in a constant
                  polynomial")))

  (testing "degree"
    (is (= -1 (p/degree (p/constant 1 0))))
    (is (= -1 (p/degree (p/make []))))
    (is (= -1 (p/degree (p/make [0 0]))))
    (is (= 1 (p/degree (p/make [-1 1]))))
    (is (= 1 (p/degree (p/make [0 1]))))
    (is (= 1 (p/degree (p/make [-1 2 0]))))
    (is (= 2 (p/degree (p/make [-1 0 2]))))

    (is (= p/zero-degree (p/degree 0)))
    (is (= p/zero-degree (p/lowest-degree 0)))
    (is (= 0 (p/degree 10)))
    (is (= 0 (p/lowest-degree 10))))

  (checking "monic?, normalize" 100
            [p (sg/polynomial :arity 1)
             c (gen/fmap inc gen/nat)]
            (is (= (p/normalize p)
                   (p/normalize
                    (p/normalize p)))
                "normalize is idempotent")

            (is (p/monic?
                 (p/normalize
                  (p/scale-l c p)))
                "normalizing a polynomial turns it monic."))

  (testing "monic? only responds true to scalar one."
    (is (p/monic? 1))
    (is (not (p/monic? 2))))

  (checking "scale, scale-l" 100 [p (sg/polynomial)]
            (is (v/zero? (p/scale-l 0 p)))
            (is (v/zero? (p/scale-l p 0)))
            (is (v/zero? (p/scale 0 p)))
            (is (v/zero? (p/scale p 0))))

  (checking "map-exponents works on scalars" 100
            [c  gen/nat
             n (gen/fmap inc gen/nat)]
            (is (= (p/c*xn 1 c n)
                   (p/map-exponents
                    (fn [m] (xpt/assoc m 0 n)) c 1))
                "mapping the power product promotes the constant to a
 polynomial."))

  (checking "univariate->dense with coeff" 100
            [x sg/small-integral]
            (is (= [x] (p/univariate->dense x))))

  (testing "univariate->dense unit"
    (is (= [1] (p/univariate->dense 1 0)))
    (is (= [1 0] (p/univariate->dense 1 1))))

  (checking "univariate->dense with coeff" 100
            [x (sg/polynomial :arity (gen/return 1))
             n gen/nat]
            (is (= (inc (max n (p/degree x)))
                   (count (p/univariate->dense x n)))
                "univariate->dense pads the resulting vector to make it
                represent a polynomial of at least degree n.")

            (is (= (inc n)
                   (count
                    (p/univariate->dense 1 n)))
                "converting a constant to dense of degree n has n entries (1
                extra for the constant)"))

  (checking "reciprocal polynomials" 100
            [p (sg/polynomial :arity (gen/return 1))]
            (let [p+p*   (p/add p (p/reciprocal p))
                  coeffs (p/univariate->dense p+p* (p/degree p))]
              (is (= coeffs (rseq coeffs))
                  "p+p* is palindromic"))

            (let [p-p*   (p/sub p (p/reciprocal p))
                  coeffs (p/univariate->dense p-p* (p/degree p))]
              (is (= (g/negate coeffs) (rseq coeffs))
                  "p-p* is anti-palindromic")))

  (testing "reciprocal unit"
    (let [x      (p/identity)
          p      (g/+ (g/square x) x -1)
          p+p*   (p/add p (p/reciprocal p))
          coeffs (p/univariate->dense p+p* (p/degree p))]
      (is (= coeffs (rseq coeffs))
          "p+p* is palindromic for p(x) = x^2+x-1")))

  (checking "reciprocal of a constant acts as identity" 100
            [x sg/number]
            (is (= x (p/reciprocal x))))

  (testing "reciprocal"
    (is (= (p/make 3 {[3 0 0] 5
                      [2 0 1] 2
                      [1 0 0] 0
                      [0 2 1] 3})
           (p/reciprocal
            (p/make 3 {[0 0 0] 5
                       [1 0 1] 2
                       [2 0 1] 0
                       [3 2 1] 3})))
        "note that, including zeros, the exponents for the first variable are
        flipped."))

  (checking "drop-leading-term" 100
            [xs (gen/vector (gen/fmap inc gen/nat) 2 20)
             c  (gen/fmap inc gen/nat)]
            (is (v/zero? (p/drop-leading-term c))
                "dropping the leading term from a constant returns 0.")

            (is (= (p/make xs)
                   (p/drop-leading-term
                    (p/make (conj xs c))))
                "adding on a nonzero term then DROPPING it gives back the same
                poly as if you'd never added.")))

(deftest special-poly-tests
  (is (= '[0
           1
           x
           (+ (expt x 2) x)
           (+ (expt x 3) (* 3 (expt x 2)) x)
           (+ (expt x 4) (* 6 (expt x 3)) (* 7 (expt x 2)) x)
           (+ (expt x 5) (* 10 (expt x 4)) (* 25 (expt x 3)) (* 15 (expt x 2)) x)]
         (map #(-> (p/touchard %)
                   (p/->expression ['x])
                   (v/freeze))
              (range -1 6)))
      "Touchard matches examples from https://mathworld.wolfram.com/BellPolynomial.html"))

(deftest arithmetic-tests
  (let [coeffs (gen/fmap #(g/modulo % 1000) sg/small-integral)]
    (testing "algebraic laws"
      (sl/ring 50 (sg/polynomial :arity 3 :coeffs coeffs)
               "polynomial is a ring"
               :commutative? true
               :with-one? true)

      (sl/ring 50 (sg/polynomial :arity 1 :coeffs coeffs)
               "polynomial arity 1 is a ring"
               :commutative? true
               :with-one? true)))

  (testing "add constant"
    (is (= (p/make [3 0 2])
           (g/add (p/make [0 0 2])
                  (p/constant 3))))

    (is (= (p/make [0 0 2])
           (g/add (p/make [2 0 2])
                  (p/constant -2)))))

  (checking "dense add, sub, negate" 100
            [[l r] (gen/sized
                    (fn [size]
                      (gen/tuple
                       (gen/vector sg/small-integral size)
                       (gen/vector sg/small-integral size))))]
            (is (= (p/make (map g/+ l r))
                   (g/+ (p/make l)
                        (p/make r))))

            (is (= (p/make (map g/- l r))
                   (g/- (p/make l)
                        (p/make r))))

            (is (= (p/make (map g/negate l))
                   (g/negate (p/make l)))))

  (testing "add/sub unit tests"
    (is (v/zero?
         (g/add (p/make [0 0 2])
                (p/make [0 0 -2]))))

    (is (= (p/make [])
           (g/add (p/make [0 0 2])
                  (p/make [0 0 -2]))))

    (is (= (p/make [3])
           (g/add (p/make [3 0 2])
                  (p/make [0 0 -2]))))

    (is (= (p/make [-1 1])
           (g/add (p/make [0 1])
                  (p/make [-1]))))

    (is (v/zero?
         (g/sub (p/make [0 0 2])
                (p/make [0 0 2]))))

    (is (= (p/make [-3])
           (g/sub (p/make [0 0 2])
                  (p/make [3 0 2]))))

    (is (= (p/make [0 1 2])
           (g/sub (p/make [3 1 2])
                  (p/make [3]))))

    (is (= (p/make [-2 -2 -1])
           (g/sub (p/make [1])
                  (p/make [3 2 1]))))

    (is (= (p/make [0 0 1 0 1 -1])
           (g/sub (p/make [1 0 1 0 1])
                  (p/make [1 0 0 0 0 1]))))

    (is (= (p/make [0 0 -1 0 -1 1])
           (g/sub (p/make [1 0 0 0 0 1])
                  (p/make [1 0 1 0 1]))))

    (is (= (p/make [-1 -2 -3])
           (p/negate (p/make [1 2 3])))))

  (testing "addition with symbols"
    (is (= (p/make [(g/+ 'a 'c) (g/+ 'b 'd) 'c])
           (g/add (p/make '[a b c])
                  (p/make '[c d])))))

  (checking "p+p=2p" 30 [p (sg/polynomial)]
            (is (= (g/add p p)
                   (g/mul p (p/constant (p/bare-arity p) 2)))))

  (checking "pq-div-p=q" 30
            [[p q] (gen/let [arity gen/nat]
                     (gen/tuple (sg/polynomial :arity arity)
                                (sg/polynomial :arity arity
                                               :nonzero? true)))]
            (let [p*q (g/mul p q)
                  [Q R] (p/divide p*q q)]
              (is (p/divisible? p*q q))
              (is (v/zero? R))
              (is (= p Q))))

  (testing "mul"
    (is (= (p/make [])
           (g/mul (p/make [1 2 3])
                  (p/make [0]))))

    (is (= (p/make [])
           (g/mul (p/make [0])
                  (p/make [1 2 3]))))
    (is (= (p/make [])
           (g/mul (p/make [])
                  (p/make [1 2 3]))))

    (is (= (p/make [1 2 3])
           (g/mul (p/make [1 2 3])
                  (p/make [1]))))

    (is (= (p/make [1 2 3])
           (g/mul (p/make [1])
                  (p/make [1 2 3]))))

    (is (= (p/make [3 6 9])
           (g/mul (p/make [1 2 3])
                  (p/make [3]))))

    (is (= (p/make [0 1 2 3])
           (g/mul (p/make [0 1])
                  (p/make [1 2 3]))))

    (is (= (p/make [0 -1 -2 -3])
           (g/mul (p/make [0 -1])
                  (p/make [1 2 3]))))

    (is (= (p/make [-1 0 1])
           (g/mul (p/make [1 1])
                  (p/make [-1 1]))))

    (is (= (p/make [1 3 3 1])
           (g/mul (p/make [1 1])
                  (g/mul (p/make [1 1])
                         (p/make [1 1])))))

    (is (= (p/make [1 -4 6 -4 1])
           (g/mul (g/mul (p/make [-1 1])
                         (p/make [-1 1]))
                  (g/mul (p/make [-1 1])
                         (p/make [-1 1]))))))

  (testing "expt"
    (let [x+1 (p/make [1 1])]
      (is (= (p/make [1])
             (g/expt x+1 0)))

      (is (= x+1 (g/expt x+1 1)))

      (is (= (p/make [1 2 1])
             (g/expt x+1 2)))

      (is (= (p/make [1 3 3 1])
             (g/expt x+1 3)))

      (is (= (p/make [1 4 6 4 1])
             (g/expt x+1 4)))

      (is (= (p/make [1 5 10 10 5 1])
             (g/expt x+1 5)))))

  (testing "div, psuedo-remainder"
    (is (= [(p/make [1 1])
            (p/make [])]
           (p/divide (p/make [-1 0 1])
                     (p/make [-1 1]))))

    (is (= [(p/make [-10 1])
            (p/make [-32 -21])]
           (p/divide (p/make [-42 0 -12 1])
                     (p/make [1 -2 1]))))

    (is (= [(p/make [3 1 1])
            (p/make [5])]
           (p/divide (p/make [-4 0 -2 1])
                     (p/make [-3 1]))))

    (is (= [(p/make [-5 0 3])
            (p/make [60 -27 -11])]
           (p/divide (p/make [-45 18 72 -27 -27 0 9])
                     (p/make [21 -9 -4 0 3]))))

    (let [U (p/make [-5 2 8 -3 -3 0 1 0 1])
          V (p/make [21 -9 -4 0 5 0 3])
          [pr d] (p/pseudo-remainder U V)]
      #?(:clj (is (= [(p/make [#sicm/ratio -2/9
                               0
                               #sicm/ratio 1/3])
                      (p/make [#sicm/ratio -1/3
                               0
                               #sicm/ratio 1/9
                               0
                               #sicm/ratio -5/9])]
                     (p/divide U V))))

      (is (= [(p/make [-3 0 1 0 -5]) 2]
             [pr d]))

      (is (zero?
           (g/- (g/* (p/make [(g/expt 3 d)])
                     U)
                (g/+ (g/* (p/make [-2 0 3]) V)
                     pr)))))

    (testing "examples from http://www.mathworks.com/help/symbolic/mupad_ref/pdivide.html"
      (let [p (p/make [1 1 0 1])
            q (p/make [1 1 3])]
        (is (= [(p/make [10 7]) 2]
               (p/pseudo-remainder p q))))

      (let [p (p/make [3 0 4])
            q (p/make [2 2])]
        (is (= [(p/make [28]) 2]
               (p/pseudo-remainder p q))))

      (is (= [(p/make 2 []) (p/make 2 [[[2 1] 1] [[1 2] 1]])]
             (p/divide (p/make 2 [[[2 1] 1] [[1 2] 1]])
                       (p/make 2 [[[1 2] 1]]))))

      (is (= [1 0] (p/divide (p/make [3])
                             (p/make [3]))))

      (is (= [0 1] (p/pseudo-remainder
                    (p/constant 7)
                    (p/constant 2)))))))

(deftest poly-core
  (testing "other coefficient rings: GF(11)"
    (sl/ring 50 (sg/polynomial
                 :arity 1
                 :coeffs (gen/fmap #(modular/make % 11)
                                   gen/small-integer))
             "polynomial is a ring"
             :commutative? true
             :with-one? true))

  (testing "other coefficient rings, unit: GF(2), unit"
    (let [mod2 #(modular/make % 2)
          x0 (mod2 0)
          x1 (mod2 1)
          P (p/make [x1 x0 x1])]
      (is (= (p/make [x1 x0 x0 x0 x1])
             (g/expt P 2)))

      (is (= (p/make [x1 x0 x1 x0 x1 x0 x1])
             (g/expt P 3)))

      (is (= (p/make [x1 x0 x0 x0 x0 x0 x0 x0 x1])
             (g/mul (g/expt P 3) P)))

      (is (= (p/make [])
             (g/sub P P)))
      (is (= (p/make [])
             (g/add P P)))

      (is (= (p/make [x0 x0 x1])
             (g/add P (p/make [1]))))))

  (testing "CRC polynomials"
    ;; https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
    ;; http://www.lammertbies.nl/comm/info/crc-calculation.html
    (let [mod2 #(modular/make % 2)
          o (mod2 0)
          i (mod2 1)
          x8 (p/make [o o o o o o o o i])
          CRC-8-ATM (p/make [i i i o o o o o i])
          M (p/make [i i i 0 i o i])
          Mx8 (g/mul x8 M)
          [_ r1] (p/divide Mx8 CRC-8-ATM)
          CRC-16-CCITT (p/make [i o o o o i o o o o o o i o o o i])
          x16 (g/mul x8 x8)
          T (p/make [o o i o i o i])
          Tx16 (g/mul x16 T)
          [_ r2] (p/divide Tx16 CRC-16-CCITT)]
      (is (= (p/make [o i o o o i o i])
             r1))

      (is (= (p/make [i o o o i i i o o i o i i])
             r2)))))

(defn ->poly [x]
  (a/expression-> p/analyzer x (fn [p _] p)))

(deftest poly-evaluate
  (testing "arity 1"
    (let [p (->poly '(+ 2 (* x 3)))]
      (is (= 14 (p/evaluate p [4])))
      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (p/evaluate p [3 2]))
          "Too many arguments supplied."))

    (is (= 256 (-> (->poly '(expt x 8))
                   (p/evaluate [2]))))

    (is (= 272 (-> (->poly '(+ (expt x 4) (expt x 8)))
                   (p/evaluate [2])))))

  (testing "arity 2"
    (let [p (->poly '(expt (+ x y) 2))]
      (is (= 25 (p/evaluate p [2 3])))))

  (testing "arity 3"
    (let [p (->poly '(+ (expt x 3) (expt y 2) z 1))]
      (is (= 19 (p/evaluate p [2 3 1])))))

  (testing "arity 4"
    (let [p (->poly '(expt (- w x y z) 2))]
      (is (= 36 (p/evaluate p [10 1 2 1])))))

  (testing "arity 5"
    (let [p (->poly '(expt (- v w x y z) 2))]
      (is (= 16 (p/evaluate p [10 1 2 1 2])))))

  (testing "arity 10"
    (let [p (->poly '(expt (- x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) 3))]
      (is (= 216 (p/evaluate p [10 1 2 1 2 -3 1 -2 -1 3])))))

  (let [arity 20]
    (checking "evaluate matches ->expression" 10
              [p  (sg/polynomial :arity arity)
               xs (gen/vector sg/symbol arity)]
              (is (every?
                   v/zero?
                   (for [idx (range (inc arity))]
                     (let [sub-xs (subvec xs 0 idx)
                           padded (into sub-xs (repeat (- arity idx) 0))]
                       (g/simplify
                        (g/- (apply p padded)
                             (an/literal-number
                              (p/->expression p padded)))))))
                  "For every subsequence up to and including the full sequence
                      of args, [[p/->expression]] matches the final result (but
                      not necessarily the same result!) as calling `apply`.")))

  (testing "horner-with-error example"
    (let [p (p/make [1 4 3 2 5])
          x  3.2
          [px p'x p''x err] (p/horner-with-error p x)]
      (is (ish? px (p x)))
      (is (ish? p'x ((D p) x)))
      (is (ish? p''x (((g/square D) p) x)))
      (is (< err 1e-11))))

  (testing "constant polynomial evaluation"
    (let [p1 (p/make [3])
          p2 (p/make 2 [[[0 0] 5]])
          p3 (p/make 3 [[[1 0 0] 1]])
          p4 (p/make 3 [[[0 1 0] 1]])
          p5 (p/make 3 [[[0 0 1] 1]])]
      (is (= 3 (p/evaluate p1 [99])))
      (is (= 5 (p/evaluate p2 [99 98])))
      (is (= 7 (p/evaluate p3 [7 8 9])))
      (is (= 8 (p/evaluate p4 [7 8 9])))
      (is (= 9 (p/evaluate p5 [7 8 9])))))

  (testing "partial application"
    (let [P (->poly '(+ 1 (* 2 x) (* 3 x y) (* 4 x y z)))]
      (is (= (->poly '(+ 3 (* 3 y) (* 4 y z))) (p/evaluate P [1])))
      (is (= (->poly '(+ 9 (* 8 z))) (p/evaluate P [1 2])))
      (is (= 33 (p/evaluate P [1 2 3])))
      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (p/evaluate P [1 2 3 4]))
          "Too many arguments supplied.")))

  (let [pos (gen/fmap inc gen/nat)]
    (checking "arg-scale, shift" 30
              [term-count (gen/choose 2 10)
               factor pos
               p (gen/fmap p/make (gen/vector pos term-count))]
              (is (v/zero?
                   (g/simplify
                    (g/- (p (g/* 'x factor))
                         ((p/arg-scale p [factor]) 'x))))
                  "arg-scale")

              (is (v/zero?
                   (g/simplify
                    (g/- (p (g/+ 'x factor))
                         ((p/arg-shift p [factor]) 'x))))
                  "arg-scale"))))

(deftest extend-contract-tests
  (checking "extend, contract for coeffs" 100
            [x sg/number]
            (is (false?
                 (p/contractible? x 0))
                "contractible is false for all coeffs")

            (is (= x (p/extend x 10))
                "extend is identity for coeffs"))

  (testing "contract, extend unit tests"
    (is (= (p/make 1 {[1] 2 [2] 3})
           (-> (p/make 2 {[0 1] 2 [0 2] 3})
               (p/contract 0))))

    (is (= (-> (p/make 1 {[1] 2 [2] 3})
               (p/extend 0))
           (p/make 2 {[0 1] 2 [0 2] 3})))

    (is (= (-> (p/make 1 {[1] 2 [2] 3})
               (p/extend 12))
           (p/make 13 {[1] 2 [2] 3}))))

  (checking "extend, contract are inverses" 50
            [p (sg/polynomial)]
            (is  (= p (-> (p/extend p 0)
                          (p/contract 0)))
                 "extending creates an empty index, and contracting removes
 it.")) )

(deftest poly-partial-derivatives
  (testing "partial-derivative with constants"
    (is (= [] (p/partial-derivatives 10)))
    (is (= 0 (p/partial-derivative 10 0))))

  (let [V (p/make [1 2 3 4])
        U (p/make 2 [[[1 1] 3] [[2 2] 4] [[0 0] 5] [[0 3] 7] [[4 0] -2]])]
    (testing "univariate and multivariate polynomials work with D operator"
      (is (= '(+ (* 12 (expt x 2)) (* 6 x) 2)
             (v/freeze
              (g/simplify
               ((D V) 'x)))))

      (is (= '(down (+ (* -8 (expt x 3)) (* 8 x (expt y 2)) (* 3 y))
                    (+ (* 8 (expt x 2) y) (* 21 (expt y 2)) (* 3 x)))
             (v/freeze
              (g/simplify
               ((D U) 'x 'y))))))

    (is (= (p/make [2 6 12]) (p/partial-derivative V 0)))
    (is (= [(p/make [2 6 12])] (p/partial-derivatives V)))
    (is (= (p/make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]]) (p/partial-derivative U 0)))
    (is (= (p/make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]]) (p/partial-derivative U 1)))
    (is (= [(p/make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]])
            (p/make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]])]
           (p/partial-derivatives U)))))

(deftest poly-as-simplifier
  (testing "expr"
    (let [exp1 (expression-of (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (expression-of (g/expt (g/+ 1 'y) 5))
          exp3 (expression-of (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{* + x} (variables-in exp1)))
      (is (= [(p/make [-3 -2 1]) '(x)]
             (a/expression-> p/analyzer exp1 receive)))

      (is (= [(p/make [-3 -2 1]) '(x)]
             (a/expression-> p/analyzer exp1 receive)))

      (is (= [(p/make [1 5 10 10 5 1]) '(y)]
             (a/expression-> p/analyzer exp2 receive)))

      (is (= [(p/make [0 -11 5 -30 10 -7 1]) '(y)]
             (a/expression-> p/analyzer exp3 receive)))))

  (testing "monomial order"
    (let [poly-simp #(a/expression->
                      p/analyzer
                      (expression-of %)
                      (fn [p vars]
                        (a/->expression p/analyzer p vars)))]
      (is (= '(+ (expt x 2) x 1)
             (poly-simp (g/+ 'x (g/expt 'x 2) 1))))

      (is (= '(+ (expt x 4) (* 4 (expt x 3)) (* 6 (expt x 2)) (* 4 x) 1)
             (poly-simp (g/expt (g/+ 1 'x) 4))))

      (is (= '(+ (expt x 4)
                 (* 4 (expt x 3) y)
                 (* 6 (expt x 2) (expt y 2))
                 (* 4 x (expt y 3))
                 (expt y 4))
             (poly-simp (g/expt (g/+ 'x 'y) 4))))

      (is (= '(+ (expt x 4)
                 (* 4 (expt x 3) y)
                 (* 6 (expt x 2) (expt y 2))
                 (* 4 x (expt y 3))
                 (expt y 4))
             (poly-simp (g/expt (g/+ 'y 'x) 4))))))

  (testing "expr-simplify"
    (let [poly-simp #(a/expression->
                      p/analyzer
                      %
                      (fn [p vars]
                        (a/->expression p/analyzer p vars)))
          exp1 (expression-of (g/+ (g/* 'x 'x 'x)
                                   (g/* 'x 'x)
                                   (g/* 'x 'x)))
          exp2 (expression-of (g/+ (g/* 'y 'y)
                                   (g/* 'x 'x 'x)
                                   (g/* 'x 'x)
                                   (g/* 'x 'x)
                                   (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2)))
             (poly-simp exp1)))

      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2)))
             (poly-simp exp2)))

      (is (= 'y (poly-simp exp3)))

      (is (= '(+ g1 g2)
             (poly-simp (expression-of (g/+ 'g1 'g2)))))
      (is (= '(* 2 g1)
             (poly-simp (expression-of (g/+ 'g1 'g1)))))

      (is (= 3 (poly-simp '(+ 2 1))))

      (is (= '(+ b (* -1 f))
             (poly-simp '(- (+ a b c) (+ a c f)))))

      (is (= '(+ (* -1 b) f)
             (poly-simp '(- (+ a c f) (+ c b a))))))))

(deftest lower-raise-tests
  (is (= (p/make 1 {[1] (p/constant 2 2)
                    [2] (p/constant 2 2)})
         (p/lower-arity
          (p/make 3 {[1 0 0] 2 [2 0 0] 2}))))

  (testing "lower, raise are inverse"
    (let [->poly (fn [x]
                   (let [[p _] (p/expression-> x)]
                     p))
          f2 (->poly
              '(+ (expt x2 2)
                  (* 2 (expt x1 2) x2)
                  (expt x1 2)
                  1))
          d2 (->poly
              '(+ (* 2 (expt x1 2) (expt x2 2))
                  (* x1 x2)
                  (* 2 x1)))]
      (is (= (p/make [0
                      (p/make [2 1])
                      (p/make [0 0 2])])
             (p/lower-arity d2)))

      (is (= (p/make [0
                      (p/make [2 1 2 1])
                      (p/make [0 0 2 0 2])
                      (p/make [2 5 2])
                      (p/make [0 0 2 4])])
             (p/lower-arity
              (g/* d2 f2))))

      (is (= (p/make [0
                      0
                      (p/make [4 4 5 4 1])
                      (p/make [0 0 8 4 8 4])
                      (p/make [4 12 9 2 4 0 4])
                      (p/make [0 0 8 20 8])
                      (p/make [0 0 0 0 4 8])])
             (g/* (p/lower-arity d2)
                  (p/lower-arity (g/* d2 f2)))))))

  (checking "lower-arity and raise-arity are inverse" 30
            [p (gen/let [arity (gen/choose 2 10)]
                 (sg/polynomial :arity arity
                                :nonzero? true))]
            (is (= p (-> (p/lower-arity p)
                         (p/raise-arity (p/arity p))))))

  (checking "raising a constant to an explicit arity always gives a polynomial" 100
            [x sg/any-integral
             arity (gen/choose 2 10)]
            (let [raised (p/raise-arity x arity)]
              (is (p/polynomial? raised))

              (is (= (p/constant arity x)
                     raised)))))

(deftest evaluation-homomorphism-tests
  (checking "evaluation-homomorphism" 20
            [[p q xs] (gen/let [arity (gen/choose 1 6)]
                        (gen/tuple
                         (sg/polynomial :arity arity)
                         (sg/polynomial :arity arity)
                         (gen/vector sg/bigint arity)))]
            (is (= (u/bigint
                    (g/mul (p/evaluate p xs)
                           (p/evaluate q xs)))
                   (u/bigint
                    (p/evaluate (g/mul p q) xs)))))

  (testing "specific test cases from generative tests"
    (let [p (p/make 4 [[[0 0 0 0] -2] [[1 6 3 3] 3]])
          q (p/make 4 [[[0 0 0 3] 3] [[4 0 6 2] 1]])
          xs [-2 3 -3 -3]]
      (is (= (g/mul (p/evaluate p xs)
                    (p/evaluate q xs))
             (p/evaluate (g/mul p q) xs))))

    (let [p (p/make 5 [])
          q (p/make 5 [[[0 5 4 0 1] -2]
                       [[2 0 4 7 3] 4]
                       [[1 6 0 1 9] -9]
                       [[4 4 3 2 4] -5]
                       [[6 1 8 1 5] -3]
                       [[7 3 8 2 2] 9]
                       [[3 5 3 6 9] 2]
                       [[3 8 4 6 9] -8]
                       [[1 8 7 9 8] -2]])
          xs (map u/bigint [-9 7 0 9 -9])]
      (is (= (g/mul (p/evaluate p xs)
                    (p/evaluate q xs))
             (p/evaluate (g/mul p q) xs))))))

(deftest from-points-tests
  (is (zero? (p/from-points []))
      "no points returns a 0")

  (checking "single point polynomial is a constant" 100
            [x  sg/any-integral
             fx sg/any-integral]
            (is (= fx (p/from-points [[x fx]]))))

  (let [poly (p/from-points [[1 1] [2 4]])]
    (is (p/polynomial? poly))
    (is (= (g/- (g/* 3 (p/identity)) 2)
           poly)
        "matching polynomial == 3x-2")
    (is (= 1 (poly 1)))
    (is (= 4 (poly 2))))

  (let [poly (p/from-points [[1 1] [2 4] [3 9]])]
    (is (= (g/square (p/identity))
           poly)
        "matching polynomial == x^2")
    (is (= 1 (poly 1)))
    (is (= 4 (poly 2)))
    (is (= 9 (poly 3)))
    (is (= 16 (poly 4))
        "just for fun..."))

  (testing "symbolic from-points"
    (let [poly (p/from-points '[[x1 y1] [x2 y2] [x3 y3]])]
      (is (v/= 'y1 (g/simplify (poly 'x1))))
      (is (v/= 'y2 (g/simplify (poly 'x2))))
      (is (v/= 'y3 (g/simplify (poly 'x3)))))))

(deftest analyzer-tests
  (testing "expression-> unwraps internal literals"
    (is (every?
         (complement x/literal?)
         (tree-seq coll? seq
                   (-> (p/from-points '[[x y] [x2 y2]])
                       (p/->expression ['z]))))
        "EVEN if the original rf has symbolic coefficients, these are unwrapped
         in the process of generating the bare expression."))

  (let [new-analyzer (fn [] (a/make-analyzer
                            p/analyzer
                            (a/monotonic-symbol-generator "k%08d")))
        A #((a/default-simplifier
             (new-analyzer)) %)]
    (is (= '(+ x 1) (A '(+ 1 x))))
    (is (= '(+ x 1) (A '[+ 1 x])))
    (is (= '(* y (sin y) (cos (+ (expt (sin y) 4) (* 2 (sin y)) 1)))
           (A '(* y (sin y) (cos (+ 1 (sin y) (sin y) (expt (sin y) 4)))))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt cos 2) (expt sin 2)))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt sin 2) (expt cos 2)))))

    (is (= '(+ (up (+ (/ d a) (/ (* -1 b) a))) (* -1 (up (/ (* -1 y) (+ a b)))))
           (A '(- (up (+ (/ d a)
                         (/ (- b) a)))
                  (up (/ (- y) (+ a b))))))
        "This test exploded without the `doall` that forces add-symbol! calls in
        analyze.clj.")

    (is (= 'x (A '(* (/ 1 2) (+ x x)))))
    (is (= '(+ (* -1 m (expt ((D phi) t) 2) (r t)) (* m (((expt D 2) r) t)) ((D U) (r t)))
           (A '(- (* (/ 1 2) m (+ (((expt D 2) r) t) (((expt D 2) r) t)))
                  (+ (* (/ 1 2) m (+ (* ((D phi) t) ((D phi) t) (r t))
                                     (* ((D phi) t) ((D phi) t) (r t))))
                     (* -1 ((D U) (r t))))))))))
