#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.simplify.rules-test
  (:require [clojure.test :refer [is deftest testing]]
            [pattern.rule :as pr :refer [rule-simplifier template]]
            [emmy.complex :as c]
            [emmy.generic :as g]
            [emmy.numbers]
            [emmy.ratio]
            [emmy.simplify :as s]
            [emmy.simplify.rules :as r]
            [emmy.value :as v]))

(deftest algebraic-tests
  (testing "unary elimination"
    (let [rule (r/unary-elimination '+ '*)
          f    (rule-simplifier rule)]
      (is (= '(+ x y z a)
             (f '(+ x y (* z) (+ a)))))))

  (testing "associative"
    (let [rule (r/associative '+ '*)
          f    (rule-simplifier rule)]
      (is (= '(+ x y z a (* b c d) cake face)
             (f '(+ x (+ y (+ z a) (* b (* c d))
                         (+ cake face))))))))

  (testing "constant elimination"
    (let [rule (r/constant-elimination '* 1)
          f    (rule-simplifier rule)]
      (is (= '(* x)
             (f '(* x))
             (f '(* 1 1 1 x 1))
             (f '(* 1 x 1 1 1 1))))))

  (testing "constant promotion"
    (let [rule (r/constant-promotion '* 0)
          f    (rule-simplifier rule)]
      (is (= 0
             (f 0)
             (f '(* x 0))
             (f '(* 0 x))))))

  (testing "commutative"
    (let [rule (r/commutative '+ '*)
          f    (rule-simplifier rule)]
      (is (= '(* 2 3 a b c (+ a b c))
             (f '(* c a b (+ c a b) 3 2)))
          "sort by numbers, symbols, expressions ")

      (let [expr '(* c a b (+ c a b))]
        (is (= (f (f expr))
               (f expr))
            "commutative rule is idempotent"))))

  (testing "idempotent"
    (let [rule (r/idempotent 'and 'or)
          f    (rule-simplifier rule)]
      (is (= '(and a b (or b c d) c d)
             (f '(and a b b
                      (or b c c c d d d d)
                      (or b c c c d d d d)
                      c c c d)))
          "duplicates are removed from arg lists."))))

(deftest exponent-contract-tests
  (let [contract (rule-simplifier
                  r/exponent-contract)]
    (is (= '(expt (expt x 2) 7)
           (r/exponent-contract
            '(expt (expt (expt x 2) 3) 4)))
        "by default, applies a single step")

    (is (= '(expt x 9)
           (contract
            '(expt (expt (expt x 2) 3) 4)))
        "nested exponents")

    (is (= '(* (expt x 3))
           (contract
            '(* x x x)))
        "non-exponent groups")

    (is (= '(* y (expt x 7))
           (contract
            '(* y x (expt x 2) x (expt x 2) x)))
        "expts with singletons mixed in")))

(deftest logexp-tests
  (let [rule (fn [] (r/logexp s/*rf-simplify*))]
    (is (= '(expt x 3)
           ((rule) '(exp (* 3 (log x)))))
        "(log x) in the power cancels out the e base.")

    (is (= 'x ((rule) '(exp (log x))))
        "These always get simplified.")

    (binding [r/*log-exp-simplify?* true]
      (is (= '(+ x y)
             ((rule) '(log (exp (+ x y)))))
          "when simplification is on, we can cancel log/exp forms."))

    (binding [r/*log-exp-simplify?* false]
      (is (= '(log (exp (+ x y)))
             ((rule) '(log (exp (+ x y)))))
          "else, acts as identity."))

    (binding [r/*sqrt-expt-simplify?* true]
      (is (= '(exp (/ (+ x y) 2))
             ((rule) '(sqrt (exp (+ x y)))))
          "when sqrt/expt simplification is on, we assume that the value under
          the root is positive, and push the sqrt inside of exp."))

    (binding [r/*sqrt-expt-simplify?* false]
      (is (= '(sqrt (exp (+ x y)))
             ((rule) '(sqrt (exp (+ x y)))))
          "else, acts as identity."))

    (is (= '(* (/ 1 2) (log x))
           ((rule) '(log (sqrt x))))
        "Drop the internal sqrt down as a 1/2 exponent."))

  (testing "exp-contract"
    (is (= '(exp (* 2 x))
           (r/exp-contract '(expt (exp x) 2)))
        "Test for bugfix from issue #439 ")))

(deftest magnitude-tests
  (is (= '(expt x 10)
         (r/magnitude '(magnitude (expt x 10))))
      "even powers")

  (is (= '(* (magnitude x) (expt x 10))
         (r/magnitude '(magnitude (expt x 11))))
      "odd powers")

  (is (= '(magnitude x)
         (r/magnitude '(magnitude (expt x 1))))
      "power == 1")

  (is (= '(* (magnitude x) (expt x -4))
         (r/magnitude '(magnitude (expt x -3))))
      "mag of negative exponent")

  (is (= '(* 1 2 (magnitude y)
             (* (magnitude x) (expt x 10)))
         (r/magnitude
          '(magnitude (* 1 -2 y (expt x 11)))))
      "real numbers and integers get their magnitudes applied, odd exponents
      pulled apart."))

(deftest miscsimp-tests
  (let [s (r/miscsimp s/*rf-simplify*)]
    (testing "expt of `i` stays complex"
      (is (= -1 (s (list 'expt c/I 2))))
      (is (= (g/- c/I) (s (list 'expt c/I 3))))
      (is (= 1 (s (list 'expt c/I 4))))
      (is (= c/I (s (list 'expt c/I 5)))))))

(deftest simplify-square-roots-test
  (let [s (r/simplify-square-roots  s/*rf-simplify*)]
    (testing "even powers"
      (is (= '(expt x 4)
             (s '(expt (sqrt x) 8)))
          "sqrt inside of expt")

      (is (= '(expt x 4)
             (s '(sqrt (expt x 8))))
          "expt inside of sqrt"))

    (testing "odd powers"
      (is (= '(* (sqrt x) (expt x 3))
             (s '(expt (sqrt x) 7)))
          "sqrt inside of expt")

      (is (= '(* (sqrt x) (expt x 3))
             (s '(sqrt (expt x 7))))
          "expt inside of sqrt"))

    (testing "simplify across division boundary"
      (testing "no products, straight division"
        (is (= '(sqrt x) (s '(/ x (sqrt x)))))
        (is (= '(/ 1 (sqrt x)) (s '(/ (sqrt x) x)))))

      (testing "product on top only"
        (is (= '(* 2 (sqrt x) 3)
               (s '(/ (* 2 x 3) (sqrt x)))))
        (is (= '(/ (* 2 3) (sqrt x))
               (s '(/ (* 2 (sqrt x) 3) x)))))

      (testing "product on bottom only"
        (is (= '(/ 1 (* 2 (sqrt x) 3))
               (s '(/ (sqrt x) (* 2 x 3)))))
        (is (= '(/ (sqrt x) (* 2 3))
               (s '(/ x (* 2 (sqrt x) 3))))))

      (testing "product in num, denom"
        (is (= '(/ (* 2 (sqrt x) 3)
                   (* y z))
               (s '(/ (* 2 x 3)
                      (* y z (sqrt x)))))
            "sqrt on bottom")

        (is (= '(/ (* 2 3)
                   (* y z (sqrt x)))
               (s '(/ (* 2 (sqrt x) 3)
                      (* y z x))))
            "sqrt on top")))))

(deftest sqrt-expand-contract-test
  (testing "sqrt-expand works with division"
    (let [expand (r/sqrt-expand  s/*rf-simplify*)]
      (is (= '(+ (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))
             (expand '(+ (sqrt (/ a b)) (sqrt (/ c b))))))
      (is (= '(- (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))
             (expand '(- (sqrt (/ a b)) (sqrt (/ c b))))))))

  (let [sqrt-contract (r/sqrt-contract  s/*rf-simplify*)]
    (testing "cancels square roots if the values are equal"
      (is (= '(* a (sqrt (* b d)) c e)
             (sqrt-contract
              '(* a (sqrt b) c (sqrt d) e)))
          "square roots get pushed to the end.")

      (is (= '(* a b c e)
             (sqrt-contract
              '(* a (sqrt b) c (sqrt b) e)))))

    (testing "sqrt-contract undoes expansion over division"
      (is (= '(+ (sqrt (/ a b)) (sqrt (/ c b)))
             (sqrt-contract
              '(+ (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b))))))

      (is (= '(- (sqrt (/ a b)) (sqrt (/ c b)))
             (sqrt-contract
              '(- (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))))))))

(deftest specfun->logexp-test
  (is (= (template
          (+ x (/ (- (log (+ 1 (* ~c/I z)))
                     (log (- 1 (* ~c/I z))))
                  ~(c/complex 0.0 2.0))))
         (r/specfun->logexp '(+ x (atan z))))
      "lame test... but this is meant to show that complex numbers appear in the
      replacement."))

(deftest divide-numbers-through-test
  (let [d r/divide-numbers-through]
    (is (= #sicm/ratio 1/2 (d '(/ 1 2))))
    (is (= 'x (d '(* 1 x))))
    (is (= '(* x y z) (d '(* 1 x y z))))
    (is (= '(*) (d '(* 1))))

    (is (= '(+ (* (/ 1 3) a)
               (* (/ 1 3) b)
               (* (/ 1 3) c))
           (v/freeze
            (d '(/ (+ a b c) 3)))))))

(deftest triginv-tests
  (testing "arctan"
    (let [triginv (r/triginv s/*rf-simplify*)]
      (is (= '(atan y x)
             (triginv '(atan y x))))

      (is (= '(/ pi 4)
             (v/freeze
              (triginv '(atan 1 1)))))

      (is (= '(/ pi 4)
             (v/freeze
              (triginv '(atan x x)))))

      (is (= '(- (/ (* 3 pi) 4))
             (v/freeze
              (triginv '(atan -1 -1)))))

      (is (= '(atan -1)
             (triginv '(atan -1 1))))

      (is (= '(atan -1)
             (triginv '(atan (* -1 x) x))))

      (is (= '(atan 1 -1)
             (triginv '(atan 1 -1))))

      (is (= '(atan 1 -1)
             (triginv '(atan x (* -1 x)))))

      (is (= 'z (triginv
                 '(atan
                   (* x (sin z) y)
                   (* y (cos z) x))))))))

(deftest sincos-flush-ones-test
  (let [s (r/sincos-flush-ones s/*rf-simplify*)]
    (is (= '(+ 1 a b c c d e f g)
           (s '(+ a b c (expt (sin x) 2) c d (expt (cos x) 2) e f g))))

    (is (= '(+ c (expt (sin x) 2) d (* (expt (cos x) 2) (cos x)) e)
           (s '(+ c (expt (sin x) 2) d (expt (cos x) 3) e))))

    (is (= '(+ c (* (expt (sin x) 2) (expt (sin x) 2) (sin x))
               d (* (expt (cos x) 2) (cos x)) e)
           (s '(+ c (expt (sin x) 5) d (expt (cos x) 3) e))))))

(deftest trig-tests
  (testing "the eight covered cases from sincos-random"
    (let [rule (r/sincos-random s/*rf-simplify*)]
      (is (= '(+ 2 3 (* (- (expt (sin x) 2)) (expt (cos x) 2)))
             (rule '(+ 2 (- (expt (sin x) 2)) 3 (expt (sin x) 4)))
             (rule '(+ 2 (expt (sin x) 4) 3 (- (expt (sin x) 2))))))

      (is (= '(+ 2 3 (* (- (expt (cos x) 2)) (expt (sin x) 2)))
             (rule '(+ 2 (- (expt (cos x) 2)) 3 (expt (cos x) 4)))
             (rule '(+ 2 (expt (cos x) 4) 3 (- (expt (cos x) 2))))))

      (is (= '(+ 2 3 (* (- (* (expt (sin x) 2) z)) (expt (cos x) 2)))
             (rule '(+ 2 (- (* (expt (sin x) 2) z)) 3 (* z (expt (sin x) 4))))
             (rule '(+ 2 (* z (expt (sin x) 4)) 3 (- (* (expt (sin x) 2) z))))))

      (is (= '(+ 2 3 (* (- (* (expt (cos x) 2) z)) (expt (sin x) 2)))
             (rule '(+ 2 (- (* (expt (cos x) 2) z)) 3 (* z (expt (cos x) 4))))
             (rule '(+ 2 (* z (expt (cos x) 4)) 3 (- (* (expt (cos x) 2) z))))))))

  (testing "high degree cosines unwrap the (expt ... 1) remainder."
    (let [r (rule-simplifier r/split-high-degree-sincos)]
      (is (= '(+ 1 2 (* (expt (cos x) 2)
                        (expt (cos x) 2)
                        (cos x)))
             (r '(+ 1 2 (expt (cos x) 5))))))))

(deftest sin-sq->cos-sq-test
  (let [s r/sin-sq->cos-sq]
    (is (= '(+ 3 x
               (* (* (* (expt (sin x) 1)
                        (- 1 (expt (cos x) 2)))
                     (- 1 (expt (cos x) 2))) (- 1 (expt (cos x) 2))))
           (s '(+ 3 x (expt (sin x) 7)))))))

(deftest partials-test
  (let [full-rule (pr/pipe
                   r/canonicalize-partials
                   (rule-simplifier r/exponent-contract))
        expr '(((partial 0) ((partial 1) ((partial 0) f))) (up x y z))]
    (is (= '(((* (partial 0) (partial 0) (partial 1)) f) (up x y z))
           (r/canonicalize-partials expr))
        "This rule only expand and orders the partials.")

    (is (= '(((* (expt (partial 0) 2) (partial 1)) f) (up x y z))
           (full-rule expr))
        "The full rule collects products into exponents.")))

(deftest complex-test
  (testing "complex-trig"
    (is (= '(cosh 1)
           (r/complex-trig (list 'cos c/I)))
        "cos i, no factors")

    (is (= (list '* c/I '(sinh 1))
           (r/complex-trig (list 'sin c/I)))
        "sin i, no factors")

    (is (= '(cosh z)
           (r/complex-trig
            (template (cos (* z ~c/I))))
           (r/complex-trig
            (template (cos (* ~c/I z)))))
        "cos i, 1 factor")

    (is (= (list '* c/I '(sinh z))
           (r/complex-trig
            (template (sin (* z ~c/I))))
           (r/complex-trig
            (template (sin (* ~c/I z)))))
        "sin i, 1 factor")

    (is (= '(cosh (* z x))
           (r/complex-trig
            (template (cos (* z ~c/I x))))
           (r/complex-trig
            (template (cos (* ~c/I z x))))
           (r/complex-trig
            (template (cos (* z x ~c/I)))))
        "cos of i and more factors")

    (is (= '(cosh (* z x))
           (r/complex-trig
            (template (cos (* z ~c/I x))))
           (r/complex-trig
            (template (cos (* ~c/I z x))))
           (r/complex-trig
            (template (cos (* z x ~c/I)))))
        "cos of i and more factors")

    (is (= (template (* ~c/I (sinh (* z x))))
           (r/complex-trig
            (template (sin (* z ~c/I x))))
           (r/complex-trig
            (template (sin (* ~c/I z x))))
           (r/complex-trig
            (template (sin (* z x ~c/I)))))
        "sin of i and more factors")))
