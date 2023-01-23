#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.rational-function-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.abstract.number :as an]
            [emmy.calculus.derivative :as deriv :refer [D]]
            [emmy.complex :as c]
            [emmy.expression :as x]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.laws :as sl]
            [emmy.matrix]
            [emmy.polynomial :as p]
            [emmy.rational-function :as rf]
            [emmy.simplify]
            [emmy.structure :as s]
            [emmy.value :as v]))

(def ^{:doc "Generating RFs quickly gets out of hand with larger ones. Stick to
            a small number of tests to make sure it all completes."}
  test-limit 10)

(deftest protocol-tests
  (checking "IArity" test-limit [r (sg/rational-function)]
            (is (= (f/arity r)
                   [:between 0 (rf/arity r)])))

  (checking "->seq" test-limit [r (sg/rational-function)]
            (is (= [(rf/bare-u r)
                    (rf/bare-v r)]
                   (seq r))
                "calling `seq` returns a pair of numerator, denominator"))

  (checking "meta, with-meta"
            test-limit [r (sg/rational-function)
                        m (gen/map gen/keyword gen/any)]
            (is (nil? (meta r))
                "nil by default")

            (is (= m (meta (with-meta r m)))
                "metadata works")

            (is (= m (meta
                      (rf/->RationalFunction
                       (rf/bare-arity r)
                       (rf/bare-u r)
                       (rf/bare-v r)
                       m)))
                "four-arity constructor allows metadata"))

  (let [x+1     (p/make [1 1])
        x-1     (p/make [-1 1])
        x+1:x-1 (rf/make x+1 x-1)]
    (testing "zero?, one-like"
      (is (v/zero? (v/zero-like x+1:x-1)))
      (is (v/zero? (g/* x+1:x-1 (v/zero-like x+1:x-1)))))

    (testing "one?, one-like"
      (is (v/one? (v/one-like x+1:x-1)))
      (is (= x+1:x-1 (g/* x+1:x-1 (v/one-like x+1:x-1)))))

    (testing "identity?, identity-like"
      (is (v/identity? (v/identity-like x+1:x-1)))
      (is (= (g/* (p/make [0 1]) x+1:x-1)
             (g/* x+1:x-1 (v/identity-like x+1:x-1)))
          "identity is `x`, multiplying should be equivalent to multiplying by
          x."))

    (testing "v/freeze"
      (is (= '(/ (polynomial 1 [[{} 1] [{0 1} 1]])
                 (polynomial 1 [[{} -1] [{0 1} 1]]))
             (v/freeze x+1:x-1))))

    (testing "v/numerical?"
      (is (not (v/numerical? x+1:x-1))))

    (testing "v/kind"
      (is (= ::rf/rational-function
             (v/kind x+1:x-1))))

    (testing "equality between types"
      (let [rf (rf/->RationalFunction 10 (p/constant 10 2) 3 nil)
            r  #sicm/ratio 2/3]
        (is (= rf r)
            "rf on left with a CONSTANT polynomial numerator, non-poly denominator
          equals a ratio.")

        (is (v/= r rf)
            "the other way requires v/= in Clojure."))))

  (checking "negative?" test-limit [r (sg/rational-function)]
            (is (not
                 (rf/negative? (rf/abs r))))

            (is (or (rf/negative? r)
                    (rf/negative?
                     (rf/negate r)))))

  (testing "coef-sgn handling of complex"
    (is (= (rf/->RationalFunction 1
                                  (g/negate (p/identity))
                                  (c/complex 1 -10)
                                  nil)
           (rf/make (p/identity)
                    (c/complex -1 10)))
        "complex denominator is negated, since its real part is negative.")) )

(deftest rational-function-tests
  (testing "rf/make constructor tests"
    (is (= (rf/make
            (p/make 2 {[0 2] 2
                       [1 1] 3})
            (p/make 2 {[1 0] 6
                       [0 2] 1}))
           (-> (rf/make
                (p/make 2 {[1 2] 2
                           [2 1] 3})
                (p/make 2 {[1 2] #sicm/ratio 1/2
                           [2 0] 3}))
               (rf/make 2)))
        "rf/make can handle rational functions in the numerator OR denominator;
      all denominators are processed out, including rational coefficient
      denominators.")

    (let [
          ;; constant arity 1 polynomial
          p #(p/make 1 [[[0] %]])

          ;; ratio of constant arity 1 polynomials
          rf #(rf/->RationalFunction 1 (p %1) (p %2) nil)
          x+1 (p/make [1 1])
          x-1 (p/make [-1 1])
          x+1:x-1 (rf/make x+1 x-1)
          x-1:x+1 (rf/make x-1 x+1)
          one (p/make [1])]
      (is (= (rf/make (p/make [-1 -2 -3]) (p/make [-4 -5 6]))
             (rf/make (p/make [1 2 3]) (p/make [4 5 -6]))))

      (is (= (rf/make (p/make [1 2 3]) (p/make [-4 5 6]))
             (rf/make (p/make [1 2 3]) (p/make [-4 5 6]))))

      (is (= one (rf/mul x+1:x-1 x-1:x+1)))
      (is (= one (rf/mul x-1:x+1 x+1:x-1)))

      (is (= (rf/make (p/make [1 -1]) (p/make [1 1]))
             (rf/negate x-1:x+1)))

      (is (= x+1:x-1 (rf/invert x-1:x+1)))

      (is (= one (rf/mul x-1:x+1 (rf/invert x-1:x+1))))

      (is (= (rf/make (p/make [2 0 2]) (p/make [-1 0 1]))
             (rf/add x-1:x+1 x+1:x-1)))

      (is (= (rf/make (p/make [2 0 2]) (p/make [-1 0 1]))
             (rf/add x+1:x-1 x-1:x+1)))

      (is (= (rf/make (p/make [1 2 1]) (p/make [1 -2 1]))
             (rf/expt x+1:x-1 2)))

      (is (= (rf/make (p/make [1 -2 1]) (p/make [1 2 1]))
             (rf/expt x+1:x-1 -2)))

      (is (= (p 3) (rf/add (rf 3 2) (rf 3 2))))

      (is (= #sicm/ratio 5/3
             (rf/div (rf 5 2) (rf 3 2))))

      (is (= #sicm/ratio 14/3
             (rf/div (rf 8 3) (rf 4 7))))

      (is (= (rf/make (p/make [0 15 10]) (p/make [0 0 15 18]))
             (rf/make (p/make [0 #sicm/ratio 1/2 #sicm/ratio 1/3])
                      (p/make [0 0 #sicm/ratio 1/2 #sicm/ratio 3/5])))))))

(deftest rf-arithmetic
  (testing "rational functions forms a field"
    (let[rf-gen (sg/rational-function
                 1
                 {:coeffs sg/rational}
                 {:coeffs sg/rational})]
      (sl/field 10 rf-gen "rational-function")))

  (testing "invert-hilbert-matrix"
    (let [p #(p/constant 1 %) ;; constant arity 1 polynomial

          ;; arity 1 rational function out of two constants
          rf #(rf/->RationalFunction 1 (p %1) (p %2) nil)
          N 3
          H (s/up* (for [i (range 1 (inc N))]
                     (s/up* (for [j (range 1 (inc N))]
                              (rf 1 (+ i j -1))))))]
      (is (= (s/down
              (s/down 9 -36 30)
              (s/down -36 192 -180)
              (s/down 30 -180 180))
             (g/invert H)))))

  (checking "square, cube" test-limit
            [r (sg/rational-function)]
            (is (= (g/square r)
                   (g/mul r r)))

            (is (= (g/cube r)
                   (g/mul r (g/square r))))))

(deftest rf-operations
  (let [x+1 (p/make [1 1])
        x-1 (p/make [-1 1])]
    (is (= x+1 (g/mul x-1 (rf/make x+1 x-1))))))

(defn rf-simp [expr]
  (letfn [(cont [a b]
            (rf/->expression a b))]
    (rf/expression-> expr cont)))

(deftest from-points-tests
  (is (zero? (rf/from-points []))
      "no points returns a 0")

  (checking "single point rational function is a constant" 100
            [x  sg/any-integral
             fx sg/any-integral]
            (is (= fx (rf/from-points [[x fx]]))))

  (testing "from-points with two points"
    (let [f (rf/from-points [[1 1] [2 4]])]
      (is (rf/rational-function? f))
      (is (= 1 (f 1)))
      (is (= 4 (f 2)))))

  (testing "from-points with three points"
    (let [f (rf/from-points [[1 1] [2 4] [3 9]])]
      (is (= 1 (f 1)))
      (is (= 4 (f 2)))
      (is (= 9 (f 3)))))

  (testing "symbolic from-points. Only works with two entries so far!"
    (let [f (rf/from-points '[[x1 y1] [x2 y2]])]
      (is (v/= 'y1 (g/simplify (f 'x1))))
      (is (v/= 'y2 (g/simplify (f 'x2)))))))

(deftest rf-as-simplifier
  (let [arity 20]
    (checking "evaluate matches ->expression" test-limit
              [n  (sg/polynomial :arity arity)
               xs (gen/vector sg/symbol arity)]
              (let [rf (rf/->RationalFunction arity n 12 nil)]
                (is (every?
                     v/zero?
                     (for [idx (range (inc arity))]
                       (let [sub-xs (subvec xs 0 idx)
                             padded (into sub-xs (repeat (- arity idx) 0))]
                         (g/simplify
                          (g/- (apply rf padded)
                               (an/literal-number
                                (rf/->expression rf padded)))))))
                    "For every subsequence up to and including the full sequence
                      of args, [[rf/->expression]] matches the final result (but
                      not necessarily the same result!) as calling `apply`."))))

  (checking "arg-scale, shift" test-limit
            [r (sg/rational-function)
             factor (gen/fmap inc gen/nat)]
            (let [scaled (rf/arg-scale r [factor])]
              (is (v/zero?
                   (g/simplify
                    (g/- (r (g/* 'x factor))
                         (rf/evaluate scaled ['x]))))))

            (let [shifted (rf/arg-shift r [factor])]
              (is (v/zero?
                   (g/simplify
                    (g/- (r (g/+ 'x factor))
                         (rf/evaluate shifted ['x])))))))

  (testing "expression-> unwraps internal literals"
    (is (every?
         (complement x/literal?)
         (tree-seq coll? seq
                   (-> (rf/from-points '[[x y] [x2 y2]])
                       (rf/->expression ['z]))))
        "EVEN if the original rf has symbolic coefficients, these are unwrapped
         in the process of generating the bare expression."))

  (testing "expr"
    (let [exp1 (x/expression-of (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (x/expression-of (g/expt (g/+ 1 'y) 5))
          exp3 (x/expression-of (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))]
      (is (= [(p/make [-3 -2 1]) '(x)] (rf/expression-> exp1)))
      (is (= [(p/make [-3 -2 1]) '(x)] (rf/expression-> exp1)))
      (is (= [(p/make [1 5 10 10 5 1]) '(y)] (rf/expression-> exp2)))
      (is (= [(p/make [0 -11 5 -30 10 -7 1]) '(y)] (rf/expression-> exp3)))))

  (testing "expr-simplify"
    (let [exp1 (x/expression-of
                (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
          exp2 (x/expression-of
                (g/+ (g/* 'y 'y) (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x) (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2))) (rf-simp exp1)))
      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2))) (rf-simp exp2)))
      (is (= 'y (rf-simp exp3)))
      (is (= '(+ g1 g2) (rf-simp (x/expression-of (g/+ 'g1 'g2)))))
      (is (= 12 (rf-simp '(+ 3 9))))
      (is (= '(* 2 g1) (rf-simp (x/expression-of (g/+ 'g1 'g1)))))
      (is (= '(+ b (* -1 f)) (rf-simp '(- (+ a b c) (+ a c f)))))
      (is (= '(+ (* -1 b) f) (rf-simp '(- (+ a c f) (+ c b a)))))
      (is (= '(* a c e) (rf-simp '(/ (* a b c d e f) (* b d f)))))
      (is (= '(+ (* B a) (* B b) (* B c) (* B d)) (rf-simp '(+ (* B a) (* B b) (* B c) (* B d)))))
      (is (= '(+ a b c d) (rf-simp '(/ (+ (* B a) (* B b) (* B c) (* B d)) B))))
      (is (= '(/ 1 (+ a b c d)) (rf-simp '(/ B (+ (* B a) (* B b) (* B c) (* B d))))))
      (is (= '(+ a b) (rf-simp '(/ (+ (* B a) (* B b)) B))))
      (is (= '(/ 1 (+ a b)) (rf-simp '(/ B (+ (* B a) (* B b))))))
      (is (= '(/ (+ (* (expt dX 2) m2)
                    (* (expt dY 2) m2)
                    (* (expt dx 2) m1)
                    (* (expt dy 2) m1)
                    (* 2 m1 m2))
                 2)
             (rf-simp
              '(/ (+ (* K (expt dX 2) m2)
                     (* K (expt dY 2) m2)
                     (* K (expt dx 2) m1)
                     (* K (expt dy 2) m1)
                     (* 2 K m1 m2))
                  (* 2 K)))))))

  (testing "gcd"
    (is (= '(* x y) (rf-simp '(gcd (* w x y) (* x y z)))))
    (is (= '(/ s c) (rf-simp '(gcd (/ (* r s) c) (/ (* s t) (* c))))))
    (is (= 2 (rf-simp '(gcd (* 2 x y) 2))))
    (is (= 3 (rf-simp '(gcd 9 (* x 6 y)))))
    (is (= '(* 7 y) (rf-simp '(gcd (* 14 x y) (* 21 y z)))))
    (is (= '(* (/ 1 6) y)
           (v/freeze
            (rf-simp
             '(gcd (* (/ 5 2) x y) (* (/ 7 3) y z)))))
        "Can handle rational gcd!"))

  (testing "quotients"
    (is (= '(/ 1 (* 2 x))
           (rf-simp
            (x/expression-of (g/divide 1 (g/* 2 'x))))))

    (is (= 4 (rf-simp
              (x/expression-of (g/divide (g/* 28 'x) (g/* 7 'x))))))

    (is (= '(/ 1 (expt x 21))
           (rf-simp
            (x/expression-of (g/divide (g/expt 'x 7) (g/expt 'x 28))))))))

(deftest calculus-tests
  (let [x+1     (p/make [1 1])
        x-1     (p/make [-1 1])
        x+1:x-1 (rf/make x+1 x-1)]
    (is (= '(/ -2 (+ (expt x 2) (* -2 x) 1))
           (v/freeze
            (g/simplify
             ((D x+1:x-1) 'x)))))

    (is (= [(rf/partial-derivative x+1:x-1 0)]
           (rf/partial-derivatives x+1:x-1)))

    (is (= ((rf/partial-derivative x+1:x-1 0) 'x)
           ((D x+1:x-1) 'x)
           (((deriv/partial 0) x+1:x-1) 'x))
        "D operator works, partial too with variable index.")))
