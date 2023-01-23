#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.abstract.number :as an]
            [emmy.expression :as e]
            [emmy.generic :as g]
            [emmy.value :as v]))

(deftest expressions
  (testing "value protocol impl"
    (is (v/zero? (e/make-literal ::blah 0)))
    (is (v/one? (e/make-literal ::blah 1)))
    (is (v/identity? (e/make-literal ::blah 1)))

    (is (not (v/zero? (e/make-literal ::blah 10))))
    (is (v/zero? (v/zero-like (e/make-literal ::blah 10))))

    (is (not (v/one? (e/make-literal ::blah 10))))
    (is (v/one? (v/one-like (e/make-literal ::blah 10))))

    (is (not (v/identity? (e/make-literal ::blah 10))))
    (is (v/identity? (v/identity-like (e/make-literal ::blah 10))))

    (is (not (v/exact? (e/make-literal ::blah 10.5))))
    (is (v/exact? (e/make-literal ::blah 10)))

    (is (= '(sin 1 2 3)
           (v/freeze
            (e/literal-apply ::blah 'sin [1 2 3]))))

    (is (e/literal?
         (e/literal-apply ::blah 'sin [1 2 3])))

    (is (e/literal?
         (an/literal-number '(* 4 3))))

    (is (not (e/literal? "face")))
    (is (not (e/literal? 'x)))

    (is (not (e/abstract?
              (e/make-literal ::blah 12))))

    (is (e/abstract?
         (an/literal-number 12))))

  (checking "(= expr non-expr) compares non-expr against internally captured
    expression" 100
            [x gen/any-equatable]
            (if (coll? x)
              (is (#?(:clj not= :cljs =)
                   (an/literal-number x) x)
                  "in CLJ, expressions don't support `seq`, so can't compare
                  against proper collections. Not so in CLJS!")
              (is (= (an/literal-number x) x)
                  "Anything else's equality passes through."))

            (when #?(:clj true :cljs (instance? IComparable x))
              (is (zero? (compare (an/literal-number x) x))
                  "comparison properly passes through.")))

  (testing "metadata support"
    (let [m {:real? true :latex! "face"}]
      (is (not= (e/make-literal ::blah 12)
                (-> (e/make-literal ::blah 12)
                    (with-meta m)))
          "Metadata affects equality")

      (is (= m (meta
                (-> (e/make-literal ::blah 12)
                    (with-meta m))))
          "metadata can round-trip across expressions.")

      (is (nil? (meta
                 (e/make-literal ::blah 12)))
          "metadata defaults to nil")))

  (testing "literal-type"
    (is (= ::e/numeric
           (e/literal-type
            (an/literal-number 12))))

    (is (= ::blah
           (e/literal-type
            (e/make-literal ::blah 12)))))

  (testing "fmap"
    (is (= (e/make-literal ::blah 9)
           (e/fmap g/square (e/make-literal ::blah 3)))))

  (testing "expression-of"
    (is (= 9 (e/expression-of
              (e/fmap g/square (e/make-literal ::blah 3))))
        "e/expression-of returns the wrapped expression from literals.")

    (is (= 'x (e/expression-of 'x))
        "symbols get round-tripped")

    (checking "expression-of acts as identity for non Literal" 100
              [x gen/any-equatable]
              (is (= x (e/expression-of x)))))

  (testing "variables-in"
    (let [expr '(+ x (* 3 y) [a [b 9 c] [3 4 5 d]])]
      (is (= '#{a b c d x y * +}
             (e/variables-in expr)
             (e/variables-in (e/make-literal ::blah expr)))))
    (is (= '#{x} (e/variables-in 'x))))

  (testing "evaluate"
    (is (= 12 (e/evaluate '(+ 3 4 x) {'x 5} {'+ +})))
    (is (= 0 (e/evaluate '(+ 3 (* 4 y) x) {'x 5 'y -2} {'* * '+ +})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (e/evaluate '(+ 3 (* 4 y) x) {'x 5 'y -2} {'+ +}))))

  (testing "substitute"
    (is (= 'x
           (e/substitute '(* (* (* x y) y) y)
                         '(* x y) 'x))
        "Substitutions occur in postwalk order, so the lower-level replacements
        make new potential replacements available higher up.")

    (is (= '(* (* (* 1 2) 2) 2)
           (e/substitute '(* (* (* x y) y) y)
                         {'x 1 'y 2}))
        "substitute works with a dict too")

    (is (= 8 (e/evaluate
              (e/substitute '(* (* (* x y) y) y)
                            {'x 1 'y 2})
              {} {'* * '- -}))
        "of course we can evaluate the results."))

  (testing "compare"
    (testing "empty seq is only equal to itself, less than anything else"
      (is (= -1 (e/compare () 10)))
      (is (= 0 (e/compare () ()))))

    (testing "for types that don't play nice we resort to hashing."
      (is (= -1 (e/compare '(+ x y) #sicm/complex "1+2i")))
      (is (= 1 (e/compare #sicm/complex "1+2i" '(+ x y)))))

    ;; TODO add more tests as we start to explore this function.
    ))

(deftest string-form-test
  (let [expr (g/+ 'x 'x)]
    (is (re-matches
         #"\(\* 2 x\)\r?\n"
         (with-out-str
           (e/print-expression expr))))

    (is (= (str (e/expression->string expr) "\n")
           (with-out-str
             (e/print-expression expr)))))

  (is (= "+" (e/expression->string g/+)))
  (is (= "nil" (e/expression->string nil)))
  (is (= "(up nil 3 (+ x 2))" (e/expression->string [nil 3 (g/+ 2 'x)])))
  (is (= "1" (e/expression->string
              ((g/+ (g/square g/sin)
                    (g/square g/cos))
               'x))))

  (is (#{"(/ (+ (* -1 (expt (cos x) 4)) 1N) (expt (cos x) 2))"
         "(/ (+ (* -1 (expt (cos x) 4)) 1) (expt (cos x) 2))"}
       (e/expression->string
        ((g/+ (g/square g/sin) (g/square g/tan)) 'x)))
      "This expression evaluates to one or the other, depending on what's been
      evaluated."))
