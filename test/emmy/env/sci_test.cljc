#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.env.sci-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sci.core :as sci]
            [emmy.env :as e]
            [emmy.env.sci :as es]
            [emmy.operator :as o]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn eval [form]
  (let [ctx (sci/fork es/context)]
    (sci/binding [sci/ns @sci/ns]
      (sci/eval-form ctx '(require '[emmy.env :refer :all]))
      (sci/eval-form ctx form))))

(deftest pattern-tests
  (is (= ['(+ 2 1) "done!"]
         (eval
          '(do (require '[pattern.rule :as r :refer [=>]])
               (let [R (r/ruleset
                        (+ 10 _) => "done!"
                        (+ ?a ?b) => (+ ?b ?a))]
                 [(R '(+ 1 2))
                  (R (R '(+ 11 10)))])))))

  (is (= '(+ 6)
         (eval
          '(do (require '[pattern.rule :as r :refer [=>]])
               (let [R (r/term-rewriting
                        (r/rule (+ ?a ?b ??c) => (+ ?b ??c)))]
                 (R '(+ 1 2 3 4 5 6))))))))

(deftest basic-sci-tests
  (is (= [:at-least 0]
         (eval '(arity
                 (fn ([x] 1) ([x y] x)))))
      "This isn't a GOOD thing; but this documents that arity inside an SCI
      environment isn't something we can trust.")

  (is (v/= 1 (eval '(simplify (+ (square (sin 'x))
                                 (square (cos 'x))))))
      "simplifications work inside sci")

  (is (= "{\\cos}^{2}\\left(x\\right) + {\\sin}^{2}\\left({x}^{2}\\right)"
         (eval '(->TeX
                 (simplify (+ (square (sin (square 'x)))
                              (square (cos 'x))))))))

  (is (= (e/literal-function 'U)
         (eval '(literal-function 'U)))
      "Literal functions use value equality.")

  (is (= o/identity
         (eval
          '(do (require '[emmy.operator :as o])
               o/identity)))
      "can sci internally require namespaces?")

  (is (v/= '(* 10 face)
           (eval
            '(do (require '[emmy.env :as e])
                 (e/simplify (e/* 'face 10)))))
      "emmy.env is available as a namespace and also included as the
      default bindings in `user`.")

  (testing "sci-specific macro definitions"
    (is (= 2.0 (eval
                '(do (require '[emmy.algebra.fold :as af])
                     (let [sum (af/fold->sum-fn (af/kbk-n 2))]
                       (sum [1.0 1e100 1.0 -1e100])))))
        "compensated summation with a macro inside SCI")

    (is (= [true true true true]
           (eval '(let-coordinates [[x y]     R2-rect
                                    [r theta] R2-polar]
                    (let [p ((point R2-rect) (up 1 2))]
                      [(= 1 (x p))
                       (= 2 (y p))
                       (= (sqrt 5) (r p))
                       (= (atan 2) (theta p))]))))
        "let-coordinates macro works")

    (is (= [true true]
           (eval '(using-coordinates
                   [x y] R2-rect
                   (let [p ((point R2-rect) (up 1 2))]
                     [(= 1 (x p))
                      (= 2 (y p))]))))
        "using-coordinates works!")

    (is (= [true true true true]
           (eval '(do
                    (require '[emmy.calculus.manifold :as m])

                    (def old-prototype
                      (m/coordinate-prototype R2-rect))

                    (define-coordinates [x y] R2-rect)

                    (let [p ((point R2-rect) (up 1 2))]
                      [(= 1 (x p))
                       (= 2 (y p))
                       ;; Note that `R2-rect` is actually rebound.
                       (= ['x0 'x1] old-prototype)
                       (= ['x 'y] (m/coordinate-prototype R2-rect))]))))
        "define-coordinates version of that test")

    (is (eval
         '(do (define-coordinates (up x y) R2-rect)

              (let [circular (- (* x d:dy) (* y d:dx))]
                (= '(+ (* 3 x0) (* -2 y0))
                   (freeze
                    (simplify
                     ((circular (+ (* 2 x) (* 3 y)))
                      ((point R2-rect) (up 'x0 'y0)))))))))
        "define-coordinates works with a test from form_field_test.cljc")))

(deftest more-sci-tests
  ;; breaking these out to give the simplifier some repeatability.
  (testing "internal defn, funky symbols, internal with-literal-functions macro"
    (is (= "down(- m r(t) (Dφ(t))² + m D²r(t) + DU(r(t)), m (r(t))² D²φ(t) + 2 m r(t) Dφ(t) Dr(t))"
           (eval
            '(do (defn L-central-polar [m U]
                   (fn [[_ [r] [rdot φdot]]]
                     (- (* (/ 1 2) m
                           (+ (square rdot)
                              (square (* r φdot))))
                        (U r))))
                 (with-literal-functions [U r φ]
                   (let [L     (L-central-polar 'm U)
                         state (up r φ)]
                     (->infix
                      (simplify
                       (((Lagrange-equations L) state) 't)))))))))))
