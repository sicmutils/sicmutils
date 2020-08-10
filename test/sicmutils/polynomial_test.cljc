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

(ns sicmutils.polynomial-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.math.numeric-tower :as nt]
            [sicmutils.analyze :as a]
            [sicmutils.expression :refer [variables-in]]
            [sicmutils.generic :as g]
            [sicmutils.modint :as modular]
            [sicmutils.numbers]
            [sicmutils.polynomial :as p]
            [sicmutils.simplify]
            [sicmutils.value :as v]))

(deftest poly-core
  (testing "kind"
    (is (= ::p/polynomial (v/kind (p/make [])))))

  (testing "zero"
    (is (v/nullity? (p/make [])))
    (is (v/nullity? (p/make [0])))
    (is (v/nullity? (p/make [])))
    (is (v/nullity? (p/make 2 [])))
    (is (v/nullity? (p/make 2 [])))
    (is (not (v/nullity? (p/make [1])))))

  (testing "unity"
    (is (v/unity? (p/make [1])))
    (is (v/unity? (p/make 2 [[[0 0] 1]])))
    (is (v/unity? (p/make 3 [[[0 0 0] 1]])))
    (is (not (v/unity? (p/make 3 [[[0 0 0] 1] [[0 0 1] 2]]))))
    (is (not (v/unity? (p/make [1.1]))))
    (is (v/unity? (p/make [1.0])))
    (is (v/unity? (p/make [(p/make [1])])))
    (is (not (v/unity? (p/make [(p/make [2])])))))

  (testing "make-constant"
    (is (= (p/make [99]) (p/make-constant 1 99)))
    (is (= (p/make 2 [[[0 0] 88]]) (p/make-constant 2 88)))
    (is (= (p/make 3 [[[0 0 0] 77]]) (p/make-constant 3 77))))

  (testing "degree"
    (is (= (p/degree (p/make [])) -1))
    (is (= (p/degree (p/make [-1 1])) 1))
    (is (= (p/degree (p/make [0 1])) 1))
    (is (= (p/degree (p/make [-1 0 2])) 2))
    (is (= (p/degree (p/make [-1 2 0])) 1))
    (is (= (p/degree (p/make [0 0])) -1)))

  (testing "zero-like"
    (is (= (p/make []) (v/zero-like (p/make [1 2 3]))))
    (is (= (p/make 2 []) (v/zero-like (p/make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (p/make 3 []) (v/zero-like (p/make 3 [[[1 2 1] 4] [[0 1 0] 5]])))))

  (testing "one-like"
    (is (= (p/make [1]) (v/one-like (p/make [1 2 3]))))
    (is (= (p/make 2 [[[0 0] 1]]) (v/one-like (p/make 2 [[[1 0] 1] [[2 1] 3]]))))
    (is (= (p/make 3 [[[0 0 0] 1]]) (v/one-like (p/make 3 [[[1 2 1] 4] [[0 1 0] 5]]))))
    ;; we can't deduce the unit element from the zero polynomial over an
    ;; "unknown" ring
    (is (thrown? IllegalArgumentException (v/one-like (p/make 2 [])))))

  (testing "add constant"
    (is (= (p/make [3 0 2]) (p/add (p/make [0 0 2]) (p/make [3]))))
    (is (= (p/make [0 0 2]) (p/add (p/make [2 0 2]) (p/make [-2])))))

  (testing "add/sub"
    (is (v/nullity? (p/add (p/make [0 0 2]) (p/make [0 0 -2]))))
    (is (= (p/make []) (p/add (p/make [0 0 2]) (p/make [0 0 -2]))))
    (is (= (p/make [3]) (p/add (p/make [3 0 2]) (p/make [0 0 -2]))))
    (is (= (p/make [-1 1]) (p/add (p/make [0 1]) (p/make [-1]))))
    (is (v/nullity? (p/sub (p/make [0 0 2]) (p/make [0 0 2]))))
    (is (= (p/make [-3]) (p/sub (p/make [0 0 2]) (p/make [3 0 2]))))
    (is (= (p/make [0 1 2]) (p/sub (p/make [3 1 2]) (p/make [3]))))
    (is (= (p/make [-2 -2 -1]) (p/sub (p/make [1]) (p/make [3 2 1]))))
    (is (= (p/make [0 0 1 0 1 -1]) (p/sub (p/make [1 0 1 0 1]) (p/make [1 0 0 0 0 1]))))
    (is (= (p/make [0 0 -1 0 -1 1]) (p/sub (p/make [1 0 0 0 0 1]) (p/make [1 0 1 0 1]))))
    (is (= (p/make [-1 -2 -3]) (p/negate (p/make [1 2 3])))))

  (testing "with symbols"
    (is (= (p/make [(g/+ 'a 'c) (g/+ 'b 'd) 'c]) (p/add (p/make '[a b c]) (p/make '[c d])))))

  (testing "mul"
    (is (= (p/make []) (p/mul (p/make [1 2 3]) (p/make [0]))))
    (is (= (p/make []) (p/mul (p/make [0]) (p/make [1 2 3]))))
    (is (= (p/make []) (p/mul (p/make []) (p/make [1 2 3]))))
    (is (= (p/make [1 2 3]) (p/mul (p/make [1 2 3]) (p/make [1]))))
    (is (= (p/make [1 2 3]) (p/mul (p/make [1]) (p/make [1 2 3]))))
    (is (= (p/make [3 6 9]) (p/mul (p/make [1 2 3]) (p/make [3]))))
    (is (= (p/make [0 1 2 3]) (p/mul (p/make [0 1]) (p/make [1 2 3]))))
    (is (= (p/make [0 -1 -2 -3]) (p/mul (p/make [0 -1]) (p/make [1 2 3]))))
    (is (= (p/make [-1 0 1]) (p/mul (p/make [1 1]) (p/make [-1 1]))))
    (is (= (p/make [1 3 3 1]) (p/mul (p/make [1 1]) (p/mul (p/make [1 1]) (p/make [1 1])))))
    (is (= (p/make [1 -4 6 -4 1]) (p/mul (p/mul (p/make [-1 1]) (p/make [-1 1]))
                                         (p/mul (p/make [-1 1]) (p/make [-1 1]))))))

  (testing "div"
    (is (= [(p/make [1 1]) (p/make [])]
           (p/divide (p/make [-1 0 1]) (p/make [-1 1]))))
    (is (= [(p/make [-10 1]) (p/make [-32 -21])]
           (p/divide (p/make [-42 0 -12 1]) (p/make [1 -2 1]))))
    (is (= [(p/make [3 1 1]) (p/make [5])]
           (p/divide (p/make [-4 0 -2 1]) (p/make [-3 1]))))
    (is (= [(p/make [-5 0 3]) (p/make [60 -27 -11])]
           (p/divide (p/make [-45 18 72 -27 -27 0 9]) (p/make [21 -9 -4 0 3]))))
    (let [U (p/make [-5 2 8 -3 -3 0 1 0 1])
          V (p/make [21 -9 -4 0 5 0 3])
          [pr d] (p/pseudo-remainder U V)]
      (is (= [(p/make [-2/9 0 1/3]) (p/make [-1/3 0 1/9 0 -5/9])] (p/divide U V)))
      (is (= [(p/make [-3 0 1 0 -5]) 2] [pr d]))
      (is (= (p/make []) (p/sub (p/mul (p/make [(nt/expt 3 d)]) U) (p/add (p/mul (p/make [-2 0 3]) V) pr)))))
    ;; examples from http://www.mathworks.com/help/symbolic/mupad_ref/pdivide.html
    (let [p (p/make [1 1 0 1])
          q (p/make [1 1 3])]
      (is (= [(p/make [10 7]) 2] (p/pseudo-remainder p q))))
    (let [p (p/make [3 0 4])
          q (p/make [2 2])]
      (is (= [(p/make [28]) 2] (p/pseudo-remainder p q))))
    (is (= [(p/make 2 []) (p/make 2 [[[2 1] 1] [[1 2] 1]])]
           (p/divide (p/make 2 [[[2 1] 1] [[1 2] 1]]) (p/make 2 [[[1 2] 1]]))))
    (is (= [(p/make [1]) (p/make [])] (p/divide (p/make [3]) (p/make [3]))))
    (is (= [(p/make [0]) 1] (p/pseudo-remainder (p/make [7]) (p/make [2])))))

  (testing "expt"
    (let [x+1 (p/make [1 1])]
      (is (= (p/make [1]) (p/expt x+1 0)))
      (is (= x+1 (p/expt x+1 1)))
      (is (= (p/make [1 2 1]) (p/expt x+1 2)))
      (is (= (p/make [1 3 3 1]) (p/expt x+1 3)))
      (is (= (p/make [1 4 6 4 1]) (p/expt x+1 4)))
      (is (= (p/make [1 5 10 10 5 1]) (p/expt x+1 5)))))

  (testing "other coefficient rings: GF(2)"
    (let [mod2 #(modular/make % 2)
          x0 (mod2 0)
          x1 (mod2 1)
          P (p/make [x1 x0 x1])]
      (is (= (p/make [x1 x0 x0 x0 x1]) (p/expt P 2)))
      (is (= (p/make [x1 x0 x1 x0 x1 x0 x1]) (p/expt P 3)))
      (is (= (p/make [x1 x0 x0 x0 x0 x0 x0 x0 x1]) (p/mul (p/expt P 3) P)))
      (is (= (p/make []) (p/sub P P)))
      (is (= (p/make []) (p/add P P)))
      (is (= (p/make [x0 x0 x1]) (p/add P (p/make [1]))))))

  (testing "CRC polynomials"
    ;; https://en.wikipedia.org/wiki/Computation_of_cyclic_redundancy_checks
    ;; http://www.lammertbies.nl/comm/info/crc-calculation.html
    (let [mod2 #(modular/make % 2)
          o (mod2 0)
          i (mod2 1)
          x8 (p/make [o o o o o o o o i])
          CRC-8-ATM (p/make [i i i o o o o o i])
          M (p/make [i i i 0 i o i])
          Mx8 (p/mul x8 M)
          [_ r1] (p/divide Mx8 CRC-8-ATM)
          CRC-16-CCITT (p/make [i o o o o i o o o o o o i o o o i])
          x16 (p/mul x8 x8)
          T (p/make [o o i o i o i])
          Tx16 (p/mul x16 T)
          [_ r2] (p/divide Tx16 CRC-16-CCITT)]
      (is (= (p/make [o i o o o i o i]) r1))
      (is (= (p/make [i o o o i i i o o i o i i]) r2))))

  (testing "monomial order"
    (let [x3 [3 0 0]
          x2z2 [2 0 2]
          xy2z [1 2 1]
          z2 [0 0 2]
          monomials [x3 x2z2 xy2z z2]
          monomial-sort #(sort-by identity % monomials)]
      (is (= [z2 xy2z x2z2 x3] (monomial-sort p/lex-order)))
      (is (= [z2 x3 x2z2 xy2z] (monomial-sort p/graded-reverse-lex-order)))
      (is (= [z2 x3 xy2z x2z2] (monomial-sort p/graded-lex-order))))))

(def ^:private poly-analyzer (p/->PolynomialAnalyzer))
(defn ^:private ->poly [x] (a/expression-> poly-analyzer x (fn [p _] p)))

(deftest poly-evaluate
  (testing "arity 1"
    (let [p (->poly '(+ 2 (* x 3)))]
      (is (= 14 (p/evaluate p [4])))
      (is (= 11 (p/evaluate p [3 2]))))
    (is (= 256 (p/evaluate (->poly '(expt x 8)) [2])))
    (is (= 272 (p/evaluate (->poly '(+ (expt x 4) (expt x 8))) [2]))))

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

  (testing "constant polys"
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
      (is (= 33 (p/evaluate P [1 2 3 4]))))))

(deftest poly-partial-derivatives
  (let [V (p/make [1 2 3 4])
        U (p/make 2 [[[1 1] 3] [[2 2] 4] [[0 0] 5] [[0 3] 7] [[4 0] -2]])]
    (is (= (p/make [2 6 12]) (p/partial-derivative V 0)))
    (is (= [(p/make [2 6 12])] (p/partial-derivatives V)))
    (is (= (p/make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]]) (p/partial-derivative U 0)))
    (is (= (p/make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]]) (p/partial-derivative U 1)))
    (is (= [(p/make 2 [[[0 1] 3] [[1 2] 8] [[3 0] -8]])
            (p/make 2 [[[1 0] 3] [[2 1] 8] [[0 2] 21]])]
           (p/partial-derivatives U)))))

(deftest poly-as-simplifier
  (testing "arity"
    (let [^sicmutils.polynomial.Polynomial p (p/make [0 1])]
      (is (= 1 (.-arity p)))))

  (testing "make-vars"
    (is (= (list (p/make [0 1])) (a/new-variables poly-analyzer 1)))
    (is (= [(p/make 3 [[[1 0 0] 1]])
            (p/make 3 [[[0 1 0] 1]])
            (p/make 3 [[[0 0 1] 1]])] (a/new-variables poly-analyzer 3))))

  (testing "expr"
    (let [exp1 (:expression (g/* (g/+ 1 'x) (g/+ -3 'x)))
          exp2 (:expression (g/expt (g/+ 1 'y) 5))
          exp3 (:expression (g/- (g/expt (g/- 1 'y) 6) (g/expt (g/+ 'y 1) 5)))
          receive (fn [a b] [a b])]
      (is (= '#{* + x} (variables-in exp1)))
      (is (= [(p/make [-3 -2 1]) '(x)] (a/expression-> poly-analyzer exp1 receive)))
      (is (= [(p/make [-3 -2 1]) '(x)] (a/expression-> poly-analyzer exp1 receive)))
      (is (= [(p/make [1 5 10 10 5 1]) '(y)] (a/expression-> poly-analyzer exp2 receive)))
      (is (= [(p/make [0 -11 5 -30 10 -7 1]) '(y)] (a/expression-> poly-analyzer exp3 receive)))))

  (testing "monomial order"
    (let [poly-simp #(a/expression-> poly-analyzer (:expression %) (fn [p vars] (a/->expression poly-analyzer p vars)))]
      (is (= '(+ (expt x 2) x 1) (poly-simp (g/+ 'x (g/expt 'x 2) 1))))
      (is (= '(+ (expt x 4) (* 4 (expt x 3)) (* 6 (expt x 2)) (* 4 x) 1) (poly-simp (g/expt (g/+ 1 'x) 4))))
      (is (= '(+
               (expt x 4)
               (* 4 (expt x 3) y)
               (* 6 (expt x 2) (expt y 2))
               (* 4 x (expt y 3))
               (expt y 4))
             (poly-simp (g/expt (g/+ 'x 'y) 4))))
      (is (= '(+
               (expt x 4)
               (* 4 (expt x 3) y)
               (* 6 (expt x 2) (expt y 2))
               (* 4 x (expt y 3))
               (expt y 4))
             (poly-simp (g/expt (g/+ 'y 'x) 4))))))

  (testing "expr-simplify"
    (let [poly-simp #(a/expression-> poly-analyzer % (fn [p vars] (a/->expression poly-analyzer p vars)))
          exp1 (:expression (g/+ (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x)))
          exp2 (:expression (g/+ (g/* 'y 'y) (g/* 'x 'x 'x) (g/* 'x 'x) (g/* 'x 'x) (g/* 'y 'y)))
          exp3 'y]
      (is (= '(+ (expt x 3) (* 2 (expt x 2))) (poly-simp exp1)))
      (is (= '(+ (expt x 3) (* 2 (expt x 2)) (* 2 (expt y 2))) (poly-simp exp2)))
      (is (= 'y (poly-simp exp3)))
      (is (= '(+ g1 g2) (poly-simp (:expression (g/+ 'g1 'g2)))))
      (is (= '(* 2 g1) (poly-simp (:expression (g/+ 'g1 'g1)))))
      (is (= 3 (poly-simp '(+ 2 1))))
      (is (= '(+ b (* -1 f)) (poly-simp '(- (+ a b c) (+ a c f)))))
      (is (= '(+ (* -1 b) f) (poly-simp '(- (+ a c f) (+ c b a))))))))

(defn generate-poly
  [arity]
  (gen/fmap #(p/make arity %)
            (gen/vector
             (gen/tuple
              (gen/vector gen/pos-int arity)
              gen/int))))

(defn generate-nonzero-poly
  [arity]
  (gen/such-that (complement v/nullity?) (generate-poly arity)))

(def ^:private num-tests 50)

(defspec ^:long p+p=2p num-tests
  (prop/for-all [^sicmutils.polynomial.Polynomial p (gen/bind gen/nat generate-poly)]
                (= (p/add p p) (p/mul p (p/make-constant (.-arity p) 2)))))

(defspec ^:long p-p=0 num-tests
  (prop/for-all [p (gen/bind gen/nat generate-poly)]
                (v/nullity? (p/sub p p))))

(defspec ^:long pq-div-p=q num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-nonzero-poly arity)]
                  (let [[Q R] (p/divide (p/mul p q) q)]
                    (and (v/nullity? R)
                         (= Q p))))))

(defspec ^:long p+q=q+p num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)]
                  (= (p/add p q) (p/add q p)))))

(defspec ^:long pq=qp num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)]
                  (= (p/mul p q) (p/mul q p)))))

(defspec ^:long p*_q+r_=p*q+p*r num-tests
  (gen/let [arity gen/nat]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)
                   r (generate-poly arity)]
                  (= (p/mul p (p/add q r)) (p/add (p/mul p q) (p/mul p r))))))

(defspec ^:long lower-and-raise-arity-are-inverse num-tests
  (prop/for-all [p (gen/bind (gen/choose 2 10) generate-nonzero-poly)]
                (= p (p/raise-arity (p/lower-arity p)))))

(defspec ^:long evaluation-homomorphism num-tests
  (gen/let [arity (gen/choose 1 6)]
    (prop/for-all [p (generate-poly arity)
                   q (generate-poly arity)
                   xs (gen/vector gen/int arity)]
                  (= (*' (p/evaluate p xs) (p/evaluate q xs))
                     (p/evaluate (p/mul p q) xs)))))

(deftest analyzer-test
  (let [new-analyzer (fn [] (a/make-analyzer
                            (p/->PolynomialAnalyzer)
                            (a/monotonic-symbol-generator "k%08d")))
        A #((new-analyzer) %)]
    (is (= '(+ x 1) (A '(+ 1 x))))
    (is (= '(+ x 1) (A '[+ 1 x])))
    (is (= 'x (A '(* 1/2 (+ x x)))))
    (is (= '(* y (sin y) (cos (+ (expt (sin y) 4) (* 2 (sin y)) 1)))
           (A '(* y (sin y) (cos (+ 1 (sin y) (sin y) (expt (sin y) 4)))))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt cos 2) (expt sin 2)))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt sin 2) (expt cos 2)))))

    (comment
      (is (= '(+ (* -1 m (expt ((D phi) t) 2) (r t)) (* m (((expt D 2) r) t)) ((D U) (r t)))
             (A '(- (* 1/2 m (+ (((expt D 2) r) t) (((expt D 2) r) t)))
                    (+ (* 1/2 m (+ (* ((D phi) t) ((D phi) t) (r t))
                                   (* ((D phi) t) ((D phi) t) (r t))))
                       (* -1 ((D U) (r t))))))))
      )))
