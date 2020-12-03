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

(ns sicmutils.function-test
  (:refer-clojure :exclude [partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.generic :as g]
            [sicmutils.generators :as sg]
            [sicmutils.matrix :as m]
            [sicmutils.value :as v]
            [sicmutils.operator :as o]
            [sicmutils.series :as series]
            [sicmutils.structure :as s :refer [literal-up
                                               literal-down
                                               up down]]
            [sicmutils.simplify :as ss :refer [hermetic-simplify-fixture]]
            [sicmutils.function :as f]))

(use-fixtures :once hermetic-simplify-fixture)

(def ^:private near (v/within 1.0e-6))

(deftest equations-moved-from-simplify
  (testing "moved-from-simplify"
    (let [xy    (s/up (f/literal-function 'x)
                      (f/literal-function 'y))
          xyt   (xy 't)
          U     (f/literal-function 'U)
          xyt2  (g/square xyt)
          Uxyt2 (U xyt2)]
      (is (= '(up x y) (g/simplify xy)))
      (is (= '(up (x t) (y t)) (g/simplify xyt)))
      (is (= '(+ (expt (x t) 2) (expt (y t) 2)) (g/simplify xyt2)))
      (is (= '(U (+ (expt (x t) 2) (expt (y t) 2))) (g/simplify Uxyt2)))))

  (testing "moved-from-matrix"
    (is (= '(matrix-by-rows [(f x) (g x)] [(h x) (k x)])
           (g/simplify
            ((m/by-rows (map f/literal-function '[f g])
                        (map f/literal-function '[h k])) 'x))))

    (let [R2f #(f/literal-function % [0 1] 0)]
      (is (= '(matrix-by-rows [(f x y) (g x y)] [(h x y) (k x y)])
             (g/simplify
              ((m/by-rows [(R2f 'f) (R2f 'g)]
                          [(R2f 'h) (R2f 'k)]) 'x 'y)))))))

(deftest function-basic
  (let [f (f/literal-function 'F)]
    (testing "a"
      (is (= '(F x) (g/simplify (f 'x))))
      (is (= '(F 7) (g/simplify (f (g/+ 3 4))))))
    (testing "kind"
      (is (= ::f/function (v/kind f))))
    (testing "arity > 1"
      (let [g (f/literal-function 'g [0 0] 0)]
        (is (= '(g a b) (g/simplify (g 'a 'b))))))))

(deftest trig-tests
  (testing "tan, sin, cos"
    (let [f (g/- g/tan (g/div g/sin g/cos))]
      (is (zero? (g/simplify (f 'x))))))

  (testing "sin/asin"
    (let [f (f/compose g/sin g/asin)]
      (is (near 0.5 (f 0.5)))
      (testing "outside real range"
        (is (near 10 (g/magnitude (f 10)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "cos/acos"
    (let [f (f/compose g/cos g/acos)]
      (is (near 0.5 (f 0.5)))

      (testing "outside real range"
        (is (near 5 (g/magnitude (f -5)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "tan/atan"
    (let [f (f/compose g/tan g/atan)]
      (is (near (/ 0.5 0.2) (f 0.5 0.2))
          "two-arity version!")
      (is (near 0.5 (f 0.5))
          "one-arity version")))

  (testing "cot"
    (let [f (g/- g/cot (g/invert g/tan))]
      (is (zero? (g/simplify (f 'x))))))

  (testing "tanh"
    (let [f (g/- (g/div g/sinh g/cosh) g/tanh)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "sec"
    (let [f (g/- (g/invert g/cos) g/sec)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "csc"
    (let [f (g/- (g/invert g/sin) g/csc)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "sech"
    (let [f (g/- (g/invert g/cosh) g/sech)]
      (is (zero?
           (g/simplify (f 'x))))))

  (testing "cosh"
    (is (near ((g/cosh g/square) 2)
              (g/cosh 4))))

  (testing "sinh"
    (is (near ((g/sinh g/square) 2)
              (g/sinh 4))))

  (testing "acosh"
    (let [f (f/compose g/cosh g/acosh)]
      (is (near 10 (f 10))
          "TODO this can't handle the full generic simplification yet. Sub in
          when we get more rules.")

      (testing "outside real range"
        (is (near 5 (g/magnitude (f -5)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "asinh"
    (let [f (f/compose g/sinh g/asinh)]
      (is (near 10 (f 10)))

      (testing "outside real range"
        (is (near 5 (g/magnitude (f -5)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals."))))

  (testing "atanh"
    (let [f (f/compose g/tanh g/atanh)]
      (is (near 0.5 (f 0.5)))

      (testing "outside real range"
        (is (near 10 (g/magnitude (f 10)))
            "This kicks out a complex number, which doesn't yet compare
          immediately with reals.")))))

(deftest complex-tests
  (let [f (f/literal-function 'f)]
    (testing "gcd/lcm unit"
      (is (= (g/gcd 10 5)
             (let [deferred (g/gcd (g/+ 1 g/square) 5)]
               (deferred 3))))
      (is (= (g/lcm 10 6)
             (let [deferred (g/lcm (g/+ 1 g/square) 6)]
               (deferred 3)))))

    (checking "gcd/lcm works with fns"
              100
              [l gen/nat r gen/nat]
              (is (= ((g/gcd f r) l)
                     (g/gcd (f l) r)))

              (is (= ((g/lcm f r) l)
                     (g/lcm (f l) r))))

    (checking "complex accessors work with fns"
              100
              [z sg/complex]
              (is (= (g/imag-part (f z)) ((g/imag-part f) z)))
              (is (= (g/real-part (f z)) ((g/real-part f) z)))
              (is (= (g/magnitude (f z)) ((g/magnitude f) z)))
              (is (= (g/angle (f z))     ((g/angle f) z))))))

(defn transpose-defining-relation [T g a]
  "T is a linear transformation T:V -> W
  the transpose of T, T^t:W* -> V*
  Forall a in V, g in W*,  g:W -> R
  (T^t(g))(a) = g(T(a))."
  (g/- (((g/transpose T) g) a)
       (g (T a))))

(deftest transpose-test
  (testing "transpose"
    (let [f #(str "f" %)
          g #(str "g" %)]
      (is (= "fg" (f (g ""))))
      (is (= "gf" (((g/transpose f) g) ""))
          "g/transpose for functions returns a fn that takes ANOTHER fn, and
    returns a fn that applies them in reverse order. Like a curried andThen (the
    reverse of compose)."))

    (let [T   (f/literal-function 'T '(-> (UP Real Real) (UP Real Real Real)))
          DT  (D T)
          DTf (fn [s]
                (fn [x] (g/* (DT s) x)))
          a (literal-up 'a 2)
          g (fn [w] (g/* (literal-down 'g 3) w))
          s (up 'x 'y)]
      (is (v/nullity? (transpose-defining-relation (DTf s) g a))
          "This function, whatever it is (see scmutils function.scm) satisfies
          the transpose defining relation.")

      (is (= '(+ (* a↑0 g_0 (((partial 0) T↑0) (up x y)))
                 (* a↑0 g_1 (((partial 0) T↑1) (up x y)))
                 (* a↑0 g_2 (((partial 0) T↑2) (up x y)))
                 (* a↑1 g_0 (((partial 1) T↑0) (up x y)))
                 (* a↑1 g_1 (((partial 1) T↑1) (up x y)))
                 (* a↑1 g_2 (((partial 1) T↑2) (up x y))))
             (g/simplify
              (((g/transpose (DTf s)) g) a)))
          "A wild test of transpose from scmutils function.scm that I don't
    understand!"))))

(deftest string-form-test
  (is (= "1" (ss/expression->string
              ((g/+ (g/square g/sin) (g/square g/cos)) 'x))))

  (is (= "(/ (+ (* -1 (expt (cos x) 4)) 1) (expt (cos x) 2))"
         (ss/expression->string
          ((g/+ (g/square g/sin) (g/square g/tan)) 'x)))))

(deftest literal-functions
  (testing "domain in Rⁿ, range R"
    (let [f (f/literal-function 'f)             ;; f : R -> R
          g (f/literal-function 'g [0 0] 0)]     ;; g : R x R -> R
      (is (= '(f x) (g/simplify (f 'x))))
      (is (= '(g x y) (g/simplify (g 'x 'y))))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/simplify (f 'x 'y))))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/simplify (g 'x))))))

  (testing "structured range"
    (let [h (f/literal-function 'h 0 (up 0 0 0))
          k (f/literal-function 'k 0 (up 0 (up 0 0) (down 0 0)))
          q (f/literal-function 'q 0 (down (up 0 1) (up 2 3)))]
      (is (= '(up (h↑0 t) (h↑1 t) (h↑2 t)) (g/simplify (h 't))))
      (is (= '(up (k↑0 t)
                  (up (k↑1↑0 t) (k↑1↑1 t))
                  (down (k↑2_0 t) (k↑2_1 t)))
             (g/simplify (k 't))))
      (is (= '(down (up (q_0↑0 t) (q_0↑1 t))
                    (up (q_1↑0 t) (q_1↑1 t))) (g/simplify (q 't))))
      (is (= '(down (up 0 0) (up 0 0)) (g/simplify ((v/zero-like q) 't))))))

  (testing "R^n -> structured range"
    (let [h (f/literal-function 'h [0 1] 0)]
      (is (= '(h x y) (g/simplify (h 'x 'y)))))
    (let [m (f/literal-function 'm [0 1] (up 1 2 3))]
      (is (= '(up (m↑0 x y) (m↑1 x y) (m↑2 x y))
             (g/simplify (m 'x 'y)))))
    (let [z (f/literal-function 'm [0 1] (up (down 1 2) (down 3 4)))]
      (is (= '(up (down (m↑0_0 x y) (m↑0_1 x y))
                  (down (m↑1_0 x y) (m↑1_1 x y)))
             (g/simplify (z 'x 'y)))))
    (let [g (f/literal-function 'm [0 1 2] (down (down 1 2 3)
                                                 (down 4 5 6)
                                                 (down 7 8 9)))]
      (is (= '(down
               (down (m_0_0 x y z) (m_0_1 x y z) (m_0_2 x y z))
               (down (m_1_0 x y z) (m_1_1 x y z) (m_1_2 x y z))
               (down (m_2_0 x y z) (m_2_1 x y z) (m_2_2 x y z)))
             (g/simplify (g 'x 'y 'z))))))

  (testing "R -> Rⁿ"
    ;; NB: GJS doesn't allow a function with vector range, because
    ;; if this were parallel with structures this would mean
    ;; having an applicable vector of functions, and such a thing
    ;; isn't handy. This could probably be done, but for the time
    ;; being it's easy enough just to make the range an up tuple,
    ;; which is just as useful as well as being explicit about the
    ;; variance.
    #_(let [h (f/literal-function 'h 0 [0 1])]
        (is (= 'foo (h 'x))))))

(deftest function-signature-conversion
  (let [k f/sicm-signature->domain-range]
    (is (= [[0] 0] (k '(-> Real Real))))
    (is (= [[0 0] 0] (k '(-> (X Real Real) Real))))
    (is (= [[0 0] 0] (k '(-> (X* Real 2) Real))))
    (is (= [[0] [0 0]] (k '(-> Real (X Real Real)))))
    (is (= [[0] [0 0]] (k '(-> Real (X* Real 2)))))
    (is (= [[0 0] [0 0]] (k '(-> (X Real Real) (X Real Real)))))
    (is (= [[0 0] [0 0]] (k '(-> (X* Real 2) (X* Real 2)))))
    (is (= [[0] (up 0 0)] (k '(-> Real (UP Real Real)))))
    (is (= [[0] (up 0 0)] (k '(-> Real (UP* Real 2)))))
    (is (= [[(up 0 0)] 0] (k '(-> (UP Real Real) Real))))
    (is (= [[(up 0 0)] 0] (k '(-> (UP* Real 2) Real))))
    (is (= [[(up 0 0)] (up 0 0)] (k '(-> (UP Real Real) (UP Real Real)))))
    (is (= [[(up 0 0)] (up 0 0)] (k '(-> (UP* Real 2) (UP* Real 2)))))
    (is (= [[(up 0 (up 0 0) (down 0 0))] 0]
           (k '(-> (UP Real (UP Real Real) (DOWN Real Real)) Real))))))

(deftest function-algebra
  (let [add2 (fn [x] (g/+ x 2))
        explog (g/exp g/log)
        mul3 #(* 3 %)]
    (testing "unary"
      (is (= 4 (add2 2)))
      (is (= -4 ((g/- add2) 2)))
      (is (= 9 ((g/sqrt add2) 79)))
      (is (= #sicm/ratio 1/9 ((g/invert add2) 7)))
      (is (= 1.0 (explog 1.0)))
      (is (near 99.0 (explog 99.0)))
      (is (near 20.08553692 ((g/exp add2) 1.0)))
      (is (near 4.718281828 ((add2 g/exp) 1.0))))

    (testing "binary"
      (is (= 12 ((g/+ add2 4) 6)))
      (is (= 14 ((g/+ add2 mul3) 3)))
      (is (= 10 ((g/+ mul3 4) 2)))
      (is (= 32 ((g/expt 2 add2) 3)))
      (is (= 25 ((g/expt add2 2) 3)))
      (is (= ::v/function (v/kind (g/expt add2 2))))

      (testing "cross-product"
        (let [deferred (g/cross-product #(g/* 2 %)
                                        #(g/+ (s/up 4 3 1) %))
              v (s/up 1 2 3)]
          (is (= (g/cross-product (g/* 2 v)
                                  (g/+ (s/up 4 3 1) v))
                 (deferred v))
              "Slightly tougher since this works with structures"))))

    (testing "arity 2"
      (let [f (fn [x y] (+ x y))
            g (fn [x y] (* x y))
            h (g/+ f g)
            k (g/+ 4 (g/- f 2))
            m (g/+ g (g/- f 2))]
        (is (= 11 (h 2 3)))
        (is (= 7 (k 2 3)))
        (is (= 9 (m 2 3)))))

    (testing "arity 0"
      (let [f (fn [] 3)
            g (fn [] 4)
            h (g/+ f g)
            k (g/- f g)
            j (g/* f g)
            q (g/divide f g)]
        (is (= 7 (h)))
        (is (= -1 (k)))
        (is (= 12 (j)))
        (is (= #sicm/ratio 3/4 (q)))))

    (testing "at least 0 arity"
      (let [add (fn [& xs] (reduce + 0 xs))
            mul (fn [& xs] (reduce * 1 xs))
            add+mul (g/+ add mul)
            add-mul (g/- add mul)
            mul-add (g/- mul add)]
        (is (= [:at-least 0] (v/arity add)))
        (is (= [:at-least 0] (v/arity mul)))
        (is (= [:at-least 0] (v/arity add+mul)))
        (is (= 33 (add+mul 2 3 4)))
        (is (= -15 (add-mul 2 3 4)))
        (is (= 15 (mul-add 2 3 4)))))))

(deftest operators
  (let [f (fn [x] (+ x 5))
        double (fn [f] (fn [x] (* 2 (f x))))
        double-op (o/make-operator double "double")]
    (is (= 12 ((double f) 1)))
    (is (= 24 ((double (double f)) 1)))
    (is (= 12 ((double-op f) 1)))
    (is (= 24 ((double-op (double-op f)) 1)))
    (is (= 24 (((g/* double-op double-op) f) 1)))           ;; * for operators is composition
    (is (= 144 (((g/* double double) f) 1)))                ;; * for functions is pointwise multiply
    (is (= 2 ((double-op identity) 1)))
    (is (= 6 (((g/expt double-op 0) f) 1)))
    (is (= 12 (((g/expt double-op 1) f) 1)))
    (is (= 24 (((g/expt double-op 2) f) 1)))
    (is (= 18 (((g/+ double-op double-op double-op) identity) 3)))
    (is (= 24 (((g/+ double-op 4 double-op) identity) 3)))))

(deftest function-differential
  (testing "structural utilities"
    (is (f/symbolic-derivative? '(D f)))
    (is (not (f/symbolic-derivative? '(e f))))
    (is (not (f/iterated-symbolic-derivative? '(expt D 2))))
    (is (f/iterated-symbolic-derivative? '((expt D 2) f)))
    (is (= '((expt D 2) f) (f/symbolic-increase-derivative '(D f))))
    (is (= '((expt D 3) f) (f/symbolic-increase-derivative '((expt D 2) f))))))

(deftest moved-from-series
  (testing "series"
    (is (= '[(* 2 (f x)) (* 3 (f x))]
           (g/simplify
            (take 2 ((g/* (series/series 2 3)
                          (f/literal-function 'f)) 'x)))))
    (is (= '[(* 2 (f y)) (* 3 (f y))]
           (g/simplify
            (take 2 ((g/* (f/literal-function 'f)
                          (series/series 2 3)) 'y))))))

  (let [simp4 (fn [x] (g/simplify (take 4 x)))
        S (series/series (f/literal-function 'f)
                         (f/literal-function 'g))
        T (series/series (f/literal-function 'F [0 1] 0)
                         (f/literal-function 'G [0 1] 0))
        U (series/series (f/literal-function 'W [(s/up 0 0)] 0)
                         (f/literal-function 'Z [(s/up 0 0)] 0))
        V (series/series g/sin g/cos g/tan)]

    (testing "with functions"
      (is (= '((* (sin x) (f x)) (* (sin x) (g x)) 0 0)
             (g/simplify (take 4 ((g/* S g/sin) 'x)))))
      (is (= '((* (sin x) (f x)) (* (sin x) (g x)) 0 0)
             (g/simplify (take 4 ((g/* g/sin S) 'x))))))

    (testing "and derivatives"
      (is (= '(((D f) x) ((D g) x) 0 0)
             (g/simplify (take 4 ((D S) 'x)))))
      (is (= '((F x y) (G x y) 0 0) (simp4 (T 'x 'y))))
      (is (= '((((partial 0) F) x y) (((partial 0) G) x y) 0 0) (simp4 (((partial 0) T) 'x 'y))))
      (is (= '((((partial 1) F) x y) (((partial 1) G) x y) 0 0) (simp4 (((partial 1) T) 'x 'y))))
      (is (= '((((partial 0) W) (up r θ)) (((partial 0) Z) (up r θ)) 0 0) (simp4 (((partial 0) U) (up 'r 'θ)))))
      (is (= '((((partial 1) W) (up r θ)) (((partial 1) Z) (up r θ)) 0 0) (simp4 (((partial 1) U) (up 'r 'θ)))))
      (is (= '[(sin t) (cos t) (tan t) 0] (simp4 (V 't))))
      (is (= '[(cos t) (* -1 (sin t)) (/ 1 (expt (cos t) 2)) 0] (simp4 ((D V) 't)))))

    (testing "f -> Series"
      (let [F (fn [k] (series/series (fn [t] (g/* k t)) (fn [t] (g/* k k t))))]
        (is (= '((* q z) (* (expt q 2) z) 0 0) (simp4 ((F 'q) 'z))))
        (is (= '(z (* 2 q z) 0 0) (simp4 (((D F) 'q) 'z))))))))
