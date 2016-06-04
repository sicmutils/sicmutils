;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns sicmutils.infix-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            ))

(def ^:private s->infix (compose ->infix simplify))
(def ^:private s->TeX (compose ->TeX simplify))
(defn ^:private s->JS
  [x & options]
  (apply ->JavaScript (simplify x) options))

(deftest basic
  (testing "raw epxressions"
    (is (= "Df(x, y)" (->infix '((D f) x y))))
    (is (= "D(f + g)(x, y)" (->infix '((D (+ f g)) x y))))
    (is (= "D(f g)(x, y)" (->infix '((D (* f g)) x y))))
    (is (= "(sin cos)(t)" (->infix '((* sin cos) t))))
    (is (= "- cos(t ω + φ) a m ω² + cos(t ω + φ) a k"
           (->infix '(+ (* -1 (cos (+ (* t ω) φ)) a m (expt ω 2))
                        (* (cos (+ (* t ω) φ)) a k)))))
    (is (= "(a + b c)(x, y, z)" (->infix '((+ a (* b c)) x y z))))
    (is (= "(a (b + c))(u, v)" (->infix '((* a (+ b c)) u v))))
    (is (= "f(g)(h)(k)(r, s)" (->infix '((((f g) h) k) r s))))
    (is (= "f(r, s)(x, y)" (->infix '((f r s) x y))))
    (is (= "a + b + c f(u)(v)" (->infix '(+ a b (* c ((f u) v))))))
    (is (= "a b (c + (f / g)(v))" (->infix '(* a b (+ c ((/ f g) v))))))
    (is (= "foo bar" (->infix '(* foo bar))))
    (is (= "a b (f / g)(v)" (->infix '(* a b ((/ f g) v)))))
    (is (= "a" (->infix '(* a))))
    (is (= "(- x)³" (->infix '(expt (- x) 3))))
    (is (= "(- x)⁴" (->infix '(expt (- x) 4))))
    (is (= "a" (->infix '(+ a))))
    (is (= "1 / a" (->infix '(/ a))))
    (is (= "- a" (->infix '(- a)))))

  (testing "with-simplifier"
    (with-literal-functions [[f [0 0] 0] h k]
      (is (= "a + b + c" (s->infix (+ 'a 'b 'c))))
      (is (= "a b c" (s->infix (* 'a 'b 'c))))
      (is (= "a b + a c" (s->infix (* 'a (+ 'b 'c)))))
      (is (= "b c + a" (s->infix (+ 'a (* 'b 'c)))))
      (is (= "- a + b - c + d" (s->infix (+ (* -1 'a) 'b (* -1 'c) 'd))))
      (is (= "a - b + c - d" (s->infix (+ 'a (* -1 'b) 'c (* -1 'd)))))
      (is (= "- a" (s->infix (- 'a))))
      (is (= "- a" (s->infix (- 0 'a))))
      (is (= "(- a) / b" (s->infix (/ (- 'a) 'b))))
      (is (= "1 / (a + b)" (s->infix (/ (+ 'a 'b)))))
      (is (= "x⁴" (s->infix (expt (- 'x) 4))))
      (is (= "- x³" (s->infix (expt (- 'x) 3))))
      (is (= "a - b - c" (s->infix (- 'a 'b 'c))))
      (is (= "f(b, c) a" (s->infix (* 'a (f 'b 'c)))))
      (is (= "f(2 h + 2 k, c) a" (s->infix (* 'a (f (* 2 (+ 'h 'k)) 'c)))))
      (is (= "f(x, y)" (s->infix (f 'x 'y))))
      (is (= "down(∂₀(f)(x, y), ∂₁(f)(x, y))" (s->infix ((D f) 'x 'y))))
      (is (= "sin(t) cos(t)" (s->infix ((* sin cos) 't)))))))

(deftest exponents
  (is (= '"x⁴ + 4 x³ + 6 x² + 4 x + 1"
         (s->infix (expt (+ 1 'x) 4))))
  (is (= "x¹²" (s->infix (expt 'x 12))))
  (is (= "y¹⁵ + 3 x⁴ y¹⁰ + 3 x⁸ y⁵ + x¹²"
         (s->infix (expt (+ (expt 'x 4) (expt 'y 5)) 3))))
  (is (= "x² + x^-2" (s->infix '(+ (expt x 2) (expt x -2)))))
  (is (= "sin²(x)" (->infix '((expt sin 2) x))))
  (is (= "(x y)²" (->infix '(expt (* x y) 2))))
  ;; these are wrong: need extra parentheses
  (is (= "sin(x)²" (s->infix ((expt sin 2) 'x))))
  (is (= "sin(x)^y" (s->infix (expt (sin 'x) 'y))))
  (is (= "(a + b)²" (->infix '(expt (+ a b) 2))))
  (is (= "(a + b)^(x + y)" (->infix '(expt (+ a b) (+ x y)))))
  (is (= "(a + b)^x" (->infix '(expt (+ a b) x))))
  (is (= "a^(x + y)" (->infix '(expt a (+ x y)))))
  (is (= "x^y" (s->infix (expt 'x 'y)))))

(deftest more-with-D
  (with-literal-functions [f g [p [] 0]]
    (is (= "f(s)" (s->infix (f 's))))
    (is (= "(f + g)(x, y)" (->infix '((+ f g) x y))))
    (is (= "f(x) g(x)" (s->infix ((* f g) 'x))))
    (is (= "f(t)" (s->infix (f 't))))
    (is (= "Df(s)" (s->infix ((D f) 's))))
    (is (= "D²(f)(s)" (s->infix (((expt D 2) f) 's))))))

(deftest structures
  (is (= "down(up(1, 2), up(3, 4))" (->infix (simplify (down (up 1 2) (up 3 4)))))))

(deftest variable_subscripts
  (is (= "x₀ + y₁ + z₂" (s->infix (+ 'x_0 'y_1 'z_2)))))

(deftest TeX-easy
  (is (= "a + b" (s->TeX (+ 'a 'b))))
  (is (= "\\lambda + \\mu" (s->TeX (+ 'lambda 'mu))))
  (is (= "x_0 + y_s" (s->TeX (+ 'x_0 'y_s))))
  (is (= "\\dfrac{1}{x}" (s->TeX (/ 1 'x))))
  (is (= "\\dfrac{a + b}{c + d}" (s->TeX (/ (+ 'a 'b) (+ 'c 'd)))))
  (is (= "\\dfrac{a}{b}" (s->TeX (/ 'a 'b)))))

(defn ^:private make-symbol-generator
  [p]
  (let [i (atom 0)]
    (fn [] (format "%s%d" p (swap! i inc)))))

(deftest JS
  (is (= "function(a, b, theta) {\n  return Math.sin(theta) + a + b;\n}"
         (s->JS (+ 'a 'b (sin 'theta)))))
  (is (= "function(j) {\n  return 1 / j;\n}"
         (s->JS (invert 'j))))
  (is (= "function(y) {\n  return Math.pow(2.71828, y);\n}"
         (s->JS (expt 2.71828 'y))))
  (is (= "function(a, b, x) {\n  return Math.exp(Math.log(x) * b) * a;\n}"
         (s->JS (* 'a (exp (* 'b (log 'x)))))))
  (is (= "function(x) {\n  var _1 = Math.sin(x);\n  return Math.pow(_1, 2) + _1;\n}"
         (s->JS (+ (sin 'x) (expt (sin 'x) 2)))))
  (is (= (str "function(x, dx) {\n"
              "  var t1 = Math.sin(x);\n"
              "  return -1/2 * t1 * Math.pow(dx, 2) + Math.cos(x) * dx + t1;\n"
              "}")
         (s->JS (reduce + (take 3 (taylor-series-terms sin 'x 'dx)))
                :symbol-generator (make-symbol-generator "t")
                :parameter-order '[x dx])))
  (is (= "function(x, y) {\n  return [1, x + y, 2];\n}" (s->JS (up 1 (+ 'x 'y) 2))))
  (is (= "function(a, b) {\n  return [[1, a], [b, 2]];\n}" (s->JS (down (up 1 'a) (up 'b 2))))))
