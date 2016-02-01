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
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer :all]
            [sicmutils.infix :refer :all]
            [sicmutils.env :refer :all]
            ))

(def ^:private s->infix (compose ->infix simplify))

(deftest basic
  (testing "raw epxressions"
    (is (= "a * f(2 * (h + k), c)" ((make-renderer) '(* a (f (* 2 (+ h k)) c)))))
    (is (= "D(f)(x, y)" (->infix '((D f) x y))))
    (is (= "(sin cos)(t)" (->infix '((* sin cos) t))))
    (is (= "-1 cos(t ω + φ) a m ω² + cos(t ω + φ) a k"
           (->infix '(+ (* -1 (cos (+ (* t ω) φ)) a m (expt ω 2))
                        (* (cos (+ (* t ω) φ)) a k)))))
    (is (= "(a + b c)(x, y, z)" (->infix '((+ a (* b c)) x y z))))
    (is (= "(a (b + c))(u, v)" (->infix '((* a (+ b c)) u v))))
    (is (= "f(g)(h)(k)(r, s)" (->infix '((((f g) h) k) r s))))
    (is (= "f(r, s)(x, y)" (->infix '((f r s) x y))))
    (is (= "a + b + c f(u)(v)" (->infix '(+ a b (* c ((f u) v))))))
    (is (= "a b (c + (f / g)(v))" (->infix '(* a b (+ c ((/ f g) v))))))
    (is (= "foo bar" (->infix '(* foo bar))))
    (is (= "a b (f / g)(v)" (->infix '(* a b ((/ f g) v))))))
  (testing "with-simplifier"
    (with-literal-functions [[f [0 0] 0] h k]
      (is (= "a + b + c" (s->infix (+ 'a 'b 'c))))
      (is (= "a b c" (s->infix (* 'a 'b 'c))))
      (is (= "a b + a c" (s->infix (* 'a (+ 'b 'c)))))
      (is (= "b c + a" (s->infix (+ 'a (* 'b 'c)))))
      (is (= "f(b, c) a" (s->infix (* 'a (f 'b 'c)))))
      (is (= "f(2 h + 2 k, c) a" (s->infix (* 'a (f (* 2 (+ 'h 'k)) 'c)))))
      (is (= "f(x, y)" (s->infix (f 'x 'y))))
      (is (= "down(∂(0)(f)(x, y), ∂(1)(f)(x, y))" (s->infix ((D f) 'x 'y))))
      (is (= "sin(t) cos(t)" (s->infix ((* sin cos) 't)))))))

(deftest exponents
  (is (= '"x⁴ + 4 x³ + 6 x² + 4 x + 1"
         (->infix (simplify (expt (+ 1 'x) 4)))))
  (is (= "x¹²" (->infix (simplify (expt 'x 12)))))
  (is (= "y¹⁵ + 3 x⁴ y¹⁰ + 3 x⁸ y⁵ + x¹²"
         (->infix (simplify (expt (+ (expt 'x 4) (expt 'y 5)) 3)))))
  (is (= "expt(y, 10) + 2 expt(x, 4) expt(y, 5) + expt(x, 8)"
         ((make-renderer :juxtapose-multiply true)
          (simplify (expt (+ (expt 'x 4) (expt 'y 5)) 2)))))
  (is (= "x² + expt(x, -2)" (->infix (simplify '(+ (expt x 2) (expt x -2)))))))

(deftest structures
  (is (= "down(up(1, 2), up(3, 4))" (->infix (simplify (down (up 1 2) (up 3 4)))))))
