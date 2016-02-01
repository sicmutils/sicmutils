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
  (:require [clojure.test :refer :all]
            [sicmutils.infix :refer :all]
            ))

(deftest basic
  (is (= "a + b + c" (->infix '(+ a b c))))
  (is (= "a b c" (->infix '(* a b c))))
  (is (= "a (b + c)" (->infix '(* a (+ b c)))))
  (is (= "a + b c" (->infix '(+ a (* b c)))))
  (is (= "a f(b, c)" (->infix '(* a (f b c)))))
  (is (= "a f(2 (h + k), c)" (->infix '(* a (f (* 2 (+ h k)) c)))))
  (is (= "f(x, y)" (->infix '(f x y))))
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
