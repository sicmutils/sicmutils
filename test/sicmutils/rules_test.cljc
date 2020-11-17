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

(ns sicmutils.rules-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.rules :as r]))

(deftest simplify-square-roots-test
  (let [s r/simplify-square-roots]
    (is (= '(expt x 4) (s '(expt (sqrt x) 8))))
    (is (= '(* (sqrt x) (expt x 3)) (s '(expt (sqrt x) 7))))
    (is (= '(expt x 4) (s '(sqrt (expt x 8)))))
    (is (= '(sqrt (expt x 7)) (s '(sqrt (expt x 7)))))

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

(deftest divide-numbers-through-test
  (let [d r/divide-numbers-through]
    (is (= #sicm/ratio 1/2 (d '(/ 1 2))))
    (is (= 'x (d '(* 1 x))))
    (is (= '(* x y z) (d '(* 1 x y z))))
    (is (= '(*) (d '(* 1))))
    (is (= '(+ (/ a 3) (/ b 3) (/ c 3)) (d '(/ (+ a b c) 3))))))

(deftest sincos-flush-ones-test
  (let [s r/sincos-flush-ones]
    (is (= '(+ 1 a b c c d e f g)
           (s '(+ a b c (expt (sin x) 2) c d (expt (cos x) 2) e f g))))
    (is (= '(+ (* (expt (cos x) 2) (expt (cos x) 1)) c (expt (sin x) 2) d e)
           (s '(+ c (expt (sin x) 2)  d (expt (cos x) 3) e ))))
    (is (= '(+ (* (expt (sin x) 2) (expt (sin x) 1) (expt (sin x) 2))
               (* (expt (cos x) 2) (expt (cos x) 1))
               c d e)
           (s '(+ c (expt (sin x) 5)  d (expt (cos x) 3) e ))))))

(deftest sin-sq->cos-sq-test
  (let [s r/sin-sq->cos-sq]
    (is (= '(+ 3 x
               (* (* (* (expt (sin x) 1)
                        (- 1 (expt (cos x) 2)))
                     (- 1 (expt (cos x) 2))) (- 1 (expt (cos x) 2))))
           (s '(+ 3 x (expt (sin x) 7)))))))

(deftest sqrt-expand-contract-test
  (testing "sqrt-expand works with division"
    (is (= '(+ (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))
           (r/sqrt-expand '(+ (sqrt (/ a b)) (sqrt (/ c b))))))
    (is (= '(- (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))
           (r/sqrt-expand '(- (sqrt (/ a b)) (sqrt (/ c b)))))))
  (testing "sqrt-contract undoes expansion over division"
    (is (= '(+ (sqrt (/ a b)) (sqrt (/ c b)))
           (r/sqrt-contract '(+ (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b))))))
    (is (= '(- (sqrt (/ a b)) (sqrt (/ c b)))
           (r/sqrt-contract '(- (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b))))))
    )

  )
