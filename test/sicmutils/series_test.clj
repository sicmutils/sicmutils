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

(ns sicmutils.series-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all]
            [sicmutils
             [env :refer :all]
             [series :as series]
             [simplify :refer [hermetic-simplify-fixture]]]))

(use-fixtures :once hermetic-simplify-fixture)

(defn ^:private simp4 [x] (simplify (series/take 4 x)))

(deftest series-test
  (testing "basics"
    (let [Q (series/starting-with 4)
         R (series/starting-with 4 3)
         S (series/starting-with 4 3 2)
         ones (series/generate (constantly 1))
         nats0 (series/generate identity)
         nats (series/generate inc)]
     (is (= '(4 0 0 0 0 0 0 0) (series/take 8 Q)))
     (is (= '(4 3 0 0 0 0 0 0) (series/take 8 R)))
     (is (= '(4 3 2 0 0 0 0 0) (series/take 8 S)))
     (is (= '(8 6 2 0 0 0 0 0) (series/take 8 (+ R S))))
     (is (= 4 (series/sum S 0)))
     (is (= 7 (series/sum S 1)))
     (is (= 9 (series/sum S 2)))
     (is (= 7 (series/sum R 8)))
     (is (= 4 (series/sum Q 20)))
     (is (= 9 (series/sum S 3)))
     (is (= 9 (series/sum S 4)))
     (is (= '(0 1 2 3) (series/take 4 nats0)))
     (is (= '(0 1 4 9) (series/take 4 (series/generate square))))
     (is (= '(0 2 6 12) (series/take 4 (+
                                        nats0
                                        (series/generate square)))))
     (is (= '(3 6 9 12) (series/take 4 (* 3 nats))))
     (is (= '(-3 -6 -9 -12) (series/take 4 (negate (* 3 nats)))))
     (is (= '(1 4 9 16) (series/take 4 (series/map square nats))))
     (is (= '(3 6 9 12) (series/take 4 (* nats 3))))
     (is (= '(ε (* 2 ε) (* 3 ε) (* 4 ε)) (simplify (series/take 4 (* nats 'ε)))))
     (is (= '(ε (* 2 ε) (* 3 ε) (* 4 ε)) (simplify (series/take 4 (* 'ε nats)))))
     (is (= '(0 -2 -6 -12)
            (series/take 4 (negate (+ nats0 (series/generate square))))))
     (is (= '(17/4 7/2 11/4 1) (series/take 4 (+ (* 1/4 nats) S))))
     (is (= '(0 m (* 2 m) (* 3 m))
            (->> nats0
                 (* 'm)
                 series/->seq
                 (take 4)
                 simplify)))
     (is (= '(0 r (* 2 r) (* 3 r))
            (simplify (take 4 (series/->seq (* 'r nats0))))))
     (is (= '(3 5 7 0 0 0 0 0)
            (series/take 8
                         (+ (series/starting-with 1 2 3)
                              (series/starting-with 2 3 4)))))
     (is (= '(1 4 10 12 9 0 0)
            (simplify
             (series/take 7
                          (*
                           (series/starting-with 1 2 3)
                           (series/starting-with 1 2 3))))))
     ;; the tetrahedral numbers
     (is (= '(1 4 10 20 35 56 84)
            (take 7
                  (series/->seq
                   (square
                    nats)))))
     (is (= '(m (* 4 m) (* 10 m) (* 20 m))
            (->> (series/generate inc)
                 square
                 (* 'm)
                 series/->seq
                 (take 4)
                 simplify)))
     (is (= '(1 2 3 4 5 6)
            (->> ones
                 square
                 series/->seq
                 (take 6))))
     ;; the triangular numbers, via convolution
     (is (= '(1 3 6 10 15 21)
            (->> (* ones nats)
                 series/->seq
                 (take 6))))
     ;; again, via partial sums
     (is (= '(1 3 6 10 15 21)
            (series/take 6
                         (series/partial-sums
                          nats))))
     (is (= '(1 2 3 4 5 6)
            (series/take 6
                         (series/partial-sums
                          ones))))
     (is (= '((* 2 (f x)) (* 3 (f x)))
            (simplify
             (series/take 2
                          ((* (series/starting-with 2 3)
                                (literal-function 'f)) 'x)))))
     (is (= '((* 2 (f y)) (* 3 (f y)))
            (simplify
             (series/take 2
                          ((* (literal-function 'f)
                                (series/starting-with 2 3)) 'y)))))
     ))

  (let [S (series/starting-with (literal-function 'f)
                                (literal-function 'g))
        T (series/starting-with (literal-function 'F [0 1] 0)
                                (literal-function 'G [0 1] 0))
        U (series/starting-with (literal-function 'W [(up 0 0)] 0)
                                (literal-function 'Z [(up 0 0)] 0))
        V (series/starting-with sin cos tan)]
    (testing "with functions"
      (is (= '[(* (f x) (sin x)) (* (sin x) (g x)) 0 0]
             (simplify (series/take 4 ((* S sin) 'x)))))
      (is (= '[(* (f x) (sin x)) (* (sin x) (g x)) 0 0]
             (simplify (series/take 4 ((* sin S) 'x))))))
    (testing "and derivatives"
      (is (= '(((D f) x) ((D g) x) 0 0)
             (simplify (series/take 4 ((D S) 'x)))))
      (is (= '((F x y) (G x y) 0 0) (simp4 (T 'x 'y))))
      (is (= '((((∂ 0) F) x y) (((∂ 0) G) x y) 0 0) (simp4 (((∂ 0) T) 'x 'y))))
      (is (= '((((∂ 1) F) x y) (((∂ 1) G) x y) 0 0) (simp4 (((∂ 1) T) 'x 'y))))
      (is (= '((((∂ 0) W) (up r θ)) (((∂ 0) Z) (up r θ)) 0 0) (simp4 (((∂ 0) U) (up 'r 'θ)))))
      (is (= '((((∂ 1) W) (up r θ)) (((∂ 1) Z) (up r θ)) 0 0) (simp4 (((∂ 1) U) (up 'r 'θ)))))
      (is (= '[(sin t) (cos t) (tan t) 0] (simp4 (V 't))))
      (is (= '[(cos t) (* -1 (sin t)) (/ 1 (expt (cos t) 2)) 0] (simp4 ((D V) 't)))))
    (testing "f -> Series"
      (let [F (fn [k] (series/starting-with (fn [t] (* k t)) (fn [t] (* k k t))))]
        (is (= '((* q z) (* (expt q 2) z) 0 0) (simp4 ((F 'q) 'z))))
        (is (= '(z (* 2 q z) 0 0) (simp4 (((D F) 'q) 'z))))))))

(deftest taylor
  (let [taylor-series-expander (fn [f x h]
                                 (((exp (* h D)) f) x))]
    (is (= '(+ (* 1/24 (expt dx 4) (sin x))
               (* -1/6 (expt dx 3) (cos x))
               (* -1/2 (expt dx 2) (sin x))
               (* dx (cos x))
               (sin x))
           (simplify (reduce + (series/take 5 (taylor-series-expander sin 'x 'dx))))))
    (is (= '(1
             (* 1/2 dx)
             (* -1/8 (expt dx 2))
             (* 1/16 (expt dx 3))
             (* -5/128 (expt dx 4))
             (* 7/256 (expt dx 5)))
           (simplify (series/take 6 (taylor-series-expander #(sqrt (+ 1 %)) 0 'dx)))))))
