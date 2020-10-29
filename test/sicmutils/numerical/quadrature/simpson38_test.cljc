;;
;; Copyright © 2020 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.numerical.quadrature.simpson38-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.quadrature.simpson :as qs]
            [sicmutils.numerical.quadrature.simpson38 :as qs38]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.numsymb]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]
            [sicmutils.util.stream :as us]))

(use-fixtures :once hermetic-simplify-fixture)

(defn simpson38-step
  "Implements a single step of Simpson's 3/8 rule, as laid out in the [Wikipedia
  entry on Newton Cotes
  formulas](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas).
  This is equivalent to fitting a cubic to the $a$, $b$ and the two midpoints at
  1/3 and 2/3 across the interval, using the values of $f$ at each point."
  [f a b]
  (let [h (/ (- b a) 3)
        l-interior (/ (+ (* 2 a) b) 3)
        r-interior (/ (+ a (* 2 b)) 3)]
    (* (/ (* 3 h) 8)
       (+ (f a)
          (* 3 (f l-interior))
          (* 3 (f r-interior))
          (f b)))))

(deftest simpson38-tests
  (testing "Simpson's 3/8 Method is equivalent to a single step of Richardson
  extrapolation on the Trapezoid method, if the Trapezoid method refines its
  grid by 3x each step."
    (f/with-literal-functions [f]
      (let [a 'a
            b 'b
            l-interior (/ (+ (* 2 a) b) 3)
            r-interior (/ (+ a (* 2 b)) 3)
            t1 (qt/single-trapezoid f a b)
            t3 (+ (qt/single-trapezoid f a l-interior)
                  (qt/single-trapezoid f l-interior r-interior)
                  (qt/single-trapezoid f r-interior b))

            ;; Richardson extrapolation step with t=3, p=2
            richardson-step (let [t**2 (g/square 3)]
                              (/ (- (* t**2 t3) t1)
                                 (- t**2 1)))]
        (is (zero?
             (g/simplify
              (g/- richardson-step
                   (simpson38-step f a b))))
            "Both methods are equivalent!"))))

  (testing "Simpson's method is identical to the average of the left and
  right riemann sums"
    ;; Reference: The derivation at the [Simpson's Rule Wikipedia
    ;; page](https://en.wikipedia.org/wiki/Simpson%27s_rule#Averaging_the_midpoint_and_the_trapezoidal_rules)
    (let [points (us/powers 2 1)
          f       (fn [x] (/ 4 (+ 1 (* x x))))
          [a b]   [0 1]
          mid-estimates  (qm/midpoint-sequence f a b {:n points})
          trap-estimates (qt/trapezoid-sequence f a b {:n points})]
      (ish? (take 10 (qs/simpson-sequence f a b))
            (take 10 (map (fn [mid trap]
                            (/ (+ (* 2 mid) trap)
                               3))
                          mid-estimates
                          trap-estimates)))))

  (testing "Simpson's Method converges, and the interface works properly."
    (let [pi-test (fn [x] (/ 4 (+ 1 (* x x))))]
      (is (ish? {:converged? true
                 :terms-checked 13
                 :result 3.141592643655686}
                (qt/integral pi-test 0 1)))

      (is (ish? {:converged? false
                 :terms-checked 4
                 :result 3.138988494491089}
                (qt/integral pi-test 0 1 {:maxterms 4}))
          "options get passed through."))))
