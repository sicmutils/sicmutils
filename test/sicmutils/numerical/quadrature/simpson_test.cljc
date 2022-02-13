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

(ns sicmutils.numerical.quadrature.simpson-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [sicmutils.abstract.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.quadrature.simpson :as qs]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.numsymb]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]
            [sicmutils.util.stream :as us]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn simpson-step
  "Implements a single step of Simpson's method, as laid out in the [Wikipedia
  entry on Newton Cotes
  formulas](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas).
  This is equivalent to fitting a quadratic to the $a$, $b$ and the midpoint
  using the values of $f$ at each point."
  [f a b]
  (let [h (/ (- b a) 2)
        midpoint (/ (+ a b) 2)]
    (* (/ h 3)
       (+ (f a)
          (* 4 (f midpoint))
          (f b)))))

(deftest simpson-tests
  (testing "Simpson's Method is equivalent to a single step of Richardson
  extrapolation on the Trapezoid method."
    (f/with-literal-functions [f]
      (let [midpoint (/ (+ 'a 'b) 2)
            t1 (qt/single-trapezoid f 'a 'b)
            t2 (+ (qt/single-trapezoid f 'a midpoint)
                  (qt/single-trapezoid f midpoint 'b))

            ;; Richardson extrapolation step with t=2, p=2
            richardson-step (let [t**2 (g/square 2)]
                              (/ (- (* t**2 t2) t1)
                                 (- t**2 1)))]
        (is (v/zero?
             (g/simplify
              (- richardson-step
                 (simpson-step f 'a 'b))))
            "Both methods are equivalent."))))

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
                 :terms-checked 5
                 :result 3.1415926535528365}
                (qs/integral pi-test 0 1)))

      (is (ish? {:converged? false
                 :terms-checked 4
                 :result 3.1415926512248222}
                (qs/integral pi-test 0 1 {:maxterms 4}))
          "options get passed through."))))
