#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.simpson38-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [emmy.abstract.function :as f]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.numerical.quadrature.simpson38 :as qs38]
            [emmy.numerical.quadrature.trapezoid :as qt]
            [emmy.numsymb]
            [emmy.simplify :as s :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

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
        (is (v/zero?
             (g/simplify
              (g/- richardson-step
                   (simpson38-step f a b))))
            "Both methods are equivalent!"))))

  (testing "Simpson's 3/8 Method converges, and the interface works properly."
    (let [pi-test (fn [x] (/ 4 (+ 1 (* x x))))]
      (is (ish? {:converged? true
                 :terms-checked 4
                 :result 3.141592653589161}
                (qs38/integral pi-test 0 1)))

      (is (ish? {:converged? false
                 :terms-checked 3
                 :result 3.141592653128903}
                (qs38/integral pi-test 0 1 {:maxterms 3}))
          "options get passed through."))))
