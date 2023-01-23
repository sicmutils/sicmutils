#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.boole-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [emmy.abstract.function :as f]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.numerical.quadrature.boole :as qb]
            [emmy.numerical.quadrature.trapezoid :as qt]
            [emmy.numsymb]
            [emmy.simplify :as s :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn boole-step
  "Implements a single step of Boole's method, as laid out in the [Wikipedia
  entry on Newton Cotes
  formulas](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas)."
  [f a b]
  (let [h     (/ (- b a) 4)
        mid   (/ (+ a b) 2)
        l-mid (/ (+ a mid) 2)
        r-mid (/ (+ mid b) 2)]
    (* (/ (* 2 h) 45)
       (+ (* 7 (f a))
          (* 32 (f l-mid))
          (* 12 (f mid))
          (* 32 (f r-mid))
          (* 7 (f b))))))

(deftest boole-tests
  (testing "Boole's Method is equivalent to TWO steps of Richardson
  extrapolation on the Trapezoid method."
    (f/with-literal-functions [f]
      (let [a 'a
            b 'b
            mid   (/ (+ a b) 2)
            l-mid (/ (+ a mid) 2)
            r-mid (/ (+ mid b) 2)
            t1 (qt/single-trapezoid f a b)
            t2 (+ (qt/single-trapezoid f a mid)
                  (qt/single-trapezoid f mid b))
            t4 (+ (qt/single-trapezoid f a l-mid)
                  (qt/single-trapezoid f l-mid mid)
                  (qt/single-trapezoid f mid r-mid)
                  (qt/single-trapezoid f r-mid b))

            ;; Richardson extrapolation step with t=2, p=2,4,6... since the
            ;; error series of the Trapezoid rule = the even naturals.
            richardson-step (fn [p a b]
                              (let [t**p (g/expt 2 p)]
                                (/ (- (* t**p b) a)
                                   (- t**p 1))))]
        (is (v/zero?
             (g/simplify
              (- (richardson-step 4
                                  (richardson-step 2 t1 t2)
                                  (richardson-step 2 t2 t4))
                 (boole-step f a b))))
            "A boole step is equivalent to 2 pairwise Richardson steps with p=2,
            and then a final combining step with p=4 (since we're now cancelling
            a quartic error term)."))))

  (testing "Boole's rule converges, and the interface works properly."
    (let [pi-test (fn [x] (/ 4 (+ 1 (* x x))))]
      (is (ish? {:converged? true
                 :terms-checked 4
                 :result 3.1415926537080376}
                (qb/integral pi-test 0 1)))

      (is (ish? {:converged? false
                 :terms-checked 3
                 :result 3.141592661142563}
                (qb/integral pi-test 0 1 {:maxterms 3}))
          "options get passed through."))))
