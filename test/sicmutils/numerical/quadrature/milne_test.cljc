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

(ns sicmutils.numerical.quadrature.milne-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [sicmutils.numerical.quadrature.boole :as qb]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.numsymb]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(defn milne-step
  "Implements a single step of Boole's method, as laid out in the [Wikipedia
  entry on Newton Cotes
  formulas](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas).
  This is equivalent to fitting a quadratic to the $a$, $b$ and the midpoint
  using the values of $f$ at each point."
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

(deftest milne-tests
  (testing "Milne's Method is equivalent to TWO steps of Richardson
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
        (is (zero?
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
