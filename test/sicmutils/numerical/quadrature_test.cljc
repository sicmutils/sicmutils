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

(ns sicmutils.numerical.quadrature-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.numerical.quadrature :as q]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [same :refer [ish? zeroish? with-comparator]
             #?@(:cljs [:include-macros true])]))

(def ^:private near (v/within 1e-6))

(def ^:private natural-log (partial q/definite-integral / 1.))

(def ^:private sine (partial q/definite-integral #(Math/cos %) 0.))

(defn bessel-j0 [x]
  (/ (q/definite-integral #(Math/cos (- (* x (Math/sin %)))) 0. Math/PI) Math/PI))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (q/definite-integral #(* % %) 0. 1.)))
    (is (near 0.5 (q/definite-integral identity 0. 1.)))
    (is (near 3 (q/definite-integral (constantly 1.0) 0. 3.)))
    (is (near 0 (q/definite-integral (constantly 0.0) 0. 1000.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 0 (sine Math/PI)))
    (is (near 1 (sine (/ Math/PI 2))))
    (is (near 0.7651976 (bessel-j0 1)))
    (is (near -0.2459358 (bessel-j0 10))))

  (testing "harder"
    (let [near (v/within 5e-3)
          g 9.8
          integrand (fn [theta0]
                      (fn [theta]
                        (/ (Math/sqrt (* 2 g (- (Math/cos theta)
                                                (Math/cos theta0)))))))
          L (fn [epsilon a] (* 4 (q/definite-integral
                                  (integrand a)
                                  0
                                  a
                                  {:tolerance epsilon})))]
      (let [epsilon 1e-9
            f (partial L epsilon)]
        (is (near 2.00992 (f 0.15)))
        (is (near 2.01844 (f 0.30)))
        (is (near 2.03279 (f 0.45)))
        (is (near 2.0532 (f 0.60)))
        (is (near 2.08001 (f 0.75)))
        (is (near 2.11368 (f 0.9))))))

  (testing "elliptic integral"
    (let [F (fn [phi k]
              (q/definite-integral #(/ (Math/sqrt (- 1 (* k k (Math/pow (Math/sin %) 2))))) 0 phi
                {:relative-accuracy 0.01
                 :absolute-accuracy 0.001
                 :max-evaluations 1000000}))]
      (is (near 0.303652 (F 0.3 (Math/sqrt 0.8))))
      (is (near 1.30567 (F 1.2 (Math/sqrt 0.4)))))))

(deftest from-scmutils
  (testing "Factorial"
    (letfn [(foo [n]
              (let [f (fn [x] (Math/pow (Math/log (/ 1 x)) n))
                    opts {:method :open-closed
                          :tolerance 1e-15}]
                (q/definite-integral f 0 1 opts)))]
      (is (ish? 1 (foo 0)))
      (is (ish? 1 (foo 1)))
      (is (ish? 2 (foo 2)))
      (is (ish? 6 (foo 3)))
      (is (ish? 24 (foo 4)))
      (is (ish? 120 (foo 5)))))

  (testing "Euler's constant"
    ;; https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant
    (let [f (fn [x] (* (Math/log x) (Math/exp (- x))))]
      (is (ish? {:converged? true
                 :result -0.5772156649015159}
                (q/definite-integral f 0 ##Inf
                  {:info? true
                   :tolerance 1e-14})))))

  (let [f (fn [x] (* x (Math/exp (- (* x x)))))]
    (is (zeroish?
         (+ (q/definite-integral f ##-Inf 0)
            (q/definite-integral f 0 ##Inf)))))

  (comment
    ;; SHOULD be pi but didn't work in their system.
    (* 2
       ((make-definite-integrator
         (lambda->numerical-procedure
          '(lambda (x) (/ (sin x) x)))
         0.0
         :+infinity
         .01)
        'integral))

    (defn witch [x]
      (/ 4.0 (+ 1.0 (* x x))))

    (def integrator (q/make-definite-integrator))

    (integrator 'set-method! 'romberg)
    (integrator 'set-error! 1e-12)
    (integrator 'set-integrand! witch)
    (integrator 'set-lower-limit! 0.0)
    (integrator 'set-upper-limit! 1.0)
    (integrator 'integral)
    ;; Value: 3.141592653589793
    ;; Easy as pi.
    ))
