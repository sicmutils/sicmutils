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

(def ^:private natural-log (partial i/definite-integral / 1.))

(def ^:private sine (partial i/definite-integral #(Math/cos %) 0.))

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
    ;; Apache commons math doesn't seem to have an integrator handy that works
    ;; well on integrals with a singularity at the edge
    (let [near (v/within 5e-3)
          g 9.8
          integrand (fn [theta0]
                      (fn [theta]
                        (/ (Math/sqrt (* 2 g (- (Math/cos theta) (Math/cos theta0)))))))
          ;; the integrand is improper at the right endpoint: the function is not
          ;; defined there. Delta is the "indent" from the right endpoint used to
          ;; avoid the singularity
          L (fn [epsilon delta a] (* 4 (q/definite-integral
                                        (integrand a)
                                        0 (- a delta)
                                        :epsilon epsilon)))]
      (let [delta 1e-6
            epsilon 1e-9
            f (partial L epsilon delta)]
        (is (near 2.00992 (f 0.15)))
        (is (near 2.01844 (f 0.30)))
        (is (near 2.03279 (f 0.45)))
        (is (near 2.0532 (f 0.60)))
        (is (near 2.08001 (f 0.75)))
        (is (near 2.11368 (f 0.9))))
      ;; experiment with "squeezing" the end of the numerical integration
      ;; interval closer to the singularity on the right. How does that
      ;; affect convergence? We can see that it's much more important than
      ;; the epsilon value of the integrator; we can relax that to 1e-6 and
      ;; yet get more accurate answers by reducing delta to 1e-10. Mathematica
      ;; reports that the correct answer is 2.03279.
      (with-comparator (v/within 1e-6)
        (is (ish? [1.1505131
                   1.7583257
                   1.9461327
                   2.0053914
                   2.0241265
                   2.0300509
                   2.0319244
                   2.0325168
                   2.0327042]
                  (map #(L 1e-6 % 0.45) (map #(Math/pow 10 (- %)) (range 1 10))))))))
  (testing "elliptic integral"
    (let [F (fn [phi k]
              (q/definite-integral #(/ (Math/sqrt (- 1 (* k k (Math/pow (Math/sin %) 2))))) 0 phi
                :relative-accuracy 0.01
                :absolute-accuracy 0.001
                :max-evaluations 1000000))]
      (is (near 0.303652 (F 0.3 (Math/sqrt 0.8))))
      (is (near 1.30567 (F 1.2 (Math/sqrt 0.4)))))))

(deftest elliptic-tests
  (testing "elliptic"
    (is (near 1.30567  (i/elliptic-f 1.2 (Math/sqrt 0.4)))))

  (testing "direct elliptic"
    (is (near 0.200212 (i/elliptic-f 0.2 0.4)))
    (is (near 0.841935 (i/elliptic-f 0.8 0.7)))
    (is (near 0.303652 (i/elliptic-f 0.3 (g/sqrt 0.8))))
    (is (near 0.300712 (i/elliptic-f 0.3 0.4)))
    (is (near 0.738059 (i/elliptic-f 0.7 0.8))))

  (testing "general pendulum periods"
    (let [period (fn [theta_0]
                   (/ (* 8 (i/elliptic-f (/ theta_0 2) (/ (g/sin (/ theta_0 2)))))
                      (* (g/sqrt (* 2 9.8))
                         (g/sqrt (- 1 (g/cos theta_0))))))]
      (is (near 2.009916 (period 0.15)))
      (is (near 2.018438 (period 0.30)))
      (is (near 2.032791 (period 0.45)))
      (is (near 2.053204 (period 0.60)))
      (is (near 2.080013 (period 0.75)))
      (is (near 2.113680 (period 0.90)))
      (is (near 2.154814 (period 1.05)))
      (is (near 2.204206 (period 1.20)))
      (is (near 2.262882 (period 1.35)))
      (is (near 2.332176 (period 1.50)))
      (is (near 2.413836 (period 1.65)))
      (is (near 2.510197 (period 1.80)))
      (is (near 2.624447 (period 1.95))))))


(deftest from-scmutils-defint-interface
  (comment
    ;; Value: -.5772156647120303
    (u/definite-integral
      (fn [x] (* (g/exp (- x)) (g/log x))) 0.0 :+infinity)

    (defn foo [m]
      (q/definite-integral
        (fn [x] (g/expt (g/log (/ 1 x)) n)) 0.0 1.0))
    ;; Value: .9999999998053075
    (foo 1)

    ;; Value: 1.9999999997719113
    (foo 2)

    ;; Value: 5.999999999805274
    (foo 3)

    ;; Value: 23.999999999815316
    (foo 4)

    ;; Value: 119.99999999980271
    (foo 5)

    ;; Value: 719.9999999997759
    (foo 6))

  )
(deftest from-scmutils-quadrature-interface
  (comment
    (define (foo n)
      (define int
        (make-definite-integrator
         (lambda (x) (expt (log (/ 1 x)) n))
         0.0
         1.0
         1e-12))
      (int 'set-method! 'open-closed)
      (int 'integral))

    ;;; Do you recognize the function FOO?
    ;; Value: 1.
    (foo 0)
    ;; Value: .9999999999979357
    (foo 1)


    ;; Value: 1.9999999999979101
    (foo 2)


    ;; Value: 5.99999999999799
    (foo 3)


    ;; Value: 23.999999999997893
    (foo 4)

    ;; Value: 119.99999999999828
    (foo 5))

  (comment
    (define (bar)
      (define int
        (make-definite-integrator
         (lambda (x) (* (exp (- x)) (log x)))
         0.0
         :+infinity
         1e-11))
      (int 'set-method! 'open-open)
      (int 'integral))
    ;;; Do you recognize this constant?

    ;; Euler's Constant
    ;; https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant Value:

    ;; -.5772156648993277
    (bar))

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

    (define witch
      (lambda->numerical-procedure
       '(lambda (x)
                (/ 4.0 (+ 1.0 (* x x))))))

    (define integrator (make-definite-integrator))

    (integrator 'set-method! 'romberg)
    (integrator 'set-error! 1e-12)
    (integrator 'set-integrand! witch)
    (integrator 'set-lower-limit! 0.0)
    (integrator 'set-upper-limit! 1.0)
    (integrator 'integral)
    ;; Value: 3.141592653589793
    ;; Easy as pi.
    )
  )
