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

(ns sicmutils.numerical.integrate-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [sicmutils.numerical.integrate :as i]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(def ^:private near (v/within 1e-6))

#?(:clj
   (def ^:private natural-log (partial i/definite-integral / 1.)))

#?(:clj
   (def ^:private sine (partial i/definite-integral #(Math/cos %) 0.)))

#?(:clj
   (defn bessel-j0 [x]
     (/ (i/definite-integral #(Math/cos (- (* x (Math/sin %)))) 0. Math/PI) Math/PI)))

#?(:clj
   (deftest integrals
     (testing "easy"
       (is (near 0.333333 (i/definite-integral #(* % %) 0. 1.)))
       (is (near 0.5 (i/definite-integral identity 0. 1.)))
       (is (near 3 (i/definite-integral (constantly 1.0) 0. 3.)))
       (is (near 0 (i/definite-integral (constantly 0.0) 0. 1000.)))
       (is (near 1.0 (natural-log (Math/exp 1.))))
       (is (near 0 (sine Math/PI)))
       (is (near 1 (sine (/ Math/PI 2))))
       (is (near 0.7651976 (bessel-j0 1)))
       (is (near -0.2459358 (bessel-j0 10))))
     (testing "harder"
       ;; Apache commons math doesn't seem to have an integrator handy that works
       ;; well on integrals with a singularity at the edge
       (let [sort-of-near (v/within 5e-3)
             integrand (fn [g theta0]
                         (fn [theta]
                           (/ (Math/sqrt (* 2 g (- (Math/cos theta) (Math/cos theta0)))))))
             epsilon 0.000001
             L (fn [a] (* 4 (i/definite-integral (integrand 9.8 a)
                             0 (- a epsilon)
                             :method :legendre-gauss
                             :max-iterations 200
                             :points 64
                             :max-evaluations 500000) ))]
         (is (sort-of-near 2.00992 (L 0.15)))
         (is (sort-of-near 2.01844 (L 0.30)))
         (is (sort-of-near 2.03279 (L 0.45)))
         (is (sort-of-near 2.0532 (L 0.60)))
         (is (sort-of-near 2.08001 (L 0.75)))
         (is (sort-of-near 2.11368 (L 0.9)))))
     (testing "elliptic integral"
       (let [F (fn [phi k]
                 (i/definite-integral #(/ (Math/sqrt (- 1 (* k k (Math/pow (Math/sin %) 2))))) 0 phi
                   :relative-accuracy 0.01
                   :absolute-accuracy 0.001
                   :max-evaluations 1000000))]
         (is (near 0.303652 (F 0.3 (Math/sqrt 0.8))))
         (is (near 1.30567 (F 1.2 (Math/sqrt 0.4))))))))

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
